#!/usr/bin/env python3
# -*- coding: UTF-8 -*-
import argparse
import copy
import datetime
import getopt
import json
import logging
import os
import re
import sqlite3
import subprocess as sp
import sys
import shutil
from shutil import which
from typing import List
from _ctypes import PyObj_FromPtr


class NoIndent(object):
    """ Value wrapper. """

    def __init__(self, value):
        self.value = value


class MyEncoder(json.JSONEncoder):
    FORMAT_SPEC = '@@{}@@'
    regex = re.compile(FORMAT_SPEC.format(r'(\d+)'))

    def __init__(self, **kwargs):
        # Save copy of any keyword argument values needed for use here.
        self.__sort_keys = kwargs.get('sort_keys', None)
        super(MyEncoder, self).__init__(**kwargs)

    def default(self, obj):
        return (self.FORMAT_SPEC.format(id(obj)) if isinstance(obj, NoIndent)
                else super(MyEncoder, self).default(obj))

    def encode(self, obj):
        format_spec = self.FORMAT_SPEC # Local var to expedite access.
        json_repr = super(MyEncoder, self).encode(obj) # Default JSON.

        # Replace any marked-up object ids in the JSON repr with the
        # value returned from the json.dumps() of the corresponding
        # wrapped Python object.
        for match in self.regex.finditer(json_repr):
            # see https://stackoverflow.com/a/15012814/355230
            id = int(match.group(1))
            no_indent = PyObj_FromPtr(id)
            json_obj_repr = json.dumps(no_indent.value,
                                       sort_keys=self.__sort_keys)

            # Replace the matched id string with json formatted representation
            # of the corresponding Python object.
            json_repr = json_repr.replace(
                '"{}"'.format(format_spec.format(id)), json_obj_repr)

        return json_repr


def write_bug_report_part(output_file_handler, bug_info_dict, trace_id=None):
    # write header
    output_file_handler.write("# Bug Report\n")

    # write bug report body
    output_file_handler.write("```json\n")
    trace_info = []
    if trace_id is None:
        trace_key = "Trace"
    else:
        trace_key = "Trace_" + str(int(trace_id))
    for each_t in bug_info_dict[trace_key]:
        trace_info.append(NoIndent(each_t))
    # deep copy the bug report and remove all the redundant traces
    bug_info_dict_copy = copy.deepcopy(bug_info_dict)
    result_dict = {
        k: v
        for k, v in bug_info_dict_copy.items()
        if not re.fullmatch(r'Trace*', k)
    }
    result_dict["Trace_" + str(trace_id)] = trace_info
    output_file_handler.write(
        json.dumps(result_dict, sort_keys=False, indent=4, cls=MyEncoder))

    # write terminator
    output_file_handler.write("\n```\n\n")
    return


def write_code_snippets(output_file_handler, code_common_part,
                        code_caller_part, language):
    # write header
    output_file_handler.write("# Code Snippet\n")

    # write code snippet body
    output_file_handler.write("```" + language.upper() + "\n")

    # write the common part
    for snippet in code_common_part:
        output_file_handler.writelines(snippet)
    if code_caller_part is not None:
        # write the caller part
        for caller_path in code_caller_part:
            for snippet in caller_path:
                output_file_handler.writelines(snippet)

    # write terminator
    output_file_handler.write("```\n")


def generate_code_snippets_files(code_snippets_common_part,
                                 code_snippets_caller_part, bug_info_dict,
                                 output_file_prefix, language, trace_id):
    """generate_code_snippets_files(code_snippets_common_part, code_snippets_caller_part, bug_info_dict, output_file_prefix, trace_id):
    This function is used to contract the caller parts and common parts of extracted code snippets and write them into
    a file. The format of output file name is 'output_file_prefix_Trace_{%d}_snippets_{%d}.txt'.

    Note: If the caller part is empty (i.e., the option '-m' is set to be 0), only the common part will be export.
    """
    if os.path.exists(output_file_prefix):
        shutil.rmtree(output_file_prefix)
    os.makedirs(output_file_prefix)

    code_snippet_idx = 1
    # if the caller is not empty, we write the both parts.
    if code_snippets_caller_part:
        for caller_part in code_snippets_caller_part:
            # write the two parts.
            output_file = output_file_prefix + "/Trace_" + str(
                trace_id) + "_snippets_" + str(code_snippet_idx) + ".txt"
            with open(output_file, "w") as f:
                # write bug report
                write_bug_report_part(f, bug_info_dict, trace_id)

                # write the two parts
                write_code_snippets(f, code_snippets_common_part, caller_part,
                                    language)
            code_snippet_idx = code_snippet_idx + 1
    else:
        # only write the common part
        output_file = output_file_prefix + "/Trace_" + str(
            trace_id) + "_snippets_" + str(code_snippet_idx) + ".txt"
        with open(output_file, "w") as f:
            # write bug report part
            write_bug_report_part(f, bug_info_dict, trace_id)

            # write the common part
            write_code_snippets(f, code_snippets_common_part, None, language)


def process_sql_searched_list(results, ctags_line_info, search_type="func"):
    result_list = []
    for result in results.fetchall():
        symbol_name = result[0]
        line = result[1]
        file = result[2] if result[2].startswith('/') else os.path.join(
            './', result[2])
        # get the function/macro information by ctags file.
        symbol_info = search_by_ctags(file, line, ctags_line_info, search_type)
        if len(symbol_info) <= 0:
            # print()
            logging.warning(
                f"Can't find the function/macro '{symbol_name}' at line {line} in file '{file}'."
            )
        else:
            result_list.append(symbol_info)
    return result_list


def get_caller_func_list(cq_db_con, search_func_info, ctags_line_info):
    """get_caller_func_list(cq_db_con, search_func_info, ctags_line_info):
    This function is used to obtain the caller list of the given function by using the information
    extracted from ctags file. The output list may contain several function dicts.
    """
    # query the sqlite3 database generated by 'cqmakedb' command to obtain the caller function
    callee_func = search_func_info['function']
    search_caller_sql = "SELECT symtbl.symName,linestbl.linenum,filestbl.filePath FROM symtbl INNER JOIN linestbl ON " \
                        "symtbl.lineID=linestbl.lineID AND symtbl.symID IN (SELECT callerID FROM calltbl WHERE " \
                        "calledID IN (SELECT symID FROM symtbl WHERE symName=?)) INNER JOIN filestbl ON " \
                        "(linestbl.fileID=filestbl.fileID);"
    res = cq_db_con.execute(search_caller_sql, (callee_func, ))
    # process the search results
    caller_func_list = process_sql_searched_list(res, ctags_line_info, "func")

    return caller_func_list


def get_current_called_func_info(cq_db_con, search_func_info, ctags_line_info,
                                 call_loc):
    """get_current_called_func_info(cq_db_con, search_func_info, ctags_line_info, call_loc):
    This function is used to obtain the callee list called at specific location in a function by using the information
    extracted from ctags file. The output list may contain several function dicts.
    """
    # query the sqlite3 database generated by 'cqmakedb' command to obtain the callee function
    caller_func = search_func_info['function']
    # remove the prefix './' of a filename.
    cur_func_file = search_func_info['file']
    caller_file = "%" + (cur_func_file if not cur_func_file.startswith('./')
                         else cur_func_file.replace('./', '', 1))
    local_search_callee_sql = "select symtbl.symName,linestbl.linenum,filestbl.filePath from symtbl inner join linestbl " \
                              "inner join filestbl where symtbl.symType='$' and symtbl.lineID=linestbl.lineID and " \
                              "linestbl.fileID=filestbl.fileID and filestbl.filePath like ? and symtbl.symName in " \
                              "(select symtbl.symName from symtbl where symtbl.symID in (select calledID from calltbl " \
                              "where callerID in (select symtbl.symID from symtbl where symtbl.symName=?) and calledID " \
                              "in (select symtbl.symID from symtbl inner join linestbl where symtbl.lineID=linestbl.lineID " \
                              "and linestbl.linenum=? and linestbl.fileID in (select filestbl.fileID from filestbl " \
                              "where filestbl.filePath like ?))));"
    local_search_res = cq_db_con.execute(local_search_callee_sql, (
        caller_file,
        caller_func,
        call_loc,
        caller_file,
    ))
    # process the search results
    callee_func_list = process_sql_searched_list(local_search_res,
                                                 ctags_line_info, "func")

    if len(callee_func_list) == 0:
        # cannot find the definition of called in the caller file, we need to search it globally.
        global_search_callee_sql = "select symtbl.symName,linestbl.linenum,filestbl.filePath from symtbl inner join " \
                                   "linestbl inner join filestbl where symtbl.symType='$' and symtbl.lineID=linestbl.lineID " \
                                   "and linestbl.fileID=filestbl.fileID and symtbl.symName in (select symtbl.symName " \
                                   "from symtbl where symtbl.symID in (select calledID from calltbl where callerID in " \
                                   "(select symtbl.symID from symtbl where symtbl.symName=?) and calledID in " \
                                   "(select symtbl.symID from symtbl inner join linestbl where " \
                                   "symtbl.lineID=linestbl.lineID and linestbl.linenum=? and linestbl.fileID in " \
                                   "(select filestbl.fileID from filestbl where filestbl.filePath like ?))))"
        global_search_res = cq_db_con.execute(global_search_callee_sql, (
            caller_func,
            call_loc,
            caller_file,
        ))
        # process the search results.
        callee_func_list = process_sql_searched_list(global_search_res,
                                                     ctags_line_info, "func")
        if len(callee_func_list) > 1:
            logging.warning(
                f"cannot determine which file the function '{callee_func_list[0]['function']}' belongs to."
            )
            return []
            sys.exit(2)

    return callee_func_list


def get_current_macro_info(cq_db_con, search_macro_info, ctags_line_info,
                           macro_loc):
    """get_current_macro_info(cq_db_con, search_macro_info, ctags_line_info, macro_loc):
    This function is used to obtain the macro list used at specific location in a function by using the information
    extracted from ctags file. The output list may contain several macro dicts.
    """
    # query the sqlite3 database generated by 'cqmakedb' command to obtain the callee function
    # remove the prefix './' of a filename.
    cur_macro_file = search_macro_info['file']
    caller_file = "%" + (cur_macro_file if not cur_macro_file.startswith('./')
                         else cur_macro_file.replace('./', '', 1))

    local_search_macro_sql = "select symtbl.symName,linestbl.linenum,filestbl.filePath from symtbl inner join linestbl " \
                             "inner join filestbl where symtbl.lineID=linestbl.lineID and linestbl.fileID=filestbl.fileID " \
                             "and symtbl.symType='#' and symtbl.symName in (select symtbl.symName from symtbl inner " \
                             "join linestbl on symtbl.lineID=linestbl.lineID and linestbl.lineID in (select " \
                             "linestbl.lineID from linestbl where linestbl.linenum=? and fileID in (select " \
                             "filestbl.fileID from filestbl where filestbl.filePath like ?)))"
    local_search_res = cq_db_con.execute(local_search_macro_sql, (
        macro_loc,
        caller_file,
    ))

    # process the search results.
    used_macro_list = process_sql_searched_list(local_search_res,
                                                ctags_line_info, "macro")

    if len(used_macro_list) > 1:
        macro_set, repeated_macro_list = set(), []
        for macro in used_macro_list:
            macro_set.add(macro["function"])
            if macro["function"] in macro_set:
                repeated_macro_list.append(macro)
        # there are multiple macros with the same name.
        if len(used_macro_list) != len(macro_set):
            logging.warning(
                f"the files to which the following macros belongs cannot be determined: {repeated_macro_list}"
            )

    return used_macro_list


def dfs_extract_caller_code_snippet(project_folder, cq_db_con, ctags_line_info,
                                    max_search_level, current_search_level,
                                    search_func_info, cur_snippets_stack,
                                    all_code_snippets):
    """dfs_extract_caller_code_snippet(project_folder, cq_db_con, ctags_line_info, max_search_level,
        current_search_level,search_func_info, cur_snippets_stack, all_code_snippets):
    This function recursively obtains the caller code snippets in DFS manner. If the current search level reaches
    the preset maximum level or no caller function can be found, then we store the caller trace to the result list.
    """
    if current_search_level <= max_search_level:
        # obtain the caller of the search function
        caller_func_list = get_caller_func_list(cq_db_con, search_func_info,
                                                ctags_line_info)

        # the current search function has caller
        if caller_func_list:
            for func in caller_func_list:
                func_code_snippet = extract_func_snippet(project_folder, func)
                cur_snippets_stack.append(func_code_snippet)
                # obtain the code snippet of the caller function
                dfs_extract_caller_code_snippet(project_folder, cq_db_con,
                                                ctags_line_info,
                                                max_search_level,
                                                current_search_level + 1, func,
                                                cur_snippets_stack,
                                                all_code_snippets)
                cur_snippets_stack.pop()
        else:
            # the current search function has no caller, we need to store the caller trace to the result list
            all_code_snippets.append(cur_snippets_stack.copy())
    else:
        # the maximum search level is reached, store the caller trace to the result list
        all_code_snippets.append(cur_snippets_stack.copy())


def extract_func_snippet(project_folder,
                         func_info,
                         add_line_num=True,
                         extract_all=False):
    """extract_func_snippet(project_folder, func_info):
    Obtain the code snippet of the given function. This function also checks whether the corresponding file exist.
    """
    # obtain the information of given function
    func_file_path = os.path.join(project_folder, func_info['file'])
    func_name = func_info['function']
    start = int(func_info['line'])
    end = int(func_info['end'])

    # comment for the function.
    if extract_all == True:
        code_snippet = ["    // " + func_info['file'] + "\n"]
    else:
        code_snippet = [
            "    // " + func_info['file'] + ", line: " + str(start) + "-" +
            str(end) + "\n"
        ]
    # check if the corresponding file exist
    if os.path.exists(func_file_path):
        with open(func_file_path, 'r') as f:
            lines = f.readlines()
            if extract_all == True:
                start = 1
                end = len(lines)
            if add_line_num:
                CodeLines = []
                for i in range(start - 1, end):
                    if (start + end) / 2 < 100:
                        CodeLines.append("{:<2d} ".format(i + 1) + lines[i])
                    elif (start + end) / 2 < 1000:
                        CodeLines.append("{:<3d} ".format(i + 1) + lines[i])
                    elif (start + end) / 2 < 10000:
                        CodeLines.append("{:<4d} ".format(i + 1) + lines[i])
                    else:
                        CodeLines.append("{:<5d} ".format(i + 1) + lines[i])
                return code_snippet + CodeLines + ["\n"]
            else:
                return code_snippet + lines[start - 1:end] + ["\n"]

    # the file corresponding to this function cannot be found.
    logging.error(
        "cannot find the file '%s' to extract code snippet of function '%s'" %
        (func_info, func_name))
    exit(5)


def search_by_ctags(file, line, ctags_line_info, search_type="method"):
    if file in ctags_line_info:
        # iterate all the tags
        for tag in ctags_line_info[file]:
            # if search_type == "method":
            if search_type == "method":
                try:
                    if tag['kind'] == 'method' and tag['line'] <= line <= tag['end']:
                        return {
                            "file": tag['path'],
                            "function": tag['name'],
                            "line": tag['line'],
                            "end": tag['end']
                        }
                    elif tag['line'] <= line <= tag['end']:
                        return {
                            "file": tag['path'],
                            "function": tag['name'],
                            "line": tag['line'],
                            "end": tag['line']
                        }    
                except:
                    continue

    # cannot find the function according to the given information
    return []


def extract_code_snippet_by_trace(project_folder, cq_db_con, ctags_line_info,
                                  bug_info_dict, max_search_level,
                                  output_file_prefix, language, trace_id,
                                  trace):
    """extract_code_snippet_by_trace(project_folder, cq_db_con, ctags_line_info,
                                  max_search_level, output_file_prefix, trace_id, trace):
    This function extract code snippets according to the trace in a bug report file.
    """
    step_func_info_list = []

    # iterate all the steps in given trace to generate (file, func) pair list
    # each (file, func) pair represents the backtrack point in given trace
    for step in trace[:]:
        step_file = step['filename'] if step['filename'].startswith(
            '/') else os.path.join('./', step['filename'])
        step_file_path = os.path.join(project_folder, step['filename'])
        # check if the project file exists
        if os.path.exists(step_file_path):
            step_line = int(step['line_number'])

            # search the function by line
            step_func_info = search_by_ctags(step_file, step_line,
                                             ctags_line_info, "method")
            if len(step_func_info) <= 0:
                # the function of current step cannot be found. strictly, we should not go further. however, in order to
                # obtain as many functions as possible to make the extracted code snippets more precise, we need to skip
                # this abnormal case.
                logging.warning(f"Can't find the step function at line {step_line} in file '{step_file_path}'")
                continue

            # search the called functions by line
            cur_called_func_list = get_current_called_func_info(
                cq_db_con, step_func_info, ctags_line_info, step_line)
            # search the macro by line
            # cur_used_macro_list = get_current_macro_info(
            #     cq_db_con, step_func_info, ctags_line_info, step_line)

            # append the (file, func) pair to the step list
            if step_func_info not in step_func_info_list:
                step_func_info_list.append(step_func_info)
            # add the called function if it exists
            if len(cur_called_func_list) > 0:
                # put the called functions at the front of this list.
                for called_func in cur_called_func_list:
                    if called_func not in step_func_info_list:
                        step_func_info_list = [called_func
                                               ] + step_func_info_list
            # add the used macro if it exists
            # if len(cur_used_macro_list) > 0:
            #     for used_macro in cur_used_macro_list:
            #         if used_macro not in step_func_info_list:
            #             step_func_info_list = [used_macro
            #                                    ] + step_func_info_list
        else:
            logging.error(
                f"[Trace_{trace_id}] the file '{step_file}' does not exist")
            exit(5)

    # extract the code snippet, it contains two parts:
    # 1. the common part constructed by the function info list
    # 2. the caller part of the last function of the function info list

    # step 1: extract the code snippets of the common part
    code_snippets_common_part = []
    for ff_pair in step_func_info_list:
        code_snippets_common_part.append(
            extract_func_snippet(project_folder, ff_pair))

    # step 2: extract the code snippets of the caller part of the last function
    search_func_info = step_func_info_list[-1]
    code_snippets_caller_part = []
    # we do not generate caller part if the max search level is less than 1
    if max_search_level >= 1:
        dfs_extract_caller_code_snippet(project_folder, cq_db_con,
                                        ctags_line_info, max_search_level, 1,
                                        search_func_info, [],
                                        code_snippets_caller_part)

    # get file list
    # bug_file_set = set()
    # bug_file_set.add(bug_info_dict['file'])
    # for eachinfo in bug_info_dict:
    #     if 'Trace' in eachinfo:
    #         for eacht in bug_info_dict[eachinfo]:
    #             bug_file_set.add(eacht['filename'])
    #
    # # if only one file in the bug report, and the number of lines are less than 300. (wcventure)
    # if len(bug_file_set) == 1:
    #     # bug_file_set set to list
    #     bug_file_set = list(bug_file_set)
    #     func_file_path = os.path.join(project_folder, list(bug_file_set)[0])
    #     # count the number of lines of func_file_path
    #     with open(func_file_path, 'r') as f:
    #         lines = f.readlines()
    #         if len(lines) < 300:
    #             # extract all the lines of the file
    #             code_snippets_caller_part = []
    #             code_snippets_common_part = []
    #             code_snippets_common_part.append(extract_func_snippet(project_folder, search_func_info, True, True))
    # End (wcventure)

    # build full code snippets
    generate_code_snippets_files(code_snippets_common_part,
                                 code_snippets_caller_part, bug_info_dict,
                                 output_file_prefix, language, trace_id)


def extract_code_snippets(project_folder,
                          cq_db_con,
                          ctags_line_info,
                          bug_info_dict,
                          output_file_prefix,
                          language,
                          max_search_level=0):
    """extract_code_snippets(project_folder, cq_db_con, ctags_line_info, bug_info_dict,
                          output_file_prefix, max_search_level=0):
    For each trace in a bug report file, this function extracts and preserves several code snippets.
    """
    # iterate all available traces
    # if the trace does not in bug_info_dict
    if "Trace" in bug_info_dict:
        # extract code snippet according to the trace
        extract_code_snippet_by_trace(project_folder, cq_db_con,
                                      ctags_line_info, bug_info_dict,
                                      max_search_level, output_file_prefix,
                                      language, None, bug_info_dict["Trace"])
    else:
        for trace_id in range(0, 10):
            # if the trace does not in bug_info_dict
            if "Trace_" + str(trace_id) not in bug_info_dict:
                continue
            # extract code snippet according to the trace
            extract_code_snippet_by_trace(
                project_folder, cq_db_con, ctags_line_info, bug_info_dict,
                max_search_level, output_file_prefix, language, trace_id,
                bug_info_dict["Trace_" + str(trace_id)])


def get_bug_report(bug_report_file_path):
    # load the json report file into a dictionary
    # read the JSON report file and convert it into a Python dictionary
    with open(bug_report_file_path, "r") as bug_report_file_handle:
        return json.load(bug_report_file_handle)


def get_ctags_info(ctags_file_path):
    # create an empty list to store the information from the file
    line_info_dict = {}

    with open(ctags_file_path, 'r') as ctags_file_handler:
        # read each line in the file
        for line in ctags_file_handler.readlines():
            # load lines and ignore pseudo-tags
            line_data = json.loads(line)
            if line_data['_type'] == 'ptag':
                continue
            # add the line info to the list
            if line_data['path'] not in line_info_dict:
                line_info_dict[line_data['path']] = [line_data]
            else:
                line_info_dict[line_data['path']].append(line_data)

    return line_info_dict


def create_codequery_db(project_folder, codequery_db_file, analysis_language):
    """create_codequery_db(project_folder, codequery_db_file)

    Create CodeQuery database for C/C++ language.
    This function generates four files ('cscope.files', 'cscope.out', codequery_db_file, 'tags') in the project
    folder by the following steps:
    1. change the working directory to the project folder;
    2. remove old files;
    3. create the four files (e.g. for C/C++ language):
        3.1 create a 'cscope.files' file with all the C/C++ source files listed in it:
            find . -iname "*.c"    > ./cscope.files
            find . -iname "*.cpp" >> ./cscope.files
            find . -iname "*.cxx" >> ./cscope.files
            find . -iname "*.cc " >> ./cscope.files
            find . -iname "*.h"   >> ./cscope.files
            find . -iname "*.hpp" >> ./cscope.files
            find . -iname "*.hxx" >> ./cscope.files
            find . -iname "*.hh " >> ./cscope.files
        3.2 create a cscope database:
            cscope -cb;
        3.3 create a ctags database:
            ctags --fields=+ine -n -L ./cscope.files --output-format=json -o ./tags;
        3.4 run cqmakedb to create a CodeQuery database out of the cscope and ctags databases:
            cqmakedb -s codequery_db_file -c ./cscope.out -t ./tags -p.
    4. change the working directory back.
    """
    # check analysis language
    if analysis_language not in [
            'c,c++', 'java', 'python', 'ruby', 'go', 'javascript'
    ]:
        logging.error(
            'the given analysis language "%s" is not currently supported' %
            analysis_language)
        exit(4)

    # preserve current working directory
    current_working_dir = os.getcwd()

    try:
        # change working directory to delete and create temporary files.
        os.chdir(project_folder)

        # delete old files
        for f in list(
            {"cscope.files", "cscope.out", "tags", codequery_db_file}):
            if os.path.exists(f):
                os.remove(f)

        # create cscope.files
        create_scope_files_cmd = ''
        if analysis_language == 'c,c++':
            create_scope_files_cmd = 'find . -iname "*.c" > cscope.files && ' + \
                                     'find . -iname "*.cpp" >> cscope.files && ' + \
                                     'find . -iname "*.cxx" >> cscope.files && ' + \
                                     'find . -iname "*.cc" >> cscope.files && ' + \
                                     'find . -iname "*.h" >> cscope.files && ' + \
                                     'find . -iname "*.hpp" >> cscope.files && ' + \
                                     'find . -iname "*.hxx" >> cscope.files && ' + \
                                     'find . -iname "*.hh" >> cscope.files'
        elif analysis_language == 'java':
            create_scope_files_cmd = 'find . -iname "*.java" > cscope.files'
        elif analysis_language == 'python':
            create_scope_files_cmd = 'find . -iname "*.py" > cscope.files'
        elif analysis_language in ['ruby', 'go', 'javascript']:
            create_scope_files_cmd = 'find . -iname "*.rb" > cscope.files && ' + \
                                     'find . -iname "*.go" >> cscope.files && ' + \
                                     'find . -iname "*.js" >> cscope.files'
        sp.run([create_scope_files_cmd], check=True, shell=True)

        # create cscope database
        if analysis_language in ['c,c++', 'java']:
            if which('cscope') is None:
                print(
                    "Error: command 'cscope' does not exists, please install it."
                )
                sys.exit(3)
            sp.run(['cscope -cb'], check=True, shell=True)
        elif analysis_language in ['python']:
            if which('pycscope') is None:
                print(
                    "Error: command 'pycscope' does not exists, please install it."
                )
                sys.exit(3)
            sp.run(['pycscope -i cscope.files'], check=True, shell=True)
        elif analysis_language in ['ruby', 'go', 'javascript']:
            if which('startcope') is None:
                print(
                    "Error: command 'startcope' does not exists, please install it."
                )
                sys.exit(3)
            sp.run(['startcope -e cscope'], check=True, shell=True)

        # create ctags database
        sp.run([
            'ctags --fields=+ine -n -L ./cscope.files --output-format=json -o ./tags'
        ],
               check=True,
               shell=True)

        # create CodeQuery database
        sp.run([
            'cqmakedb -s ' + codequery_db_file +
            ' -c ./cscope.out -t ./tags -p'
        ],
               check=True,
               shell=True)

        # change the working directory back
        os.chdir(current_working_dir)

        # check the generated codequery database
        if not os.path.exists(os.path.join(project_folder, codequery_db_file)):
            logging.error(
                "failed to create CodeQuery database for language '%s'" %
                analysis_language)
            exit(4)
    except sp.CalledProcessError as err:
        logging.error('%s' % err)
        exit(4)


def determine_code_query_info(project_folder,
                              codequery_db_file,
                              analysis_language="c,c++"):
    """determine_codequery_database(project_folder, codequery_db_file, analysis_language)

    Check whether the codequery database of given project exists. If not, this function
    create a database for it.

    Arguments:
        project_folder: the root folder of the project
        codequery_db_file: the database generated by the command 'cqmakedb' (sqlite3 format).
        analysis_language: the programming language that needs to be analyzed in the project,
            currently supported languages are: C/C++(c,c++), Java(java), Python(python), Ruby(rb),
            Go(go), JavaScript(js)
    Notes: detailed database generation steps can be found at:
        https://github.com/ruben2020/codequery/blob/master/doc/HOWTO-LINUX.md
    """

    # whether the codequery database exists
    codequery_db_file_path = os.path.join(project_folder, codequery_db_file)
    if not os.path.exists(codequery_db_file_path):
        logging.info('The CodeQuery database does not exist, creating ...')
        # check if relevant commands exist
        if which('ctags') is None:
            print("Error: command 'ctags' does not exists, please install it.")
            sys.exit(3)
        if which('cqmakedb') is None:
            print(
                "Error: command 'cqmakedb' does not exists, please install it."
            )
            sys.exit(3)

        # create the database
        create_codequery_db(project_folder, codequery_db_file,
                            analysis_language)

    return get_ctags_info(os.path.join(project_folder, 'tags'))


def print_help(argv: List[str], print_description=False) -> None:
    if print_description:
        print(
            'NAME\n\t%s - extract code snippets based on the given bug report'
            % argv[0])

    print('USAGE\n\t%s [OPTION]...' % argv[0])
    print('OPTIONS')
    print('\t-f, --project-folder:\n\t\t the root folder of a project')
    print(
        '\t-d, --codequery-database:\n\t\t the project codequery database file (it will'
        ' be automatically created if not exist)')
    print('\t-r, --report:\n\t\t the bug report (json format)')
    print(
        '\t-o, --output-prefix:\n\t\t the prefix of output code snippets. the output file format is '
        '"output_file_prefix_Trace_{%d}_snippets_{%d}.txt"')
    print(
        '\t-m, --max-level:\n\t\t the maximum search level of caller function (default: 0)'
    )
    print(
        '\t-l, --analysis-language:\n\t\t the analysis language for the project (default: c,c++)'
    )
    print('EXAMPLE')
    print(
        '\t%s -f /path/to/project -d project.db -r /path/to/bug_report.json -o "UVA_code_snippet" '
        '-l "c,c++" -m 1' % argv[0])


def parse_args(argv: List[str]) -> argparse.Namespace:
    # parse the command-line arguments using the 'getopt' module.
    try:
        opts, args = getopt.getopt(argv[1:], "hf:d:r:o:l:m:", [
            "help", "project=", "database", "report=", "output=", "level",
            "language"
        ])
    except getopt.GetoptError as err:
        print('Error: %s' % err)
        print_help(argv)
        sys.exit(1)

    project_folder = ""
    codequery_db_file = "project.db"
    bug_report_path = ""
    output_file_prefix = ""
    max_search_level = 0
    # analysis_language = "c,c++"
    analysis_language = "java"

    # process the options list into elements of a list
    # parse command line arguments
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print_help(argv, True)
        elif opt in ("-f", "--project-folder"):
            project_folder = arg
        elif opt in ("-d", "--codequery-database"):
            codequery_db_file = arg
        elif opt in ("-r", "--report"):
            bug_report_path = arg
        elif opt in ("-o", "--output-prefix"):
            output_file_prefix = arg
        elif opt in ("-m", "--max-level"):
            max_search_level = int(arg)
        elif opt in ('-l', "--analysis-language"):
            analysis_language = arg

    # check if the folder is specified
    if project_folder == "":
        print('Error: -f is not specified, or the string is not specified')
        sys.exit(2)
    # check if the bug report is specified
    if bug_report_path == "":
        print('Error: -r is not specified, or the string is not specified')
        sys.exit(2)
    # check if the output prefix is specified
    if output_file_prefix == "":
        print('Error: -o is not specified, or the string is not specified')
        sys.exit(2)

    return project_folder, codequery_db_file, bug_report_path, output_file_prefix, max_search_level, \
        analysis_language


def main(argv: List[str]) -> None:
    # set logging.INFO rather than logging.DEBUG.
    logging.basicConfig(level=logging.INFO)

    # parse the command line arguments.
    project_folder, cq_db_file, bug_report_file_path, output_file_prefix, \
        max_search_level, analysis_language = parse_args(argv)
    cq_db_file_path = os.path.join(project_folder, cq_db_file)

    # obtain or create the tag database for the given project.
    ctags_line_info = determine_code_query_info(project_folder, cq_db_file,
                                                analysis_language)

    if os.path.exists(cq_db_file_path):
        with sqlite3.connect(cq_db_file_path) as cq_db_conn:
            # get the bug report.
            bug_info_dict = get_bug_report(bug_report_file_path)

            # extract code snippet
            extract_code_snippets(project_folder, cq_db_conn, ctags_line_info,
                                  bug_info_dict, output_file_prefix,
                                  analysis_language, max_search_level)


if __name__ == "__main__":
    main(sys.argv)
