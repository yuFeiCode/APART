#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

import os, sys
import getopt
import datetime
import shutil
import argparse
from typing import List

current_path = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.dirname(current_path))
from pybaselib.baselib import *

sys.path.append(os.path.dirname(os.path.dirname(os.path.dirname(current_path))))
from conf.jsoninfo import *
load_json_config(True)

SnippetFolder = "/home/zhouyufei/LLM4SA/CodeSnippet"
tmp_folder = "/home/zhouyufei/LLM4SA/test/PMD/gpt-4o-mini"

Bug_type = ['_NPD.json',
            '_UVA.json',
            ]

def get_inspect_result(tmp_folder):
    with open(tmp_folder + "/final_result.txt", "r") as f:
        iresult = f.read().strip()
    return iresult


def Inspect_Main(pfolder, bug_report, output_file, llm_model, api_key_index, bug_type):
    # extract the code snippet
    pyextractor = " " + os.path.dirname(os.path.dirname(os.path.abspath(__file__))) + "/code-extractor/extract_code_snippet.py" 
    m_index = 0
    # for each in Bug_type:
    #     if bug_report.endswith(each):
    #         m_index = 1
    #         break
    extractCMD = "python3" + pyextractor + " -f " + pfolder + " -r " + bug_report + " -o " + SnippetFolder + " -m " + str(m_index)
    print(extractCMD)
    exe_ret = os.system(extractCMD)
    if exe_ret != 0:
        print("error in main.py line 44: ", exe_ret)
        sys.exit(0)

    # get the snippet file from SnippetFolder folder
    snippet_file_list = []
    for root, dirs, files in os.walk(SnippetFolder):
        for file in files:
            snippet_file_list.append(os.path.join(root, file))

    # change m to 0
    if len(snippet_file_list) >= 4:
        extractCMD = "python3" + pyextractor + " -f " + pfolder + " -r " + bug_report + " -o " + SnippetFolder + " -m 0"
        print(extractCMD)
        exe_ret = os.system(extractCMD)
        if exe_ret != 0:
            print("error in main.py line 44: ", exe_ret)
            sys.exit(0)
        snippet_file_list = []
        for root, dirs, files in os.walk(SnippetFolder):
            for file in files:
                snippet_file_list.append(os.path.join(root, file))

    pyinspector = " " + os.path.dirname(os.path.abspath(__file__)) + "/inspector.py"
    iresult_list = []
    for each_snippet in snippet_file_list:
        print("\nInspecting: ", each_snippet)
        # _, tmp_folder, _= split_file_name(bug_report)
        file_name = os.path.splitext(json_file)[0]
        result_dir = tmp_folder + '/' + file_name
        
        if not os.path.exists(result_dir):
            os.makedirs(result_dir) 
            
        # inspectCMD = "python3" + pyinspector + " -c " + each_snippet + " -o " + result_dir +" -m " + llm_model + " -t " + bug_type
        inspectCMD = "python3" + pyinspector + " -c " + each_snippet + " -o " + result_dir +" -m " + llm_model
        
        # if not os.path.exists(result_dir):
        #     os.makedirs(result_dir)  
        # if os.path.exists(tmp_folder):
        #     shutil.rmtree(tmp_folder)
        print(inspectCMD)
        exe_ret = os.system(inspectCMD) 
        if exe_ret != 0:
            print("error in main.py line 63 (ChatGPT network Err?), exe_ret =", exe_ret)
            sys.exit(0)  
        iresult = get_inspect_result(result_dir)
        iresult_list.append(iresult)
    print("\n")

    print("iresult_list =", iresult_list)
    if "true positive" in iresult_list or "real bug" in iresult_list:
        final_result = "real bug"
    elif "unknown" in iresult_list and "false alarm" in iresult_list:
        if iresult_list.count("unknown") >= iresult_list.count("false alarm"):
            final_result = "unknown"
        else:
            final_result = "false alarm"
    elif "unknown" in iresult_list:
        final_result = "unknown"
    else:
        final_result = "false alarm"
    print("final_result =", final_result)

    with open(output_file, "w") as f:
        f.write(final_result)


def main(argv: List[str]) -> None:
    # Parse the command line arguments
    pfolder, bug_report, output_file, llm_model, api_key_index, bug_type = parse_args(argv)

    # Start
    try:
        Inspect_Main(pfolder, bug_report, output_file, llm_model, api_key_index, bug_type)
    except Exception as e:
        print(e)
        raise e
    # End


def parse_args(argv: List[str]) -> argparse.Namespace:
    # Parse the command line arguments using the getopt module
    try:
        opts, args = getopt.getopt(argv, "hf:r:o:a:m:t:", ["help", "Folder=", "Report=", "Output=", "API_KEY=", "MODEL=", "BUG_TYPE="])

    except getopt.GetoptError:
        print('Error: main.py -f <pfolder> -r <bug report> -o <output file> -m <model> -t <bug_type>')
        sys.exit(2)

    valid_model = [ "gpt-3.5-turbo",
                    "gpt-3.5-turbo-0613",
                    "gpt-3.5-turbo-16k",
                    "gpt-3.5-turbo-16k-0613",
                    "gpt-4",
                    "gpt-4-0613",
                    "gpt-4-32k",
                    "gpt-4-32k-0613",
                    "gpt-4o-mini"]
    valid_bug_type = [ "all",
                    "auto",
                    "uva",
                    "npd",
                    "ml",
                    "dbz",
                    "bof",
                    "uaf"
                    ]
    pfolder = ""
    bug_report = ""
    output_file = ""
    llm_model = "gpt-3.5-turbo"
    api_key_no = 1
    bug_type = "auto"

    # Process the options list into elements of a list
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print('Syntax:')
            print('\tmain.py main.py -f <pfolder> -r <bug report> -o <output file> -m <model> -t <bug_type>', "\n")
            print('Options:')
            print('\n\t-m <model>\t', end='')
            for i in range(0, len(valid_model)):
                if i == 0:
                    print('', valid_model[i])
                else:
                    print('\t\t\t', valid_model[i])
            print('\n\t-t <bug_type>\t', end='')
            for i in range(0, len(valid_bug_type)):
                if i == 0:
                    print('', valid_bug_type[i])
                else:
                    print('\t\t\t', valid_bug_type[i])
            sys.exit()
        elif opt in ("-f", "--folder"):
            pfolder = arg
        elif opt in ("-r", "--bug-report"):
            bug_report = arg
        elif opt in ("-o", "--output"):
            output_file = arg
        elif opt in ("-m", "--model"):
            llm_model = arg
        elif opt in ("-t", "--bug-type"):
            bug_type = arg
            bug_type = bug_type.lower()

    if not llm_model in valid_model:
        print('Error: ' + llm_model + ' is not a valid model')
        sys.exit(2)

    if pfolder == "" and output_file == "":
        print('Error: file is not specified, or the string is not specified')
        print('Tips: Using -h to view help')
        sys.exit(2)

    print("OPENAI_API_KEY = ", os.environ.get('OPENAI_API_KEY'), "\nBalance = ", os.environ.get('BALANCE'))
    print('pfolder = ' + pfolder + ', output_file = ' + output_file + ', bug_report = ' + bug_report + ', llm_model = ' + llm_model)
    print('')

    # Print the arguments list, which contains all arguments that don't start with '-' or '--'
    for i in range(0, len(args)):
        print('Argument %s is: %s' % (i + 1, args[i]))
    
    return pfolder, bug_report, output_file, llm_model, api_key_no, bug_type


if __name__ == "__main__":
    #starttime = datetime.datetime.now()
    
    ROOT_DIR = "/home/zhouyufei/LLM4SA/test"
    json_input_dir = f"{ROOT_DIR}/PMD/json_input"

    # 获取文件夹中所有的JSON文件
    json_files = [f for f in os.listdir(json_input_dir) if f.endswith('.json')]
    
    for json_file in json_files:  
          
        file_name = os.path.splitext(json_file)[0]

        args = [
            "-t","",
            "-f",f"{ROOT_DIR}/datasets_files/activemq-5.2.0",
            "-r", f"{ROOT_DIR}/PMD/json_input/{json_file}",  # 结果文件路径
            # "-c", f"{ROOT_DIR}/CodeSnippet/Trace_None_snippets_1.txt",
            "-m", "gpt-4o-mini",
            "-o", f"{ROOT_DIR}/PMD/gpt-4o-mini/{file_name}/final_result.txt"
        ]
        main(args)
    # main(sys.argv[1:])
    #endtime = datetime.datetime.now()
    #print("\nstart time: ", starttime)
    #print("end time: ", endtime)
    #print("@@@ Finished @@@")
