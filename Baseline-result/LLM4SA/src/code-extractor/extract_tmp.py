#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

import os, sys
import argparse
import getopt
import datetime
import logging
import json
import hashlib
import shutil
from typing import List

# Formats a ctags file into a list of lists, where each sublist is a line of the file.
# Args: ctags_file (str): The path to a ctags file.
# Returns: list of lists: A list of lists, where each sublist is a line of the file.
def format_ctags_file(ctags_file):
    # Create an empty list to store the information from the file
    line_info_list = []
    # Open the file
    with open(ctags_file, "r") as ctags_file_handle:
        # Read each line in the file
        for line in ctags_file_handle.readlines():
            # Ignore lines that start with "!_TAG_"
            if line.startswith("!_TAG_"):
                continue
            # Split the line on tab characters
            line_info = line.strip().split("\t")
            # Add the line info to the list
            line_info_list.append(line_info)
    # Return the list
    return line_info_list


# Optional: ctags --fields=+ne --languages="c++,c" --c-types=f -R .
def determine_ctags_file(p_foler):
    # whether the ctags file exists
    ctags_file = p_foler + "/tags"
    if os.path.exists(ctags_file):
        print("====================================")
        print("ctags file exists\n")
    else:
        # check ctags --version exists
        if os.system("ctags --version") != 0:
            print("====================================")
            print("ctags --version not exists")
            print("please install ctags\n")
            sys.exit(2)
        else:
            # create ctags file
            print("====================================")
            print("ctags file not exists")
            print("create ctags file now\n")
            os.system("ctags --fields=+ne --languages=\"c++,c\" --c-types=f -R .")
    # format ctags file
    return format_ctags_file(ctags_file)


def get_bug_report(json_reprot):
    # Load the json report file into a dictionary
    bug_info_dict = {}
    # Read the JSON report file and convert it into a Python dictionary.
    with open(json_reprot, "r") as json_file_handle:
        bug_info_dict = json.load(json_file_handle)
    # return the bug info
    return bug_info_dict


# This function is used to get the function name from the line number of a file.
# It will return a list of dictionaries, each dictionary contains the information of a function. 
# Each dictionary has the following format:
# {"file": <file name>, "function": <function name>, "start": <start line number>, "end": <end line number>}
def get_code_extract_list(bug_info_dict, line_info_list):
    # Create an empty list to store the list
    main_list = []
    trace_list = []

    # Get the function name from the line number of a file
    for each_fn in line_info_list:

        # get the start and end line number of the function
        start = 0
        end = 0
        for i in range(4, len(each_fn)):
            if "line:" in each_fn[i]:
                start = int(each_fn[i].replace("line:", ""))
            elif "end:" in each_fn[i]:
                end = int(each_fn[i].replace("end:", ""))
            
            if start != 0 and end != 0:
                break
        
        # append main_list
        if each_fn[1] == bug_info_dict["file"]:    
            # get the bug line number
            bug_line = int(bug_info_dict["line"])
            # If the bug line is within the range of the function, add it to the list
            if bug_line > start and bug_line < end and (start != 0 or end != 0):
                # Assert that the function name stored in the bug_info_dict is the same as the function name we are currently processing
                assert bug_info_dict["procedure"] == each_fn[0]
                # If the assert passes, add the bug info to the main list
                main_list.append({"file": each_fn[1], "function":each_fn[0], "start": start, "end": end})

        # append trace_list
        for i in range(1, 10):
            # if key not in bug_info_dict
            if "Trace_" + str(i) not in bug_info_dict:
                break
            # Loop through each element in the trace list
            for each_item in bug_info_dict["Trace_" + str(i)]:
                # If the filename matches that of the function
                if each_fn[1] == each_item["filename"]:
                    # If the line number is between the start and end
                    if each_item["line_number"] > start and each_item["line_number"] < end:
                        tmp_dict = {"file": each_fn[1], "function":each_fn[0], "start": start, "end": end}
                        if tmp_dict not in trace_list:
                            trace_list.append(tmp_dict)
    
    # filt the trace_list from main_list
    # Loop through each element in main_list
    for each_ma in main_list[:]:
        # Loop through each element in trace_list
        for each_tr in trace_list[:]:
            # If the element is in both lists, remove it from the trace_list
            if each_ma == each_tr:
                trace_list.remove(each_tr)

    return main_list, trace_list


def get_snippet_from_line(p_foler, file, function, start, end):
    extracted_code = []
    extracted_code.append("   // " + file + ": line " + str(start) + "-" + str(end) + ", function: " + function)
    with open(p_foler+"/"+file, "r") as f:
        lines = f.readlines()
        for i in range(start-1, end):
            if (start+end)/2 < 100:
                extracted_code.append("{:<2d} ".format(i+1) + lines[i].replace("\n",""))
            elif (start+end)/2 < 1000:
                extracted_code.append("{:<3d} ".format(i+1) + lines[i].replace("\n",""))
            elif (start+end)/2 < 10000:
                extracted_code.append("{:<4d} ".format(i+1) + lines[i].replace("\n",""))
            else:
                extracted_code.append("{:<5d} ".format(i+1) + lines[i].replace("\n",""))
    
    return extracted_code


def extract_code_snippet(p_foler, line_list):
    code_snippet_list= []
    for each_item in line_list:
        #get the code snippet from the line
        single_code_snippet = get_snippet_from_line(p_foler, each_item["file"], each_item["function"], each_item["start"], each_item["end"])
        #add the code snippet to the list
        code_snippet_list.append(single_code_snippet)
    return code_snippet_list


def write_output_file(output_file, code_snippet_list):
    #open a file for writing
    with open(output_file, "w") as f:
        #loop through each item in the list
        for each_item in code_snippet_list:
            #loop through each line in the item
            for each_line in each_item:
                #write each line to the file
                f.write(each_line + "\n")
            #add a blank line after each item
            f.write("\n")


def main(argv: List[str]) -> None:
    # set logging.INFO rather than logging.DEBUG
    logging.basicConfig(level=logging.INFO)

    # Parse the command line arguments
    p_foler, json_reprot, output_file = parse_args(argv)

    # whether the ctags file exists
    line_info_list = determine_ctags_file(p_foler)

    # get the bug report
    bug_info_dict = get_bug_report(json_reprot)
    
    # get the extract code list
    main_list, trace_list = get_code_extract_list(bug_info_dict, line_info_list)

    # get the extract code list
    code_snippet_list = extract_code_snippet(p_foler, main_list)
    trace_snippet_list = extract_code_snippet(p_foler, trace_list)
    if trace_list != []:
        code_snippet_list.extend(trace_snippet_list)

    # write the output file
    write_output_file(output_file, code_snippet_list)
    
    # End
    

def parse_args(argv: List[str]) -> argparse.Namespace:
    # Parse the command line arguments using the getopt module
    try:
        opts, args = getopt.getopt(argv, "hf:r:o:", ["help", "ProjectFolder=", "Report=", "OutputCode="])

    except getopt.GetoptError:
        print('Error: main.py -f <project folder> -r <path to BugReport.json> -o <output code snippet>')
        sys.exit(2)

    project_folder = ""
    bug_report_path  = ""
    output_file = "code_snippet.c"

    # Process the options list into elements of a list
    # Parse command line arguments
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print('main.py -f <project folder> -r <path to BugReport.json>')
            sys.exit()
        elif opt in ("-f", "--project-folder"):
            project_folder = arg
        elif opt in ("-r", "--reprot"):
            bug_report_path = arg
        elif opt in ("-o", "--output"):
            output_file = arg
    # Check if the folder is specified
    if project_folder == "":
        print('Error: -f is not specified, or the string is not specified')
        print('Tips: Using -h to view help')
        sys.exit(2)

    # Check if the report is specified
    if bug_report_path == "":
        print('Error: -r is not specified, or the string is not specified')
        print('Tips: Using -h to view help')
        sys.exit(2)

    # Print the arguments list, which contains all arguments that don't start with '-' or '--'
    for i in range(0, len(args)):
        print('Argument %s is: %s' % (i + 1, args[i]))

    return project_folder, bug_report_path, output_file


if __name__ == "__main__":
    starttime = datetime.datetime.now()
    main(sys.argv[1:])
    endtime = datetime.datetime.now()
    print("\nstart time: ", starttime)
    print("end time: ", endtime)
    print("@@@ Finished @@@")