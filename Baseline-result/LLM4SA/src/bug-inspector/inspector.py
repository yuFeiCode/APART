#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

import os, sys, re
import logging
import pickle
import shutil
import getopt
import datetime
import argparse
from typing import List
from prompt import *
from prompt_bof import *
from prompt_uva import *
from prompt_uaf import *
from prompt_npd import *
from prompt_ml import *
from prompt_dyz import *

current_path = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.dirname(current_path))
from pybaselib.baselib import *
from utils.gptcore import *

sys.path.append(os.path.dirname(os.path.dirname(current_path)))
from conf.jsoninfo import *
load_json_config(True)

n_choices = 7

def query_llm_and_get_result(snippet_file, bug_type, llm_model):
    # open the snippet_file and get strings
    snippet_file_str = ""
    with open(snippet_file, "r") as f:
        snippet_file_str = f.read()
    
    useOpenKey = False
    
    # prompt
    prompt_list = []
    
    if bug_type == "auto":
        bug_type = "all"
        pass

    print("&&&&&&&&&&&&&&&& bug_type =", bug_type, "&&&&&&&&&&&&&&&&\n")
    
    if bug_type == "all":
        sys_prompt = position_prompt
        prompt_list.append({'role': 'system', 'content': sys_prompt})
        pass
    elif bug_type == "uva":
        sys_prompt = uva_position_prompt
        prompt_list.append({'role': 'system', 'content': sys_prompt})
        prompt_list.append({'role': 'user', 'content': uva_example_q1})
        prompt_list.append({'role': 'assistant', 'content': uva_example_a1})
        prompt_list.append({'role': 'user', 'content': uva_example_q2})
        prompt_list.append({'role': 'assistant', 'content': uva_example_a2})
        prompt_list.append({'role': 'user', 'content': uva_example_q3})
        prompt_list.append({'role': 'assistant', 'content': uva_example_a3})
        prompt_list.append({'role': 'user', 'content': uva_example_q4})
        prompt_list.append({'role': 'assistant', 'content': uva_example_a4})
        pass
    elif bug_type == "bof":
        sys_prompt = bof_position_prompt
        prompt_list.append({'role': 'system', 'content': sys_prompt})
        prompt_list.append({'role': 'user', 'content': bof_example_q1})
        prompt_list.append({'role': 'assistant', 'content': bof_example_a1})
        prompt_list.append({'role': 'user', 'content': bof_example_q5})
        prompt_list.append({'role': 'assistant', 'content': bof_example_a5})
        prompt_list.append({'role': 'user', 'content': bof_example_q3})
        prompt_list.append({'role': 'assistant', 'content': bof_example_a3})
        prompt_list.append({'role': 'user', 'content': bof_example_q4})
        prompt_list.append({'role': 'assistant', 'content': bof_example_a4})
        pass
    elif bug_type == "npd":
        sys_prompt = npd_position_prompt
        prompt_list.append({'role': 'system', 'content': sys_prompt})
        prompt_list.append({'role': 'user', 'content': npd_example_q1})
        prompt_list.append({'role': 'assistant', 'content': npd_example_a1})
        prompt_list.append({'role': 'user', 'content': npd_example_q2})
        prompt_list.append({'role': 'assistant', 'content': npd_example_a2})
        prompt_list.append({'role': 'user', 'content': npd_example_q3})
        prompt_list.append({'role': 'assistant', 'content': npd_example_a3})
        prompt_list.append({'role': 'user', 'content': npd_example_q4})
        prompt_list.append({'role': 'assistant', 'content': npd_example_a4})
        pass
    elif bug_type == "ml":
        sys_prompt = ml_position_prompt
        prompt_list.append({'role': 'system', 'content': sys_prompt})
        prompt_list.append({'role': 'user', 'content': ml_example_q1})
        prompt_list.append({'role': 'assistant', 'content': ml_example_a1})
        prompt_list.append({'role': 'user', 'content': ml_example_q5})
        prompt_list.append({'role': 'assistant', 'content': ml_example_a5})
        prompt_list.append({'role': 'user', 'content': ml_example_q3})
        prompt_list.append({'role': 'assistant', 'content': ml_example_a3})
        prompt_list.append({'role': 'user', 'content': ml_example_q4})
        prompt_list.append({'role': 'assistant', 'content': ml_example_a4})
        pass
    elif bug_type == "dbz":
        pass
    elif bug_type == "uaf":
        sys_prompt = uaf_position_prompt
        prompt_list.append({'role': 'system', 'content': sys_prompt})
        prompt_list.append({'role': 'user', 'content': uaf_example_q1})
        prompt_list.append({'role': 'assistant', 'content': uaf_example_a1})
        prompt_list.append({'role': 'user', 'content': uaf_example_q2})
        prompt_list.append({'role': 'assistant', 'content': uaf_example_a2})
        pass
    else:
        pass

    chat_inspect = BaseChatClass(prompt_list, useOpenKey=useOpenKey)

    question = snippet_file_str

    logging.info(f"\U0001f47b: {question}\n")
    full_reply_content_list, tokens_usage = chat_inspect.get_respone(question, model=llm_model, maxTokens=2048, temperature_arg=0.7, n_choices=n_choices)
    # chat_inspect.show_conversation(chat_inspect.conversation_list)
    logging.info(f"total_tokens: {tokens_usage}\n")

    return full_reply_content_list


def LLMInspect_Main(snippet_file, bug_type, output_file, llm_model):
    
    if snippet_file is None or output_file is None:
        print("No file or type")
        return

    # set logging.INFO rather than logging.DEBUG
    logging.basicConfig(level=logging.INFO)

    if not os.path.exists(snippet_file):
        logging.error("snippet_file is not exist")
        sys.exit(-1)
    
    print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("+++++++++++++++++++++++++++ start +++++++++++++++++++++++++++++")
    print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n")
    
    answer_list = query_llm_and_get_result(snippet_file, bug_type, llm_model)

    # mkdir output_file folder
    if os.path.exists(output_file):
        shutil.rmtree(output_file)
    os.makedirs(output_file)

    aid = 1
    unknown_num = 0
    TP_num = 0
    FP_num = 0
    final_result = ""
    for each_answer in answer_list:
        # read line
        Alines = each_answer.split("\n")
        Flag = ""
        if "@ real bug @" in Alines[-1]:
            TP_num += 1
            Flag = "tp"
        elif "@ false alarm @" in Alines[-1]:
            FP_num += 1
            Flag = "fp"
        elif " unknown " in Alines[-1]:
            unknown_num += 1
            Flag = "unknown"
        else:
            Flag = "unknown"
            print(Alines[-1])
            print("Error: unknown result")
            
            
        # save the answer to output_file
        with open(output_file + "/" + str(aid) + "_" + Flag + ".txt", "w") as f:
            f.write(each_answer)
        aid += 1
    
    confident_value = int(n_choices/2 + 1)

    if TP_num >= confident_value:
        final_result = "real bug"
    elif FP_num >= confident_value:
        final_result = "false alarm"
    else:
        final_result = "unknown"

    with open(output_file + "/final_result.txt", "w") as f:
        f.write(final_result)

    print("\n+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("++++++++++++++++++++++++++++ End ++++++++++++++++++++++++++++++")
    print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    print("")
    print("muti-result:", final_result, "\n")


def main(argv: List[str]) -> None:
    # Parse the command line arguments
    snippet_file, bug_type, output_file, llm_model = parse_args(argv)

    # Start
    try:
        LLMInspect_Main(snippet_file, bug_type, output_file, llm_model)
    except Exception as e:
        print(e)
        raise e
    # End


def parse_args(argv: List[str]) -> argparse.Namespace:
    # Parse the command line arguments using the getopt module
    try:
        opts, args = getopt.getopt(argv, "hc:t:o:m:", ["help", "Code_Snippet=", "Bug_Type=", "Output=", "MODEL="])

    except getopt.GetoptError:
        print('Error: main.py -c <code snippet> -t <bug type> -o <output file> -m <model>')
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

    valid_bug_type = [  "all",
                        "auto",
                        "uva",
                        "npd",
                        "bof",
                        "dbz",
                        "uaf"]

    snippet_file = ""
    bug_type = "all"
    output_file = ""
    llm_model = "gpt-3.5-turbo"
    api_key_no = 0

    # Process the options list into elements of a list
    for opt, arg in opts:
        if opt in ("-h", "--help"):
            print('Syntax:')
            print('\tmain.py main.py -c <code snippet> -t <bug type> -o <output file> -m <model>', "\n")
            print('Options:')
            print('\t-t <bug type>\t', end='')
            for i in range(0, len(valid_bug_type)):
                if i == 0:
                    print('', valid_bug_type[i])
                else:
                    print('\t\t\t', valid_bug_type[i])
            print('\n\t-m <model>\t', end='')
            for i in range(0, len(valid_model)):
                if i == 0:
                    print('', valid_model[i])
                else:
                    print('\t\t\t', valid_model[i])
            sys.exit()
        elif opt in ("-c", "--code-snippet"):
            snippet_file = arg
        elif opt in ("-t", "--bug-type"):
            bug_type = arg
            bug_type = bug_type.lower()
        elif opt in ("-o", "--output"):
            output_file = arg
        elif opt in ("-m", "--model"):
            llm_model = arg

    if not llm_model in valid_model:
        print('Error: ' + llm_model + ' is not a valid model')
        sys.exit(2)

    if not bug_type in valid_bug_type:
        print('Error: ' + bug_type + ' is not a valid bug type')
        sys.exit(2)

    if snippet_file == "" and output_file == "":
        print('Error: file is not specified, or the string is not specified')
        print('Tips: Using -h to view help')
        sys.exit(2)

    print("OPENAI_API_KEY = ", os.environ.get('OPENAI_API_KEY'), "\nBalance = ", os.environ.get('BALANCE'))
    print('snippet_file = ' + snippet_file + ', output_file = ' + output_file + ', bug_type = ' + bug_type + ', llm_model = ' + llm_model)
    print('')

    # Print the arguments list, which contains all arguments that don't start with '-' or '--'
    for i in range(0, len(args)):
        print('Argument %s is: %s' % (i + 1, args[i]))
    
    return snippet_file, bug_type, output_file, llm_model


if __name__ == "__main__":

    main(sys.argv[1:])

