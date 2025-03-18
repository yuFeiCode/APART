#!/usr/bin/env python3
# -*- coding: UTF-8 -*-

import os, sys, re

current_path = os.path.dirname(os.path.abspath(__file__))
sys.path.append(os.path.dirname(os.path.dirname(current_path)))
from conf.jsoninfo import *

def split_file_name(file_name):
    prefix_str = ""
    suffix_str = ""
    mfName = ""

    if '/' in file_name:
        print(file_name)
        prefix_str, suffix_str = file_name.rsplit('/', 1)
        prefix_str = prefix_str + '/'
    else:
        suffix_str = file_name
    
    if '.' in suffix_str:
        mfName, suffix_str = suffix_str.rsplit('.', 1)
        suffix_str = '.' + suffix_str

    # print("prefix_str: ", prefix_str, "\nmfName: ", mfName, "\nsuffix_str: ", suffix_str)
    return prefix_str, mfName, suffix_str


# formate time
def formateTime(seconds):
    '''
    Turn a time interval in seconds into the format of hh:mm:ss.sssss
    '''
    hours, rem = divmod(seconds, 3600)
    minutes, s = divmod(rem, 60)
    return '{:0>2}:{:0>2}:{:08.5f}'.format(int(hours), int(minutes), s)


# write to file
def write_to_file(file_name, content):
    with open(file_name, 'w') as f:
        f.write(content)

# remove one line from the file based on the given line number
def remove_one_line_from_the_file(filename, linenum):
    # open the file in read mode and store the lines in a list
    with open(filename, "r") as f:
        lines = f.readlines()
    # check if the line number is valid
    if 0 < linenum <= len(lines):
        # remove the line at the given index
        lines.pop(linenum - 1)
        # open the file in write mode and write the modified lines
        with open(filename, "w") as f:
            for line in lines:
                f.write(line)
    else:
        # print an error message if the line number is invalid
        print("remove_one_line_from_the_file(): Invalid line number")