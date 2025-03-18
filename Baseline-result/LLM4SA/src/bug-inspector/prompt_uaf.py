uaf_position_prompt = """\
# Task Description
As an expert in C/C++ code review, I possess advanced skills in analyzing program code using well-known static analysis tools.\
Additionally, I have extensive experience in finding a type of bug called use-before-initialization.\
It is worth noting that these static analysis tools frequently produce a significant number of warnings, which may consist of both false positives and redundancies.\
Consequently, it becomes essential for me to manually inspect and verify each warning.\

I will be provided with the code snippet and the bug report, and my task will be to examine the bug based on the calling context of the code snippet.\
Initially, I should explain whether the bug can occur in the calling context of the code snippet.\
When I meet function calls, I need to accurately correspond to each pair of formal and actual parameters and synchronize changes to them.\
Please closely examine the scenarios in which each conditional branch evaluates as true or false and their feasibility given specific inputs.\
In other cases, functions will return with a return code. The caller then checks the return code to determine if the function was executed successfully. \
For example, "if(!func(...)) return" In this case, I should consider these return value checks and only go to successful conditions (means won't return directly)\
Thinking step by step.\
I only report a genuine bug when I am highly confident and have accurately identified a specific pathway that triggers it. Otherwise, it is considered a false alarm.\
Additionally, My analysis should be field-sensitive. This means if some functions initialize the fields of their parameters (i.e, for func(struct some_struct* ptr), it may initialize ptr->config).\
Suppose I have difficulty analyzing fields without more information, such as a function definition, caller, and callee, try my best to guess the behavior of the function.\

Lastly, I will be asked to determine whether the bug is a real bug or a false alarm.\
In the last line of my answer, I should conclude with '@@@ real bug @@@', '@@@ false alarm @@@' or '@@@ unknown @@@'.\n
"""



uaf_example_q1 = """\
# Bug Report
```json
{
    "bug_type": "Use-after-free",
    "line": "2146",
    "column": "7",
    "procedure": "delete_breakpoint",
    "file": "debug.c",
    "qualifier": {
        "CSA": "Use of memory after it is freed"
    },
    "Trace_Nnode": [
        {"filename": "debug.c", "line_number": "2145", "column_number": "29", "description": "Entering loop body"},
        {"filename": "debug.c", "line_number": "2147", "column_number": "3", "description": "Calling 'delete_commands_item'"},
        {"filename": "debug.c", "line_number": "4833", "column_number": "1", "description": "Entered call from 'delete_breakpoint'"},
        {"filename": "debug.c", "line_number": "4840", "column_number": "2", "description": "Memory is released"},
        {"filename": "debug.c", "line_number": "2147", "column_number": "3", "description": "Returning; memory was released via 1st parameter"},
        {"filename": "debug.c", "line_number": "2145", "column_number": "2", "description": "Looping back to the head of the loop"},
        {"filename": "debug.c", "line_number": "2145", "column_number": "29", "description": "Entering loop body"},
        {"filename": "debug.c", "line_number": "2146", "column_number": "7", "description": "Use of memory after it is freed"}
    ]
}
```

# Code Snippet
```C,C++
    // ./debug.c, line: 4834-4841
4834 delete_commands_item(struct commands_item *c)
4835 {
4836 	efree(c->cmd_string);
4837 	free_cmdarg(c->arg);
4838 	c->next->prev = c->prev;
4839 	c->prev->next = c->next;
4840 	efree(c);
4841 }
    // ./debug.c, line: 2130-2158
2130 delete_breakpoint(BREAKPOINT *b)
2131 {
2132 	INSTRUCTION *pc = b->bpi;
2133 	struct commands_item *c;
... 
2140 	pc->opcode = Op_no_op;
2141 	pc->source_line = 0;
2142 	pc->break_pt = NULL;
2143 
2144 	/* delete commands */
2145 	for (c = b->commands.next; c != &b->commands; c = c->next) {
2146 		c = c->prev;
2147 		delete_commands_item(c->next);
2148 	}
...
```
"""

uaf_example_a1 = """\
The bug report claims that there is a Use-After-Free bug in `delete_breakpoint` at line 2146. The variable used at line 2146 is `c->prev`.\
To confirm if it is a real bug, I should analyze the code snippet based on the error trace in the bug report.\

According to the error trace, we should analyze `delete_breakpoint` first. We can see that `c` is declared at line 2133 as a `commands_item` pointer.\
Then the program executes the loop at line 2145. This involves changes in the link list. I should analze it carefully.\
I use node1, node2, and node3 to represent `c->prev`, `c` and `c->next` respectively.\
At line 2146, `c` is set to `c->prev`. `c->prev` means the previous node of `c`. So `c` is node1 now.\
Then at line 2147, `delete_commands_item` is called with `c->next`.\
With this calling context, the `c` in `delete_commands_item` is the same as `c->next` in `delete_breakpoint`,\
which means that the `c` in `delete_commands_item` and the `c->next` in `delete_breakpoint` is all node2. So the `c` in `delete_commands_item` is node1.
Then at line 4838, `c->next->prev` is set to `c->prev`, which means that the previous node of `c->next`(node3) is set to the previous node of `c` which is node1.\
Then at line 4839, `c->prev->next` is set to `c->next`, which means that the next node of `c->prev`(node1) is the next node of `c` which is node3.\
After that, node2 is remove from the link list and then is freed at line 4838.\
The changes in `delete_commands_item` will be synchronized to `delete_breakpoint`, so the next node of `c`(node1) in `delete_breakpoint` is node3.\
The node3 is not freed. 

Therefore, there is no use after free bug in code basd on the analysis above. The bug report is a false alarm.

@@@ false alarm @@@
"""


# which means that changs to the `c` in `delete_commands_item` will be synchronized to `c->next` in `delete_breakpoint`.\


uaf_example_q2 = """\
# Bug Report
```json
{
    "bug_type": "Use-after-free",
    "line": "48",
    "column": "9",
    "procedure": "aaa_15_bad",
    "file": "aaa_15.c",
    "qualifier": {
        "CSA": "Use of memory after it is freed"
    },
    "Trace_None": [
        {"filename": "aaa_15.c", "line_number": "32", "column_number": "24", "description": "Memory is allocated"},
        {"filename": "aaa_15.c", "line_number": "33", "column_number": "13", "description": "Assuming 'data' is not equal to NULL"},
        {"filename": "aaa_15.c", "line_number": "37", "column_number": "9", "description": "Memory is released"},
        {"filename": "aaa_15.c", "line_number": "48", "column_number": "9", "description": "Use of memory after it is freed"}
    ]
}
```

# Code Snippet
```C,C++
    // ./aaa_15.c, line: 24-56
24 void aaa_15_bad()
25 {
26     char * data;
27     /* Initialize data */
28     data = NULL;
29     switch(6)
30     {
31     case 6:
32         data = (char *)malloc(100*sizeof(char));
33         if (data == NULL) {exit(-1);}
34         memset(data, 'A', 100-1);
35         data[100-1] = '\0';
36         /* POTENTIAL FLAW: Free data in the source - the bad sink attempts to use data */
37         free(data);
38         break;
39     default:
40         /* INCIDENTAL: CWE 561 Dead Code, the code below will never run */
41         printLine("Benign, fixed string");
42         break;
43     }
44     switch(7)
45     {
46     case 7:
47         /* POTENTIAL FLAW: Use of data that may have been freed */
48         printLine(data);
49         /* POTENTIAL INCIDENTAL - Possible memory leak here if data was not freed */
50         break;
51     default:
52         /* INCIDENTAL: CWE 561 Dead Code, the code below will never run */
53         printLine("Benign, fixed string");
54         break;
55     }
56 }
```
"""

uaf_example_a2 = """\
The bug report claims that there is a Use-after-free bug in `aaa_15_bad` at line 48.\
To confirm if it is a real bug, I should analyze the code snippet based on the error trace in the bug report.\
The variable used at line 48 is `data`, so I should analyze if `data` is used at line 48 after it is freed.\

According to the error trace, `data` is declared at line 28 in the code snippet.\
Then the program will execute the first `switch` block. The condition is `6`, so the block of `case 6` is executed.\
In the `case 6` block, `data` is allocated memory with `malloca` at line 32 and then freed at line 37.\
Then the program will execute the second `switch` block. The condition is `7`, so the block of `case 7` is executed.\
In the `case 6` block, `data` is used as the parameter of `printLine`.\
However, `data` is already freed at line 37. Therefore, `data` is indeed used after free at line 48.

Basd on the analysis above, I can conclude that there is indeed a Use-after-free bug in `aaa_15_bad` at line 48.\

@@@ real bug @@@
"""