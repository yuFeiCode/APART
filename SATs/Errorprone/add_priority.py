import os
import pandas as pd

# Define the directory paths
input_directory = r"D:\Gitee-code\enhance_SATs\SAT_tool_result\(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)\Errorprone"
output_directory = r"D:\Gitee-code\enhance_SATs\SAT_tool_result\(2024-10-21)add-priority-for-SAT(除了PMD之外，prority越小，代表优先级越高)\Errorprone\test"

# Create the output directory if it doesn't exist
os.makedirs(output_directory, exist_ok=True)

# Define the list of releases
all_eval_releases = ['activemq-5.2.0', 'activemq-5.3.0', 'activemq-5.8.0',
                     'camel-2.10.0', 'camel-2.11.0', 
                     'derby-10.5.1.1',
                     'groovy-1_6_BETA_2', 
                     'hbase-0.95.2',
                     'hive-0.12.0', 
                     'jruby-1.5.0', 'jruby-1.7.0.preview1',
                     'lucene-3.0.0', 'lucene-3.1', 'wicket-1.5.3']

# Process each file
for release in all_eval_releases:
    input_filename = f"{release}-line-lvl-result.txt"
    input_filepath = os.path.join(input_directory, input_filename)
    output_filepath = os.path.join(output_directory, input_filename)
    
    # Read the file
    df = pd.read_csv(input_filepath)
    
    # Add the priority column with value 2
    df['priority'] = 2
    
    # Save the updated dataframe to the new file in the test folder
    df.to_csv(output_filepath, index=False)

print("Priority column added and files saved to the test folder successfully.")
