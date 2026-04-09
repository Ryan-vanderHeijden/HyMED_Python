import pandas as pd
import glob
import os

def merge_csv_files(input_folder, output_file, file_pattern='*.csv'):
    """
    Reads all CSV files from a specified folder, merges them into a single 
    DataFrame, and saves the result to a new CSV file.

    Args:
        input_folder (str): The directory containing the CSV files.
        output_file (str): The name of the resulting merged CSV file.
        file_pattern (str): The pattern to match the CSV files (e.g., '*.csv').
    """
    # 1. Find all matching files
    # The glob.glob() function returns a list of file paths that match the pattern.
    search_path = os.path.join(input_folder, file_pattern)
    all_files = glob.glob(search_path)
    
    if not all_files:
        print(f"No files found matching the pattern '{file_pattern}' in '{input_folder}'.")
        return

    # 2. Read and Concatenate
    # We use a list comprehension and pd.concat for efficiency.
    try:
        data_frames = [pd.read_csv(file) for file in all_files]
        
        # Concatenate all DataFrames in the list
        merged_df = pd.concat(data_frames, ignore_index=True)
        
    except Exception as e:
        print(f"An error occurred during file reading or concatenation: {e}")
        return

    # 3. Save the merged DataFrame
    try:
        merged_df.to_csv(output_file, index=False)
        print(f"✅ Successfully merged {len(all_files)} files into '{output_file}'.")
        print(f"Total rows in the merged file: {len(merged_df)}")
        
    except Exception as e:
        print(f"An error occurred while writing the output file: {e}")
        
        

# --- Configuration ---
# Set the folder where your CSVs are located (e.g., 'data' folder)
INPUT_DIRECTORY = 'Processed/VT_gages/drought_props/' 

# Set the name for your final merged file
OUTPUT_FILENAME = 'VT_drought_props_long.csv'

# Run the merge function
merge_csv_files(INPUT_DIRECTORY, OUTPUT_FILENAME)