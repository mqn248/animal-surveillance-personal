import os
import pandas as pd

file_path2 = 'D:/CEMA PROJECTS/animalHealthSurveillance/animal-surveillance/dummy_data/remove_data.xlsx' 
sheet_name = 'Cleaned' 

# set file path and sheet name 
file_path = 'D:/CEMA PROJECTS/animalHealthSurveillance/animal-surveillance/dummy_data/animal_data2.xlsx' 
sheet_name = 'Sheet1'  
rename_col = {'Sub-County': 'Sub_County', 
            #'sub location': 'Sub_Location',
            'Date of Start of Outbreak/Event':'Start_Outbreak_Event',
            'Date Report':'Report_Date','Disease/ Condition':'Disease_Condition',
            'Nature of Diagnosis':'Nature_of_Diagnosis', 'Test Used':'Test_Used',
            'Species Affected':'Species_Affected','Number at Risk':'Number_at_Risk',
            'Number Sick':'Number_Sick', 'Number Dead':'Number_Dead','Number Slaughtered':'Number_Slaughtered',
            'Number Destroyed':'Number_Destroyed','Production System':'Production_System',
            'Number of Humans Affected (If zoonosis)':'Number_Humans_Affected_zoonosis',
            'Disease Control Method':'Disease_Control_Method','Number Vaccinated':'Number_Vaccinated',
            'Organisation (GOK, Private)':'Organisation_GOK_Private'
} 

# clean data
def clean_pipeline(file_path, sheet_name=0, rename_columns=None, drop_duplicates=True):
    """
    This function performs the following

    Parameters:
    - file_path (str): Path to the Excel file.
    - Reads the specific excel sheet
    - rename_columns
    - Drop duplicate rows (default is True).
    
    Returns:
    - Cleaned pandas DataFrame.
    """
    
    # Read Excel data into a DataFrame
    animal_records = pd.read_excel(file_path, sheet_name=sheet_name)
    
    # Rename cols
    if rename_columns:
        animal_records.rename(columns=rename_col, inplace=True)

    # Drop duplicate records
    if drop_duplicates:
        animal_records.drop_duplicates(inplace=True)

    # Remove trailing white spaces from colnmaes
    animal_records.columns = animal_records.columns.str.strip()  
        
    # Combine into one Date column
    animal_records['Start_Outbreak_Event'] = pd.to_datetime(animal_records[['Year_Start', 'Month_Start', 'Date of Start of Outbreak/Event Day']].astype(str).agg('-'.join, axis=1), format='%Y-%B-%d')
    animal_records['Report_Date'] = pd.to_datetime(animal_records[['Year_Report', 'Month_Report', 'Date Report Day']].astype(str).agg('-'.join, axis=1), format='%Y-%B-%d')
    return animal_records

# call clean function
cleaned_animals = clean_pipeline(file_path, sheet_name=sheet_name, rename_columns=rename_col) 
cleaned_animals.to_excel("Cleaned Animal data.xlsx")