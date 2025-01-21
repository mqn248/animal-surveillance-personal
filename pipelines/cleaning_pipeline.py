import os
import pandas as pd

# set file path and sheet name 
file_path = 'D:/CEMA PROJECTS/animalHealthSurveillance/animal-surveillance/dummy_data/animal_data.xlsx' 
file_path2 = 'D:/CEMA PROJECTS/animalHealthSurveillance/animal-surveillance/dummy_data/remove_data.xlsx' 
sheet_name = 'Cleaned'  
sheet_name2 = 'Remove'
column_name = 'Remove this'

# get neccessary colnames
def extract_colnames(file_path, sheet_name=0):
    animal_variables = pd.read_excel(file_path, sheet_name=sheet_name)

    # create a dict for necessary cols
    column_dict = animal_variables.columns.tolist()

    return column_dict

animal_dict = extract_colnames(file_path, sheet_name)
print("Keep the following cols:",animal_dict)


# get colnames to drop
def remove_colnames(file_path2, sheet_name2=0, column_name=None):
    remove_variables = pd.read_excel(file_path2, sheet_name=sheet_name2)

    if column_name and column_name in remove_variables.columns:
        animal_data = remove_variables[column_name].tolist()
        return animal_data
    else:
        print("Column '{column_name}' not found in the Excel sheet.")

animal_dict = remove_colnames(file_path2, sheet_name2=sheet_name2, column_name=column_name)
print("Remove the following:",animal_dict)

