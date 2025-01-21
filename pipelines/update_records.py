import os
import pandas as pd
from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from animal_models import animal_health
from sqlalchemy.orm import sessionmaker
from sqlalchemy.exc import IntegrityError
from dotenv import load_dotenv

# Load the .env file
load_dotenv()

# Import the user credentials
user = os.getenv('DB_USER')
password = os.getenv('DB_PASSWORD')
server = os.getenv('DB_SERVER')
db_animal = os.getenv('DB_NAME')
port = os.getenv('DB_PORT')

# Initialize app
app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = f'postgresql://{user}:{password}@{server}:{port}/{db_animal}'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
db = SQLAlchemy(app)

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

    # Remove trailing white spaces from colnames
    animal_records.columns = animal_records.columns.str.strip()  
    animal_records = animal_records.applymap(lambda x: x.strip() if isinstance(x, str) else x)  
   
    return animal_records

def populate_animal_data():
    # Dictionary - skip columns that are not necessary
    skip_columns = ['Unnamed: 38','Submitter Name', 'Submitter Username', 'Submitter Name', 'Submitter Organization', 
    'Submitter Role', 'Id', 'Location', 'Location', 'Other', 'Abortion', 'Number Affected', 'Sudden Death',
    'Hemorrhagic Signs','Neurologic signs','Animal Bites', 'Respiratory Signs', 'Oral/Foot Lesions','Cutaneous/Skin Lesions', 'Test Used', 
    'Gastrointestinal tract syndromes', 'Other Syndromes', 'No name column', 'Other Disease', 'Other control method','','Unnamed: 53', None]

    # Create the session to call the db
    session = db.session

    # call clean function
    cleaned_animals = clean_pipeline(file_path, sheet_name=sheet_name, rename_columns=rename_col) 
    # drop skipped columns
    cleaned_animals = cleaned_animals.drop(columns=skip_columns, errors='ignore')
    for index, row in cleaned_animals.iterrows():
        print(f"Row data: {row.to_dict()}")
                    
        try:
            # Check if a record with the same Condition_name and Interventions exists
            animals = session.query(animal_health).filter_by(Disease_Condition=row['Disease_Condition'],County=row['County']).first()

            if animals:
                # Update attributes
                animals.County = row['County']
                animals.Sub_County = row['Sub_County']
                animals.Ward = row['Ward']
                animals.Latitude = row['Latitude']
                animals.Longitude = row['Longitude'] 
                animals.Locality = row['Locality'] 
                animals.Start_Outbreak_Event = row['Start_Outbreak_Event'] 
                animals.Report_Date = row['Report_Date'] 
                animals.Disease_Condition = row['Disease_Condition'] 
                animals.Nature_of_Diagnosis = row['Nature_of_Diagnosis'] 
                animals.Species_Affected = row['Species_Affected'] 
                animals.Number_at_Risk = row['Number_at_Risk'] 
                animals.Number_Sick = row['Unit_cost'] 
                animals.Number_Dead = row['Number_Sick'] 
                animals.Number_Slaughtered = row['Number_Slaughtered'] 
                animals.Number_Destroyed = row['Number_Destroyed'] 
                animals.Production_System = row['Production_System'] 
                animals.Number_Humans_Affected_zoonosis = row['Number_Humans_Affected_zoonosis'] 
                animals.Disease_Control_Method = row['Disease_Control_Method'] 
                animals.Number_Vaccinated = row['Number_Vaccinated'] 
                animals.Organisation_GOK_Private = row['Organisation_GOK_Private']
                animals.Source = row['Source']
                print(f"Updated existing animal record: {animals.Disease_Condition}, {animals.County}")
            else:
                # Insert as new records
                animals = animal_health(
                    County = row['County'],
                    Sub_County = row['Sub_County'],
                    Ward = row['Ward'],
                    Latitude = row['Latitude'],
                    Longitude = row['Longitude'] ,
                    Locality = row['Locality'] ,
                    Start_Outbreak_Event = row['Start_Outbreak_Event'] ,
                    Report_Date = row['Report_Date'] ,
                    Disease_Condition = row['Disease_Condition'] ,
                    Nature_of_Diagnosis = row['Nature_of_Diagnosis'] ,
                    Species_Affected = row['Species_Affected'] ,
                    Number_at_Risk = row['Number_at_Risk'] ,
                    Number_Sick = row['Number_Sick'] ,
                    Number_Dead = row['Number_Dead'] ,
                    Number_Slaughtered = row['Number_Slaughtered'] ,
                    Number_Destroyed = row['Number_Destroyed'] ,
                    Production_System = row['Production_System'] ,
                    Number_Humans_Affected_zoonosis = row['Number_Humans_Affected_zoonosis'] ,
                    Disease_Control_Method = row['Disease_Control_Method'] ,
                    Number_Vaccinated = row['Number_Vaccinated'] ,
                    Organisation_GOK_Private = row['Organisation_GOK_Private'],
                    Source = row['Source']
                )
                session.add(animals)
                print(f"Added new condition: {animals.Disease_Condition}, {animals.County}")

            session.commit()

        except Exception as Error:
            print(f"Population not successful: {Error}")
            session.rollback()

    session.commit()

# Execute the populate_data function
if __name__ == "__main__":
    with app.app_context(): 
        populate_animal_data()        
        
