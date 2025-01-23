# import libraries
from sqlalchemy import create_engine
import pandas as pd
import os

# define the access fields
#aws
pass_d = "c3mA_hUb"
user = "postgres"
server = "animalsurveillance.craswukoq326.us-east-1.rds.amazonaws.com"
db = "postgres"
port = "5432"

dirr = 'D:/CEMA PROJECTS/animalHealthSurveillance/animal-surveillance/dummy_data/dummy'


# creating an extract function specific for excel files only

def extract():
    # do a try and catch function
    # create a variable directory and store the file path
    try:
        directory = dirr
        # do a loop and scan through the files in the directory to checking for csv files only
        for filename in os.listdir(directory):
            # extract the file name without the extension
            file_wo_ext = os.path.splitext(filename)[0]
            # check if the file is a csv file then proceed to process
            if filename.endswith(".xlsx"):
                file = os.path.join(directory, filename)
                # check if it is a file the read it with pandas
                if os.path.isfile(file):
                    read_file = pd.read_excel(file)
                    # call the load function to load the read dataframe into the database
                    load(read_file, file_wo_ext)
                    print("Data extracted successfully")
    # in the case of an error
    except Exception as e:
        print("Data extract error: " + str(e))


# define the load function to the postgres database
def load(read_file, tbl):
    try:
        # load all the road in the dataframe
        rows_imported = 0
        # create an engine to link to the dB
        engine = create_engine(f'postgresql://{user}:{pass_d}@{server}:{port}/{db}')
        print(f'importing rows {rows_imported} to {rows_imported + len(read_file)}')

        # save the imported data to postgres as a table, create new tables if not existing
        read_file.to_sql(f"{tbl}", engine, if_exists='replace', index=False)
        # iterate over the length of the extracted file
        rows_imported += len(read_file)
        # after successful load
        print("Data loaded successfully")

    except Exception as e:
        print("Data load error: " + str(e))


# Now we need to call the extract function
try:
    read_file = extract()
except Exception as e:
    print("Error while extracting data: " + str(e))
