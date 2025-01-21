import os
from flask import Flask
from flask_sqlalchemy import SQLAlchemy
from dotenv import load_dotenv

#  Load the .env file
load_dotenv()

# import the user credentials
user = os.getenv('DB_USER')
password = os.getenv('DB_PASSWORD')
server = os.getenv('DB_SERVER')
db_health = os.getenv('DB_NAME')
port = os.getenv('DB_PORT')

# initialise the database
app = Flask(__name__)
app.config['SQLALCHEMY_DATABASE_URI'] = f'postgresql://{user}:{password}@{server}:{port}/{db_health}'
app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
db = SQLAlchemy(app)

# we will create models to create new tables in the database
class animal_health(db.Model):
    __tablename__ = 'kabs_records'
    record_ID = db.Column(db.Integer, primary_key=True, autoincrement=True)
    County = db.Column(db.String)
    Sub_County = db.Column(db.String)
    Ward = db.Column(db.String)
    Latitude = db.Column(db.BigInteger)
    Longitude = db.Column(db.BigInteger)
    Locality = db.Column(db.String)
    Start_Outbreak_Event = db.Column(db.DATE)
    Report_Date = db.Column(db.DATE)
    Disease_Condition = db.Column(db.String)
    Nature_of_Diagnosis = db.Column(db.String)
    Species_Affected = db.Column(db.String)
    Number_at_Risk = db.Column(db.Integer)
    Number_Sick = db.Column(db.Integer)
    Number_Dead = db.Column(db.Integer)
    Number_Slaughtered = db.Column(db.Integer)
    Number_Destroyed = db.Column(db.Integer)
    Production_System = db.Column(db.String)
    Number_Humans_Affected_zoonosis = db.Column(db.String)
    Disease_Control_Method = db.Column(db.String)
    Number_Vaccinated = db.Column(db.Integer)

if __name__ == '__main__':
    app.run(debug=True)