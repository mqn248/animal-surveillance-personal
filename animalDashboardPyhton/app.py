"""
Name: Kariithi Anne
Project: Animal Surveillance Dashboard
Version: Trial 1 with dash for python

Objective: We want to create an interactiove map 
for the Department of Veterinary Services. This is 
to track animal surveillance in the counrty.

Best regards. Cheers to this new journey.

Tool: Dash, Flask, PostgreSQL
"""
import dash
import requests
import pandas as pd
import geopandas as gpd
import plotly_express as px
from dash import dcc, html, Input, Output
from bs4 import BeautifulSoup

"""
Load kenya shapefiles and vds data
"""
animal = pd.read_csv('D:/CEMA PROJECTS/animalHealthSurveillance/animal-surveillance/animalDashboardPyhton/data/aggregate_groupings.csv')
kenya = gpd.read_file('D:/CEMA PROJECTS/animalHealthSurveillance/animal-surveillance/animalDashboardPyhton/clean shapefiles/county_shapefile.shp')
kenya_geojson = kenya.to_json()

app = dash.Dash(__name__, suppress_callback_exceptions=True)

"""
First part is to define the dashboard layout.
    - About/Summary page
        - World News(Fetch from animal news page)
        - Local News(Will be added with time)
    - Biosurveillance tab
        - National Summary(by Country and filter by disease)
        - County summary (filter by county and disease)
    - Animal Diseases
        - Introduction Page
            - Summary of the different breeds of cattle
            - Map (filter by Different species, Production System, 
                Nature of Diagnosis)    
        - Priority Diseases tab
            - Anthrax
                - Frequency of occurrence per county
                - Maps to show anthrax distribution
                - Trend analysis(Start_date and Report_Date)                    
            - Brucellosis
"""

app.layout = html.Div([
    # Track the Url
    dcc.Location(
        id = "url",
        refresh = False
    ),

    # Create our Nav bar
    html.Div(
        className = "navbar",
        children = [
            html.A("About", href = "/", className = "nav-link"),            
            html.A("Animal Biosurveillance", href= "/biosurv", className ="nav-link"),
            html.A("Diseases", href = "/diseases", className = "nav-link"),
        ],
        style ={
            'position': 'fixed',
            'top': '0',
            'width': '100%',
            'z-index': '1000',
            'background-color': '#007BFF',  
            'padding': '10px',
            'color': 'white',
            'display': 'flex',  
            'justify-content': 'center',  
            'align-items': 'center',
            'gap': '20px' 
        }
    ),

    html.Div(id = "page-content", style = {
        'padding-top' : '20px'
    })
])

"""
Callbacks for the navbar tabs
"""
@app.callback(
    Output('page-content', 'children'),
    [Input('url', 'pathname')])

def animal_surveillance(pathname):    
    if pathname == '/':        
        return html.Div([
            html.H2("World and National Updates"),

            # Side Navbar with two tabs (World News, Local News)
            dcc.Tabs(id='about-tabs', value='tab-summary', children=[
                dcc.Tab(label='World News', value='world-news', className='side-tab'),
                dcc.Tab(label='Kenya News', value='local-news', className='side-tab'),
            ], style={
                'display': 'flex',
                'flexDirection': 'row', 
                'width': '100%', 
                'background-color': '#f8f9fa'
                }),
            
            # Content area for selected tab
            html.Div(id='about-content', 
            style={
                'padding': '20px', 
                'background-color': 'white', 
                'box-shadow': '0px 4px 8px rgba(0, 0, 0, 0.1)'})
        ])
    elif pathname == '/biosurv':
       return html.Div([
            html.H1('Animal Biosurveillance Summary'),
            html.P("National and County summary of animal surveillance in Kenya"),
            
            # National and County summary tabs
            dcc.Tabs(id='biosurv-tabs', value='animal-summary', children=[
                dcc.Tab(label='National Summary', value='a-glance', className='side-tab'),
                dcc.Tab(label='County Summary', value='county-glance', className='side-tab'),
            ], style={
                'display': 'flex',
                'flexDirection': 'row', 
                'width': '100%', 
                'background-color': '#f8f9fa'
                }),
            
            # Content area for selected tab
            html.Div(id='biosurv-content', 
            style={
                'padding': '20px', 
                'background-color': 'white', 
                'box-shadow': '0px 4px 8px rgba(0, 0, 0, 0.1)'})
        ]) 
    elif pathname == "/diseases":
         return html.Div([
            html.H1('Animal Diseases Summary'),
            html.P("Display all the surveillance on priority disease in the country")
        ])           
    else:
        return html.Div([
             html.H2("Page not found")                                         
        ])   

"""
callbacks for the about page
"""
@app.callback(
    Output('about-content', 'children'),
    [Input('about-tabs', 'value')]
)
def about_content(selected_tab):
    if selected_tab == 'world-news':        
        return html.Div([
            html.H3("World Animal Health News"),
            html.Iframe(
                src="https://www.woah.org/en/home/", 
                style={
                    "width": "100%",
                    "height": "600px"}),    
        ])
     
    elif selected_tab == 'local-news':
        return html.Div([
            html.H3("Local Animal Health News - Kenya"),
            html.P("In Kenya, animal health is a critical component of food security and rural livelihoods. "
                   "The government has been working with local and international partners to combat zoonotic diseases, "
                   "improve veterinary services, and promote biosurveillance in various counties."),
        ])

"""
callbacks for the biosurveillance page
"""
@app.callback(
    Output('biosurv-content', 'children'),
    [Input('biosurv-tabs','value')]
)

def biosurv_content(selected_tab):
    if selected_tab == 'a-glance':
        return html.Div([
            html.H2("National Summary"),

            # selectInput disease names
            dcc.Dropdown(
                id = 'disease-dropdown',
                options=[{
                    'label': str(Disease),
                    'value': str(Disease)
                }
                for Disease in animal['Disease_Condition'].unique()],
                value = str(animal['Disease_Condition'].unique()[0]),
                clearable = False,
            ),

            # plot map
            dcc.Graph(
                id = 'national-disease',
            )
        ])
    elif selected_tab == 'county-glance':
        return html.Div([
            html.H3("County Summary"),
        ])

"""
callbacks for national summary
"""
@app.callback(
    Output('national-disease', 'figure'),
    [Input('disease-dropdown', 'value')]
)

def update_national(selected_disease): 
    filtered_data = animal[animal['Disease_Condition'] == selected_disease]   
    animal_data = kenya.merge(filtered_data, left_on='county', right_on='County', how = 'left')
    
    # check if data is empty , if not return map
    if animal_data.empty:
        return px.choropleth()

    # normalise the data
    animal_data['County'] = animal_data['County'].str.lower()
    kenya['county'] = kenya['county'].str.lower()

    # handle missing values
    animal_data['Total_Number_Risk'] = animal_data['Total_Number_Risk'].fillna(0)
    animal_data['Total_Vaccinated'] = animal_data['Total_Vaccinated'].fillna(0)

    fig = px.choropleth(
        animal_data,
        geojson=kenya_geojson,
        locations='county', 
        featureidkey='properties.county',  
        color='Total_Number_Risk',  
        hover_name='County',  
        hover_data={'Total_Number_Risk': True, 'Total_Vaccinated': True},
        title=f"Distribution of {selected_disease} in Kenya",
        center={"lat": 0.0236, "lon": 37.9062}
    )

    # focus on Kenya
    fig.update_geos(fitbounds="locations", visible=False)

    fig.update_layout(
        coloraxis_colorbar={
            'title': f'Number at Risk from {selected_disease}'
        },
        margin={"r":0,"t":0,"l":0,"b":0}  
    )
    
    return fig   


if __name__ == '__main__':
    app.run_server(debug=True)


            