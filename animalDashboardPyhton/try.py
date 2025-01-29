import dash
from dash import dcc, html, Input, Output
import plotly.express as px
import geopandas as gpd
import pandas as pd

# Load shapefile and data
kenya_shapefile = gpd.read_file('path_to_kenya_shapefile.shp')
kenya_geojson = kenya_shapefile.to_json()

# Example animal data
data = {
    'County': ['Nairobi', 'Mombasa', 'Kisumu'],  # Example counties
    'Number_Sick': [50, 30, 70],
    'Number_Dead': [10, 5, 20],
    'Number_Vaccinated': [100, 80, 150]
}

animal_df = pd.DataFrame(data)

# Create Dash app
app = dash.Dash(__name__)

app.layout = html.Div([
    dcc.Dropdown(
        id='metric-dropdown',
        options=[
            {'label': 'Number Sick', 'value': 'Number_Sick'},
            {'label': 'Number Dead', 'value': 'Number_Dead'},
            {'label': 'Number Vaccinated', 'value': 'Number_Vaccinated'}
        ],
        value='Number_Sick',  # Default value
        clearable=False
    ),
    dcc.Graph(id='kenya-map')
])

# Callback to update map based on selected metric
@app.callback(
    Output('kenya-map', 'figure'),
    [Input('metric-dropdown', 'value')]
)
def update_map(selected_metric):
    # Merge animal data with shapefile (you might need to join on a common field like County name)
    merged_data = kenya_shapefile.merge(animal_df, left_on='COUNTY_NAME_FIELD', right_on='County')

    # Create the map
    fig = px.choropleth_mapbox(
        merged_data,
        geojson=kenya_geojson,
        locations='COUNTY_NAME_FIELD',  # Replace with actual field name for county
        featureidkey="properties.COUNTY_NAME_FIELD",  # GeoJSON key for the county name
        color=selected_metric,  # Use the selected metric (e.g., Number_Sick, Number_Dead)
        mapbox_style="carto-positron",
        hover_name='COUNTY_NAME_FIELD',  # County name on hover
        hover_data={selected_metric: True},  # Show the metric on hover
        center={"lat": 0.0236, "lon": 37.9062},  # Center the map on Kenya
        zoom=5
    )

    # Add map styling and title
    fig.update_layout(margin={"r":0,"t":0,"l":0,"b":0})

    return fig

@app.callback(
    Output('national-disease-map', 'figure'),
    [Input('disease-dropdown', 'value')]
)
def update_national_map(selected_disease):
    # Filter data based on the selected disease
    filtered_df = df[df['disease'] == selected_disease]

    # Create a choropleth map using Plotly
    fig = px.choropleth(
        filtered_df,
        geojson=kenya_geojson,
        locations='county',  # The column in your CSV that matches the county names
        featureidkey='properties.NAME_1',  # This should match the county field in your GeoJSON
        color='number_of_cases',
        hover_name='county',
        hover_data=['number_of_cases'],
        title=f"Distribution of {selected_disease} in Kenya",
    )
    
    # Update map to focus on Kenya
    fig.update_geos(fitbounds="locations", visible=False)
    
    return fig
