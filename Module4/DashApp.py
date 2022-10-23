###Load Libraries
import pandas as pd
import plotly.express as px
from plotly.subplots import make_subplots
import plotly.graph_objects as go 

import dash
import dash_html_components as html
import dash_core_components as dcc
from dash.dependencies import Input, Output

###End Load Libraries


###Load Data

#1 data
soql_1 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,count(spc_common)' +\
        '&$group=spc_common,boroname,health')
df = pd.read_json(soql_1 + '&$limit=700000')
df_1 = df.dropna() #drop NA values
df_1 = df_1.rename(columns={"spc_common": "species", "boroname": "borough", "count_spc_common": "count"})

#2 data
soql_2 = ('https://data.cityofnewyork.us/resource/nwxe-4ae8.json?' +\
        '$select=spc_common,boroname,health,steward,count(spc_common)' +\
        '&$group=spc_common,boroname,health,steward')
df2 = pd.read_json(soql_2 + '&$limit=700000')
df_2 = df2.dropna()
df_2 = df_2.rename(columns={"spc_common": "species", "boroname": "borough", "count_spc_common":"count"})

def new_col(row):
  if row['steward'] == 'None':
    return 'No'
  else :
    return 'Yes'

df_2['steward_present'] = df_2.apply(new_col, axis=1)

df_3 = df_2.copy().drop(['steward'], axis=1)

df_4 = df_3.groupby(['species','borough','health','steward_present'])['count'].sum().to_frame().reset_index()

###End load data


###Static Viz
#1
species = 'American beech' #only dynamic filter needed in the dash would be tree type
df_plot1 = df_1[(df_1['species'] == species)] 

fig = px.histogram(df_plot1, x='borough', y='count', color='health', #colors depend on health rating
                   category_orders={"health": ["Good","Fair", "Poor"]}, #organize health rating values
                   color_discrete_sequence=["#109618", "#FF9900", "#DC3912"], #assign and orgnize colors to match ratings
title='American Beech tree Health throughout the 5 Boroughs',
labels={"borough": "Borough","count": "Count"})

fig.update_xaxes(type='category', title_text='Borough', showline=True, linewidth=.5, linecolor='lightgrey')
fig.update_yaxes(title_text='Tree Count', showline=True, linewidth=.2, linecolor='lightgrey', gridwidth=.2 ,gridcolor='lightgrey')
fig.update_layout(barmode='group', #plots all boroughs in a grouped bar chart for the selected tree type
                  xaxis={'categoryorder':'total descending'}, 
                  legend_title='Health Status', 
                  plot_bgcolor= 'rgba(0, 0, 0, 0)')

fig.show()

#2
species = 'American beech' #only dynamic filter needed in the dash would be tree type
df_plot2 = df_4[(df_4['species'] == species)] 

rating_color = {"#FF9900":"Fair","#109618":"Good","#DC3912": "Poor"}

fig = go.Figure()

fig.update_layout(barmode='stack', width=900, height=800, uniformtext_minsize=8, 
                  uniformtext_mode='hide', plot_bgcolor= 'rgba(0, 0, 0, 0)', 
                  legend_title='Health Status')
fig.update_xaxes(linewidth=.5, linecolor='lightgrey')
fig.update_yaxes(linewidth=.2, linecolor='lightgrey', gridwidth=.2 ,gridcolor='lightgrey')

for r, c in zip(df_plot2['health'].unique(), rating_color):
    df_plot = df_plot2[df_plot2['health'] == r]
    fig.add_trace(
        go.Bar(
            x=[df_plot['borough'], df_plot['steward_present']],
            y=df_plot['count'],
            marker_color=c,
            name=r,
            text=df_plot['count'],
            width=0.45,
            textposition='auto')
    )

fig.show()

###End Static Viz


### ---------- Dash ---------- ###
app = dash.Dash(__name__, external_stylesheets='yeti')

species = df_1['species'].unique()

app.layout = html.Div(children=[
    html.H1(children = 'NYC Tree Health and Stewardship Effectiveness'),
    html.P(children = 'Tree health and stewardship impact plots are generated after selecting a tree species. Data obtained from the NYC open data using SOQL commands.'),    
    html.P(children = 'Select a tree species:'),
    dcc.Dropdown(
        id='species',
        options=[{'label': s, 'value': s} for s in species],
        value=species[0] #default
    ),
    #Q1
    html.Div(id='health-app'),
    html.H2(children = 'Tree Health comparison'),
    dcc.Graph(id='health-bar'),
    #Q2
    html.Div(id='steward-app'),
    html.H2(children = 'Impact of Stewardship on Tree Health'),
    dcc.Graph(id='steward'),
    html.P(children = 'This visualization can be used to compare stewardship effectivness on tree health across the 5 boroughs'),

])

##Q1##

@app.callback(
    dash.dependencies.Output('health-bar', 'figure'), 
    [dash.dependencies.Input('species', 'value')]
)

def health_app(species):
    #filter 
    df_plot1 = df_1[(df_1['species'] == species)] 
    
    #plot begin
    fig = px.histogram(df_plot1, x='borough', y='count', color='health', #colors depend on health rating
                   category_orders={"health": ["Good","Fair", "Poor"]}, #organize health rating values
                   color_discrete_sequence=["#109618", "#FF9900", "#DC3912"], #assign and orgnize colors to match ratings
    title='American Beech tree Health throughout the 5 Boroughs',
    labels={"borough": "Borough","count": "Count"})

    fig.update_xaxes(type='category', title_text='Borough', showline=True, linewidth=.5, linecolor='lightgrey')
    fig.update_yaxes(title_text='Tree Count', showline=True, linewidth=.2, linecolor='lightgrey', gridwidth=.2 ,gridcolor='lightgrey')
    fig.update_layout(barmode='group', #plots all boroughs in a grouped bar chart for the selected tree type
                  xaxis={'categoryorder':'total descending'}, 
                  legend_title='Health Status', 
                  plot_bgcolor= 'rgba(0, 0, 0, 0)')
    return fig
    #plot end

##Q2##

@app.callback(
    dash.dependencies.Output('steward', 'figure'), 
    [dash.dependencies.Input('species', 'value')]
)

def species_app(species):
    #filter
    df_plot2 = df_4[(df_4['species'] == species)]
    
    #begin plot

    rating_color = {"#FF9900":"Fair","#109618":"Good","#DC3912": "Poor"}

    fig2 = go.Figure()

    fig2.update_layout(barmode='stack', width=900, height=800, uniformtext_minsize=8, 
                  uniformtext_mode='hide', plot_bgcolor= 'rgba(0, 0, 0, 0)', 
                  legend_title='Health Status')
    fig2.update_xaxes(linewidth=.5, linecolor='lightgrey')
    fig2.update_yaxes(linewidth=.2, linecolor='lightgrey', gridwidth=.2 ,gridcolor='lightgrey')

    for r, c in zip(df_plot2['health'].unique(), rating_color):
        df_plot = df_plot2[df_plot2['health'] == r]
        fig.add_trace(
            go.Bar(
                x=[df_plot['borough'], df_plot['steward_present']],
                y=df_plot['count'],
                marker_color=c,
                name=r,
                text=df_plot['count'],
                width=0.45,
                textposition='auto')
        )

        
    return fig2

    #end plot

if __name__ == '__main__':
       app.run_server(debug=False)
