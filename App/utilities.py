import streamlit as st
import sys
# insert at 1, 0 is the script path (or '' in REPL)
sys.path.append('/PrologDialogue')
from PrologDialogue import queries
import os

def discretization(value):
    parameter=None
    if "Tired" in value:
        parameter='low_energy'
    if "Normal" in value:
        parameter='medium_energy'
    if "Energic" in value:
        parameter='high_energy'
    if "Sad" in value:
        parameter='low_valence'
    if "Flat" in value:
        parameter='medium_valence'
    if "Happy" in value:
        parameter='high_valence'
    if "No" in value:
        parameter='low_danceable'
    if "Maybe..." in value:
        parameter='medium_danceable'
    if "YES!" in value:
        parameter='high_danceable'
    return parameter


def save_values(features):
    mood_parameters=[]
    for value in features:
        mood_parameters.append(discretization(value))
    return mood_parameters


def return_tracks(features):
    list_features=save_values(features)
    valence=list_features[0]
    energy=list_features[1]
    danceability=list_features[2]
    queries.getTracksByFeatures(7,valence, energy,danceability)


if __name__ == '__main__':
    print(os.getcwd())