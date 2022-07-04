import streamlit as st

def discretization(value):
    print(value)
    parameter=None
    if "Tired" in value:
        parameter='low_energy'
    if "Normal" in value:
        parameter='medium_energy'
    if "Energic" in value:
        parameter='high_energy'
    if "Sad" in value:
        parameter='low_valence'
    if "Normal" in value:
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


