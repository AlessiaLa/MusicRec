import streamlit as st
import pandas as pd
import base64
import threading
import os
import SessionState
import utilities
import menu
import sys


session_state = SessionState.get(button_start=False, button_submit_mood=False,button_submit_preferences=False,button_submit_sugg_kind=False, colonna_scelta='Seleziona')

happiness = ('Sad', 'Flat', 'Happy')
energy_values = ('Tired', 'Normal', 'Energic')
danceability = ('No', 'Maybe...', 'YES!')
page_names = ('Mood', 'Preferences')
suggest = ('Artists', 'Tracks')

st.set_page_config(page_title = "MusicRec",page_icon = "🔎")


st.markdown("<h1 style='text-align: center; color: black;'>Welcome to </h1>"
                                            "<h1 style='text-align: center; color: red;'>MusicRec!</h1>",
                                            unsafe_allow_html=True)

left_0,center_0,right_0=st.beta_columns([4,11,10])
with center_0:
    st.markdown("![Alt Text](https://media.giphy.com/media/tqfS3mgQU28ko/giphy.gif)",
                                                unsafe_allow_html=True)

st.markdown("<h2 style='text-align: center; color: black;'>Ready to dive into music with me? 🙌 </h2>",
            unsafe_allow_html=True)


left_1, center_1, right_1 =st.beta_columns([10,2,10])
with center_1:
    start_button=st.button('Start')

if start_button:
    session_state.button_start = True


if session_state.button_start:
    st.title("How are you feeling today?")
    valence = st.select_slider("Are you happy?",happiness)
    energy=st.select_slider('Are you energic?',energy_values)
    dance=st.select_slider('Are you in the mood for dancing?', danceability)
    submit_mood = st.button("Submit Mood")
    if submit_mood:
        st.write(f'Your choice:{valence, energy, dance}')
        session_state.valence = valence
        session_state.energy = energy
        session_state.dance = dance
        st.write(session_state.valence,session_state.energy,session_state.dance)
        session_state.button_submit_mood = True
        session_state.suggestions = utilities.return_tracks([session_state.valence, session_state.energy, session_state.dance])

cache_ids=[]
if session_state.button_submit_mood:
    st.title('Give me an idea of your musical tastes...')
    retry = st.button('Load other suggestions')
    if retry:
        session_state.suggestions=utilities.return_tracks([session_state.valence, session_state.energy, session_state.dance])
    preferences = st.multiselect('Select the songs that you like', list(session_state.suggestions.keys()))
    st.title('Do you like any song among these?')
    submit_preferences = st.button("Submit Preferences")
    if submit_preferences:
        for i in preferences:
            cache_ids.append(session_state.suggestions.get(i))
        print(cache_ids)
        session_state.preferences_ids=cache_ids
        #session_state.suggestions_ids = cache_ids
        st.write(f'Your preferences:{preferences}')
        session_state.preferences = preferences
        session_state.button_submit_preferences = True
        #print(session_state.preferences)

if session_state.button_submit_preferences:
    st.title('What kind of suggestion would you like? ')
    sugg_kind = st.radio('Artist or tracks?', suggest)
    submit_sugg_kind= st.button('Submit kind of suggestion')
    if submit_sugg_kind:
        st.write(f'You want a: {sugg_kind} suggestion')
        session_state.button_submit_sugg_kind = True
        session_state.sugg_kind = sugg_kind
        print(session_state.sugg_kind)

if session_state.button_submit_sugg_kind:
    st.title('First suggestion basing on what you liked...')
    if session_state.sugg_kind == 'Artists':
        print('suggestion for artists')
        results=utilities.suggestionArtists(session_state.preferences_ids)
        st.write(list(results))
    if session_state.sugg_kind == 'Tracks':
        results=utilities.suggestionsTracks(session_state.preferences_ids)
        print(list(results.keys()))
        st.write(list(results.keys()))
        print('suggestion for tracks')