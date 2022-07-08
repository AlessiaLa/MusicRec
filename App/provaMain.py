import streamlit as st
import pandas as pd
import base64
import threading
import os
import SessionState
import utilities
import menu


session_state = SessionState.get(button_start=False, button_submit_mood=False, colonna_scelta='Seleziona')

happiness = ('Sad', 'Flat', 'Happy')
energy_values = ('Tired', 'Normal', 'Energic')
danceability = ('No', 'Maybe...', 'YES!')
page_names = ('Mood', 'Preferences')

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
    print('mood')
    st.title("How are you feeling today?")
    valence = st.select_slider("Are you happy?",happiness)
    energy=st.select_slider('Are you energic?',energy_values)
    dance=st.select_slider('Are you in the mood for dancing?', danceability)
    submitted = st.button("Submit")
    if submitted:
        st.write(f'Your choice:{valence, energy, dance}')
        session_state.button_submit_mood=True
        utilities.save_values([valence,energy,dance])
    st.write("Outside the form")

if session_state.button_submit_mood:
    print('preferences')
    st.write('the system will search for suggestions')