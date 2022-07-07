import streamlit as st
import pandas as pd
import base64
import threading
import os
import SessionState
import menu


session_state = SessionState.get(button_carica=False, colonna_scelta='Seleziona')

happiness = ('Sad', 'Flat', 'Happy')
energy_values = ('Tired', 'Normal', 'Energic')
danceability = ('No', 'Maybe...', 'YES!')
page_names = ('Mood', 'Preferences')


st.markdown("<h1 style='text-align: center; color: white;'>Welcome to </h1>"
                                            "<h1 style='text-align: center; color: red;'>MusicRec!</h1>",
                                            unsafe_allow_html=True)
st.markdown("![Alt Text](https://media.giphy.com/media/tqfS3mgQU28ko/giphy.gif)",
                                            unsafe_allow_html=True)

st.markdown("<h2 style='text-align: center; color: white;'>Ready to dive into music with me? 🙌 </h2>",
            unsafe_allow_html=True)

start_button=st.button('Start')

if start_button:
    session_state.button_carica = True


if session_state.button_carica:
    print('hello')
    st.write("Inside the form")
    slider_val = st.slider("Form slider")
    checkbox_val = st.checkbox("Form checkbox")  # Every form must have a submit button.
    submitted = st.button("Submit")
    if submitted:
        st.write("slider", slider_val, "checkbox", checkbox_val)
        menu.preferences()
    st.write("Outside the form")


