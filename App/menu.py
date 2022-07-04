# Contents of ~/my_app/streamlit_app.py
import streamlit as st
import utilities


"st.session_state object:", st.session_state

def preferences():
    print('wei')



happiness=('Sad', 'Flat', 'Happy')
energy_values = ('Tired', 'Normal', 'Energic')
danceability= ('No', 'Maybe...', 'YES!')

# def menu():
#     if 'selected' not in st.session_state:
#         with st.sidebar:
#             chosen = st.multiselect("select your page", page_names, key='selected')
#     st.write(f'Your choice:{chosen}')
#             # for key in st.session_state.keys():
#             #     print(st.write(st.session_state[key]))

def mood():
    if 'valence' and 'energy' and 'danceable' not in st.session_state:
        mood = st.form(key='mood')
        with mood:
            st.title("How are you feeling today?")
            valence = mood.select_slider("Are you happy?", happiness, key='valence')
            energy = mood.select_slider('Are you energic?', energy_values, key='energy')
            dance = mood.select_slider("Are you in the mood for dancing?", danceability,key='danceable')
            submit = mood.form_submit_button('Submit')
            if submit:
                valence_discretized=utilities.discretization(valence)
                energy_discretized=utilities.discretization(energy)
                dance_discretized=utilities.discretization(dance)
                print(valence_discretized,energy_discretized,dance_discretized)
                preferences()



