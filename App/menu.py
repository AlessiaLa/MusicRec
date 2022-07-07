# Contents of ~/my_app/streamlit_app.py
import streamlit as st
import utilities


"st.session_state object:", st.session_state

def preferences(valence,energy, dance):
    print('preferences is called')
    st.write(valence, energy, dance)



happiness = ('Sad', 'Flat', 'Happy')
energy_values = ('Tired', 'Normal', 'Energic')
danceability = ('No', 'Maybe...', 'YES!')

# def menu():
#     if 'selected' not in st.session_state:
#         with st.sidebar:
#             chosen = st.multiselect("select your page", page_names, key='selected')
#     st.write(f'Your choice:{chosen}')
#             # for key in st.session_state.keys():
#             #     print(st.write(st.session_state[key]))



with st.form(key="my_form"):
    st.write("Inside the form")
    slider_val = st.slider(label="Form slider")
    checkbox_val = st.checkbox(label="Form checkbox")
    print(slider_val)
    # Every form must have a submit button.
    submitted = st.form_submit_button(label="Submit")
    print(submitted)
if submitted:
    st.write(f'hello {slider_val}')

st.write("Outside the form")


def mood():
    #if 'valence' and 'energy' and 'danceable' not in st.session_state:
    mood_form = st.form(key='mood',clear_on_submit=False)
    with mood_form:
        st.title("How are you feeling today?")
        valence = mood_form.select_slider("Are you happy?", happiness, key='valence')
        energy = mood_form.select_slider('Are you energic?', energy_values, key='energy')
        dance = mood_form.select_slider("Are you in the mood for dancing?", danceability, key='danceable')
        #st.write(f'Your choice:{valence, energy, dance}')
        for key in st.session_state.keys():
             print(st.write(st.session_state[key]))
        if mood_form.form_submit_button('Submit'):
            # valence_discretized=utilities.discretization(valence)
            # energy_discretized=utilities.discretization(energy)
            # dance_discretized=utilities.discretization(dance)
            print("valence", valence, "energy", energy, "danceability", dance)



