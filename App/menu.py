# Contents of ~/my_app/streamlit_app.py
import streamlit as st


def preferences():
    st.write("page preferences")

def mood():
    st.write('page mood')


# def menu():
#     if 'selected' not in st.session_state:
#         with st.sidebar:
#             chosen = st.multiselect("select your page", page_names, key='selected')
#     st.write(f'Your choice:{chosen}')
#             # for key in st.session_state.keys():
#             #     print(st.write(st.session_state[key]))

def main():
    mood = st.form(key='mood')
    with mood:
        st.write("Tell us something about your mood")
        submit = mood.form_submit_button('Submit')
