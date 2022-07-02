# Contents of ~/my_app/streamlit_app.py
import streamlit as st
from streamlit.scriptrunner import RerunException, RerunData

"st.session_state object:",st.session_state

def preferences():
    st.write("page preferences")

def mood():
    st.write('page mood')

def new():
    st.write("try")

page_names = ('Mood', 'Preferences')

def main():
    if 'selected' not in st.session_state:
        with st.sidebar:
            selected_page = st.sidebar.multiselect("Select a page", page_names)
            st.session_state.selected=selected_page
            print(selected_page)
            if selected_page=="Mood":
                mood()
            if selected_page=="Preferences":
                preferences()
            else:
              new()

#"st.session_state object:", st.session_state



# def main():
#     st.session_state.dropbox=None
#     if 'dropbox' not in st.session_state:
#         with st.sidebar:
#             st.session_state.dropbox = st.multiselect("Tell me about your...", page_names)
#             print(st.session_state.dropbox)
#         if "Preferences" in st.session_state.dropbox:
#             preferences()
#


    # last_y1 = st.session_state.y1
    # st.session_state.y1 = st.multiselect(
    #     "Select year(s)",
    #     ['2013', '2014', '2015', '2016', '2017', '2018', '2019'],
    #     st.session_state.y1)
    # print(st.session_state.y1)

