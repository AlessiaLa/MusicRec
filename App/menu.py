# Contents of ~/my_app/streamlit_app.py
import streamlit as st
from streamlit_option_menu import option_menu



def preferences():
    st.write("page preferences")

def mood():
    st.write('page mood')

def new():
    st.write("try")

page_names = ['Mood', 'Preferences']



# def main():
#     selected_page = st.sidebar.multiselect("Select a page", page_names)
#     print(selected_page)
#     if selected_page=="Mood":
#         mood()
#     if selected_page=="Preferences":
#         preferences()
#     else:
#       new()


def main():

        with st.sidebar:
            choose = option_menu("App Gallery",
                                 ["About", "Photo Editing", "Project Planning", "Python e-Course", "Contact"],
                                 icons=['house', 'camera fill', 'kanban', 'book', 'person lines fill'],
                                 menu_icon="app-indicator", default_index=0,
                                 styles={
                                     "container": {"padding": "5!important", "background-color": "#fafafa"},
                                     "icon": {"color": "orange", "font-size": "25px"},
                                     "nav-link": {"font-size": "16px", "text-align": "left", "margin": "0px",
                                                  "--hover-color": "#eee"},
                                     "nav-link-selected": {"background-color": "#02ab21"},
                                 }
                                 )
            if choose not in st.session_state:
                st.session_state['page'] = choose
            print(choose)
            if choose=="About":
                preferences()