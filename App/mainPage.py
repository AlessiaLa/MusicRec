import streamlit as st
import pandas as pd
import base64
import threading
import os
import menu

page_names = ('Mood', 'Preferences')

# def menu():
#     if 'selected' not in st.session_state:
#         with st.sidebar:
#             chosen = st.multiselect("select your page", page_names, key='selected')
#     st.write(f'Your choice:{chosen}')
#     # for key in st.session_state.keys():
#     #     print(st.write(st.session_state[key]))




interface1=st.container()
with interface1:
    l, m, r = interface1.columns(3)
    with m:
        placeholder_main1 = st.markdown("<h1 style='text-align: center; color: white;'>Welcome to </h1>"
                                        "<h1 style='text-align: center; color: red;'>MusicRec!</h1>",
                                        unsafe_allow_html=True)

interface2=st.container()
with interface2:
    a,b,c,d,e,f=st.columns(6)
    with b:
        placeholder_main2 = st.markdown("![Alt Text](https://media.giphy.com/media/tqfS3mgQU28ko/giphy.gif)",
                                        unsafe_allow_html=True)
    placeholder_main3 = st.markdown(
        "<h2 style='text-align: center; color: white;'>Ready to dive into music with me? 🙌 </h2>",
        unsafe_allow_html=True)


interface3=st.empty()
clicked=False
if 'start' not in st.session_state:
    with interface3:
            g,h,i,j,k,l,m = st.columns(7)
            with j:
                placeholder_main4 = st.empty()
                if placeholder_main4.button("Start"):
                    clicked=True
                    st.session_state.start=True

    if clicked:
        placeholder_main1.empty()
        placeholder_main2.empty()
        placeholder_main3.empty()
        placeholder_main4.empty()
        menu.main()


