import streamlit as st
import pandas as pd
import base64
import threading
from openpage import threadFunc
import os

def App1page(session,placeholder1,placeholder2,placeholder3,placeholder4):
    if session==1:
        placeholder1.empty()
        placeholder2.empty()
        placeholder3.empty()
        placeholder4.empty()
    placeholder_module = st.empty()
    placeholder_module = st.markdown("<h1 style='text-align: center; color: white;'>Ciao  </h1>"
                "<h1 style='text-align: center; color: red;'>MusicRec!</h1>", unsafe_allow_html=True)

def main_page():

    placeholder_main1=st.empty()
    placeholder_main2=st.empty()
    placeholder_main3=st.empty()
    placeholder_main4= st.empty()


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


    interface3=st.container()
    with interface3:
        g,h,i,j,k,l,m = st.columns(7)
        with j:
            clicked = st.button("Start")




    if clicked:
        session=1
        App1page(session,placeholder_main1,placeholder_main2,placeholder_main3,placeholder_main4)




if __name__ == "__main__":
    main_page()