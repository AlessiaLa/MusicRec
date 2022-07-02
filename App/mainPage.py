import streamlit as st
import pandas as pd
import base64
import threading
from openpage import threadFunc
import os

def App1page(session,p1,p2,p3,p4):
    if session==1:
        p1.empty()
        p2.empty()
        p3.empty()
        p4.empty()
    placeholder_module = st.empty()
    placeholder_module = st.markdown("<h1 style='text-align: center; color: white;'>Ciao  </h1>"
                "<h1 style='text-align: center; color: red;'>MusicRec!</h1>", unsafe_allow_html=True)

def main_page():
    placeholder_main1=st.empty()
    placeholder_main2=st.empty()
    placeholder_main3=st.empty()
    placeholder_main4 = st.empty()
    placeholder_main1.markdown("<h1 style='text-align: center; color: white;'>Welcome to </h1>"
                "<h1 style='text-align: center; color: red;'>MusicRec!</h1>", unsafe_allow_html=True)
    #st.markdown("<h1 style='text-align: center; color: red;'>MusicRec!</h1>", unsafe_allow_html=True)

    _left, lm, mid, mr, _right = st.columns(5)
    with mid:
        placeholder_main2.markdown("![Alt Text](https://media.giphy.com/media/tqfS3mgQU28ko/giphy.gif)")
        placeholder_main3.markdown("<h2 style='text-align: center; color: white;'>Ready to dive into music with me? 🙌 </h2>", unsafe_allow_html=True)
        clicked = placeholder_main4.button("Start")
    #st.markdown(f'<h1 style="color:#33ff33;font-size:24px;">{}</h1>', unsafe_allow_html=True)


    #x = st.slider('x')  # 👈 this is a widget
    #st.write(x, 'squared is', x * x)

    # _left,mid,ls, _right = st.columns(4)
    # with ls:


    if clicked:
        session=1
        # th = threading.Thread(target=threadFunc)
        # th.start()
        # # address of streamlit page that you want to open after clicking button
        # # os.system('App')
        # # os.system('cd python\Scripts')
        # #os.system(r"streamlit run App/userdialogue.py")
        # th.join()
        App1page(session,placeholder_main1,placeholder_main2,placeholder_main3,placeholder_main4)




if __name__ == "__main__":
    main_page()