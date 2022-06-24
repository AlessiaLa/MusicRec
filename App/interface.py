import streamlit as st
import pandas as pd
import base64

pages = ['Page1','Page2','Page3']


def App1page():

    st.markdown("<h1 style='text-align: center; color: white;'>Ciao  </h1>"
                "<h1 style='text-align: center; color: red;'>MusicRec!</h1>", unsafe_allow_html=True)

def main_page():
    st.markdown("<h1 style='text-align: center; color: white;'>Welcome to </h1>"
                "<h1 style='text-align: center; color: red;'>MusicRec!</h1>", unsafe_allow_html=True)
    #st.markdown("<h1 style='text-align: center; color: red;'>MusicRec!</h1>", unsafe_allow_html=True)

    _left, lm, mid, mr, _right = st.columns(5)
    with lm:
        st.markdown("![Alt Text](https://media.giphy.com/media/tqfS3mgQU28ko/giphy.gif)")


    st.markdown("<h2 style='text-align: center; color: white;'>Ready to dive into music with me? 🙌 </h2>", unsafe_allow_html=True)

    #st.markdown(f'<h1 style="color:#33ff33;font-size:24px;">{}</h1>', unsafe_allow_html=True)


    #x = st.slider('x')  # 👈 this is a widget
    #st.write(x, 'squared is', x * x)

    _left,mid,ls, _right = st.columns(4)
    with ls:
        clicked = st.button("Start")

    if clicked:
        st.session_state.runpage = App1page()
        #st.session_state.runpage()
        #st.experimental_rerun()


if __name__ == "__main__":
    main_page()