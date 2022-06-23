import streamlit as st
import pandas as pd


def header(text):
    st.markdown(f'<p style="background-color:#0066cc;color:#33ff33;font-size:24px;border-radius:2%;">{text}</p>',
                unsafe_allow_html=True)



st.title("Welcome in MusicRec")
#st.markdown(f'<h1 style="color:#33ff33;font-size:24px;">{}</h1>', unsafe_allow_html=True)
st.write("Are you ready to dive into music world with me? 🙌 ")

#x = st.slider('x')  # 👈 this is a widget
#st.write(x, 'squared is', x * x)

clicked = st.button("Yes!")

