import streamlit as st

def mood():
    st.markdown("#Mood ❄️")
    placeholder = st.sidebar.markdown("# Mood ❄")
    with placeholder:
        st.text("Tell me about you")