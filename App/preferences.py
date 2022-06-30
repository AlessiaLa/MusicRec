import streamlit as st

def preferences():
    st.set_page_config(page_title="Preferences", page_icon="📈")
    st.markdown("# Preferences")
    st.text("What are your preferences?")
    placeholder = st.sidebar.markdown("# Preferences 🎉")
    with placeholder:
        st.text("Tell me about you")