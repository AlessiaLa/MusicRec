import streamlit as st


#### TRIAL SCRATCH


page_names = ('Mood', 'Preferences')



"st.session_state object:", st.session_state

@st.cache(suppress_st_warning=True)
def interface():
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
    with interface3:
            g,h,i,j,k,l,m = st.columns(7)
            with j:
                placeholder_main4 = st.empty()
                if placeholder_main4.button("Start"):
                    clicked=True


    if clicked:
        placeholder_main1.empty()
        placeholder_main2.empty()
        placeholder_main3.empty()
        placeholder_main4.empty()
        chosen = st.multiselect("select your page", page_names, key='selected')
        st.write(f'Your choice:{chosen}')


if __name__ == "__main__":
    interface()