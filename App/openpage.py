import streamlit as st
import os
import keyboard
import threading
import time

# wait time to close page and open a new one.
wait_second = 0.01


# thread for closing page
def threadFunc():
    time.sleep(wait_second)
    #keyboard.press_and_release('ctrl+r')


# if st.button('test'):
#     th = threading.Thread(target=threadFunc)
#     th.start()
#     # address of streamlit page that you want to open after clicking button
#     #os.system('App')
#     #os.system('cd python\Scripts')
#     os.system(r"streamlit run App/userdialogue.py")
#     th.join()
