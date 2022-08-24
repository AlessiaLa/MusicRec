@ECHO OFF
pip install --user streamlit
pip install --user -q streamlit==1.3.1
pip install git+https://github.com/yuce/pyswip@master#egg=pyswip
call activate
streamlit run App/interface.py 
PAUSE
