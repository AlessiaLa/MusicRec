import string
import sys
sys.path.append('../PrologDialogue')
from kbAccess import PrologInterface
import pandas as pd

def getTracksByFeatures(n: int, dance: str, energy: str, valence: str):
    pi = PrologInterface()
    pi.set_to_consult(['track.pl', 'features.pl', 'utils.pl', 'init.pl'])
    pi.load_rules()
    query = f"getTracksByFeatures({n}, \"{dance}\", \"{energy}\", \"{valence}\", Tracks)."
    print(query)
    result = pi.query(query)
    print(result)


# if __name__ == '__main__':
#     getTracksByFeatures(10, "low_danceable", "high_energy", "low_valence")

