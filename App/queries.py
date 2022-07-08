import kbAccess


def getTracksByFeatures(n: int, dance: str, energy: str, valence: str):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['track.pl', 'features.pl', 'utils.pl', 'init.pl'])
    pi.load_rules()
    query = f"getTracksByFeatures({n}, \"{dance}\", \"{energy}\", \"{valence}\", Tracks)."
    result = pi.query(query)
    return result



