import kbAccess


def getTracksByFeatures(n: int, dance: str, energy: str, valence: str):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['track.pl', 'features.pl', 'utils.pl', 'init.pl', 'tracksuggest.pl'])
    pi.load_rules()
    query = f"getTracksByFeatures({n}, \"{dance}\", \"{energy}\", \"{valence}\", Tracks)."
    result = pi.query(query)
    return result


def getTracksName(trackids: list):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['track.pl', 'utils.pl', 'init.pl',  'tracksuggest.pl'])
    pi.load_rules()
    query = f"getTrackName({trackids}, Tracks)."
    result = pi.query(query)
    return result


def retrieveArtistsByID(tracks: list):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['album_contains.pl', 'utils.pl', 'init.pl', 'published_by.pl', 'album.pl'])
    pi.load_rules()
    query = f"retrieveArtistsByID({tracks}, Artists)."
    result = pi.query(query)
    return result


if __name__ == '__main__':


    trackIds = getTracksByFeatures(10, "high_danceable", "high_energy", "low_valence")[0]['Tracks']
    tracksName = [tracks.replace("-", "").title() for tracks in getTracksName(trackIds)[0]['Tracks']]
    Artists = [artist.title() for artist in retrieveArtistsByID(trackIds)[0]['Artists']]
    result_string = list(map(' - '.join, zip(tracksName, Artists)))
    dict_tracks = {k: v for k, v in zip(result_string, trackIds)}


    # così si mostrano tutte le canzoni con i relativi artisti
    print(dict_tracks.keys())


    mykeys = (list(dict_tracks.keys())[1:3])

    # cosi si ritornano gli id solo per alcune tracce (quelle scelte dall'utente
    print([dict_tracks[x] for x in mykeys])


