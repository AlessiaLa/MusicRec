import kbAccess


def getTracksByFeatures(n: int, dance: str, energy: str, valence: str):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['init.pl'])
    pi.load_rules()
    query = f"getTracksByFeatures({n}, \"{dance}\", \"{energy}\", \"{valence}\", Tracks)."
    result = pi.query(query)
    return result


def getTracksName(trackids: list):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['init.pl'])
    pi.load_rules()
    query = f"getTrackName({trackids}, Tracks)."
    result = pi.query(query)
    return result


def suggestTracksByGenre(NTracks:int, genres: list):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['init.pl'])
    pi.load_rules()
    query = f"getTrackByGenre({NTracks},{genres}, Tracks)."
    result = pi.query(query)
    return result



def retrieveArtistsByID(tracks: list):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['init.pl'])
    pi.load_rules()
    query = f"retrieveArtistsByID({tracks}, Artists)."
    result = pi.query(query)
    return result

def suggestionTracks(trackids: list, ntracks: int):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['init.pl'])
    pi.load_rules()
    query = f"suggestTrack({trackids}, {ntracks}, NTracks)."
    result = pi.query(query)
    return result

def suggestionArtist(trackids: list, ntracks: int):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['init.pl'])
    pi.load_rules()
    query = f"suggestArtist({trackids}, {ntracks}, N1Artists)."
    result = pi.query(query)
    return result

def retrieveAlbumsByArtist(artists: list):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['init.pl'])
    pi.load_rules()
    query = f"retrieveAlbumByArtist({artists}, Albums)."
    result = pi.query(query)
    return result

def retrieveAlbumsByTrack(trackids: list):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['init.pl'])
    pi.load_rules()
    query = f"retrieveAlbumByTrack({trackids}, Albums)."
    result = pi.query(query)
    return result

if __name__ == '__main__':

    trackIds = getTracksByFeatures(10, "high_danceable", "high_energy", "low_valence")[0]['Tracks']
    print(trackIds)
    print('----')
    tracksName = [tracks.replace("-", "").title() for tracks in getTracksName(trackIds)[0]['Tracks']]
    print(tracksName)
    print('----')
    Artists = [artist.title() for artist in retrieveArtistsByID(trackIds)[0]['Artists']]
    print(Artists)
    print('----')
    result_string = list(map(' - '.join, zip(tracksName, Artists)))
    print(result_string)
    print('----')
    dict_tracks = {k: v for k, v in zip(result_string, trackIds)}
    print(dict_tracks)


    mykeys = (list(dict_tracks.keys()))

    print([dict_tracks[x] for x in mykeys])





