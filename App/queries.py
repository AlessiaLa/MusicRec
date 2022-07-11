import kbAccess


def getTracksByFeatures(n: int, dance: str, energy: str, valence: str):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['track.pl', 'features.pl', 'utils.pl', 'init.pl', 'tracksuggest.pl'])
    pi.load_rules()
    query = f"getTracksByFeatures({n}, \"{dance}\", \"{energy}\", \"{valence}\", Tracks)."
    result = pi.query(query)
    return result



def getArtistFromTrack(tracks: list, album: list):
    pi = kbAccess.PrologInterface()
    pi.set_to_consult(['track.pl', 'album_contains.pl', 'utils.pl', 'init.pl', 'published_by.pl', 'album.pl'])
    pi.load_rules()
    query = f"retrieveAllArtists({tracks}, {album}, Artists)."
    result = pi.query(query)
    return result


if __name__ == '__main__':
    print(getArtistFromTrack(["angela", "boogie woogie baltimore"], ["cleopatra", "null"]))