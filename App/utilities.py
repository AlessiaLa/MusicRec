# insert at 1, 0 is the script path (or '' in REPL)
import os
import queries

def discretization(value):
    if "Tired" in value:
        value='low_energy'
    if "Normal" in value:
        value='medium_energy'
    if "Energic" in value:
        value='high_energy'
    if "Sad" in value:
        value='low_valence'
    if "Flat" in value:
        value='medium_valence'
    if "Happy" in value:
        value='high_valence'
    if "No" in value:
        value='low_danceable'
    if "Maybe..." in value:
        value='medium_danceable'
    if "YES!" in value:
        value='high_danceable'
    return value


def save_values(features):
    mood_parameters=[]
    for value in features:
        mood_parameters.append(discretization(value))
    return mood_parameters

def dict_to_tracklist(dict_tracks):
    tracklist = (list(dict_tracks.keys()))
    return tracklist

def return_tracks(features):
    list_features=save_values(features)
    valence = list_features[0]
    energy = list_features[1]
    danceability = list_features[2]
    trackIds = queries.getTracksByFeatures(8, danceability, energy, valence)[0]['Tracks']
    tracksName = [tracks.replace("-", "").title() for tracks in queries.getTracksName(trackIds)[0]['Tracks']]
    Artists = [artist.title() for artist in queries.retrieveArtistsByID(trackIds)[0]['Artists']]
    result_string = list(map(' - '.join, zip(tracksName, Artists)))
    dict_tracks = {k: v for k, v in zip(result_string, trackIds)}
    tracklist = dict_to_tracklist(dict_tracks)
    return tracklist




# if __name__ == '__main__':
#     print(os.getcwd())
#     wtf=return_tracks(['low_valence','low_energy','high_danceable'])
#     print(wtf)