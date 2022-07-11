# insert at 1, 0 is the script path (or '' in REPL)
import os
import queries

def discretization(value):
    parameter=None
    if "Tired" in value:
        parameter='low_energy'
    if "Normal" in value:
        parameter='medium_energy'
    if "Energic" in value:
        parameter='high_energy'
    if "Sad" in value:
        parameter='low_valence'
    if "Flat" in value:
        parameter='medium_valence'
    if "Happy" in value:
        parameter='high_valence'
    if "No" in value:
        parameter='low_danceable'
    if "Maybe..." in value:
        parameter='medium_danceable'
    if "YES!" in value:
        parameter='high_danceable'
    return parameter


def save_values(features):
    mood_parameters=[]
    for value in features:
        mood_parameters.append(discretization(value))
    return mood_parameters


def return_tracks(features):
    list_features=save_values(features)
    valence = list_features[0]
    energy = list_features[1]
    danceability = list_features[2]
    results = queries.getTracksByFeatures(8, danceability, energy, valence)
    results=results[0]['Tracks']
    return results



# if __name__ == '__main__':
#     print(os.getcwd())
#     queries.getTracksByFeatures(10, 'high_danceable', 'low_energy', 'low_valence')