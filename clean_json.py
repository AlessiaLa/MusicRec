import json
import os

path = r"C:\Users\user\Desktop\AI-Project\MusicNet\dataset\tracks_no_discretization.json"

with open(path, "r", encoding="utf-8") as f:
    data = json.loads("[" +
                      f.read().replace("}{", "},\n{") +
                       "]")



for i, node in enumerate(data):
    feature = []

    if float(data[i]['features'][1]) <= 0.33:
        feature.append('low_danceable')
    elif float(data[i]['features'][1]) >= 0.66:
        feature.append('high_danceable')
    else:
        feature.append('medium_danceable')

    if float(data[i]['features'][2]) <= 0.33:
        feature.append('low_energy')
    elif float(data[i]['features'][2]) >= 0.66:
        feature.append('high_energy')
    else:
        feature.append('medium_energy')

    if float(data[i]['features'][6]) <= 0.33:
        feature.append('low_speechiness')
    elif float(data[i]['features'][6]) >= 0.66:
        feature.append('high_speechiness')
    else:
        feature.append('medium_speechiness')

    if float(data[i]['features'][7]) <= 0.33:
        feature.append('low_acoustic')
    elif float(data[i]['features'][7]) >= 0.66:
        feature.append('high_acoustic')
    else:
        feature.append('medium_acoustic')

    if float(data[i]['features'][8]) < 0.5:
        feature.append('low_instrumental')
    elif float(data[i]['features'][8]) >= 0.75:
        feature.append('high_instrumental')
    else:
        feature.append('medium_instrumental')

    if float(data[i]['features'][9]) >= 0.8:
        feature.append('live_track')
    else:
        feature.append('studio_track')

    if float(data[i]['features'][10]) <= 0.33:
        feature.append('low_valence')
    elif float(data[i]['features'][10]) >= 0.66:
        feature.append('high_valence')
    else:
        feature.append('medium_valence')

    if float(data[i]['features'][11]) < 76:
        feature.append('slow')
    elif float(data[i]['features'][11]) >= 168:
        feature.append('fast')
    else:
        feature.append('moderate')

    data[i].update({'features': feature})
    print(data[i])

path = r"C:\Users\user\Desktop\AI-Project\MusicNet\dataset\tracks.json"
with open(path, 'w') as fp:
    json.dump(data, fp)