import json
import os

path = os.path.join(os.getcwd(), r"Dataset\json_to_neo\tracks.json")

with open(path, "r", encoding="utf-8") as f:
    data = json.loads(f.read())
#print(data[0][1])
for i, node in enumerate(data[0]):

    feature = []
    print(node['features'])
    if float(node['features'][1]) <= 0.33:
        feature.append('low_danceable')
    elif float(node['features'][1]) >= 0.66:
        feature.append('high_danceable')
    else:
        feature.append('medium_danceable')

    if float(node['features'][2]) <= 0.33:
        feature.append('low_energy')
    elif float(node['features'][2]) >= 0.66:
        feature.append('high_energy')
    else:
        feature.append('medium_energy')

    if float(node['features'][6]) <= 0.33:
        feature.append('low_speechiness')
    elif float(node['features'][6]) >= 0.66:
        feature.append('high_speechiness')
    else:
        feature.append('medium_speechiness')

    if float(node['features'][7]) <= 0.33:
        feature.append('low_acoustic')
    elif float(node['features'][7]) >= 0.66:
        feature.append('high_acoustic')
    else:
        feature.append('medium_acoustic')

    if float(node['features'][8]) < 0.5:
        feature.append('low_instrumental')
    elif float(node['features'][8]) >= 0.75:
        feature.append('high_instrumental')
    else:
        feature.append('medium_instrumental')

    if float(node['features'][9]) >= 0.8:
        feature.append('live_track')
    else:
        feature.append('studio_track')

    if float(node['features'][10]) <= 0.33:
        feature.append('low_valence')
    elif float(node['features'][10]) >= 0.66:
        feature.append('high_valence')
    else:
        feature.append('medium_valence')

    if float(node['features'][11]) < 76:
        feature.append('slow')
    elif float(node['features'][11]) >= 168:
        feature.append('fast')
    else:
        feature.append('moderate')

    node.update({'features': feature})

print(data[0])

path_saving = os.path.join(os.getcwd(), r"Dataset\json_to_neo\tracks_discretized.json")
with open(path_saving, 'w') as fp:
    json.dump(data, fp)