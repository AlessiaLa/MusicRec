
import json
import os
from collections import Counter

def get_list_from_string(string: str):
    string = string.replace('[', '')
    string = string.replace(']', '')
    string = string.replace('\"', '')
    if string == '':
        return []
    return string.replace(", ", ",").split(",")

def open_json(path):
    with open(path, "r", encoding="utf-8") as f:
        data = json.loads("[" +
                      f.read().replace("}{", "},\n{") +
                       "]")
    return data

def open_json_list(path):
    with open(path, "r", encoding="utf-8") as f:
        data = json.loads(f.read())
    return data

def save_json(path):
    path_saving = os.path.join(os.getcwd(), path)
    with open(path_saving, 'w') as fp:
        json.dump(data, fp)

def find_dupes(data):

    album_list = []
    seen = set()
    for node in data:
        for k,value in node.items():
            if k == 'albumid':
                node[k] = ''.join(str(x) for x in node[k])
            if k == 'artistnames':
                node[k] = ''.join(str(x) for x in node[k])
                node[k] = node[k].lower()
            if k == 'albumname':
                node[k] = ''.join(str(x) for x in node[k])
                node[k] = node[k].lower()

        album_list.append((node['albumname'], node['artistnames'], node['albumid']))

    dupes = [(k,a,id) if (k,a) in seen else seen.add((k,a)) for k,a, id in album_list]
    print(dupes)
    dupes = list(filter(None, dupes))
    album_dupes = [id for k,a, id in dupes]
    album_dupes = list(filter(None, album_dupes))

    return album_dupes, dupes

def delete_duplicate(data, album_dupes):
    for i, node in enumerate(data):
        for k,value in node.items():
            if k == 'albumid' and value in album_dupes:
                del data[i]
    return data

if __name__ == "__main__":

    path = os.path.join(os.getcwd(), "Dataset\\json_to_neo\\with_duplicate\\albums.json")
    path_art = os.path.join(os.getcwd(), "Dataset\\json_to_neo\\with_duplicate\\artists.json")
    path_track = os.path.join(os.getcwd(), "Dataset\\json_to_neo\\with_duplicate\\tracks_discretized.json")
    data = open_json(path)
    album_dupes, dupes = find_dupes(data)
    data = delete_duplicate(data, album_dupes)
    save_json( "Dataset\\json_to_neo\\albums.json")

    data = open_json(path_art)
    for node in data:
        for k,value in node.items():
            if k == 'artistname':
                node[k] = ''.join(str(x) for x in node[k])
                node[k] = node[k].lower()
            if k == 'albumid':
                node[k] = [x for x in node[k] if x not in album_dupes]


    save_json("Dataset\\json_to_neo\\artists.json")

    data = open_json_list(path_track)
    for i, node in enumerate(data):
        for k,value in node.items():
            if k == 'artistnames':
                node[k] = ''.join(str(x) for x in node[k])
                node[k] = node[k].lower()
            if k == 'albumname':
                node[k] = ''.join(str(x) for x in node[k])
                node[k] = node[k].lower()
            if k == 'trackname':
                node[k] = ''.join(str(x) for x in node[k])
                node[k] = node[k].lower()

        for album, artist, id in dupes:
            if node['artistnames'] == artist and node['albumname'] == album:
                print(node ['artistnames'], node['albumname'], node['trackname'])
                del data[i]

    save_json("Dataset\\json_to_neo\\tracks.json")