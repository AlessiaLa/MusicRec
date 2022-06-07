import json
import os
path  = r"C:\Users\user\.Neo4jDesktop\relate-data\dbmss\dbms-8288a492-3927-4d69-aa92-a3bcc44e3bb4\MusicNet"
base_path = r"C:\MusicNet"

album_path = os.path.join(base_path, 'album_kb.json')
with open(album_path, "r", encoding="utf-8") as f:
    data = json.loads("[" +
                      f.read().replace("}\n{", "},\n{") +
                      "]")

data[0]['u']['properties']['albumname'] = ' '.join(data[0]['u']['properties']['albumname'])
name = data[0]['u']['properties']['albumname']
id = data[0]['u']['properties']['albumid'][0]

with open('albums.txt', "w", encoding="utf-8") as file:
    for i, node in enumerate(data):
        data[i]['u']['properties']['albumname'] = ' '.join(data[i]['u']['properties']['albumname'])
        name = data[i]['u']['properties']['albumname']
        id = data[i]['u']['properties']['albumid'][0]
        file.write("album(" + "\""  + id  + "\"" + ', ' + "\"" + name + "\"" + ').\n')
