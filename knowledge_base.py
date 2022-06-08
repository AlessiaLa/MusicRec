import json
import os
path  = r"C:\Users\user\.Neo4jDesktop\relate-data\dbmss\dbms-8288a492-3927-4d69-aa92-a3bcc44e3bb4\MusicNet"

# album_path = os.path.join(path, 'album_kb.json')
# with open(album_path, "r", encoding="utf-8") as f:
#     data = json.loads("[" +
#                       f.read().replace("}\n{", "},\n{") +
#                       "]")
#
# data[0]['u']['properties']['albumname'] = ' '.join(data[0]['u']['properties']['albumname'])
# name = data[0]['u']['properties']['albumname']
# id = data[0]['u']['properties']['albumid'][0]
#
# with open('albums.txt', "w", encoding="utf-8") as file:
#     for i, node in enumerate(data):
#         data[i]['u']['properties']['albumname'] = ' '.join(data[i]['u']['properties']['albumname'])
#         name = data[i]['u']['properties']['albumname']
#         id = data[i]['u']['properties']['albumid'][0]
#         file.write("album(" + "\""  + id  + "\"" + ', ' + "\"" + name + "\"" + ').\n')



# album_path = os.path.join(path, 'track_kb.json')
# with open(album_path, "r", encoding="utf-8") as f:
#      data = json.loads(f.read())
#
# print(data['list'][0]['properties']['trackname'])
#
# with open('track.txt', "w", encoding="utf-8") as file:
#     for i, node in enumerate(data['list']):
#         if "trackname" in node['properties']:
#             print(node['properties'])
#             name =node['properties']['trackname']
#             id = node['properties']['trackids']
#             file.write("track(" + "\""  + id  + "\"" + ', ' + "\"" + name + "\"" + ').\n')



# album_path = os.path.join(path, 'artist_kb.json')
# with open(album_path, "r", encoding="utf-8") as f:
#      data = json.loads(f.read())
#
#
# with open('artist.txt', "w", encoding="utf-8") as file:
#     for i, node in enumerate(data['list']):
#         if "artistname" in node['properties']:
#             name = node['properties']['artistname']
#             file.write("artist(" + "\""  + name + "\"" + ').\n')


# album_path = os.path.join(path, 'genres_kb.json')
# with open(album_path, "r", encoding="utf-8") as f:
#      data = json.loads(f.read())
#
#
# with open('genre.txt', "w", encoding="utf-8") as file:
#     for i, node in enumerate(data['list']):
#         if "artistname" in node['properties']:
#             name = node['properties']['artistgenres']
#             file.write("genres(" + "\""  + name + "\"" + ').\n')

# album_path = os.path.join(path, 'genres_of_kb.json')
# with open(album_path, "r", encoding="utf-8") as f:
#      data = json.loads(f.read())
#
#
# with open('artistgenres.txt', "w", encoding="utf-8") as file:
#     for i, node in enumerate(data['list']):
#
#         artist = node['nodes'][0]['properties']['artistname']
#         genre = node['nodes'][1]['properties']['artistgenres']
#         file.write("artistgenres(" + "\""  + artist + "\"" + ', ' + "\"" + genre + "\"" + ').\n')

album_path = os.path.join(path, 'album_contains_kb.json')
with open(album_path, "r", encoding="utf-8") as f:
     data = json.loads(f.read())


with open('album_contains.txt', "w", encoding="utf-8") as file:
    for i, node in enumerate(data['list']):
        if 'trackname' in node['nodes'][1]['properties']:
            albumid = node['nodes'][0]['properties']['albumid'][0]
            trackids = node['nodes'][1]['properties']['trackids']
            file.write("album_contains(" + "\""  + albumid + "\"" ', ' + "\"" + trackids + "\""  + ').\n')

