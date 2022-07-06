import json
import os

path = r"C:\Users\user\Desktop\AI-Project\MusicNet\Dataset\json_to_prolog"
out_path = r"C:\Users\user\Desktop\AI-Project\MusicNet\prolog"
base_path = r"C:\MusicNet"

# album_path = os.path.join(base_path, 'album_kb.json')
# album_path = os.path.join(path, 'album_kb.json')
# with open(album_path, "r", encoding="utf-8") as f:
#     data = json.loads("[" +
#                       f.read().replace("}\n{", "},\n{") +
#                       "]")
# album_txt = os.path.join(out_path, 'album.pl')
# with open(album_txt, "w", encoding="utf-8") as file:
#     for i, node in enumerate(data):
#         data[i]['u']['properties']['albumname'] = ''.join(data[i]['u']['properties']['albumname'])
#         name = data[i]['u']['properties']['albumname']
#         id = data[i]['u']['properties']['albumid']
#         file.write("album(" + "\""  + id  + "\"" + ', ' + "\"" + name + "\"" + ').\n')
#
#
# tracks = []
# track_path = os.path.join(path, 'track_kb.json')
# with open(track_path, "r", encoding="utf-8") as f:
#     data = json.loads("[" +
#     f.read().replace("}\n{", "},\n{") +
#     "]")
# track_txt = os.path.join(out_path, 'track.pl')
# with open(track_txt, "w", encoding="utf-8") as file:
#     for i, node in enumerate(data):
#         if "trackname" in node['u']['properties']:
#             name =node['u']['properties']['trackname'].lower().replace("\\", "").replace("/", "")
#             id = node['u']['properties']['trackids']
#
#             tracks.append(name)
#             file.write("track(" + "\""  + id  + "\"" + ', ' + "\"" + name + "\"" ').\n')
#
# seen = set()
# dupes = [x for x in tracks if x in seen or seen.add(x)]
# print(dupes)
#
# features = os.path.join(out_path, 'features.pl')
# with open(features, "w", encoding="utf-8") as file:
#     for i, node in enumerate(data):
#
#         if "trackname" in node['u']['properties']:
#             id = node['u']['properties']['trackids']
#             features = node['u']['properties']['features']
#             features = str(features).replace("]", "")
#             features = str(features).replace("[", "")
#             features = str(features).replace("\'", "\"")
#             file.write("features(" + "\""  + id  + "\"" + ', ' + features + ').\n')
#
# artist_path = os.path.join(path, 'artist_kb.json')
# with open(artist_path, "r", encoding="utf-8") as f:
#      data = json.loads("[" +
#                        f.read().replace("}\n{", "},\n{") +
#                         "]")
# artist_txt = os.path.join(out_path, 'artist.pl')
# with open(artist_txt, "w", encoding="utf-8") as file:
#     for i, node in enumerate(data):
#         if "artistname" in node['u']['properties']:
#             name = node['u']['properties']['artistname']
#             file.write("artist(" + "\""  + name + "\"" + ').\n')
#


genre_path = os.path.join(path, 'genre_kb.json')
with open(genre_path, "r", encoding="utf-8") as f:
     data = json.loads("[" +
                      f.read().replace("}\n{", "},\n{") +
                       "]")

genre_txt = os.path.join(out_path, 'genre.pl')
with open(genre_txt, "w", encoding="utf-8") as file:
    for i, node in enumerate(data):
        if "artistgenres" in node['u']['properties']:
            name = node['u']['properties']['artistgenres']
            print(name)
            file.write("genres(" + "\""  + name + "\"" + ').\n')



album_path = os.path.join(path, 'genres_of_kb.json')
with open(album_path, "r", encoding="utf-8") as f:
     data = json.loads(f.read())
artistgenres = os.path.join(out_path, 'artist_genres.pl')
with open(artistgenres, "w", encoding="utf-8") as file:
    for i, node in enumerate(data['list']):
        artist = node['nodes'][0]['properties']['artistname']
        genre = node['nodes'][1]['properties']['artistgenres']
        file.write("artistgenres(" + "\""  + artist + "\"" + ', ' + "\"" + genre + "\"" + ').\n')


#
# album_path = os.path.join(path, 'contains_kb.json')
# with open(album_path, "r", encoding="utf-8") as f:
#      data = json.loads(f.read())
# album_path = os.path.join(out_path, 'album_contains.pl')
# with open(album_path, "w", encoding="utf-8") as file:
#     for i, node in enumerate(data['list']):
#         if 'trackname' in node['nodes'][1]['properties']:
#             tracks.append(node['nodes'][1]['properties']['trackname'])
#             #print(node['nodes'][1]['properties']['trackname'])
#
#             albumid = node['nodes'][0]['properties']['albumid']
#             trackids = node['nodes'][1]['properties']['trackids']
#             file.write("album_contains(" + "\""  + albumid + "\"" ', ' + "\"" + trackids + "\""  + ').\n')
#
#
# album_path = os.path.join(path, 'album_of_kb.json')
# with open(album_path, "r", encoding="utf-8") as f:
#      data = json.loads(f.read())
# album_published = os.path.join(out_path, 'published_by.pl')
# print(data['list'][0]['nodes'][0])
# with open(album_published, "w", encoding="utf-8") as file:
#     for i, node in enumerate(data['list']):
#             albumid = node['nodes'][0]['properties']['albumid']
#             trackids = node['nodes'][1]['properties']['artistname']
#             file.write("published_by(" + "\""  + albumid + "\"" ', ' + "\"" + trackids + "\""  + ').\n')
#
