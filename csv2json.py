
import re
import json

pattern_artists = '(?P<artistname>.*), (?P<artistgenres>\[.*]), (?P<albumids>\[.*])'
pattern_albums = '(?P<albumid>.*), (?P<albumname>.*), (?P<artistnames>\[.*]), (?P<trackids>\[.*])'
pattern_tracks = '(?P<trackid>.*), (?P<trackname>.*), (?P<artistnames>\[.*]), (?P<albumname>.*)'

def get_list_from_string(string: str):
    string = string.replace('[', '')
    string = string.replace(']', '')
    string = string.replace('\"', '')
    if string == '':
        return []
    return string.replace(", ", ",").split(",")


def csv2json(csv, pattern, json_filename):
    dict = {}
    list = []
    regex = re.compile(pattern)
    with open(csv, encoding='utf-8') as f:
        with open(json_filename, 'w') as fp:
            json.dump(dict, fp)
            for line in f:
                stripped_line = line.strip()
                result = regex.match(str(stripped_line))
                d = result.groupdict()
                list.append(d)
                keys_list = d.keys()
                for k in keys_list:
                    print(k)
                    if k == 'albumname':
                        d[k] = d[k].replace("," , " ")
                    d[k] = get_list_from_string(d[k])
                json.dump(d, fp)




if __name__ == "__main__":
    csv2json('artists.txt', pattern_artists, 'artists.json')
    csv2json('albums.txt', pattern_albums, 'albums.json')
    csv2json('tracks.txt', pattern_tracks, 'tracks.json')