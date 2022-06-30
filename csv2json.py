
import re
import json

pattern_artists = 'artist\((?P<artistname>".*"), (?P<artistgenres>\[.*]), (?P<albumids>\[.*])'
pattern_albums = 'album\((?P<albumid>".*"), (?P<albumname>".*"), (?P<artistnames>\[.*]), (?P<trackids>\[.*])'
pattern_tracks = 'track\((?P<trackid>".*"), (?P<trackname>".*"), (?P<artistnames>\[".*"\]), (?P<albumname>.*), (?P<features>\[.*])\).'

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
        with open(json_filename, 'w', encoding='utf-8') as fp:
            json.dump(dict, fp)
            for line in f:
                stripped_line = line.strip()

                result = regex.match(str(stripped_line))

                d = result.groupdict()
                list.append(d)
                keys_list = d.keys()
                for k in keys_list:
                    if k == 'albumname':
                        d[k] = d[k].replace("," , " ")
                        d[k] = ''.join(str(x) for x in d[k])
                    d[k] = get_list_from_string(d[k])

                    if k == 'trackname':
                        d[k] = ''.join(str(x) for x in d[k])
                        print(d[k])
                json.dump(d, fp)




if __name__ == "__main__":
    csv2json(r'C:\Users\user\Desktop\AI-Project\spotify-recommender-master\artists.pl', pattern_artists, 'artists.json')
    csv2json(r'C:\Users\user\Desktop\AI-Project\spotify-recommender-master\albums.pl', pattern_albums, 'albums.json')
    csv2json(r'C:\Users\user\Desktop\AI-Project\spotify-recommender-master\tracks.pl', pattern_tracks, 'tracks.json')