
import re
import json

def get_list_from_string(string: str):
    string = string.replace('[', '')
    string = string.replace(']', '')
    if string == '':
        return []
    return string.replace(", ", ",").split(",")


pattern = '(?P<artistname>.*), (?P<artistgenres>\[.*]), (?P<albumid>\[.*])'
regex = re.compile(pattern)
artist_dict = {}
artist_list = []

with open('artists.txt', encoding='utf-8') as f:
    with open('artists.json', 'w') as fp:
        json.dump(artist_dict, fp)
        for line in f:
            stripped_line = line.strip()

            result = regex.match(str(line))
            d = result.groupdict()
            artist_list.append(d)
            print(d)
            d['artistgenres'] = get_list_from_string(d['artistgenres'])
            d['albumid'] = get_list_from_string(d['albumid'])
            json.dump(d, fp)
        #artist_dict['artist'] = artist_list


#print(artist_dict)
#with open('artists.json', 'w') as fp:
    #json.dump(artist_dict, fp)


