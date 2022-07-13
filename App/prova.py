import json
#
# f = open('C:/Users/Alessia/Desktop/genre_kb.json', 'r',encoding='utf-8' )
# print(f.read())
def intersection(lst1, lst2):
    lst3 = [value for value in lst1 if value in lst2]
    return lst3

# Opening JSON file
path='C:/Users/Alessia/Desktop/genre_kb.json'

path_to_del = 'C:/Users/Alessia/Desktop/genre_kb_del.json'

hiphop=[]
jazz=[]
pop=[]
rock=[]
country=[]
rap=[]
soul=[]
folk=[]
classical=[]
metal=[]
funky=[]
indie=[]
house=[]
with open(path, "r", encoding="utf-8") as f:
    data = json.loads("[" +
                      f.read().replace("}\n{", "},\n{") +
                      "]")
    for i, node in enumerate(data):
        if 'hip hop' in node['u']['properties']['artistgenres']:
            hiphop.append(node['u']['properties']['artistgenres'])
        if "jazz" in node['u']['properties']['artistgenres']:
            jazz.append(node['u']['properties']['artistgenres'])
        if 'pop' in node['u']['properties']['artistgenres']:
            pop.append(node['u']['properties']['artistgenres'])
        if 'rock' in node['u']['properties']['artistgenres']:
            rock.append(node['u']['properties']['artistgenres'])
        if 'country' in node['u']['properties']['artistgenres']:
            country.append(node['u']['properties']['artistgenres'])
        if 'rap' in node['u']['properties']['artistgenres']:
            rap.append(node['u']['properties']['artistgenres'])
        if 'soul' in node['u']['properties']['artistgenres']:
            soul.append(node['u']['properties']['artistgenres'])
        if 'folk' in node['u']['properties']['artistgenres']:
            folk.append(node['u']['properties']['artistgenres'])
        if 'classical' in node['u']['properties']['artistgenres']:
            classical.append(node['u']['properties']['artistgenres'])
        if 'metal' in node['u']['properties']['artistgenres']:
            metal.append(node['u']['properties']['artistgenres'])
        if 'funk' in node['u']['properties']['artistgenres']:
            funky.append(node['u']['properties']['artistgenres'])
        if 'indie' in node['u']['properties']['artistgenres']:
            indie.append(node['u']['properties']['artistgenres'])
        if 'house' in node['u']['properties']['artistgenres']:
            house.append(node['u']['properties']['artistgenres'])


# print arrays
# print(hiphop)
# print(len(hiphop))
# print(jazz)
# print(len(jazz))
# print(pop)
# print(len(pop))
# print(rock)
# print(len(rock))
# print(country)
# print(len(country))
# print(rap)
# print(len(rap))
# print(soul)
# print(len(soul))
# print(folk)
# print(len(folk))
# print(classical)
# print(len(classical))
# print(metal)
# print(len(metal))
# print(funky)
# print(len(funky))
# print(indie)
# print(len(indie))
# print(house)
# print(len(house))
f.close()

groupoflists=[]
groupoflists=[hiphop,pop,jazz,rock,country,rap,soul,folk,classical,metal,funky,indie,house]


# check intersections
# for k in groupoflists:
#     for j in groupoflists:
#         if k != j :
#             print(intersection(k, j))

# print length of arrays
for i in groupoflists:
    print(len(i))

################### OPENING AGAIN
with open(path_to_del, "r", encoding="utf-8") as f:
    data_del = json.loads("[" +
                      f.read().replace("}\n{", "},\n{") +
                      "]")
    for i, node in enumerate(data_del):
        if 'rock' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'classical' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'metal' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'funk' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'indie' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'house' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'folk' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'soul' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'rap' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'country' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'hip hop' in node['u']['properties']['artistgenres']:
            data.pop(i)
        if "jazz" in node['u']['properties']['artistgenres']:
            data.pop(i)
        if 'pop' in node['u']['properties']['artistgenres']:
            data.pop(i)
    print(data)
    ### NON TOGLIE I DATI NON SO PERCHE'



f.close()