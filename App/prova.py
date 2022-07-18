import json
import os

# f = open('C:/Users/Alessia/Desktop/genre_kb.json', 'r',encoding='utf-8' )
# print(f.read())
def intersection(lst1, lst2):
    lst3 = [value for value in lst1 if value in lst2]
    return lst3

# Opening JSON file
path= os.path.join(os.getcwd(), '../Dataset/json_to_prolog/genre_kb.json')

out_path= os.path.join(os.getcwd(),'../Prolog')

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
punk=[]
electronic=[]
reggae=[]
latin=[]
songwriter=[]
children=[]
soundtrack=[]
relax=[]

with open(path, "r", encoding="utf-8") as f:
    data = json.loads("[" +
                      f.read().replace("}\n{", "},\n{") +
                      "]")
    for i, node in enumerate(data):
        if 'hip hop' in node['u']['properties']['artistgenres']:
            hiphop.append(node['u']['properties']['artistgenres'])
        if ("jazz" in node['u']['properties']['artistgenres']) or ('bossa nova' in node['u']['properties']['artistgenres']):
            jazz.append(node['u']['properties']['artistgenres'])
        if 'pop' in node['u']['properties']['artistgenres']:
            pop.append(node['u']['properties']['artistgenres'])
        if 'rock' in node['u']['properties']['artistgenres']:
            rock.append(node['u']['properties']['artistgenres'])
        if ('country' in node['u']['properties']['artistgenres']) or ('bluegrass' in node['u']['properties']['artistgenres']):
            country.append(node['u']['properties']['artistgenres'])
        if 'rap' in node['u']['properties']['artistgenres']:
            rap.append(node['u']['properties']['artistgenres'])
        if ('soul' in node['u']['properties']['artistgenres']) or ('r&b' in node['u']['properties']['artistgenres']) or \
            ('blues' in node['u']['properties']['artistgenres']) or ('gospel' in node['u']['properties']['artistgenres']) or \
            ('motown' in node['u']['properties']['artistgenres']) or ('swing' in node['u']['properties']['artistgenres']) or \
            ('boogie' in node['u']['properties']['artistgenres']) or ('choir' in node['u']['properties']['artistgenres']):
            soul.append(node['u']['properties']['artistgenres'])
        if ('folk' in node['u']['properties']['artistgenres']) or ('celtic' in node['u']['properties']['artistgenres']):
            folk.append(node['u']['properties']['artistgenres'])
        if ('classical' in node['u']['properties']['artistgenres']) or ('opera' in node['u']['properties']['artistgenres']) or \
            ('tenor' in node['u']['properties']['artistgenres']) or ('romantic' in node['u']['properties']['artistgenres']) or \
            ('violin' in node['u']['properties']['artistgenres']) or ('mandolin' in node['u']['properties']['artistgenres']) or \
            ('piano' in node['u']['properties']['artistgenres']) or ('soprano' in node['u']['properties']['artistgenres']) or \
            ('chanson' in node['u']['properties']['artistgenres']) or ('baroque' in node['u']['properties']['artistgenres']) or \
            ('orchestr' in node['u']['properties']['artistgenres']):
            classical.append(node['u']['properties']['artistgenres'])
        if ('metal' in node['u']['properties']['artistgenres']):
            metal.append(node['u']['properties']['artistgenres'])
        if ('funk' in node['u']['properties']['artistgenres']) or ('dance' in node['u']['properties']['artistgenres']) or \
            ('disco' in node['u']['properties']['artistgenres']):
            funky.append(node['u']['properties']['artistgenres'])
        if 'indie' in node['u']['properties']['artistgenres']:
            indie.append(node['u']['properties']['artistgenres'])
        if ('house' in node['u']['properties']['artistgenres']) or ('psych' in node['u']['properties']['artistgenres']):
            house.append(node['u']['properties']['artistgenres'])
        if ('punk' in node['u']['properties']['artistgenres']) or ('emo' in node['u']['properties']['artistgenres']) or \
            ('grunge' in node['u']['properties']['artistgenres']):
            punk.append(node['u']['properties']['artistgenres'])
        if ('electr' in node['u']['properties']['artistgenres']) or ('techn' in node['u']['properties']['artistgenres']) or \
            ('dub' in node['u']['properties']['artistgenres']) or ('garage' in node['u']['properties']['artistgenres']) or \
            ('urban' in node['u']['properties']['artistgenres']) or ('rave' in node['u']['properties']['artistgenres']) or \
            ('hardcore' in node['u']['properties']['artistgenres']):
            electronic.append(node['u']['properties']['artistgenres'])
        if ('reggae' in node['u']['properties']['artistgenres']) or ('ska' in node['u']['properties']['artistgenres']) or \
            ('afr' in node['u']['properties']['artistgenres']):
            reggae.append(node['u']['properties']['artistgenres'])
        if ('latin' in node['u']['properties']['artistgenres']) or ('salsa' in node['u']['properties']['artistgenres']) or \
            ('rumba' in node['u']['properties']['artistgenres']) or ('tango' in node['u']['properties']['artistgenres']) or \
            ('bachata' in node['u']['properties']['artistgenres']) or ('samba' in node['u']['properties']['artistgenres']) or\
            ('flamenco' in node['u']['properties']['artistgenres']):
            latin.append(node['u']['properties']['artistgenres'])
        if ('singer-songwriter' in node['u']['properties']['artistgenres']) or('cantautor' in node['u']['properties']['artistgenres']) or \
            ('autore' in node['u']['properties']['artistgenres']):
            songwriter.append(node['u']['properties']['artistgenres'])
        if ('children' in node['u']['properties']['artistgenres'] ) or ('anime' in node['u']['properties']['artistgenres']) or \
            ('cartoon' in node['u']['properties']['artistgenres']) or ('disney' in node['u']['properties']['artistgenres']):
            children.append(node['u']['properties']['artistgenres'])
        if ('soundtrack' in node['u']['properties']['artistgenres']) or ('movie' in node['u']['properties']['artistgenres']) or \
            ('theme' in node['u']['properties']['artistgenres']):
            soundtrack.append(node['u']['properties']['artistgenres'])
        if ('wave' in node['u']['properties']['artistgenres']) or ('new age' in node['u']['properties']['artistgenres']) or \
            ('world' in node['u']['properties']['artistgenres']) or ('ambient' in node['u']['properties']['artistgenres']) or \
            ('chill' in node['u']['properties']['artistgenres']) or ('vapor' in node['u']['properties']['artistgenres']) or \
            ('lo-fi' in node['u']['properties']['artistgenres']) or ('atmospheric' in node['u']['properties']['artistgenres'] ):
            relax.append(node['u']['properties']['artistgenres'])
#print arrays
print(hiphop)
print(len(hiphop))
print(jazz)
print(len(jazz))
print(pop)
print(len(pop))
print(rock)
print(len(rock))
print(country)
print(len(country))
print(rap)
print(len(rap))
print(soul)
print(len(soul))
print(folk)
print(len(folk))
print(classical)
print(len(classical))
print(metal)
print(len(metal))
print(funky)
print(len(funky))
print(indie)
print(len(indie))
print(house)
print(len(house))
print(punk)
print(len(punk))
print(songwriter)
print(len(songwriter))
print(electronic)
print(len(electronic))
print(reggae)
print(len(reggae))
print(latin)
print(len(latin))
print(children)
print(len(children))
print(soundtrack)
print(len(soundtrack))
print(relax)
print(len(relax))


groupoflists=[]
groupoflists=[hiphop,pop,jazz,rock,country,rap,soul,folk,classical,metal,funky,indie,house,punk,electronic,reggae,latin,songwriter,children,soundtrack, relax]


#check intersections
# for k in groupoflists:
#     for j in groupoflists:
#         if k != j :
#             print(intersection(k, j))

# union of genres
list_genres=[]
for i, node in enumerate(data):
    list_genres.append(node['u']['properties']['artistgenres'])

# set of genres already classified

genres_classified= hiphop+pop+jazz+rock+country+rap+soul+folk+classical+metal+funky+indie+house+punk+electronic+reggae+latin+songwriter+children+soundtrack+relax
print(len(genres_classified))

#genres not yet classified
other_genres=['other']
other_genres= other_genres + list(set(list_genres) - set(genres_classified))
print(other_genres)
print(len(other_genres))

f.close()

groupoflists.append(other_genres)


dict = {'hip hop': hiphop, 'jazz': jazz, 'pop': pop, 'funky': funky,'punk':punk, 'house': house,
        'metal': metal, 'relax':relax,'soundtrack':soundtrack, 'songwriter': songwriter, 'children': children,
        'latin': latin, 'electronic': electronic, 'reggae': reggae, 'indie': indie,
       'classical': classical, 'soul': soul, 'rap': rap, 'country':country,  'rock':rock, 'pop':pop}



genre_txt = os.path.join(out_path, 'likes.pl')
with open(genre_txt, "w", encoding="utf-8") as file:
    for k in dict.keys():
        for values in dict[k]:
            file.write("like(" "\"" + str(k) + "\", " + "\"" + str(values) + "\"" + ').\n')