from py2neo import Graph

def loadneonodes():
    graph = Graph(host='localhost', port=7687, password="password")
#Clean up
    graph.delete_all()
 
 
#Read the csv file and create the song nodes
    #file:/MusicNet/artists.json
    query = """
    CALL apoc.load.json("../../../../Documenti/GitHub/MusicNet/Dataset/artists.json") YIELD value
    MERGE (a:Artist {artistname: value.artistname})
    WITH a, value
    UNWIND value.artistgenres AS genres
    MERGE (c:Genres {artistgenres: genres})
    MERGE (a)-[:GENRES_OF]->(c);
    """
    graph.run(query)

    # file:/MusicNet/albums.json
    query = """
    CALL apoc.load.json("../../../../Documenti/GitHub/MusicNet/Dataset/albums.json") YIELD value
    MERGE (a:Album {albumid: value.albumid, albumname: value.albumname})
    WITH a, value
    UNWIND value.artistnames AS artistname
    MERGE (c:Artist {artistname: artistname})
    MERGE (a)-[:ALBUM_OF]->(c)
    WITH a, value
    UNWIND value.trackids AS tracks
    MERGE (p:Track {trackids: tracks})
    MERGE (a)-[:CONTAINS]->(p);
    """
    graph.run(query)

    #file:/MusicNet/tracks.json
    query = """
    CALL apoc.load.json("../../../../Documenti/GitHub/MusicNet/Dataset/tracks_discretized.json") YIELD value
    UNWIND value.trackid as t
    UNWIND value.trackname as name
    MATCH (n:Track {trackids: t})
    set n.trackname = name
    set n.features = value.features
    
    """
    graph.run(query)

    #file: / MusicNet / album_kb.json
    query = """

    CALL apoc.export.json.query("MATCH (u:Album) RETURN u","../../../../Documenti/GitHub/MusicNet/Dataset/album_kb.json");
    """
    graph.run(query)

# #Read the csv file and create the artist nodes
#     query = """
#     LOAD CSV WITH HEADERS FROM 'file:///datasetFinal.csv' AS column MERGE (a:Artists {a_name:column.Artist})
#
#     """
#     graph.run(query)
#
#
#
# #Read the csv file and create the genre nodes
#     query = """
#     LOAD CSV WITH HEADERS FROM 'file:///datasetFinal.csv' AS column MERGE (g:Genres {genre:column.Genre})
#
#     """
#     graph.run(query)
#
# #relation between the artists and the songs
#     query = """
#     LOAD CSV WITH HEADERS FROM 'file:///datasetFinal.csv' AS column MERGE (s:Songs{name:column.Song,genre:column.Genre,album:column.Album,duration:column.Duration})
#     MERGE (a:Artists {a_name:column.Artist})
#     MERGE (s)-[:CREATED_BY{release:toInteger(column.Year)}]->(a)
#
#     """
#     graph.run(query)
#
#
#
#
# #relation between the genre and the songs
#     query = """
#     LOAD CSV WITH HEADERS FROM 'file:///datasetFinal.csv' AS column MERGE (s:Songs{name:column.Song,genre:column.Genre,album:column.Album,duration:column.Duration})
#     MERGE (g:Genres {genre:column.Genre})
#     MERGE (s)-[:BELONGS_TO]->(g)
#
#     """
#     graph.run(query)
#
# # download json related to the relation
    #file: / MusicNet / album_of_kb.json
    query="""
    MATCH p=()-[r:ALBUM_OF]->() 
    RETURN p
    CALL apoc.export.json.query("MATCH p=()-[r:ALBUM_OF]->() 
    RETURN COLLECT(p) as list","Dataset/album_of_kb.json")
    """

    graph.run(query)

    #CALL apoc.export.json.query("MATCH (u:Album) RETURN u","MusicNet//album_kb.json")
    #"""
    #graph.run(query)




# start 
if __name__ == '__main__':
    loadneonodes()
