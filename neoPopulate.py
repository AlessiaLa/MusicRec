from py2neo import Graph

def loadneonodes():
    graph = Graph(host='localhost', port=7687, auth=('neo4j',"password"))
#Clean up
    graph.delete_all()

 
#Read the csv file and create the song nodes
    query = """
    CALL apoc.load.json("file:/MusicNet/artists.json") YIELD value
    MERGE (a:Artist {artistname: value.artistname})
    WITH a, value
    UNWIND value.artistgenres AS genres
    MERGE (c:Genres {artistgenres: genres})
    MERGE (a)-[:GENRES_OF]->(c);
    """
    graph.run(query)


    query = """
    CALL apoc.load.json("file:/MusicNet/albums.json") YIELD value
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


    query = """
    CALL apoc.load.json("file:/MusicNet/tracks.json") YIELD value
    UNWIND value.trackid as t
    UNWIND value.trackname as name
    MATCH (n:Track {trackids: t})
    SET n.trackname = name
    """
    graph.run(query)

    query = """
    CALL apoc.export.json.query("MATCH (u:Album) RETURN u","file:/MusicNet/album_kb.json");
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
    query="""
    MATCH p=()-[r:ALBUM_OF]->() 
    RETURN p
    CALL apoc.export.json.query("MATCH p=()-[r:ALBUM_OF]->() 
    RETURN COLLECT(p) as list","file:/MusicNet/album_of_kb.json")
    """

    graph.run(query)


# start 
if __name__ == '__main__':
    loadneonodes()
