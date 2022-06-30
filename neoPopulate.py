from py2neo import Graph

def loadneonodes():
    graph = Graph(host='localhost', port=7687, password="password")
#Clean up
#     graph.delete_all()
#
#
# #Read the csv file and create the song nodes
#     #file:/MusicNet/artists.json
#     query = """
#     CALL apoc.load.json("file:/MusicNet/artists.json") YIELD value
#     MERGE (a:Artist {artistname: value.artistname})
#     WITH a, value
#     UNWIND value.artistgenres AS genres
#     MERGE (c:Genres {artistgenres: genres})
#     MERGE (a)-[:GENRES_OF]->(c);
#     """
#     graph.run(query)
#
#     # file:/MusicNet/albums.json
#     query = """
#     CALL apoc.load.json("file:/MusicNet/albums.json") YIELD value
#     MERGE (a:Album {albumid: value.albumid, albumname: value.albumname})
#     WITH a, value
#     UNWIND value.artistnames AS artistname
#     MERGE (c:Artist {artistname: artistname})
#     MERGE (a)-[:ALBUM_OF]->(c)
#     WITH a, value
#     UNWIND value.trackids AS tracks
#     MERGE (p:Track {trackids: tracks})
#     MERGE (a)-[:CONTAINS]->(p);
#     """
#     graph.run(query)
#
#     #file:/MusicNet/tracks.json
#     query = """
#     CALL apoc.load.json("file:/MusicNet/tracks.json") YIELD value
#     UNWIND value.trackid as t
#     UNWIND value.trackname as name
#     MATCH (n:Track {trackids: t})
#     set n.trackname = name
#     set n.features = value.features
#
#     """
#     graph.run(query)

    #file: / MusicNet / album_kb.json
    query = """

    CALL apoc.export.json.query("MATCH (u:Album) RETURN u","file:/MusicNet//album_kb.json");
    """
    graph.run(query)


# # download json related to the relation
    #file: / MusicNet / album_of_kb.json
    query="""
    CALL apoc.export.json.query("MATCH p=()-[r:ALBUM_OF]->() 
    RETURN COLLECT(p) as list","file:/MusicNet/album_of_kb.json")
    """

    graph.run(query)

    query="""
    CALL apoc.export.json.query("MATCH p=()-[r:CONTAINS]->() 
    RETURN COLLECT(p) as list","file:/MusicNet/contains_kb.json")
    """

    graph.run(query)


    query="""
    CALL apoc.export.json.query("MATCH p=()-[r:GENRES_OF]->() 
    RETURN COLLECT(p) as list","file:/MusicNet/genres_of_kb.json")
    """

    graph.run(query)


    query="""
    CALL apoc.export.json.query("MATCH (u:Track) RETURN u","file:/MusicNet/track_kb.json");
    """

    graph.run(query)

    query = """

    CALL apoc.export.json.query("MATCH (u:Artist) RETURN u","file:/MusicNet/artist_kb.json");
    """
    graph.run(query)

    query = """

    CALL apoc.export.json.query("MATCH (u:Genre) RETURN u","file:/MusicNet/genre_kb.json");
    """
    graph.run(query)


# start 
if __name__ == '__main__':
    loadneonodes()
