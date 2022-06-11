import nltk
#nltk.download('omw-1.4')
from nltk.corpus import sentiwordnet as swn
from py2neo import Graph
import pandas as pd
from neo4j import GraphDatabase

0


'''jazz = swn.senti_synset('jazz.n.02')
print(jazz)'''

'''jazz_list=list(swn.senti_synsets('jazz'))
print(jazz_list)
for i in jazz_list:
    print(i.pos_score)'''


def collect_scores(word):
    scores=[]
    list_synsets = list(swn.senti_synsets(word))
    print(len(list(list_synsets)))
    for i in range (0,len(list(word))):
            scores.append(list_synsets[i])

    return scores



def loadneonodes():
    graph = Graph(host='localhost', port=7687, auth=('neo4j',"password"))
    # Connect to the neo4j database server
    graphDB_Driver = GraphDatabase.driver(host='localhost', port=7687, auth=('neo4j',"password"))

    # Clean up
    graph.delete_all()

#   # Query for retrieving genres
    query = """
    MATCH (n:Genres) RETURN n LIMIT 25
    """
    graph.run(query)
    df=[]
    with graphDB_Driver.session() as graphDB_Session:
        graphDB_Session.run(query)
        nodes = graphDB_Session.run(query)
        for x in nodes:
            df.append(x)

if __name__ == "__main__":
    loadneonodes()
    genres=[]
    lista_di_score = collect_scores('jazz')
    print(lista_di_score)