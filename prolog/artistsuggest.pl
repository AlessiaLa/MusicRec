% trova il wordsense del genere tale che massimizzi la similarità con il wordsense music.
getWordSense(Genre, WordSenses) :-
    findall(Rank, (wn_lch(music:n:4, Genre:n:Ind, Rank)), SimList),
    without_last(SimList, Sim),
    findall(Ind, (max_list(Sim, _, Ind)), WordSenses).


getWordSense(Genre, WordSenses) :-
    findall(Rank, (wn_lch(music:n:4, Genre:n:Ind, Rank)), SimList),
    without_last(SimList, Sim),
    findall(Ind, (max_list(Sim, _, Ind)), WordSenses).

getSimilarity2Genre(GenreA, GenreB, Sim) :-
    getWordSense(GenreA, [WordSenseA|_]),
    getWordSense(GenreB, [WordSenseB|_]),
    wn_lch(GenreA:n:WordSenseA, GenreB:n:WordSenseB, Sim), !.

% esegue il prodotto cartesiano  di similarità tra due insiemi di generi.
% fai la similarità tra tutti i generi, facendo il prodotto cartesiano, ottengo nxm similarità diviso nxm ed ottengo la mia similatià 
% GenreA, GenreB sono due liste di generi
getSimilarityGenres(GenreA, GenreB, AvgOfSimilarities) :-
     findall(Sim,(member(X,GenreA),member(Y,GenreB), getSimilarity2Genre(X, Y, Sim)), ListOfSimilarities),
     avg(ListOfSimilarities, AvgOfSimilarities).

getSuggestedArtistAggregate([Artist], GenreResult , [Similarities]) :- 
    !,
    findall(GenreA, (artistgenres(Artist, GenreA)), ListGenresA), %ritrovo tutti i generi di ArtistA
    calculateArtistSimilarity(ListGenresA, GenreResult, Similarities).

getSuggestedArtistAggregate([Artist|TArtist], GenreResult, [Similarities|TSimilarities]) :- 
    findall(GenreA, (artistgenres(Artist, GenreA)), ListGenresA), %ritrovo tutti i generi di ArtistA
    calculateArtistSimilarity(ListGenresA, GenreResult, Similarities),
    getSuggestedArtistAggregate(TArtist,GenreResult, TSimilarities).

suggestArtist(Tracks,Album, N, N1Artists) :- 
    retrieveAllArtists(Tracks, Album, Artist),
    getAllArtistsAndGenreExceptSome(Artist, Artists, GenreResult),
    getSuggestedArtistAggregate(Artist,GenreResult, Similarities),  
    sum_list(Similarities, SumSimilarities),
    rankArtist(SumSimilarities, Artists, OrderedArtist),
    take(OrderedArtist, N, N1Artists).


getAllArtistsAndGenreExceptSome(Artist, ArtistResult, GenreResult) :-
    findall(ArtistB, (artistgenres(ArtistB, Genre), Genre \= []), Artists),
    list_to_set(Artists, SetArtists),
    subtract(SetArtists, Artist, ArtistResult),
    getAllGenres(ArtistResult, GenreResult).

rankArtist(SimList, ArtistList, OrderedArtist) :- 
    list_list_pairs(SimList, ArtistList, Pairs), % data la lista di tracce e similarità  ritorna la lista di coppie
    keysort(Pairs, OrderedPairs), % Sorting by the similarity (the key)
    pairs_values(OrderedPairs, OrderedArtist). % return the list only of the tracks

calculateArtistSimilarity(GenresA, [GenreB], [Similarity]) :- !,
    getSimilarityGenres(GenresA, GenreB, Similarity).

calculateArtistSimilarity(GenresA, [GenreB|GenreT], [Similarity|SimilarityT]) :-
    getSimilarityGenres(GenresA, GenreB, Similarity),
    calculateArtistSimilarity(GenresA,  GenreT, SimilarityT). 
    
