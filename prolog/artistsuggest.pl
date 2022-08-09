


% trova il wordsense del genere tale che massimizzi la similarità con il wordsense music.
getWordSense(Genre, WordSenses) :-
    findall(Rank, (wn_path(music:n:4, Genre:n:Ind, Rank)), SimList),
    without_last(SimList, Sim),
    findall(Ind, (max_list(Sim, _, Ind)), WordSenses).


getWordSense(Genre, WordSenses) :-
    findall(Rank, (wn_path(music:n:4, Genre:n:Ind, Rank)), SimList),
    without_last(SimList, Sim),
    findall(Ind, (max_list(Sim, _, Ind)), WordSenses).

getSimilarity2Genre(GenreA, GenreB, Sim) :-
    getWordSense(GenreA, [WordSenseA|_]),
    getWordSense(GenreB, [WordSenseB|_]),
    wn_path(GenreA:n:WordSenseA, GenreB:n:WordSenseB, Sim), !.

% esegue il prodotto cartesiano  di similarità tra due insiemi di generi.
% fai la similarità tra tutti i generi, facendo il prodotto cartesiano, ottengo nxm similarità diviso nxm ed ottengo la mia similatià 
% GenreA, GenreB sono due liste di generi
getSimilarityGenres(GenreA, GenreB, AvgOfSimilarities) :-
     findall(Sim,(member(X,GenreA),member(Y,GenreB), getSimilarity2Genre(X, Y, Sim)), ListOfSimilarities),
     avg(ListOfSimilarities, AvgOfSimilarities).

getJaccardSimilarityGenre(GenreA, GenreB, Sim) :- 
    jaccard(GenreA, GenreB, Sim).


getSuggestedArtistAggregate([Artist], GenreResult , [Similarities]) :- 
    !,
    findall(GenreA, (artistgenres(Artist, GenreA)), ListGenresA), %ritrovo tutti i generi di ArtistA
    calculateArtistJSimilarity(ListGenresA, GenreResult, JSimilarity),
    calculateArtistWSimilarity(ListGenresA, GenreResult, WSimilarity),
    sum(JSimilarity, WSimilarity, Similarities).

getSuggestedArtistAggregate([Artist|TArtist], GenreResult, [Similarities|TSimilarities]) :- 
    findall(GenreA, (artistgenres(Artist, GenreA)), ListGenresA), %ritrovo tutti i generi di ArtistA
    calculateArtistJSimilarity(ListGenresA, GenreResult, JSimilarity),
    calculateArtistWSimilarity(ListGenresA, GenreResult, WSimilarity),
    sum(JSimilarity, WSimilarity, Similarities),
    getSuggestedArtistAggregate(TArtist,GenreResult, TSimilarities).

suggestArtist(Tracks, N, NArtists) :-
    retrieveArtistsByID(Tracks, Artist),
    getAllArtistsAndGenreExceptSome(Artist, Artists, GenreResult),
    getSuggestedArtistAggregate(Artist,GenreResult, Similarities),
    sum_list(Similarities, SumSimilarities),
    rankArtist(SumSimilarities, Artists, OrderedArtist),
    N1 is N*2,
    take(OrderedArtist, N1, N1Artists),
    random_permutation(N1Artists, ArtistPer), % compute a shuffle on the n*2 most similar tracks
    take(ArtistPer, N, NArtists).


getAllArtistsAndGenreExceptSome(Artist, ArtistResult, GenreResult) :-
    findall(ArtistB, (artistgenres(ArtistB, Genre), Genre \= []), Artists),
    list_to_set(Artists, SetArtists),
    subtract(SetArtists, Artist, ArtistResult),
    getAllGenres(ArtistResult, GenreResult).

rankArtist(SimList, ArtistList, ReversedArtist) :-
    list_list_pairs(SimList, ArtistList, Pairs), % data la lista di tracce e similarità  ritorna la lista di coppie
    keysort(Pairs, OrderedPairs), % Sorting by the similarity (the key)
    pairs_values(OrderedPairs, OrderedArtist),
    reverse(OrderedArtist, ReversedArtist). % return the list only of the tracks

calculateArtistJSimilarity(GenresA, [GenreB], [JSimilarity]) :- !,
    jaccard(GenresA, GenreB, JSimilarity).

calculateArtistJSimilarity(GenresA, [GenreB|GenreT], [JSimilarity|SimilarityT]) :-
    jaccard(GenresA, GenreB, JSimilarity),
    calculateArtistJSimilarity(GenresA,  GenreT, SimilarityT).

 calculateArtistWSimilarity(GenresA, [GenreB], [WSimilarity]) :- !,
    getSimilarityGenres(GenresA, GenreB, WSimilarity).

calculateArtistWSimilarity(GenresA, [GenreB|GenreT], [WSimilarity|SimilarityT]) :-
    getSimilarityGenres(GenresA, GenreB, WSimilarity),
    calculateArtistWSimilarity(GenresA,  GenreT, SimilarityT).   


retrieveAlbumByArtist([Artist], [Album]) :- !, 
    findall(Name, (published_by(AlbumID, Artist), album(AlbumID, Name)), Album).

retrieveAlbumByArtist([Artist|ArtistT], [Album|AlbumT]) :-
    findall(Name, (published_by(AlbumID, Artist), album(AlbumID, Name)), Album),
    retrieveAlbumByArtist(ArtistT, AlbumT).

minmax_normalization(L, R) :-
    list_minnum_maxnum(L,Min,Max),
    normalization(Min, Max, L, R).

normalization(Min, Max, [X|Xs], [Y|Ys]) :- 
    Sum is Max-Min,
    Xmin is X-Min,
    Y is Xmin/Sum, 
    normalization(Min, Max, Xs, Ys).

normalization(_, _, [], []).


list_minnum_maxnum([E|Es],Min,Max) :-
   V is E,
   list_minnum0_minnum_maxnum0_maxnum(Es,V,Min,V,Max).

list_minnum0_minnum_maxnum0_maxnum([]    ,Min ,Min,Max ,Max).
list_minnum0_minnum_maxnum0_maxnum([E|Es],Min0,Min,Max0,Max) :-
   V    is E,
   Min1 is min(Min0,V),
   Max1 is max(Max0,V),
   list_minnum0_minnum_maxnum0_maxnum(Es,Min1,Min,Max1,Max).