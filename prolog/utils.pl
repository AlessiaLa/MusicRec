

:- consult("C:\\Users\\user\\Desktop\\AI-Project\\MusicNet\\prolog\\wn\\wn_connect.pl").

getArtistTracks(NameArtist, NameAlbum, TrackName) :-
    artist(NameArtist),    
    published_by(IDalbum, NameArtist),
    album(IDalbum, NameAlbum),
    album_contains(IDalbum, IDtrack),
    track(IDtrack, TrackName).

% Return all the Album of a given artist 
getArtistAlbums(NameArtist, NameAlbum) :-
    artist(NameArtist), 
    published_by(X, NameArtist),
    album(X,NameAlbum).

% Return all the Tracks for a given album
getAlbumTracks(AlbumName, TrackNames) :-
    album(IDalbum, AlbumName),
    album_contains(IDalbum, IDtrack), 
    track(IDtrack, TrackNames).

%  Returns the list of genres of a given artist. 
getArtistGenres(ArtistName, Genres) :-
    artistgenres(ArtistName, Genres).

% Return the list of the Features given the ID of the Track
getFeaturesList(TrackID, [Dance, Energy, Speech, Acoustic, Instrumental, Live, Valence, Speed]) :-
    features(TrackID, Dance, Energy, Speech, Acoustic, Instrumental, Live, Valence, Speed).


% Given two ID return the similarity calculated with Jaccard on the Feature sets of the two tracks
similarityByTrackFeatures(TrackA,TrackB,Sim) :-
    getFeaturesList(TrackA, SimA),
    getFeaturesList(TrackB,SimB),
    jaccard(SimA,SimB,Sim).


jaccard(SimA,SimB,Sim) :-
    ord_intersection(SimA,SimB,I),
    ord_union(SimA,SimB,U),
    length(I,NI),
    length(U,NU),
    Sim is NI/NU.


%find all the ids of the tracks whose features match the features given in input.
% the ids are shuffled and the first 10 ids are returned 
% getTracksByFeatures("low_danceable", "high_energy", "low_valence", TenTracks).
getTracksByFeatures(N, Dance, Energy, Valence, TracksName) :- 
    findall(TrackId, (features(TrackId, Dance, Energy, _, _, _, _,Valence, _)), Tracks),
    random_permutation(Tracks, TracksPer),
    take(TracksPer, N, TenTracks),
    getTrackName(TenTracks, TracksName).


%Given the list of the ID of the Tracks return the List of the name of the same tracks 
getTrackName([], []).
getTrackName([H|T], [Track|T1]) :-
    track(H, Track),
    getTrackName(T, T1).


% getTrackIds(["treatment"], Name).
%Return the list of the tracks ID given the list of the tracks names 
getTrackIds([], []).
getTrackIds([H|T], [Track|T1]) :-
    track(Track, H),
    getTrackIds(T, T1), !.

findMostSimilarTrackAggregate([TrackId],Tracks, [Similarity]) :- 
    !,
    trackSimilarity(TrackId, Tracks, Similarity).


findMostSimilarTrackAggregate([TrackId|TracksIds], Tracks,[Similarity|TSimilarity]) :- 
    trackSimilarity(TrackId, Tracks, Similarity), 
    findMostSimilarTrackAggregate(TracksIds, Tracks, TSimilarity).


% Return a list with all the tracks ids in the kb  except the Track con cui voglio fare la similarità
getAllTracksExceptSome(TrackIds, TracksResults) :- 
   findall(TrackId, (track(TrackId, _)), Tracks),
   subtract(Tracks, TrackIds, TracksResults).


% restituisce tutte le tracce con la loro similarità alla traccia data in input
trackSimilarity(TrackIdA, [TrackIdB], [Sim]) :-
    similarityByTrackFeatures(TrackIdA,TrackIdB,Sim), !.

trackSimilarity(TrackIdA, [TrackIdB|T], [Sim|SimT]) :- 
    similarityByTrackFeatures(TrackIdA,TrackIdB,Sim),
    trackSimilarity(TrackIdA, T, SimT).


%given two list return a list of pairs
list_list_pairs([], [], []).
list_list_pairs([X|Xs], [Y|Ys], [(X-Y)|Pairs]) :-
   list_list_pairs(Xs, Ys, Pairs).


% rankTrack([0.4, 0.5, 0.77], ["ciao", "prolog", "daniela"], A].
rankTrack(SimList, TracksList, OrderedTracks) :- 
    list_list_pairs(SimList, TracksList, Pairs), % data la lista di tracce e similarità  ritorna la lista di coppie
    keysort(Pairs, OrderedPairs), % Sorting by the similarity (the key)
    pairs_values(OrderedPairs, OrderedTracks). % return the list only of the tracks



retrieveAllArtists([Track], [Album], [A]) :- 
    !,
    (
    Album = 'null'
    -> 
    track(B, Track), album_contains(AlbumID, B), published_by(AlbumID, A)
    ;
    album(AlbumID, Album), track(B, Track), album_contains(AlbumID, B), published_by(AlbumID, A), !
    ).

retrieveAllArtists([Track|TTrack], [Album|TAlbum], [A|TA]) :- 
    (
    Album = 'null'
    -> 
    track(B, Track), album_contains(AlbumID, B), published_by(AlbumID, A),
    retrieveAllArtists(TTrack, TAlbum, TA)
    ;
    album(AlbumID, Album), track(B, Track), album_contains(AlbumID, B), published_by(AlbumID, A),
    retrieveAllArtists(TTrack, TAlbum, TA)).
    

suggestTrack(Tracks, Album, N, TrackName) :- 
    findTracksIds(Tracks, Album, TrackIds),
    getAllTracksExceptSome(TrackIds, TracksTotal), 
    findMostSimilarTrackAggregate(TrackIds,TracksTotal, Similarities),
    sum_list(Similarities, SumSimilarities),
    rankTrack(SumSimilarities, TracksTotal, OrderedTracks),
    N1 is N*2, 
    take(OrderedTracks, N1, N1Tracks), % Take the first n*2 most similar tracks
    random_permutation(N1Tracks, TracksPer), % compute a shuffle on the n*2 most similar tracks
    take(TracksPer, N, NTracks),
    getTrackName(NTracks, TrackName).

% trova una traccia simile per ognuna di quelle in input (quindi per ognuna di quelle a cui l'utente ha messo like)
findTracksIds([Track], [Album], [TrackId]) :- 
    !,
    (Album = 'null'
    ->    
    track(TrackId, Track)
    ;
    album(AlbumID, Album),track(TrackId, Track), album_contains(AlbumID, TrackId), !).

% getTrackIds(["times change - live at mif","who's joe - live at mif","dream attack - live at mif"], S).
findTracksIds([Track|TTrack], [Album|TAlbum], [TrackId|T]) :-
    (Album = 'null'
    -> 
    track(TrackId, Track),
    findTracksIds(TTrack,TAlbum, T)
    ;
    album(AlbumID, Album), track(TrackId, Track), album_contains(AlbumID, TrackId), 
    findTracksIds(TTrack,TAlbum, T)).

checkTrackDuplicates(Track) :-
    findall(TrackName, (track(_, TrackName)), Tracks),
    count(Tracks, Track, N),
    N > 1.

retriveAlbumDuplicateTracks(Track, Albums) :- 
    findall(AlbumName, (track(TrackID, Track), album_contains(AlbumID,TrackID), album(AlbumID, AlbumName)), Albums).

count(L, E, N) :-
    include(=(E), L, L2), length(L2, N).

take([_|_], 0, []) :- !.
take([X|T1],N,[X|T2]):-
    N>=0,
    N1 is N-1,
    take(T1,N1,T2).

max_list(L, M, I) :- nth1(I, L, M), 
\+ ((member(E, L), E > M)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% ARTIST

% trova il wordsense del genere tale che massimizzi la similarità con il wordsense music.
getWordSense(Genre, WordSenses) :-
    findall(Rank, (wn_lch(music:n:4, Genre:n:Ind, Rank)), SimList),
    without_last(SimList, Sim),
    findall(Ind, (max_list(Sim, _, Ind)), WordSenses).


getWordSense(Genre, WordSenses) :-
    findall(Rank, (wn_lch(music:n:4, Genre:n:Ind, Rank)), SimList),
    
    without_last(SimList, Sim),
    findall(Ind, (max_list(Sim, _, Ind)), WordSenses).

without_last([_], []) :- !.
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).


% esegue la similarità tra due generi. 
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
    


sum_list([Head1], Head1) :- !.
sum_list([Head1,Head2|[]], R) :- 
    !,
    sum(Head1, Head2, R).


sum_list([Head1,Head2|Tail], R) :- 
    sum(Head1, Head2, Result),
    sum_list([Result|Tail],  R).

sum([],[],[]).
sum([H1|T1],[H2|T2],[X|L3]) :- 
    sum(T1,T2,L3), X is H1+H2.



/*
getSuggestedArtist(ArtistA, N, NArtists) :-
    getAllArtistsAndGenreExceptOne(ArtistA, Artists, PossibleGenres),
    findall(GenreA, (artistgenres(ArtistA, GenreA)), ListGenresA), %ritrovo tutti i generi di ArtistA
    calculateArtistSimilarity(ListGenresA, PossibleGenres, Similarities),
    rankArtist(Similarities, Artists, OrderedArtist),
    N1 is N*2, 
    take(OrderedArtist, N1, N1Artists), % Take the first n*2 most similar artist
    random_permutation(N1Artists, ArtistsPer), % compute a shuffle on the n*2 most similar artist
    take(ArtistsPer, N, NArtists). % take the first n


suggestArtist(Artists,N, Sug) :- 
    findArtists(Artists,N, ASug),
    flatten(ASug, Sug).

% trova una traccia simile per ognuna di quelle in input (quindi per ognuna di quelle a cui l'utente ha messo like)
findArtists([Artist],N, Sug) :- 
    !,
    N2 is N*2,
    getSuggestedArtist(Artist, N2, NArtists),  
    take(NArtists, N, Sug).

% getTrackIds(["times change - live at mif","who's joe - live at mif","dream attack - live at mif"], S).
% sussiste un problema legato ai duplicati delle tracce 
findArtists([Artist|TArtist],N, [Sug|TSug]) :-
    N2 is N*2,
    getSuggestedArtist(Artist, N2, NArtists),
    take(NArtists, N, Sug),   
    findArtists(TArtist,N, TSug).
*/



rankArtist(SimList, ArtistList, OrderedArtist) :- 
    list_list_pairs(SimList, ArtistList, Pairs), % data la lista di tracce e similarità  ritorna la lista di coppie
    keysort(Pairs, OrderedPairs), % Sorting by the similarity (the key)
    pairs_values(OrderedPairs, OrderedArtist). % return the list only of the tracks

calculateArtistSimilarity(GenresA, [GenreB], [Similarity]) :- !,
    getSimilarityGenres(GenresA, GenreB, Similarity).

calculateArtistSimilarity(GenresA, [GenreB|GenreT], [Similarity|SimilarityT]) :-
    getSimilarityGenres(GenresA, GenreB, Similarity),
    calculateArtistSimilarity(GenresA,  GenreT, SimilarityT). 
    

% ho una lista di liste di generi di tutti gli artisti, escluso quello che piace all'utente, che hanno almeno un genere
getAllArtistsAndGenreExceptOne(ArtistA, SetArtists, GenreT) :-
    findall(ArtistB, (artistgenres(ArtistB, Genre),  ArtistB \= ArtistA, Genre \= []), Artists),
    list_to_set(Artists, SetArtists),
    getAllGenres(SetArtists, GenreT).

getAllGenres([A], [G]) :- !,
    findall(Genre, (artistgenres(A, Genre)), G).

getAllGenres([A|Artists], [G|GenreT]) :-
    findall(Genre, (artistgenres(A, Genre)), G),
    getAllGenres(Artists, GenreT).

avg( List, Avg ):-
    sumlist( List, Sum ),
    length( List, Length),
    (  Length > 0
    -> Avg is Sum / Length
    ;  Avg is 0
    ).

%%%%%%%%%%%%%%%%%%%%% Album 


%findSimilarAlbum([Track|TTrack], SimilarAlbum) :-
    

