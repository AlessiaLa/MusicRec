

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
getTracksByFeatures(Dance, Energy, Valence, TracksName) :- 
    findall(TrackId, (features(TrackId, Dance, Energy, _, _, _, _,Valence, _)), Tracks),
    random_permutation(Tracks, TracksPer),
    take(TracksPer, 10, TenTracks),
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

findMostSilimarTrack(TrackId, N, NTracks) :- 
    getAllTracksExceptOne(TrackId, Tracks), % Get all the tracks from the KB except the one that i want to know the most similar track
    trackSimilarity(TrackId, Tracks, Similarity), % Get the list of tracks with their similarity
    rankTrack(Similarity, Tracks, OrderedTracks), % Ranks the tracks from the most similar to the less similar
    N1 is N*2, 
    take(OrderedTracks, N1, N1Tracks), % Take the first n*2 most similar tracks
    random_permutation(N1Tracks, TracksPer), % compute a shuffle on the n*2 most similar tracks
    take(TracksPer, N, NTracks). % take the first n


% Return a list with all the tracks ids in the kb  except the Track con cui voglio fare la similarità
getAllTracksExceptOne(TrackIdA, Tracks) :- 
   findall(TrackId, (track(TrackId, _),  TrackId \= TrackIdA), Tracks).


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
rankTrack([], []) :- true.
rankTrack([_], [_]) :- !.
rankTrack(SimList, TracksList, OrderedTracks) :- 
    list_list_pairs(SimList, TracksList, Pairs), % data la lista di tracce e similarità  ritorna la lista di coppie
    keysort(Pairs, OrderedPairs), % Sorting by the similarity (the key)
    pairs_values(OrderedPairs, OrderedTracks). % return the list only of the tracks

% esegue il suggerimento delle tracce simili 
suggestTrack(Tracks, Suggests) :- 
    findTracks(Tracks, TSug),
    flatten(TSug, Sug),
    getTrackName(Sug, Suggests).

% trova una traccia simile per ognuna di quelle in input (quindi per ognuna di quelle a cui l'utente ha messo like)
findTracks([Track], Sug) :- 
    !,
    track(TrackId,Track),
    findMostSilimarTrack(TrackId, 10, NTracks),  
    take(NTracks, 1, Sug).

% getTrackIds(["times change - live at mif","who's joe - live at mif","dream attack - live at mif"], S).
findTracks([Track|TTrack], [Sug|TSug]) :-
    track(TrackId, Track),
    findMostSilimarTrack(TrackId, 10, NTracks),
    take(NTracks, 2, Sug),   
    suggestTrackProva(TTrack, TSug).


take([_|_], 0, []) :- !.
take([X|T1],N,[X|T2]):-
    N>=0,
    N1 is N-1,
    take(T1,N1,T2).

% fai la similarità tra tutti i generi, facendo il prodotto cartesiano, ottengo nxm similarità diviso nxm ed ottengo la mia similatià 
getSimilarity2Genre(GenreA, GenreB, Sim) :-
    getWordSense(GenreA, [WordSenseA|_]),
    getWordSense(GenreB, [WordSenseB|_]),
    wn_lch(GenreA:n:WordSenseA, GenreB:n:WordSenseB, Sim), !.

% esegue il prodotto cartesiano  di similarità tra due insiemi di generi.
getSimilarityGenres(GenreA, GenreB, C) :-
     findall(Sim,(member(X,GenreA),member(Y,GenreB), getSimilarity2Genre(X, Y, Sim)), C).


max_list(L, M, I) :- nth1(I, L, M), 
\+ ((member(E, L), E > M)).

% trova il wordsense del genere tale che massimizzi la similarità con il wordsense music.
getWordSense(Genre, WordSenses) :-
    findall(Rank, (wn_lch(music:n:4, Genre:n:Ind, Rank)), SimList),
    without_last(SimList, Sim),
    findall(Ind, (max_list(Sim, _, Ind)), WordSenses).

without_last([_], []) :- !.
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).


