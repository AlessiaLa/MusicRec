

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
getTrackName([H|T], [Track|T1]) :-
    track(H, Track),
    getTrackName(T, T1).
getTrackName([], []).

% getTrackIds(["treatment"], Name).
%Return the list of the tracks ID given the list of the tracks names 
getTrackIds([H|T], [Track|T1]) :-
    track(Track, H),
    getTrackIds(T, T1).
getTrackIds([], []).


%%
findMostSilimarTrack(TrackId, N, NTracks) :- 
    getAllTracksExceptOne(TrackId, Tracks),
    trackSimilarity(TrackId, Tracks, Similarity),
    rankTrack(Similarity, Tracks, OrderedTracks), 
    N1 is N*2,
    take(OrderedTracks, N1, N1Tracks),
    random_permutation(N1Tracks, TracksPer),
    take(TracksPer, N, NTracks).

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
 
take([_|_], 0, []) :- !.
take([X|T1],N,[X|T2]):-
    N>=0,
    N1 is N-1,
    take(T1,N1,T2).



