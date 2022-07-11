
% Return the list of the Features given the ID of the Track
%getFeaturesList("0NKevst3QXMMXuV6Qch3GP", [A, B, C,R,T,Y,U,I]).
getFeaturesList(TrackID, [Dance, Energy, Speech, Acoustic, Instrumental, Live, Valence, Speed]) :-
    features(TrackID, Dance, Energy, Speech, Acoustic, Instrumental, Live, Valence, Speed).

% Given two ID return the similarity calculated with Jaccard on the Feature sets of the two tracks
similarityByTrackFeatures(TrackA,TrackB,Sim) :-
    getFeaturesList(TrackA, SimA),
    getFeaturesList(TrackB,SimB),
    jaccard(SimA,SimB,Sim).


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


% rankTrack([0.4, 0.5, 0.77], ["ciao", "prolog", "daniela"], A].
rankTrack(SimList, TracksList, OrderedTracks) :- 
    list_list_pairs(SimList, TracksList, Pairs), % data la lista di tracce e similarità  ritorna la lista di coppie
    keysort(Pairs, OrderedPairs), % Sorting by the similarity (the key)
    pairs_values(OrderedPairs, OrderedTracks). % return the list only of the tracks


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