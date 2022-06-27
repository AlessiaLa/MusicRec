

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


getFeaturesList(TrackID, [Dance, Energy, Speech, Acoustic, Instrumental, Live, Valence, Speed]) :-
    features(TrackID, Dance, Energy, Speech, Acoustic, Instrumental, Live, Valence, Speed).


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
getTracksByFeatures(Dance, Energy, Valence, TenTracks) :- 
    findall(TrackId, (features(TrackId, Dance, Energy, _, _, _, _,Valence, _)), Tracks),
    random_permutation(Tracks, TracksPer),
    take(TracksPer, 10, TenTracks).

take([X|T1],N,[X|T2]):-
    N>=0,
    N1 is N-1,
    take(T1,N1,T2).

take([_|_], 0, []) :-!.
