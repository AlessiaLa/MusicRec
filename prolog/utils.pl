% Return all the Tracks for a given artist and a given Album
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