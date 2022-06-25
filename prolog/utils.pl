
% Return all the Tracks for a given artist and a given Album
getArtistTracks(NameArtist, NameAlbum, TrackName) :-  
    artist(NameArtist),
    
    published_by(IDalbum, NameArtist),
    album(IDalbum, NameAlbum),
    album_contains(IDalbum, IDtrack),
     track(IDtrack, TrackName, _).

% Return all the Album of a given artist
getArtistAlbums(NameArtist, NameAlbum) :- 
    artist(NameArtist), 
    published_by(X, NameArtist),
    album(X,NameAlbum).

% Return all the Tracks for a given album
getAlbumTracks(AlbumName, TrackNames) :-   
    album(IDalbum, AlbumName),
     album_contains(IDalbum, IDtrack), 
     track(IDtrack, TrackNames, _).