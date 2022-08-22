

jaccard(SimA,SimB,Sim) :-
    ord_intersection(SimA,SimB,I),
    ord_union(SimA,SimB,U),
    length(I,NI),
    length(U,NU),
    Sim is NI/NU.
    
%given two list return a list of pairs
list_list_pairs([], [], []).
list_list_pairs([X|Xs], [Y|Ys], [(X-Y)|Pairs]) :-
   list_list_pairs(Xs, Ys, Pairs).


retrieveArtistsByID([TrackId], [A]) :- 
    !,
   album_contains(AlbumID, TrackId), published_by(AlbumID, A).


retrieveArtistsByID([TrackId|TTrackId], [A|TA]) :- 
    album_contains(AlbumID, TrackId), published_by(AlbumID, A), 
    retrieveArtistsByID(TTrackId, TA).



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


without_last([_], []) :- !.
without_last([X|Xs], [X|WithoutLast]) :- 
    without_last(Xs, WithoutLast).


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



minmax_normalization(L, R) :-
    list_minnum_maxnum(L,Min,Max),
    normalization(Min, Max, L, R).

normalization(_, _, [], []).
normalization(Min, Max, [X], [Y]) :- !,
    Sum is Max-Min,
    Xmin is X-Min,
    Y is Xmin/Sum.

normalization(Min, Max, [X|Xs], [Y|Ys]) :- 
    Sum is Max-Min,
    Xmin is X-Min,
    Y is Xmin/Sum, 
    normalization(Min, Max, Xs, Ys).

list_minnum_maxnum([E|Es],Min,Max) :-
   V is E,
   list_minnum0_minnum_maxnum0_maxnum(Es,V,Min,V,Max).

list_minnum0_minnum_maxnum0_maxnum([]    ,Min ,Min,Max ,Max).
list_minnum0_minnum_maxnum0_maxnum([E|Es],Min0,Min,Max0,Max) :-
   V    is E,
   Min1 is min(Min0,V),
   Max1 is max(Max0,V),
   list_minnum0_minnum_maxnum0_maxnum(Es,Min1,Min,Max1,Max).