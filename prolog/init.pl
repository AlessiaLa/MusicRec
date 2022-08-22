:- set_prolog_flag(answer_write_options,[max_depth(0)]).
:- set_prolog_flag(encoding, utf8).
:- set_prolog_flag(double_quotes, atom).
:- consult('wn/wn_connect.pl').
:- consult('album.pl').
:- consult('album_contains.pl').
:- consult('artist.pl').
:- consult('artist_genres.pl').
:- consult('features.pl').
:- consult('genre.pl').
:- consult('published_by.pl').
:- consult('track.pl').
:- consult('tracksuggest.pl').
:- consult('artistsuggest.pl').
:- consult('likes.pl').