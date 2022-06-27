%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT source v1.2 : wn_synsets module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julián-Iranzo (Universidad de Castilla-La Mancha, Spain)
Fernando Sáenz-Pérez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.
*******************************************************************************/

:- module(wn_synsets, [
    wn_word_info/1,         %% (+Word)
    wn_gloss_of/2,          %% (+Word, -Gloss)
    wn_synset_ID_of/2,      %% (+Word, -W_Synset_ID)
    wn_synset_of/2,         %% (+Word, -W_synset)
    wn_synset_of/3,         %% (+Word, -W_synset, +Verbosity)
    wn_synset_components/2, %% (+Synset_ID, -Synset_Words)
    wn_synset_components/3  %% (+Synset_ID, -Synset_Words, +Verbosity)
]).

:- if(getenv('WNDEVEL', yes)).
:- use_module(wn_portray).
:- else.
:- use_module(wn).
:- endif.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_word_info(+Word): prints the information about a word stored in the
%% database 'wn_s.pl' of Wordnet.
%%
%% NOTICE: s(synset_id, w_num, ’word’, ss_type, sense_number, tag_count).
%%
%% w_num, if present, indicates which word in the synset is being referred to.
%%
%% ss_type is a one character code indicating the synset type:
%%    n NOUN
%%    v VERB
%%    a ADJECTIVE
%%    s ADJECTIVE SATELLITE
%%    r ADVERB
%%
%% sense_number specifies the sense number of the word, within the part of
%% speech encoded in the synset_id
%%

wn_word_info(Word:Ss_type:Sense_number) :-
        !,
        wordnet:wn_s(Synset_id, W_num, Word, Ss_type, Sense_number, Tag_count),
        display_info(Synset_id, W_num, Word, Ss_type, Sense_number, Tag_count).

wn_word_info(Word:Ss_type) :-
        !,
        wordnet:wn_s(Synset_id, W_num, Word, Ss_type, Sense_number, Tag_count),
        display_info(Synset_id, W_num, Word, Ss_type, Sense_number, Tag_count).

wn_word_info(Word) :-
        wordnet:wn_s(Synset_id, W_num, Word, Ss_type, Sense_number, Tag_count),
        display_info(Synset_id, W_num, Word, Ss_type, Sense_number, Tag_count).


display_info(Synset_id, W_num, Word, Ss_type, Sense_number, Tag_count) :-
        write('INFORMATION ABOUT THE WORD \''), write(Word), write('\' : '), nl,
        tab(1), write('Synset_id = '), write(Synset_id), nl,
        tab(1), write('Word Order num. = '), write(W_num), nl,
        tab(1), write('Synset type (n:NOUN, v:VERB, a:ADJ., s:ADJ. SAT., r:ADV.) = '), write(Ss_type), nl,
        tab(1), write('Sense number = '), write(Sense_number), nl,
        tab(1), write('Tag_count = '), write(Tag_count), nl,
        tab(1), write('-----------'), nl,
        tab(1), write('Gloss: '), wordnet:wn_g(Synset_id, G), nl,
        tab(1), write(G), nl,
        tab(1), write('-----------'), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_gloss_of(+Word, -Gloss)
%% Returns the Gloss of a Word
%%
%% Word terms follow the syntax "Word[:SS_type[:Sense_num]]"
%%
wn_gloss_of(Word:W_SS_type:W_Sense_Num, Gloss) :-
    !,
    wordnet:wn_s(W_Synset_ID, _W_num, Word, W_SS_type, W_Sense_Num, _W_Tag_count),
    wordnet:wn_g(W_Synset_ID, Gloss).

wn_gloss_of(Word:W_SS_type, Gloss) :-
    !,
    wordnet:wn_s(W_Synset_ID, _W_num, Word, W_SS_type, _W_Sense_Num, _W_Tag_count),
    wordnet:wn_g(W_Synset_ID, Gloss).

wn_gloss_of(Word, Gloss) :-
    !,
    wordnet:wn_s(W_Synset_ID, _W_num, Word, _W_SS_type, _W_Sense_Num, _W_Tag_count),
    wordnet:wn_g(W_Synset_ID, Gloss).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_synset_ID_of(+Word, -W_Synset_ID),
%% W_Synset_ID is the synset ID to which W belongs.
%%
%% Word terms follow the syntax "Word[:SS_type[:Sense_num]]"
%%
wn_synset_ID_of(Word:W_SS_type:W_SS_Num, W_Synset_ID) :-
    !,
    wordnet:wn_s(W_Synset_ID, _W_num, Word, W_SS_type, W_SS_Num, _W_Tag_count).

wn_synset_ID_of(Word:W_SS_type, W_Synset_ID) :-
    !,
    wordnet:wn_s(W_Synset_ID, _W_num, Word, W_SS_type, _W_SS_Num, _W_Tag_count).

wn_synset_ID_of(Word, W_Synset_ID) :-
    wordnet:wn_s(W_Synset_ID, _W_num, Word, _W_SS_type, _W_SS_Num, _W_Tag_count).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_synset_of(+Word, -W_synset), W_synset is the synset to which Word belongs.
%% W_synset is represented by a set of words that are synonyms of Word.
%%
%% The input Word terms follow the syntax "Word[:SS_type[:Sense_num]]" and the
%% words in the output list W_synset are represented as ground terms "word:ss_type:sense_num"
%%
%% It is equivalent to calling wn_synset_of/3 setting the third parameter to ``verbose(yes)''.
%% That is,
%%            wn_synset_of(Synset_ID, Synset_Words, verbose(yes)).
%%

wn_synset_of(Word, W_synset) :-
		wn_synset_ID_of(Word, W_Synset_ID),
		wn_synset_components(W_Synset_ID, W_synset, verbose(yes)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_synset_of(+Word, -W_synset, +Verbosity),
%% W_synset is the synset to which Word belongs.
%% W_synset is represented by a set of words that are synonyms of Word.
%% Depending on the value of Verbosity (see below) it show information
%% of the type and sense of the word component of the synset.
%%
%% The input Word terms follow the syntax "Word[:SS_type[:Sense_num]]".
%% If the option Verbosity is set to "verbose(no)", the output list W_synset is a list of
%% plain words. On the other hand if the option Verbosity is "verbose(yes)", the
%% words in the output list W_synset are represented as ground terms "word:ss_type:sense_num"
%%

wn_synset_of(Word, W_synset, Verbosity) :-
        wn_synset_ID_of(Word, W_Synset_ID),
        wn_synset_components(W_Synset_ID, W_synset, Verbosity).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_synset_components(+Synset_ID, -Synset_Words),
%% Synset_Words is the list of words that compounds the synset Synset_ID.
%% The words in the output list Synset_Words are represented as ground terms
%% "word:ss_type:sense_num"
%%
%% It is equivalent to calling wn_synset_components/3 setting the third parameter
%% to ``verbose(yes)''. That is,
%%             wn_synset_components(Synset_ID, Synset_Words, verbose(yes)).
%%

wn_synset_components(Synset_ID, Synset_Words) :-
        wn_synset_components(Synset_ID, Synset_Words, verbose(yes)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_synset_components(+Synset_ID, -Synset_Words, +Verbosity),
%% Synset_Words is the list of words that compounds the synset Synset_ID
%%
%% "Verbosity" is a parameter that controls the degree of information shown.
%% It can be set to "verbose(no)" or "verbose(yes)". If the option Verbosity is set
%% to "verbose(no)", the output list W_synset is a list of plain words. In the second
%% case for each word W in the synset, its syntactic type SS_type and its sense number
%% SS_Num are shown.
%%
wn_synset_components(Synset_ID, Synset_Words, verbose(no)) :-
    integer(Synset_ID),
    Synset_ID > 100000000,
    Synset_ID < 500000000,
    findall(Word, wordnet:wn_s(Synset_ID, _, Word, _, _, _), Synset_Words).

wn_synset_components(Synset_ID, Synset_Words, verbose(yes)) :-
    integer(Synset_ID),
    Synset_ID > 100000000,
    Synset_ID < 500000000,
    findall(Word:SS_type:SS_Num, wordnet:wn_s(Synset_ID, _, Word, SS_type, SS_Num, _), Synset_Words).
