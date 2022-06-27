%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT source v1.3 : wn_hyponyms module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julián-Iranzo (Universidad de Castilla-La Mancha, Spain)
Fernando Sáenz-Pérez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.
*******************************************************************************/

:- module(wn_hyponyms, [
		wn_hyponyms/2,                  %% (+W_Hypernym, -List_SynSet_Hyponyms)
        wn_hyponyms/3,                  %% (+W_Hypernym, +Verbosity, -List_SynSet_Hyponyms)
        wn_gen_all_hyponyms_of/2,       %% (+Synset_ID, -List_all_Hyponym_IDs)
                                        %% useful for implementation tasks.
                                        %% It works with Synset_IDs.
        wn_hyponyms_upto_level/3,       %% (+W_Hypernym, +Level, -List_SynSet_Hyponyms)
        wn_hyponyms_upto_level/4,       %% (+W_Hypernym, +Level, +Verbosity, -List_SynSet_Hyponyms)
        wn_gen_hyponyms_upto_level/3,   %% (+Synset_ID, +Level, -List_Hyponym_IDs)
                                        %% useful for implementation tasks.
                                        %% It works with Synset_IDs.
        wn_display_graph_hyponyms/2     %% (+Word, +Level)
   ]).


:- use_module(wn_synsets).
:- use_module(wn_utilities).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_hyponyms(+W_Hypernym, -List_SynSet_Hyponyms)
%%% Given a word (term) "W_Hypernym" returns the list "List_SynSet_Hyponyms" of its
%%% hyponym synset_IDs

wn_hyponyms(W_Hypernym, List_SynSet_Hyponyms):- wn_hyponyms(W_Hypernym, verbose(yes), List_SynSet_Hyponyms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_hyponyms(+W_Hypernym, +Verbosity, -List_SynSet_Hyponyms)
%%% Given a word (term) "W_Hypernym" returns the list "List_SynSet_Hyponyms" of its
%%% hyponym synset_IDs
%%%
%%% The word "W_Hyponym" is a term with the following syntax:
%%%                      Word[:SS_type[:Sense_num]]
%%% Where ss_type is a one character code indicating the synset type:
%%    n NOUN
%%    v VERB
%%    a ADJECTIVE
%%    s ADJECTIVE SATELLITE
%%    r ADVERB
%%
%%% and "Sense_num" specifies the sense number (meaning) of the word, within the part of
%%% speech encoded in the synset_id. "Sense_num" is a natural number: 1, 2, 3, ...

wn_hyponyms(Word:SS_type:Sense_num, Verbosity, List_SynSet_Hyponyms) :-
        !,
        wordnet:wn_s(SynSet_ID, _, Word, SS_type, Sense_num, _),
        ((SS_type=n ; SS_type=v) ->
                wn_gen_all_hyponyms_of(SynSet_ID, List_SynSet_Hyponyms)
                ;
                (Verbosity = verbose(yes) ->
                    write(">>>> "), write(Word:SS_type:Sense_num), nl,
                    wordnet:wn_g(SynSet_ID, Glos),
                    write(Glos), nl,
                    write("Adjectives and adverbs do not have hyponyms"),
                    nl),
                fail
        ).

wn_hyponyms(Word:SS_type, Verbosity, List_SynSet_Hyponyms) :-
        !,
        wordnet:wn_s(SynSet_ID, _, Word, SS_type, _, _),
        ((SS_type=n ; SS_type=v) ->
            wn_gen_all_hyponyms_of(SynSet_ID, List_SynSet_Hyponyms)
            ;
            (Verbosity = verbose(yes) ->
                write(">>>> "), write(Word:SS_type), nl,
                wordnet:wn_g(SynSet_ID, Glos),
                write(Glos), nl,
                write("Adjectives and adverbs do not have hyponyms"),
                nl),
            fail
        ).

wn_hyponyms(Word, Verbosity, List_SynSet_Hyponyms) :-
        wordnet:wn_s(SynSet_ID, _, Word, SS_type, Sense_num, _),
        ((SS_type=n ; SS_type=v) ->
            wn_gen_all_hyponyms_of(SynSet_ID, List_SynSet_Hyponyms)
            ;
            (Verbosity = verbose(yes) ->
                write(">>>> "), write(Word:SS_type:Sense_num), nl,
                wordnet:wn_g(SynSet_ID, Glos),
                write(Glos), nl,
                write(" Adjectives and adverbs do not have hyponyms"),
                nl),
            fail
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% BEGIN DIFFERENCE LIST VERSION OF gen_all_hyponyms_of %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_gen_all_hyponyms_of(+Synset_ID, -List_all_Hyponym_IDs).
%%% Generates all hyponyms of a concept (Synset_ID).
%%% "List_all_Hyponym_IDs" is the list of all hyponyms of the synset "Synset_ID".
%%% The list "List_all_Hyponym_IDs" is a bag of synset_IDs hyponyms of "Synset_ID".
%%%
%%% NOTES: Because WordNet hierarchies are not actually trees, some synset nodes are
%%%        present several times. This can lead to overestimate some word frequencies
%%%        of use.
%%%
wn_gen_all_hyponyms_of(Synset_ID, List_all_Hyponym_IDs) :-
    gen_all_hyponyms_of([Synset_ID], List_all_Hyponym_IDs, []).

%%%
gen_all_hyponyms_of([]) --> [].

gen_all_hyponyms_of([Synset_ID|List_Synset_IDs]) -->
    gen_hyponyms_of(Synset_ID, List_Hyponym_IDs),
    gen_all_hyponyms_of(List_Hyponym_IDs),
    gen_all_hyponyms_of(List_Synset_IDs).


%%% gen_hyponyms_of(+Synset_ID, -List_Hyponym_IDs)
%%%
gen_hyponyms_of(Synset_ID, List_Hyponym_IDs) -->
    {findall(Hyponym_ID, wordnet:wn_hyp(Hyponym_ID, Synset_ID), List_Hyponym_IDs)},
    List_Hyponym_IDs.

%%% END DIFFERENCE LIST VERSION OF gen_all_hyponyms_of %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_hyponyms_upto_level(+W_Hypernym, +Level, -List_SynSet_Hyponyms)
%%% Given a word (term) "W_Hypernym" returns the list "List_SynSet_Hyponyms" of its
%%% hyponym synset_IDs (level by level) upto level "Level".

wn_hyponyms_upto_level(W_Hypernym, Level, List_SynSet_Hyponyms):-
    wn_hyponyms_upto_level(W_Hypernym, Level, verbose(yes), List_SynSet_Hyponyms).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_hyponyms_upto_level(+W_Hypernym, +Level, +Verbosity, -List_SynSet_Hyponyms)
%%% Given a word (term) "W_Hypernym" returns the list "List_SynSet_Hyponyms" of its
%%% hyponym synset_IDs (level by level) up to level "Level".
%%%
%%% The word "W_Hyponym" is a term with the following syntax:
%%%                      Word[:SS_type[:Sense_num]]
%%%

wn_hyponyms_upto_level(Word:SS_type:Sense_num, Level, Verbosity, List_SynSet_Hyponyms) :-
    !,
    wordnet:wn_s(SynSet_ID, _, Word, SS_type, Sense_num, _),
    ((SS_type=n ; SS_type=v) ->
        wn_gen_hyponyms_upto_level(SynSet_ID, Level, List_SynSet_Hyponyms)
        ;
        (Verbosity = verbose(yes) ->
            write(">>>> "), write(Word:SS_type:Sense_num), nl,
            wordnet:wn_g(SynSet_ID, Glos),
            write(Glos), nl,
            write("Adjectives and adverbs do not have hyponyms"),
            nl),
        fail
    ).

wn_hyponyms_upto_level(Word:SS_type, Level, Verbosity, List_SynSet_Hyponyms) :-
    !,
    wordnet:wn_s(SynSet_ID, _, Word, SS_type, _, _),
    ((SS_type=n ; SS_type=v) ->
        wn_gen_hyponyms_upto_level(SynSet_ID, Level, List_SynSet_Hyponyms)
        ;
        (Verbosity = verbose(yes) ->
            write(">>>> "), write(Word:SS_type), nl,
            wordnet:wn_g(SynSet_ID, Glos),
            write(Glos), nl,
            write("Adjectives and adverbs do not have hyponyms"),
            nl),
        fail
    ).

wn_hyponyms_upto_level(Word, Level, Verbosity, List_SynSet_Hyponyms) :-
    wordnet:wn_s(SynSet_ID, _, Word, SS_type, Sense_num, _),
    ((SS_type=n ; SS_type=v) ->
        wn_gen_hyponyms_upto_level(SynSet_ID, Level, List_SynSet_Hyponyms)
        ;
        (Verbosity = verbose(yes) ->
            write(">>>> "), write(Word:SS_type:Sense_num), nl,
            wordnet:wn_g(SynSet_ID, Glos),
            write(Glos), nl,
            write(" Adjectives and adverbs do not have hyponyms"),
            nl),
        fail
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% BEGIN DIFFERENCE LIST VERSION OF gen_hyponyms_upto_level %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_gen_hyponyms_upto_level(+Synset_ID, +Level, -List_Hyponym_IDs).
%%%
%%% "List_Hyponym_IDs" is the list of hyponyms upto level "Level" of the synset "Synset_ID".
%%% The list "List_Hyponym_IDs" is a bag of synset_IDs hyponyms of "Synset_ID".
%%%
%%% NOTES: Because WordNet hierarchies are not actually trees, some synset nodes are
%%%        present several times. This can lead to overestimate some word frequencies
%%%        of use.
%%%
wn_gen_hyponyms_upto_level(Synset_ID, Level, List_Hyponym_IDs) :-
    gen_hyponyms_upto_level([Synset_ID], Level, List_Hyponym_IDs, []).

%%%
gen_hyponyms_upto_level(_, 0) --> [].

gen_hyponyms_upto_level([], _) --> [].

gen_hyponyms_upto_level(List_Synset_IDs, Level) -->
    {Level>0, NewLevel is Level - 1},
    gen_hyponyms_of_this_level(List_Synset_IDs, List_Hyponym_IDs),
    gen_hyponyms_upto_level(List_Hyponym_IDs, NewLevel).

gen_hyponyms_upto_level(_List_Synset_IDs, Level) -->
    {Level <0},
    [].


%%%
gen_hyponyms_of_this_level([], []) --> [].

gen_hyponyms_of_this_level([Synset_ID|List_Synset_IDs], List_Hyponyms_IDs) -->
    gen_hyponyms_of(Synset_ID, Hs_Synset_ID),
    gen_hyponyms_of_this_level(List_Synset_IDs, Hs_List_Synset_IDs),
    {app(Hs_Synset_ID, Hs_List_Synset_IDs, List_Hyponyms_IDs)}.

%%%%%%%%%%%%%%%%%%%%%%%%%%
app(List1, List2, App_List) :- app(List1, List2, App_List, []).

app(List1, List2) --> List1, List2.


%%% END DIFFERENCE LIST VERSION OF gen_hyponyms_upto_level %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  GRAPHICAL REPRESENTATION OF HYPONYMS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_display_graph_hyponyms(+Word, +Level)
%%% It shows a graphic representation of the hyponyms corresponding to all the senses
%%% of the word 'Word', level by level.
%%%
%%% A node of the graph only shows the representative word of that hyponym synset (see below).
%%%
wn_display_graph_hyponyms(Word, Level) :-
    wn_synset_ID_of(Word, Synset_ID),
    gen_hyponym_arcs_upto_level(Synset_ID, Level, Graph_arcs),
    wn_display_graph(Graph_arcs).

gen_hyponym_arcs_upto_level(Synset_ID, Level, List_Hyponym_arcs) :-
    gen_hyponym_arcs_upto_level([Synset_ID], Level, List_Hyponym_arcs, []).

%%%
gen_hyponym_arcs_upto_level(_, 0) --> [].

gen_hyponym_arcs_upto_level([], _) --> [].

gen_hyponym_arcs_upto_level(List_Synset_IDs, Level) -->
    {(Level>0) -> NewLevel is Level - 1 ; fail},
    gen_hyponym_arcs_of_this_level(List_Synset_IDs, List_Hyponym_IDs),
    gen_hyponym_arcs_upto_level(List_Hyponym_IDs, NewLevel).

%%%
gen_hyponym_arcs_of_this_level([], []) --> [].

gen_hyponym_arcs_of_this_level([Synset_ID|List_Synset_IDs], List_Hyponyms_IDs) -->
    gen_hyponym_arcs_of(Synset_ID, Hs_Synset_ID),
    gen_hyponym_arcs_of_this_level(List_Synset_IDs, Hs_List_Synset_IDs),
    {app(Hs_Synset_ID, Hs_List_Synset_IDs, List_Hyponyms_IDs)}.


%%% gen_hyponym_arcs_of(+Synset_ID, -List_Hyponym_IDs)
%%%
gen_hyponym_arcs_of(Synset_ID, List_Hyponym_IDs) -->
    {findall(Hyponym_ID, wordnet:wn_hyp(Hyponym_ID, Synset_ID), List_Hyponym_IDs),
        ((List_Hyponym_IDs=[]) ->
        List_Hyponym_arcs = []
        ;
        wn_convert_synsetID_to_representative(Synset_ID, Representative),
        wn_convert_synsetIDs_to_representatives(List_Hyponym_IDs, List_Representatives),
        setof(arc(Representative,H_Representative), member(H_Representative,List_Representatives),List_Hyponym_arcs)
        )
    },
    List_Hyponym_arcs.


