%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT source v1.3 : wn_sim_adjectives module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julián-Iranzo (Universidad de Castilla-La Mancha, Spain)
Fernando Sáenz-Pérez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
This module defines predicates that find the adjectives which are similar
in meaning to a input adjective. Do not confuse "similar" with "synonym".
Synonym words are grouped in a synset and they are words equals in meaning.
In other words, synonym words must have the maximum (top) degree of similarity.

Most of predicates defined in this module a based on the operator "sim"

-------------------------
sim(synset_id,synset_id).
-------------------------
The "sim" operator specifies that the second synset is similar in meaning
to the first synset. This means that the second synset is a satellite of
the first synset (or viceversa), which is the cluster head. This relation
only holds for adjective synsets contained in adjective clusters.

The two addressed synsets are either two head synsets, or one head synset
and one satellite synset. There is no matching sim clause for two satellite
synsets. Because, if they would have similar meanings, they would be grouped
together in one synset.

Therefore, the predicates defined in this module are for adjectives and do not
work for other parts of speech.
*******************************************************************************/


:- module(wn_similar_adjectives, [
    wn_sim_adjectives_of/2,         % (+Adjective, -List_sim_SynSets)
    wn_sim_adjectives_of/3,         % (+Adjective, +Verbosity, -List_sim_SynSets)
    wn_display_sim_adjectives_of/1, % (+Adjective)
    wn_display_sim_adjectives_of/2, % (+Adjective, +Verbosity)
    wn_display_graph_sim_adjectives_of/1, % (Adjective)
    wn_display_graph_cluster_of/1 %(Adjective)
]).

:- use_module(wn_synsets).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_sim_adjectives_of(+Adjective, -List_sim_SynSets)
%% It is true if List_sim_SynSets is a list of similar adjectives synsets of the
%% adjective Adjective.
%% Only adjectives can be similar one of each other. That is, words of type "a" or "s".
%% There is no matching sim clause for two satellite synsets. Because, if they would
%% have similar meanings, they would be grouped together in one synset.
%%
%%% The word "Adjective" is a term with the following syntax:
%%%                      Word[:SS_type[:Sense_num]]
%%%%%%%%%%


wn_sim_adjectives_of(Adjective, List_sim_SynSets) :-
        wn_sim_adjectives_of(Adjective, verbose(yes), List_sim_SynSets).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_sim_adjectives_of(+Adjective, +Verbosity, -List_sim_SynSets)
%%%
wn_sim_adjectives_of(Adjective, Verbosity, [Head_Adj_SynSet|List_sim_SynSets]) :-
    get_head_adjective_of(Adjective, Verbosity, Head_Adj_SynSet),
    wordnet:wn_s(Head_Adj_SynSet, _W_num, _Head_Adj, _SS_type, _SS_num, _W_Tag_count),
    findall(Synset_ID, wordnet:wn_sim(Head_Adj_SynSet, Synset_ID), List_sim_SynSets).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  TEXTUAL REPRESENTATION OF LISTS OF SIMILAR ADJECTIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_display_sim_adjectives_of(+Adjective)
%%% Given a word (term) "Adjective", prints the list of representatives of its
%%% similar synsets. The head adjective is listed first.
%%%
%%% The word "Adjective" is a term with the following syntax:
%%%                      Word[:SS_type[:Sense_num]]
%%% as explained in the predicate wn_sim_adjectives_of/2


wn_display_sim_adjectives_of(Adjective) :-
    wn_display_sim_adjectives_of(Adjective, verbose(yes)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_display_sim_adjectives_of(+Adjective, +Verbosity)
%%%
wn_display_sim_adjectives_of(Adjective, Verbosity) :-
    wn_sim_adjectives_of(Adjective, Verbosity, List_sim_SynSets),
    display_sim_adjectives_list(Adjective, List_sim_SynSets).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% display_sim_adjectives_list(List_sim_SynSets)

display_sim_adjectives_list(Adjective, List_sim_SynSets) :-
    write(">>> SIMILAR ADJECTIVES OF "), write(Adjective), write(' :'), nl,
    display_sim_adjectives_list(List_sim_SynSets).

display_sim_adjectives_list([]).
display_sim_adjectives_list([SynSet_ID|List_SynSet_IDs]):-
        wn_synsets:wn_synset_components(SynSet_ID, Synset_Words, verbose(yes)),
        write(' ~~ '),
        write(Synset_Words), nl,
        display_sim_adjectives_list(List_SynSet_IDs).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  GRAPHICAL REPRESENTATION OF LISTS OF SIMILAR ADJECTIVES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_display_graph_sim_adjectives_of(+Adjective):
%%% Displays a graphical representation of all adjectives which are similar to "Adjective".
%%% This representation is shown as a hierarchy with the head adjective in the root and the
%%% satellite adjectives in the leaves.
%%%
wn_display_graph_sim_adjectives_of(Adjective) :-
    wn_sim_adjectives_of(Adjective, verbose(no), [Head_Adj_SynSet|List_sim_SynSets]),
    wn_convert_synsetID_to_representative(Head_Adj_SynSet, Representative),
    wn_convert_synsetIDs_to_representatives(List_sim_SynSets, List_Representatives),
    setof(arc(Representative,Y), member(Y, List_Representatives), Graph),
    wn_display_graph(Graph).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_display_graph_cluster_of(+Adjective):
%%% Displays a graphical representation of the cluster to which the "Adjective" belongs.
%%% A cluster is formed by two head adjectives, which are antonyms one of each other,
%%% and their corresponding satellite adjectives in the leaves.
%%%
wn_display_graph_cluster_of(Adjective) :-
    %% Similar adjectives of 'Adjective'
        wn_sim_adjectives_of(Adjective, verbose(no), [Head_Adj_SynSet|List_sim_SynSets]),
        wn_convert_synsetID_to_representative(Head_Adj_SynSet, Representative),
        wn_convert_synsetIDs_to_representatives(List_sim_SynSets, List_Representatives),
        setof(arc(Representative,Y), member(Y, List_Representatives), Graph1),
        write('Graph1 : '), write(Graph1), nl,
    %% Antonym adjective of 'Adjective'
        wn_ant(Head_Adj_SynSet, _, Ant_Adjective, _),
        write('Ant_Adjective : '), write(Ant_Adjective), nl,
    %% Similar adjectives of 'Ant_Adjective'
        findall(Synset_ID, wordnet:wn_sim(Ant_Adjective, Synset_ID), Ant_List_sim_SynSets),
        wn_convert_synsetID_to_representative(Ant_Adjective, Ant_Representative),
        wn_convert_synsetIDs_to_representatives(Ant_List_sim_SynSets, Ant_List_Representatives),
        setof(arc(Ant_Representative,Y), member(Y, Ant_List_Representatives), Graph2),
        write('Graph2 : '), write(Graph2), nl,
    %% Combine cluster
        app(Graph1, [arc(Representative,Ant_Representative),arc(Ant_Representative,Representative)|Graph2], Graph),
        wn_display_graph(Graph).





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%        AUXILIARY PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% get_head_adjective_of(Adjective, Head_Adj_SynSet)
%% It is true if Head_Adj_SynSet is a similar head adjective synset of the adjective
%% Adjective.
%% If Adjective is a head adjective (that is of type "a") this predicate returns itself
%% Otherwise if Adjective is a satellite adjective (that is of type "s") it looks for
%% the corresponding head adjective and returns it.
%% Only adjectives can be similar one of each other. That is, words of type "a" or "s".
%% There is no matching sim clause for two satellite synsets. Because, if they would
%% have similar meanings, they would be grouped together in one synset.
%%
%%% The word "Adjective" is a term with the following syntax:
%%%                      Word[:SS_type[:Sense_num]]
%%%%%%%%%%

get_head_adjective_of(Adjective:SS_type:SS_num, Verbosity, Head_Adj_SynSet) :-
    !,
    wordnet:wn_s(A_Synset_ID, _W_num, Adjective, SS_type, SS_num, _W_Tag_count),
    get_head_adjective_of(Adjective:SS_type:SS_num, A_Synset_ID, Verbosity, Head_Adj_SynSet).

get_head_adjective_of(Adjective:SS_type, Verbosity, Head_Adj_SynSet) :-
    !,
    wordnet:wn_s(A_Synset_ID, _W_num, Adjective, SS_type, SS_num, _W_Tag_count),
    get_head_adjective_of(Adjective:SS_type:SS_num, A_Synset_ID, Verbosity, Head_Adj_SynSet).

get_head_adjective_of(Adjective, Verbosity, Head_Adj_SynSet) :-
    wordnet:wn_s(A_Synset_ID, _W_num, Adjective, SS_type, SS_num, _W_Tag_count),
    get_head_adjective_of(Adjective:SS_type:SS_num, A_Synset_ID, Verbosity, Head_Adj_SynSet).

%%%%%
get_head_adjective_of(Adjective:SS_type:SS_num, A_Synset_ID, Verbosity, Head_Adj_SynSet) :-
    ((SS_type=a) ->
        Head_Adj_SynSet = A_Synset_ID
        ;
        ((SS_type=s)  ->
            wordnet:wn_sim(Head_Adj_SynSet, A_Synset_ID)
            ;
            ((Verbosity = verbose(yes)) ->
            write(">>>> "), write(Adjective:SS_type:SS_num), nl,
            wordnet:wn_g(A_Synset_ID, Glos),
            write(Glos), nl,
            write("Only adjectives can have a head adjective and related by the 'sim' relation"),
            nl),
            fail
        )
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%
app(List1, List2, App_List) :- app(List1, List2, App_List, []).

app(List1, List2) --> List1, List2.





