%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT source v1.3 : wn_hypernyms module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julián-Iranzo (Universidad de Castilla-La Mancha, Spain)
Fernando Sáenz-Pérez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.
*******************************************************************************/


:- module(wn_hypernyms, [
		wn_hypernyms/2, %% (+W_Hyponym, -List_SynSet_HyperNym)
        wn_hypernyms/3, %% (+W_Hyponym, +Verbosity, -List_SynSet_HyperNym)
        wn_display_hypernyms/1, %% (+W_Hyponym)
        wn_display_graph_hypernyms/1, %% (+W_Hyponym)
        wn_lcs/2, %% (List_of_Words, LCS)
        wn_lcs_nondet/2, %% (+List_of_Words, -LCS)
        wn_lcs/3, %% (+Word1, +Word2, -LCS)
        wn_lcs_nondet/3 %% (+Word1, +Word2, -LCS)
   ]).


:- use_module(wn_synsets).
:- use_module(wn_utilities).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_hypernyms(+W_Hyponym, -List_SynSet_HyperNym)
%%% Given a word (term) "W_Hyponym" returns the list "List_SynSet_HyperNym" of its
%%% hypernym synset_IDs

wn_hypernyms(W_Hyponym, List_SynSet_HyperNym):- wn_hypernyms(W_Hyponym, verbose(yes), List_SynSet_HyperNym).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_hypernyms(+W_Hyponym, +Verbosity, -List_SynSet_HyperNym)
%%% Given a word (term) "W_Hyponym" returns the list "List_SynSet_HyperNym" of its
%%% hypernym synset_IDs
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

wn_hypernyms(Word:SS_type:Sense_num, Verbosity, List_SynSet_HyperNym) :-
        !,
        wordnet:wn_s(SynSet_ID, _, Word, SS_type, Sense_num, _),
        ((SS_type=n ; SS_type=v) ->
                hypernym_chain(SynSet_ID, List_SynSet_HyperNym)
                ;
                (Verbosity = verbose(yes) ->
                    write(">>>> "), write(Word:SS_type:Sense_num), nl,
                    wordnet:wn_g(SynSet_ID, Glos),
                    write(Glos), nl,
                    write("Adjectives and adverbs do not have hypernyms"),
                    nl),
                fail
        ).

wn_hypernyms(Word:SS_type, Verbosity, List_SynSet_HyperNym) :-
        !,
        wordnet:wn_s(SynSet_ID, _, Word, SS_type, _, _),
        ((SS_type=n ; SS_type=v) ->
            hypernym_chain(SynSet_ID, List_SynSet_HyperNym)
            ;
            (Verbosity = verbose(yes) ->
                write(">>>> "), write(Word:SS_type), nl,
                wordnet:wn_g(SynSet_ID, Glos),
                write(Glos), nl,
                write("Adjectives and adverbs do not have hypernyms"),
                nl),
            fail
        ).

wn_hypernyms(Word, Verbosity, List_SynSet_HyperNym) :-
        wordnet:wn_s(SynSet_ID, _, Word, SS_type, Sense_num, _),
        ((SS_type=n ; SS_type=v) ->
            hypernym_chain(SynSet_ID, List_SynSet_HyperNym)
            ;
        (Verbosity = verbose(yes) ->
            write(">>>> "), write(Word:SS_type:Sense_num), nl,
            wordnet:wn_g(SynSet_ID, Glos),
            write(Glos), nl,
            write(" Adjectives and adverbs do not have hypernyms"),
            nl),
            fail
        ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  TEXTUAL REPRESENTATION OF HYPERNYMS CHAINS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_display_hypernyms(+W_Hyponym)
%%% Given a word (term) "W_Hyponym" prints a textual representation of the chain of its
%%% hypernym synsets
%%%
%%% The word "W_Hyponym" is a term with the following syntax:
%%%                      Word[:SS_type[:Sense_num]]
%%% as explained in the predicate wn_hypernyms/2
%%%
wn_display_hypernyms(W_Hyponym) :-
        wn_hypernyms(W_Hyponym, List_SynSet_HyperNym),
        display_hypernym_list(List_SynSet_HyperNym).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% display_hypernym_list(+List_SynSet_HyperNym)
%%%
display_hypernym_list([]).
display_hypernym_list([SynSet_ID|List_SynSet_IDs]):-
        wn_synsets:wn_synset_components(SynSet_ID, Synset_Words),
        write(Synset_Words), nl,
        write(' >> '),
        display_hypernym_list(List_SynSet_IDs).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% hypernym_chain(+SynSet_Hyponym, -List_SynSet_HyperNym)
%%% Given a synset_ID "SynSet_Hyponym" returns the list "List_SynSet_HyperNym" of its
%%% hypernym synset_IDs
%%%
hypernym_chain(SynSet_Hyponym, List_SynSet_HyperNym):-
        hypernym_chain(SynSet_Hyponym, [SynSet_Hyponym], List_SynSet_HyperNym).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% hypernym_chain(+SynSet_Hyponym, +List_SynSet_HyperNym_Acc, -List_SynSet_HyperNym)
%%%
hypernym_chain(SS_Hyp, List_SS_Hyper_Acc, List_SS_Hyper) :-
        (wordnet:wn_hyp(SS_Hyp, SS_Hyper),
        hypernym_chain(SS_Hyper, [SS_Hyper|List_SS_Hyper_Acc], List_SS_Hyper)
        ;
        \+(wordnet:wn_hyp(SS_Hyp, _SS_Hyper)),
        List_SS_Hyper = List_SS_Hyper_Acc
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  GRAPHICAL REPRESENTATION OF HYPERNYMS CHAINS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_display_graph_hypernyms(+Word)
%%% It shows a graphic representation of all hypernyms corresponding to all the senses
%%% of the word 'Word'.
%%%
%%% A node of the graph only shows the representative word of that hypernym synset (see below).
%%%
wn_display_graph_hypernyms(Word) :-
setof(arc(X,Y),List^H^T^(list_of_representative_hypernyms(Word,List),append(H,[X,Y|T],List)),Graph), wn_display_graph(Graph).

%%% list_of_representative_hypernyms(+Word,-List_representative_HyperNyms)
%%%
%%%
list_of_representative_hypernyms(Word,List_representative_HyperNyms) :-
        wn_hypernyms(Word, verbose(no), List_SynSet_HyperNyms),
        wn_convert_synsetIDs_to_representatives(List_SynSet_HyperNyms, List_representative_HyperNyms).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATES FOR COMPUTING THE LESS COMMON SUBSUMER (LCS) OF TWO WORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_lcs(+Word1, +Word2, -LCS)
%%% Calculates the Less Common Subsumer (LCS) of Word1 and Word2.
%%%
%%% NOTE: Deterministic version of wn_lcs_nondet/3.
%%% It evaluates all the LCSs obtained by confronting all the HyperTrees of Word1 and
%%% Word2, calculates the depth of them (measured from the root of the hierarchy) and
%%% selects the LCS with greater depth.
%%%
wn_lcs(Word1, Word2, LCS) :-
    setof((LCSi, LCSi_depth), lcs(Word1, Word2, LCSi, LCSi_depth), Assoc_list_of_LCSis),
    %% write('Assoc_list_of_LCSis : '), write(Assoc_list_of_LCSis), nl,
    deepest_LCS(Assoc_list_of_LCSis, (LCS,_Value)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_lcs_nondet(+Word1, +Word2, -LCS)
%%% Calculates the Less Common Subsumer (LCS) of Word1 and Word2.
%%%
%%% Non-deterministic version of wn_lcs/2. All possible HyperTrees are considered and
%%% an answer is delivered for each pair of those HyperTrees.
%%%
wn_lcs_nondet(Word1, Word2, LCS) :- lcs(Word1, Word2, LCS, _).


%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% lcs(+Word1, +Word2, -LCS, -LCS_depth)
%%%
%%% NOTE:
%%% This predicate, additionally, returns the depth of the LCS by efficiency
%%% reasons. We want to go through the hyperonym lists only once, so we calculate this
%%% quantity at the same time we calculate the LCS.
%%%
lcs(Word1, Word2, LCS, LCS_depth) :-
    wn_hypernyms(Word1, verbose(no), List_HyperNymSynSets1),
    wn_hypernyms(Word2, verbose(no), List_HyperNymSynSets2),
    find_lcs(List_HyperNymSynSets1, List_HyperNymSynSets2, LCS, LCS_depth).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% find_lcs(+List_HyperNymSynSets1, +List_HyperNymSynSets2, -LCS, -LCS_depth)
%%%
%%% List_HyperNymSynSets1 and List_HyperNymSynSets2 are lists of synset_IDs which are the hypernyms
%%% of the the concepts (associated to the words) W1 and W2.
%%% LCS is the less common subsumer (a specific synset_ID).
%%% LCS_depth is the depth of the LCS, that is the path length from the root of the HyperTree to the LCS.
%%%
%%% INITIALIZATION:
%%% LCS is initialized to zero (a virtual synset_ID).
%%% LCS_depth is initialized to 0 in order to consider the effect of adding a virtual root synset.
%%%

find_lcs(L_SynSets1, L_SynSets2, LCS, LCS_depth) :-
    find_lcs(L_SynSets1, L_SynSets2, 0, 0, LCS, LCS_depth).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% find_lcs(+L_SynSets1, +L_SynSets2, +LCS_Acc, +LCS_depth_Acc, -LCS, -LCS_depth).
%%%
%%% Compares two lists of (HyperNym) SynSets, element by element, until a mismatch is
%%% found. Then, the LCS and its depth (which where accumulated) are returned.
%%%
%%% LCS_Acc and LCS_depth_Acc accumulate the last synset inspected and its depth respectively.
%%% Initially LCS_Acc=0 (a virtual root synset) and LCS_depth_Acc=1 (the depth to the virtual root 0).
%%%

find_lcs([], _, LCS_Acc, LCS_depth_Acc, LCS_Acc, LCS_depth_Acc):-
    !.

find_lcs(_, [], LCS_Acc, LCS_depth_Acc, LCS_Acc, LCS_depth_Acc):-
    !.

find_lcs([SS1|L_SynSets1], [SS2|L_SynSets2], _, LCS_depth_Acc, LCS, LCS_depth) :-
    SS1=:=SS2, !,
    New_LCS_depth_Acc is LCS_depth_Acc +1,
    find_lcs(L_SynSets1, L_SynSets2, SS1, New_LCS_depth_Acc, LCS, LCS_depth).

find_lcs(_, _, LCS_Acc, LCS_depth_Acc, LCS_Acc, LCS_depth_Acc).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATES FOR COMPUTING THE LESS COMMON SUBSUMER (LCS) OF A SET OF WORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% The following predicates are USEFUL TO AUTOMATE A GENERALIZATION PROCESS of concepts.
%%% Although they also compute the Less Common Subsumer (LCS) of a set of words, we are
%%% not interested on counting information. Therefore, we addapt some of the previous predicates
%%% for efficient execution, in order to avoid redundant operations.
%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_lcs(+List_of_Words, -LCS)
%%% Returns the Less Common Subsumer LCS of a set of words.
%%%
%%% This is a deterministic predicate. If a set of words is provided and the type and sense of the
%%% words are not set or the concepts have several HyperTrees, all possible HyperTrees are
%%% considered and confronted. Then a set of LCSs is obtained (jointly with their depth) and the
%%% deepest LCS is selected.
%%%
wn_lcs([Word], Synset_ID):- wn_synset_ID_of(Word,Synset_ID).
wn_lcs([Word|List_of_Words], LCS):-
    setof(LCS_Pair, lcs_nondet([Word|List_of_Words], LCS_Pair), Assoc_list_of_LCSis),
    %% write('Assoc_list_of_LCSis = '), write(Assoc_list_of_LCSis), nl,
    deepest_LCS(Assoc_list_of_LCSis, (LCS,_Value)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_lcs_nondet(+List_of_Words, -LCS)
%%% Returns the Less Common Subsumer LCS of a set of words.
%%%
%%% This is a non-deterministic predicate. All possible HyperTrees are considered and an answer is
%%% delivered for each combination of those HyperTrees.
%%%
wn_lcs_nondet([Word], Synset_ID):- wn_synset_ID_of(Word,Synset_ID).
wn_lcs_nondet([Word|List_of_Words], LCS):- lcs_nondet([Word|List_of_Words], (LCS, _)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% lcs_nondet(+List_of_Words, -LCS_Pair)
%%% Returns the Less Common Subsumer LCS Pair of a set of words.
%%%
%%% This a deterministic predicate. All possible HyperTrees are considered and an answer is
%%% delivered for each pair of those HyperTrees. The answer is a pair (LCS, LCS depth)
%%%
lcs_nondet(List_of_Words, LCS_Pair):-
    words2hyperNyms(List_of_Words, List_of_HyperTrees),
    %% write(List_of_HyperTrees), nl,
    pairwise_find_lcs(List_of_HyperTrees, LCS_Pair).

%%%%%%%%%%%%%%%
%%% words2hyperNyms(+List_of_Words, -List_of_HyperTrees):
%%% Given a list of words returns a list compounded by one HyperTree for each one of the words.
%%%
words2hyperNyms([], []).
words2hyperNyms([Word|List_of_Words], [List_HyperNymSynSets|List_of_HyperTrees]):-
    wn_hypernyms(Word, verbose(no), List_HyperNymSynSets),
    words2hyperNyms(List_of_Words, List_of_HyperTrees).

%%%%%%%%%%%%%%%
%%% pairwise_find_lcs(+List_of_HyperTrees, -LSC_Pair)
%%% Takes the List_of_HyperTrees corresponding to a set of words and returns the common
%%% LCS and the LCS_depth.
%%%
pairwise_find_lcs(List_of_HyperTrees, LSC_Pair) :-
    pairwise_find_lcs_hyperNyms(List_of_HyperTrees, (0,0), LSC_Pair).

%%%%%%%%%%%%%%%
%%% pairwise_find_lcs_hyperNyms(+List_of_HyperTrees, -LCS_Pair_Acc, -LCS_Pair):
%%%
%%% Recursively compares the two first HyperTrees in List_of_HyperTrees and computes the
%%% New_LCS and the set of its hypernyms, New_LSC_HyperNyms. The New_LSC_HyperNyms are
%%% concatenated to the remained list of HyperTrees and the process continues until
%%% the original List_of_HyperTrees is reduced to only one HyperTree. When that point is
%%% reached, the LCS_Pair_Acc contains the LCS of the set of words and the depth of the LCS.
%%%
pairwise_find_lcs_hyperNyms([_HTree], LCS_Pair_Acc, LCS_Pair_Acc).
pairwise_find_lcs_hyperNyms([HTree1, HTree2|List_of_HyperTrees], _LCS_Pair_Acc, LCS_Pair) :-
    find_lcs_hyperNyms(HTree1, HTree2, New_LCS, New_LCS_depth, New_LSC_HyperNyms),
    pairwise_find_lcs_hyperNyms([New_LSC_HyperNyms|List_of_HyperTrees], (New_LCS, New_LCS_depth), LCS_Pair).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% find_lcs_hyperNyms(+List_HyperNymSynSets1, +List_HyperNymSynSets2, -LCS, -LCS_depth, -LSC_HyperNyms)
%%%
%%% List_HyperNymSynSets1 and List_HyperNymSynSets2 are lists of synset_IDs which are the
%%% hypernyms of the concepts (associated to the words) W1 and W2.
%%% LCS is the less common subsumer (a specific synset_ID) of the concepts (associated to the
%%% words) W1 and W2.
%%% LCS_depth is the depth of LCS, that is its length from the root of the HyperTree to the LCS.
%%% LSC_HyperNyms is the list of the hypernyms of the LCS (a common fragment of the HyperTrees
%%% List_HyperNymSynSets1 and List_HyperNymSynSets2)
%%%
%%% INITIALIZATION:
%%% LCS is initialized to zero (a virtual synset_ID).
%%% LSC_HyperNyms is initialized to [] (an empty list of HyperNyms).
%%%

find_lcs_hyperNyms(L_SynSets1, L_SynSets2, LCS, LCS_depth, LSC_HyperNyms) :-
    find_lcs_hyperNyms(L_SynSets1, L_SynSets2, 0, LCS, 0, LCS_depth, [], LSC_HyperNyms).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% find_lcs_hyperNyms(L_SynSets1,L_SynSets2, LCS_Acc,LCS, LCS_depth_Acc,LCS_depth, LSC_HyperNyms_Acc,LSC_HyperNyms).
%%%
%%% Compares two lists of (HyperNym) SynSets, element by element, until a mismatch is
%%% found. Then, the LCS is returned.
%%%
%%% LCS_Acc and LCS_depth_Acc accumulate the last synset inspected and its depth respectively.
%%% Initially LCS_Acc=0 (a virtual root synset) and LCS_depth_Acc=0 (the depth of the virtual root 0).
%%% LSC_HyperNyms accumulates the list of HyperNyms of the LCS in reverse order.
%%% Initially LSC_HyperNyms_Acc=[].
%%%

find_lcs_hyperNyms([], _, LCS_Acc, LCS_Acc, LCS_depth_Acc,LCS_depth_Acc, LSC_HyperNyms_ACC, LSC_HyperNyms):-
    !,
    reverse(LSC_HyperNyms_ACC, LSC_HyperNyms).

find_lcs_hyperNyms(_, [], LCS_Acc, LCS_Acc, LCS_depth_Acc,LCS_depth_Acc, LSC_HyperNyms_ACC, LSC_HyperNyms):-
    !,
    reverse(LSC_HyperNyms_ACC, LSC_HyperNyms).

find_lcs_hyperNyms([SS1|L_SynSets1], [SS2|L_SynSets2], _, LCS, LCS_depth_Acc,LCS_depth, LSC_HyperNyms_ACC, LSC_HyperNyms) :-
    SS1=:=SS2, !,
    New_LCS_depth_Acc is LCS_depth_Acc +1,
    find_lcs_hyperNyms(L_SynSets1,L_SynSets2, SS1,LCS, New_LCS_depth_Acc,LCS_depth, [SS1|LSC_HyperNyms_ACC], LSC_HyperNyms).

find_lcs_hyperNyms(_, _, LCS_Acc, LCS_Acc, LCS_depth_Acc,LCS_depth_Acc, LSC_HyperNyms_ACC, LSC_HyperNyms):-
    reverse(LSC_HyperNyms_ACC, LSC_HyperNyms).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% deepest_LCS(Lis_of_LCSs, Pair)
%%%% Takes a list of pairs (LCS, LCS_depth) and returns the pair with a greatest LCS_depth
%%%%
%%%% NOTE: LCS_depth is the depth of the LCS, that is the path length from the root of the
%%%%       HyperTree to the LCS.
%%%%
deepest_LCS(Lis_of_LCSs, Pair) :- deepest_LCS(Lis_of_LCSs, (0,0), Pair).

deepest_LCS([], Pair, Pair).

deepest_LCS([(LCS, Value)|Lis_of_LCSs], (_LCS_Acc, Value_Acc), Pair) :-
        Value > Value_Acc, !, deepest_LCS(Lis_of_LCSs, (LCS, Value), Pair).

deepest_LCS([(_LCS, _Value)|Lis_of_LCSs], (LCS_Acc, Value_Acc), Pair) :-
        deepest_LCS(Lis_of_LCSs, (LCS_Acc, Value_Acc), Pair).




