%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT source v1.3 : wn_sim_measures module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julián-Iranzo (Universidad de Castilla-La Mancha, Spain)
Fernando Sáenz-Pérez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.
*******************************************************************************/

:- module(wn_sim_measures, [
      	wn_path/3,        %(+Word1:SS_type1:W1_Sense_num, +Word2:SS_type2:W2_Sense_num, -Degree)
		wn_path_nondet/3, %(+Word1:SS_type1:W1_Sense_num, +Word2:SS_type2:W2_Sense_num, -Degree)
        wn_wup/3,         %(+Word1:SS_type1:W1_Sense_num, +Word2:SS_type2:W2_Sense_num, -Degree)
        wn_wup_nondet/3,  %(+Word1:SS_type1:W1_Sense_num, +Word2:SS_type2:W2_Sense_num, -Degree)
        wn_lch/3,         %(+Word1:SS_type1:W1_Sense_num, +Word2:SS_type2:W2_Sense_num, -Degree)
        wn_lch_nondet/3   %(+Word1:SS_type1:W1_Sense_num, +Word2:SS_type2:W2_Sense_num, -Degree)
   ]).

:- use_module(wn_hypernyms).
:- use_module(wn_synsets).
:- use_module(wn_utilities).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PATH SIMILARIRY MEASURE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_path(+Word1:SS_type1:W1_Sense_num, +Word2:SS_type2:W2_Sense_num, -Degree)
%%% This predicate implements the PATH similarity measure.
%%% Takes two concepts (terms -- Word:SS_type:Sense_num) and returns the degree of
%%% similarity between them.
%%%
%%% A concept can have different HyperTrees. Therefore, depending on the different HyperTrees
%%% of c1 and c2 involved in the computation, different similarity values can be obtained:
%%%
%%%     sim_PATH(c1, c2) = 1/len(c1, c2)
%%%
%%% where len(W1, W2) = (DepthW1-LCS_depth) + (DepthW2-LCS_depth) +1
%%%
%%% This predicate combines all HyperTrees of c1 and c2, computes the respective similarity
%%% values and returns the maximum (the task is done by the auxiliary predicate max_path/3).
%%%
%%% NOTE: "Word1:SS_type1:W1_Sense_num" denotes the concept c1 and "Word2:SS_type2:W2_Sense_num"
%%%       the concept c2. Note that we do not explicitly require information about
%%%       the synset type and sense number of a word.
%%%
%%%       We check that both Word1 and Word2 are nouns or verbs but not combinations of them.
%%%

wn_path(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree) :-
((nonvar(Word1), nonvar(Word2)) ->
%  (lists:member((SS_type1,SS_type2), [(n,n), (v,v)]) ->
  (lists:member((SS_type1,SS_type2), [(n,n), (v,v)]),
    (var(W1_Sense_num) ->
        wn_max_wordnet_sense(Word1, SS_type1, W1MaxSense),
        %% Nondeterministically generates all senses of Word1
        between(1, W1MaxSense, W1_Sense_num),
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_path(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_path(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
        ;
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_path(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_path(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
    )
    ;
    write("ERROR: Both Word1 and Word2 must be either nouns or verbs (but not combinations of them or adjectives)"),
    nl
  )
  ;
  write("ERROR:  Word1 or Word2 is a variable. You must enter a specific word (either noun or verb)."),
  nl
).


%%%%%%%%%%%%%%%%%%%
%%% path(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Nondeterministic predicate. It inspects a pair of HyperTrees associated to 
%%% c1 and c2 and obtains the degree of similarity between c1 and c2
%%% (according to that pair of HyperTrees)
%%%
%%% The degree of similarity obtained depends on the HyperTrees of c1 and c2 involved
%%% in the computation.
%%%
%%%         sim_PATH(c1, c2) = 1/len(c1, c2)
%%%
%%% where len(W1, W2) = (DepthW1-LCS_depth) + (DepthW2-LCS_depth) +1
%%%
path(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    lcs(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, _, LCS_depth, DepthW1, DepthW2),
    Degree is (1 / ((DepthW1-LCS_depth) + (DepthW2-LCS_depth) +1)).


%%%%%%%%%%%%%%%%%%%
%%% max_path(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% It obtains the degree of similarity between the concepts Word1:SS_type:W1_Sense_num
%%% and Word2:SS_type:W2_Sense_num using the PATH measure.
%%%
%%% A concept can have more than one HyperTree, therefore several pairs of HyperTrees
%%% are possibly considered and a list of similarity degrees 'Degs' is obtained using path/3.
%%% Finally, the maximum degree in the list 'Degs' is selected as a result.
%%%
%%% NOTE: When this predicate is executed, it is intended that all variables in
%%%       Word1:SS_type:W1_Sense_num and in Word2:SS_type:W2_Sense_num are completely
%%%       instantiated.
%%%
max_path(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
	findall(Deg ,path(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Deg), Degs),
	lists:max_list(Degs, Degree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_path_nondet(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Inspects a pair of HyperTrees associated to c1 and c2 and obtains the degree
%%% of similarity between c1 and c2 (according to that pair of HyperTrees)
%%%
%%% NOTE: Nondeterministic predicate. It is the user interface to the local predicate path/3.
%%%
wn_path_nondet(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
   ( (SS_type=n; SS_type=v) ->
    path(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree)
    ;
	write("ERROR: Both Word1 and Word2 must be nouns or verbs but not combinations of them"),
	nl
   ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% WUP SIMILARIRY MEASURE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_wup(+Word1, +Word2, -Degree)
%%% This predicate implements the WUP similarity measure.
%%% Takes two concepts (terms -- Word:SS_type:Sense_num) and returns the degree of
%%% similarity between them.
%%%
%%% A concept can have different HyperTrees. Therefore, depending on the different HyperTrees
%%% of c1 and c2 involved in the computation, different similarity values are obtained:
%%%
%%% sim_WUP(c1,c2)= 2*depth(lcs(c1,c2)) / (Depth(c1)+Depth(c2))
%%%
%%% This predicate combines all HyperTrees of c1 and c2, computes the respective similarity
%%% values and returns the maximum (the task is done by the auxiliary predicate max_wup/3).
%%%
%%% NOTE: "Word1:SS_type1:W1_Sense_num" denotes the concept c1 and "Word2:SS_type2:W2_Sense_num"
%%%       the concept c2. Note that we do not explicitly require information about
%%%       the synset type and sense number of a word.
%%%
%%%       We check that both Word1 and Word2 are nouns or verbs but not combinations of them.
%%%

wn_wup(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree) :-
((nonvar(Word1), nonvar(Word2)) ->
%  (lists:member((SS_type1,SS_type2), [(n,n), (v,v)]) ->
  (lists:member((SS_type1,SS_type2), [(n,n), (v,v)]),
    (var(W1_Sense_num) ->
        wn_max_wordnet_sense(Word1, SS_type1, W1MaxSense),
        %% Nondeterministically generates all senses of Word1
        between(1, W1MaxSense, W1_Sense_num),
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_wup(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_wup(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
        ;
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_wup(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_wup(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
    )
    ;
    write("ERROR: Both Word1 and Word2 must be either nouns or verbs (but not combinations of them or adjectives)"),
    nl
  )
  ;
  write("ERROR: Word1 or Word2 is a variable. You must enter a specific word (either noun or verb)."),
  nl
).


%%%%%%%%%%%%%%%%%%%
%%% wup(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Nondeterministic predicate. It inspects a pair of HyperTrees associated to
%%% c1 and c2 and obtains the degree of similarity between c1 and c2
%%% (according to that pair of HyperTrees)
%%%
%%% The degree of similarity obtained depends on the HyperTrees of c1 and c2 involved
%%% in the computation.
%%%
%%%     sim_WUP(c1,c2)= 2*depth(lcs(c1,c2)) / (Depth(c1)+Depth(c2))
%%%
%%%
wup(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    lcs(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, _, LCS_depth, DepthW1, DepthW2),
    Degree is (2 * LCS_depth / (DepthW1 + DepthW2)).


%%%%%%%%%%%%%%%%%%%
%%% max_wup(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% It obtains the degree of similarity between the concepts Word1:SS_type:W1_Sense_num
%%% and Word2:SS_type:W2_Sense_num using the WUP measure.
%%%
%%% A concept can have more than one HyperTree, therefore several pairs of HyperTrees
%%% are possibly considered and a list of similarity degrees 'Degs' is obtained using wup/3.
%%% Finally, the maximum degree in the list 'Degs' is selected as a result.
%%%
%%% NOTE: When this predicate is executed, it is intended that all variables in
%%%       Word1:SS_type:W1_Sense_num and in Word2:SS_type:W2_Sense_num are completely
%%%       instantiated.
%%%
max_wup(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    findall(Deg ,wup(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Deg), Degs),
    lists:max_list(Degs, Degree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_wup_nondet(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Inspects a pair of HyperTrees associated to c1 and c2 and obtains the degree
%%% of similarity between c1 and c2 (according to that pair of HyperTrees)
%%%
%%% NOTE: Nondeterministic predicate. It is the user interface to the local predicate wup/3.
%%%
wn_wup_nondet(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
 	( (SS_type=n; SS_type=v) ->
        wup(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree)
        ;
        write("ERROR: Both Word1 and Word2 must be nouns or verbs but not combinations of them"),
        nl
	).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LCH SIMILARIRY MEASURE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_lch(+Word1, +Word2, -Degree)
%%% This predicate implements the LCH similarity measure.
%%% Takes two concepts (terms -- Word:SS_type:Sense_num) and returns the degree of
%%% similarity between them. Note that we do not explicitly require information about
%%% the synset type and sense number of a word  (that can be variables).
%%%
%%% We check that both Word1 and Word2 are nouns or verbs but not combinations of them.
%%%
%%% sim_LCH (c1, c2) = −ln[ len(c1,c2) / (2 * max{depth(c)|c in WordNet})]
%%%
%%% where len(W1, W2) = (DepthW1-LCS_depth) + (DepthW2-LCS_depth) +1
%%% NOTE 1: max{depth(c)|c in WordNet} is the maximum depth of a concept in the WordNet
%%%         data base. In practice, is a fixed constant for each part of speech
%%%           MaxDepth(n) = 20    (Nouns)
%%%           MaxDepth(v) = 14    (Verbs)
%%%
%%% This predicate combines all HyperTrees of c1 and c2, computes the respective similarity
%%% values and returns the maximum (the task is done by the auxiliary predicate max_lch/3).
%%%
%%% NOTE 2: "Word1:SS_type1:W1_Sense_num" denotes the concept c1 and "Word2:SS_type2:W2_Sense_num"
%%%       the concept c2. Note that we do not explicitly require information about
%%%       the synset type and sense number of a word.
%%%
%%%       We check that both Word1 and Word2 are nouns or verbs but not combinations of them.
%%%

wn_lch(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree) :-
((nonvar(Word1), nonvar(Word2)) ->
%  (lists:member((SS_type1,SS_type2), [(n,n), (v,v)]) ->
  (lists:member((SS_type1,SS_type2), [(n,n), (v,v)]),
    (var(W1_Sense_num) ->
        wn_max_wordnet_sense(Word1, SS_type1, W1MaxSense),
        %% Nondeterministically generates all senses of Word1
        between(1, W1MaxSense, W1_Sense_num),
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_lch(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_lch(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
        ;
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_lch(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_lch(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
    )
    ;
    write("ERROR: Both Word1 and Word2 must be either nouns or verbs (but not combinations of them or adjectives)"),
    nl
  )
  ;
  write("ERROR: Word1 or Word2 is a variable. You must enter a specific word (either noun or verb)."),
  nl
).


%%%%%%%%%%%%%%%%%%%
%%% lch(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Nondeterministic predicate. It inspects a pair of HyperTrees associated to
%%% c1 and c2 and obtains the degree of similarity between c1 and c2
%%% (according to that pair of HyperTrees)
%%%
%%% The degree of similarity obtained depends on the HyperTrees of c1 and c2 involved
%%% in the computation.
%%%
%%% sim_LCH (c1, c2) = −ln[ len(c1,c2) / (2 * max{depth(c)|c in WordNet})]
%%%
%%% where len(W1, W2) = (DepthW1-LCS_depth) + (DepthW2-LCS_depth) +1
%%% NOTE 1: max{depth(c)|c in WordNet} is the maximum depth of a concept in the WordNet
%%%         data base. In practice, is a fixed constant for each part of speech
%%%           MaxDepth(n) = 20    (Nouns)
%%%           MaxDepth(v) = 14    (Verbs)
%%%
lch(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    lcs(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, _, LCS_depth, DepthW1, DepthW2),
    Len_W1_W2 is ((DepthW1-LCS_depth) + (DepthW2-LCS_depth) + 1),
    wn_maxDepth(SS_type, MaxDepth),
    Degree is ((-1) * log(Len_W1_W2 / (2 * MaxDepth))).

%%%%%%%%%%%%%%%%%%%
%%% max_lch(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% It obtains the degree of similarity between the concepts Word1:SS_type:W1_Sense_num 
%%% and Word2:SS_type:W2_Sense_num using the LCH measure.
%%%
%%% A concept can have more than one HyperTree, therefore several pairs of HyperTrees
%%% are possibly considered and a list of similarity degrees 'Degs' is obtained using lch/3.
%%% Finally, the maximum degree in the list 'Degs' is selected as a result.
%%%
%%% NOTE: When this predicate is executed, it is intended that all variables in
%%%       Word1:SS_type:W1_Sense_num and in Word2:SS_type:W2_Sense_num are completely
%%%       instantiated.
%%%
max_lch(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
	findall(Deg ,lch(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Deg), Degs),
	lists:max_list(Degs, Degree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_lch_nondet(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Inspects a pair of HyperTrees associated to c1 and c2 and obtains the degree
%%% of similarity between c1 and c2 (according to that pair of HyperTrees)
%%%
%%% NOTE: Nondeterministic predicate. It is the user interface to the local predicate lch/3.
%%%
wn_lch_nondet(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
	( (SS_type=n; SS_type=v) ->
		lch(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree)
		;
		write("Both Word1 and Word2 must be nouns or verbs but not combinations of them"),
		nl
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% END OF MODULE MAIN PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATES FOR COMPUTING THE LESS COMMON SUBSUMER (LCS) OF TWO WORDS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% The following predicates are USEFUL FOR COMPUTING SIMILARITY MEASURES. They compute
%%% the Less Common Subsumer (LCS) of two words. For efficience reasons they also compute
%%% some counting quantities as the depth of the words from the root of a HyperTree.
%%%
%%% They extend / speciallize predicates from the module wn_hypernyms.pl
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% lcs(+Word1, +Word2, -LCS, -LCS_depth, -DepthW1, -DepthW2)
%%% Returns the Less Common Subsumer LCS of two words and its depth LCS_depth from the
%%% root of the HyperTree
%%%
%%% This predicate, additionally, returns the depth of Word1 and Word2 by efficiency
%%% reasons. We want to go through the hyperonym lists only once, so we calculate these
%%% quantities at the same time we calculate the LCS.
%%%
%%% NOTE 1:
%%% This predicate is nondeterministic.
%%% It can be used without specifying the Type and Sense of a Word, but in this case
%%% the LCS for all combinations of types and senses of these two words are obtained.
%%%
%%% If you want to obtain the LCS for two precise concepts introduce Word1:W1_Type:W1_Sense
%%% and Word2:W2_Type:W2_Sense
%%%
lcs(Word1, Word2, LCS, LCS_depth, DepthW1, DepthW2) :-
    wn_hypernyms(Word1, verbose(no), List_HyperNymSynSets1),
    wn_hypernyms(Word2, verbose(no), List_HyperNymSynSets2),
    head(List_HyperNymSynSets1, H_SynSet_ID),
    wn_virtual_root(H_SynSet_ID, Root),
    find_lcs([Root|List_HyperNymSynSets1], [Root|List_HyperNymSynSets2], LCS, LCS_depth, DepthW1, DepthW2).

head([H|_], H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% find_lcs(+List_HyperNymSynSets1, +List_HyperNymSynSets2, -LCS, -LCS_depth, -DepthW1, -DepthW2)
%%%
%%% List_HyperNymSynSets1 and List_HyperNymSynSets2 are lists of synset_IDs which are the hypernyms
%%% of the the concepts (associated to the words) W1 and W2.
%%% LCS is the less common subsumer (a specific synset_ID).
%%% LCS_depth is the depth of LCS, that is its length from the root of the HyperTree to the LCS.
%%% DepthW1 and DepthW2 are the depth of the concepts (associated to the words) W1 and W2.
%%%
%%% INITIALIZATION:
%%% LCS is initialized to zero (a virtual synset_ID).
%%% LCS_depth is initialized to 0 in order to consider the effect of adding a virtual root synset.
%%%

find_lcs(L_SynSets1, L_SynSets2, LCS, LCS_depth, DepthW1, DepthW2) :-
    find_lcs(L_SynSets1, L_SynSets2, 0, 0, LCS, LCS_depth, DepthW1, DepthW2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% find_lcs(+L_SynSets1, +L_SynSets2, +LCS_Acc, +LCS_depth_Acc, -LCS, -LCS_depth, -DepthW1, -DepthW2).
%%%
%%% Compares two lists of (HyperNym) SynSets, element by element, until a mismatch is
%%% found. Then, the LCS and its depth (which where accumulated) are returned.
%%%
%%% LCS_Acc and LCS_depth_Acc accumulate the last synset inspected and its depth respectively.
%%% Initially LCS_Acc=0 (a virtual root synset) and LCS_depth_Acc=0 (the depth of the virtual root 0).
%%%

find_lcs([], L_SynSets2, LCS_Acc, LCS_depth_Acc, LCS_Acc, LCS_depth_Acc, DepthW1, DepthW2):-
    !,
    DepthW1 = LCS_depth_Acc,
    length(L_SynSets2, Length_L_SynSets2),
    DepthW2 is LCS_depth_Acc + Length_L_SynSets2.

find_lcs(L_SynSets1, [], LCS_Acc, LCS_depth_Acc, LCS_Acc, LCS_depth_Acc, DepthW1, DepthW2):-
    !,
    length(L_SynSets1, Length_L_SynSets1),
    DepthW1 is LCS_depth_Acc + Length_L_SynSets1,
    DepthW2 = LCS_depth_Acc.

find_lcs([SS1|L_SynSets1], [SS2|L_SynSets2], _, LCS_depth_Acc, LCS, LCS_depth, DepthW1, DepthW2) :-
    SS1=:=SS2, !,
    New_LCS_depth_Acc is LCS_depth_Acc +1,
    find_lcs(L_SynSets1, L_SynSets2, SS1, New_LCS_depth_Acc, LCS, LCS_depth, DepthW1, DepthW2).

find_lcs(L_SynSets1, L_SynSets2, LCS_Acc, LCS_depth_Acc, LCS_Acc, LCS_depth_Acc, DepthW1, DepthW2):-
    length(L_SynSets1, Length_L_SynSets1),
    DepthW1 is LCS_depth_Acc + Length_L_SynSets1,
    length(L_SynSets2, Length_L_SynSets2),
    DepthW2 is LCS_depth_Acc + Length_L_SynSets2.
