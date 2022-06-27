%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT source v1.3 : wn_ic_measures module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julián-Iranzo (Universidad de Castilla-La Mancha, Spain)
Fernando Sáenz-Pérez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.
*******************************************************************************/

:- module(wn_ic_measures, [
        wn_res/3,        %% (+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
        wn_res_nondet/3, %% (+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
        wn_jcn/3,        %% (+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
        wn_jcn_nondet/3, %% (+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
        wn_lin/3,        %% (+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
        wn_lin_nondet/3, %% (+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
        wn_information_content/2 %%% (+Word, -IC)
   ]).

:- use_module(wn_hypernyms).
:- use_module(wn_hyponyms).
:- use_module(wn_synsets).

:- use_module(wn_utilities).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RESNIK SIMILARIRY MEASURE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_res(+Word1, +Word2, -Degree)
%%% This predicate implements the RESNIK similarity measure, based on information content.
%%% Takes two concepts (terms -- Word:SS_type:Sense_num) and returns the degree of
%%% similarity between them.
%%%
%%% A concept can have different HyperTrees. Therefore, depending on the HyperTrees of c1
%%% and c2 involved in the computation, different LCSs are obtained, leading to different
%%% similarity values:
%%%
%%%         sim_RES(c1,c2)= IC(lcs(c1,c2))
%%%
%%% This predicate combines all HyperTrees of c1 and c2, computes the respective similarity
%%% values and returns the maximum (the task is done by the auxiliary predicate max_res/3).
%%%
%%% NOTE: "Word1:SS_type1:W1_Sense_num" denotes the concept c1 and "Word2:SS_type2:W2_Sense_num"
%%%       the concept c2. Note that we do not explicitly require information about
%%%       the synset type and sense number of a word.
%%%
%%%       We check that both Word1 and Word2 are nouns or verbs but not combinations of them.
%%%

wn_res(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree) :-
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
            max_res(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_res(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
        ;
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_res(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_res(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
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
%%% res(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Nondeterministic predicate. It obtains the degree of similarity of two concepts, associated
%%% to the concepts c1 and c2.
%%%
%%% The degree of similarity obtained depends on the HyperTrees of c1 and c2 involved
%%% in the computation. Since for different HyperTrees, different LCSs are obtained.
%%%
%%%     Degree = sim_RES(c1,c2)= IC(lcs(c1,c2))
%%%
%%%
res(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    lcs(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, LCS, Root_ID),
    information_content(LCS, Root_ID, Degree).


%%%%%%%%%%%%%%%%%%%
%%% max_res(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% It obtains the degree of similarity between the concepts Word1:SS_type:W1_Sense_num
%%% and Word2:SS_type:W2_Sense_num using the RESNIK measure.
%%%
%%% A concept can have more than one HyperTree, therefore several pairs of HyperTrees
%%% are possibly considered and a list of similarity degrees 'Degs' is obtained using res/3.
%%% Finally, the maximum degree in the list 'Degs' is selected as a result.
%%%
%%% NOTE: When this predicate is executed, it is intended that all variables in
%%%       Word1:SS_type:W1_Sense_num and in Word2:SS_type:W2_Sense_num are completely
%%%       instantiated.
%%%
max_res(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    findall(Deg ,res(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Deg), Degs),
    max_list(Degs, Degree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_res_nondet(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Inspects a pair of HyperTrees associated to c1 and c2 and obtains the degree
%%% of similarity between c1 and c2 (according to that pair of HyperTrees)
%%%
%%% NOTE: Nondeterministic predicate. It is the user interface to the private predicate res/3.
%%%
wn_res_nondet(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    ( (SS_type=n; SS_type=v) ->
        res(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree)
        ;
        write("ERROR: Both Word1 and Word2 must be nouns or verbs but not combinations of them"),
        nl
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% JIANG & CONRATH SIMILARIRY MEASURE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_jcn(+Word1, +Word2, -Degree)
%%% This predicate implements the JIANG & CONRATH similarity measure, based on information
%%% content.
%%% Takes two concepts (terms -- Word:SS_type:Sense_num) and returns the degree of
%%% similarity between them.
%%%
%%% A concept can have different HyperTrees. Therefore, depending on the HyperTrees of c1
%%% and c2 involved in the computation, different LCSs are obtained, leading to different
%%% similarity values:
%%%
%%%         sim_JCN(c1,c2)= 1/ [IC(c1) + IC(c2) - 2*IC(lcs(c1,c2))]
%%%
%%% This predicate combines all HyperTrees of c1 and c2, computes the respective similarity
%%% values and returns the maximum (the task is done by the auxiliary predicate max_jcn/3).
%%%
%%% NOTE: "Word1:SS_type1:W1_Sense_num" denotes the concept c1 and "Word2:SS_type2:W2_Sense_num"
%%%       the concept c2. Note that we do not explicitly require information about
%%%       the synset type and sense number of a word.
%%%

wn_jcn(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree) :-
((nonvar(Word1), nonvar(Word2)) ->
%  (member((SS_type1,SS_type2), [(n,n), (v,v)]) ->
  (lists:member((SS_type1,SS_type2), [(n,n), (v,v)]),
    (var(W1_Sense_num) ->
        wn_max_wordnet_sense(Word1, SS_type1, W1MaxSense),
        %% Nondeterministically generates all senses of Word1
        between(1, W1MaxSense, W1_Sense_num),
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_jcn(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_jcn(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
        ;
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_jcn(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_jcn(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
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
%%% jcn(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Nondeterministic predicate. It obtains the degree of similarity of two concepts, associated
%%% to the concepts c1 and c2.
%%%
%%% The degree of similarity obtained depends on the HyperTrees of c1 and c2 involved
%%% in the computation. Since for different HyperTrees, different LCSs are obtained.
%%%
%%%     Degree = sim_JCN(c1,c2)= 1/ [IC(c1) + IC(c2) - 2*IC(lcs(c1,c2))]
%%%
%%%
jcn(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    lcs(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, LCS, Root_ID),
    wn_synset_ID_of(Word1:SS_type:W1_Sense_num, W1_synset_ID),
    wn_synset_ID_of(Word2:SS_type:W2_Sense_num, W2_synset_ID),
    information_content(W1_synset_ID, Root_ID, W1_IC),
    information_content(W2_synset_ID, Root_ID, W2_IC),
    information_content(LCS, Root_ID, LCS_IC),
    ((W1_IC + W2_IC =:= 2*LCS_IC) ->
        %%% Zero distance between concepts W1 and W2 (usually W1 and W2 are the same word)
        %%% We use the concept of "almost zero distance" proposed by S. Patwardhan.
        frequency_of_use(Root_ID, F_root),
        Degree is 1/ (-log((F_root - 1)/ F_root))
        ;
        Degree is 1/ (W1_IC + W2_IC - 2*LCS_IC)
    ).


%%%%%%%%%%%%%%%%%%%
%%% max_jcn(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% It obtains the degree of similarity between the concepts Word1:SS_type:W1_Sense_num
%%% and Word2:SS_type:W2_Sense_num using the JIANG & CONRATH measure.
%%%
%%% A concept can have more than one HyperTree, therefore several pairs of HyperTrees
%%% are possibly considered and a list of similarity degrees 'Degs' is obtained using jcn/3.
%%% Finally, the maximum degree in the list 'Degs' is selected as a result.
%%%
%%% NOTE: When this predicate is executed, it is intended that all variables in
%%%       Word1:SS_type:W1_Sense_num and in Word2:SS_type:W2_Sense_num are completely
%%%       instantiated.
%%%
max_jcn(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    findall(Deg ,jcn(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Deg), Degs),
    max_list(Degs, Degree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_jcn_nondet(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Inspects a pair of HyperTrees associated to c1 and c2 and obtains the degree
%%% of similarity between c1 and c2 (according to that pair of HyperTrees)
%%%
%%% NOTE: Nondeterministic predicate. It is the user interface to the private predicate jcn/3.
%%%
wn_jcn_nondet(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    ( (SS_type=n; SS_type=v) ->
        jcn(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree)
        ;
        write("ERROR: Both Word1 and Word2 must be nouns or verbs but not combinations of them"),
        nl
    ).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LIN SIMILARIRY MEASURE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_lin(+Word1, +Word2, -Degree)
%%% This predicate implements the LIN similarity measure, based on information content.
%%% Takes two concepts (terms -- Word:SS_type:Sense_num) and returns the degree of
%%% similarity between them.
%%%
%%% A concept can have different HyperTrees. Therefore, depending on the HyperTrees of c1
%%% and c2 involved in the computation, different LCSs are obtained, leading to different
%%% similarity values:
%%%
%%%         sim_LIN(c1, c2) = [2 * IC(lcs(c1,c2))] / [IC(c1)+IC(c2)]
%%%
%%% This predicate combines all HyperTrees of c1 and c2, computes the respective similarity
%%% values and returns the maximum (the task is done by the auxiliary predicate max_lin/3).
%%%
%%% NOTE: "Word1:SS_type1:W1_Sense_num" denotes the concept c1 and "Word2:SS_type2:W2_Sense_num"
%%%       the concept c2. Note that we do not explicitly require information about
%%%       the synset type and sense number of a word.
%%%
%%%       We check that both Word1 and Word2 are nouns or verbs but not combinations of them.
%%%

wn_lin(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree) :-
((nonvar(Word1), nonvar(Word2)) ->
%  (member((SS_type1,SS_type2), [(n,n), (v,v)]) ->
  (lists:member((SS_type1,SS_type2), [(n,n), (v,v)]),
    (var(W1_Sense_num) ->
        wn_max_wordnet_sense(Word1, SS_type1, W1MaxSense),
        %% Nondeterministically generates all senses of Word1
        between(1, W1MaxSense, W1_Sense_num),
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_lin(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_lin(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
        )
        ;
        (var(W2_Sense_num) ->
            wn_max_wordnet_sense(Word2, SS_type2, W2MaxSense),
            %% Nondeterministically generates all senses of Word2
            between(1, W2MaxSense, W2_Sense_num),
            max_lin(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
            ;
            max_lin(Word1:SS_type1:W1_Sense_num, Word2:SS_type2:W2_Sense_num, Degree)
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
%%% lin(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Nondeterministic predicate. It obtains the degree of similarity of two concepts, associated
%%% to the concepts c1 and c2.
%%%
%%% The degree of similarity obtained depends on the HyperTrees of c1 and c2 involved
%%% in the computation. Since for different HyperTrees, different LCSs are obtained.
%%%
%%%     Degree = sim_LIN(c1,c2)= [2 * IC(lcs(c1,c2))] / [IC(c1) + IC(c2)]
%%%
%%%
lin(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    lcs(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, LCS, Root_ID),
    wn_synset_ID_of(Word1:SS_type:W1_Sense_num, W1_synset_ID),
    wn_synset_ID_of(Word2:SS_type:W2_Sense_num, W2_synset_ID),
    information_content(W1_synset_ID, Root_ID, W1_IC),
    information_content(W2_synset_ID, Root_ID, W2_IC),
    information_content(LCS, Root_ID, LCS_IC),
    ((W1_IC + W2_IC =:= 0) ->
        %%% If W1 and W2 don't have information content, we fix the degree of similarity to zero
        Degree = 0
        ;
        (LCS_IC =:= 0 ->
            %%% If LCS_IC doesn't have information content, is because the LCS is the root of the
            %%% hierarchy. This means that W1 and W2 are not related and we fix the degree of similarity
            %%% to zero
            Degree = 0
            ;
            Degree is (2*LCS_IC) / (W1_IC + W2_IC)
        )
    ).



%%%%%%%%%%%%%%%%%%%
%%% max_lin(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% It obtains the degree of similarity between the concepts Word1:SS_type:W1_Sense_num
%%% and Word2:SS_type:W2_Sense_num using the JIANG & CONRATH measure.
%%%
%%% A concept can have more than one HyperTree, therefore several pairs of HyperTrees
%%% are possibly considered and a list of similarity degrees 'Degs' is obtained using lin/3.
%%% Finally, the maximum degree in the list 'Degs' is selected as a result.
%%%
%%% NOTE: When this predicate is executed, it is intended that all variables in
%%%       Word1:SS_type:W1_Sense_num and in Word2:SS_type:W2_Sense_num are completely
%%%       instantiated.
%%%
max_lin(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    findall(Deg ,lin(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Deg), Degs),
    max_list(Degs, Degree).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_lin_nondet(+Word1:SS_type:W1_Sense_num, +Word2:SS_type:W2_Sense_num, -Degree)
%%% Inspects a pair of HyperTrees associated to c1 and c2 and obtains the degree
%%% of similarity between c1 and c2 (according to that pair of HyperTrees)
%%%
%%% NOTE: Nondeterministic predicate. It is the user interface to the private predicate lin/3.
%%%
wn_lin_nondet(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree) :-
    ( (SS_type=n; SS_type=v) ->
        lin(Word1:SS_type:W1_Sense_num, Word2:SS_type:W2_Sense_num, Degree)
        ;
        write("ERROR: Both Word1 and Word2 must be nouns or verbs but not combinations of them"),
        nl
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% END OF MODULE MAIN PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% PREDICATES FOR COMPUTING THE LESS COMMON SUBSUMER (LCS)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% lcs(+Word1, +Word2, -LCS, -Root)
%%% Calculates the Less Common Subsumer (LCS).
%%%
%%% It is an specialization of the predicate implemented in wn_sim_measures.pl
%%% The objective is to gain efficience in the obtention of the LCS.
%%%
%%% PROBLEMS WITH the Root of the hierarchy:
%%% Verbs do not have an explicit root. It may be that two verbs
%%% do not share a Less Common Subsumer. In this case LCS=Root and Root is assigned
%%% to a virtual root for verbs (Synset_ID = 200000000). Note that in this case, the
%%% information content of the LCS is 0 and the similarity of these two verbs should
%%% be around 0 also.
%%% On the other hand, nouns have a unique root hierarchy which is "entity" (synset_ID =
%%% 100001740). However, by uniformity of treatment we introduce a virtual root for names
%%% (Synset_ID = 100000000)
%%%
%%%
lcs(Word1, Word2, LCS, Root) :-
    wn_hypernyms(Word1, verbose(no), List_HyperNymSynSets1),
    wn_hypernyms(Word2, verbose(no), List_HyperNymSynSets2),
    head(List_HyperNymSynSets1, H_SynSet_ID),
    wn_virtual_root(H_SynSet_ID, Root),
    find_lcs([Root|List_HyperNymSynSets1], [Root|List_HyperNymSynSets2], LCS).

head([H|_], H).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% find_lcs(+List_HyperNymSynSets1, +List_HyperNymSynSets2, -LCS)
%%%
%%% List_HyperNymSynSets1 and List_HyperNymSynSets2 are lists of synset_IDs which are the hypernyms
%%% of the concepts (associated to the words) W1 and W2.
%%% LCS is the less common subsumer (a specific synset_ID).
%%%

find_lcs(L_SynSets1, L_SynSets2, LCS) :-
    find_lcs(L_SynSets1, L_SynSets2, 0, LCS).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% find_lcs(L_SynSets1, L_SynSets2, LCS_Acc, LCS).
%%%
%%% Compares two lists of (HyperNym) SynSets, element by element, until a mismatch is
%%% found. Then, the LCS are returned.
%%%
%%% LCS_Acc accumulate the last synset inspected.
%%% Initially LCS_Acc=0 (a virtual root synset).
%%%

find_lcs([], _L_SynSets2, LCS_Acc, LCS_Acc):-
    !.

find_lcs(_L_SynSets1, [], LCS_Acc, LCS_Acc):-
    !.

find_lcs([SS1|L_SynSets1], [SS2|L_SynSets2], _, LCS) :-
    SS1=:=SS2, !,
    find_lcs(L_SynSets1, L_SynSets2, SS1, LCS).

find_lcs(_L_SynSets1, _L_SynSets2, LCS_Acc, LCS_Acc).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%  PREDICATES FOR COMPUTING THE INFORMATION CONTENT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% wn_information_content(+Word, -IC),
%%
%% Computes the information content IC of the concepts designed by the different senses
%% of Word.
%%
wn_information_content(Word, IC):-
    wn_synset_ID_of(Word, W_synset_ID),
    wn_virtual_root(W_synset_ID, Root_ID),
    information_content(W_synset_ID, Root_ID, IC).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% information_content(+Synset_ID, +Root_ID, -IC),
%%
%% Computes the information content IC of the concept denoted by the Synset_ID. This
%% quantity is defined as:
%%
%%    IC = -ln(Frequency/Frequency_Root)
%%
%%    if frequency_of_use(Synset_ID, Frequency) and frequency_of_use(Root_ID, Frequency_Root)
%%
%% NOTES:
%% 1) Root_ID is the synset number of the concept in the root of the hierarchy.
%% 2) IC(c) is defined as the natural logarithm of the probability of encountering an instance of
%%    a concept c (meassured in terms of a relative frequency of use of the concept c in a corpus).
%% 3) Natural logarithm:  logarithm to the base of the mathematical constant e.
%%

information_content(Synset_ID, Root_ID, IC) :-
    ( Synset_ID = 100000000 ->
            %% This case appears when comparing two nouns and the LCS = 100000000
            %% that is, it is the virtual root sysnset for nouns, with zero information content
            IC = 0
            ;
      (Synset_ID > 100000000, Synset_ID < 200000000) ->
            %% Synset_ID corresponds to a noun
            frequency_of_use(Synset_ID, F),
            %%% frequency_of_use of "entity" was computed and stored as a parameter
            %%% In this case, "Frequency_Root" is a non-zero value
            frequency_of_use(Root_ID, Frequency_Root),
            (F = 0 ->
             %% smoothing Frequency
             Frequency = 0.0000000001
             ;
             Frequency = F
            ),
            IC is -log(Frequency/Frequency_Root)
            ;
      Synset_ID = 200000000 ->
            %% This case appears when comparing two verbs and the LCS = 200000000
            %% that is, it the virtual root sysnset for verbs, with zero information content
            IC = 0
            ;
      (Synset_ID > 200000000, Synset_ID < 300000000) ->
            %% Synset_ID corresponds to a verb
            frequency_of_use(Synset_ID, F),
            frequency_of_use(Root_ID, Frequency_Root),
            (F = 0 ->
                %% smoothing Frequency
                Frequency = 0.0000000001
                ;
                Frequency = F
            ),
            IC is -log(Frequency/Frequency_Root)
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% frequency_of_use(+Synset_ID, -Frequency),
%%
%% In WordNet a synset represents a concept (which is identified by a Synset_ID). This
%% predicate computes the frequency of use of a concept in a corpus (using the information
%% stored in the last parameter of the predicate wn_s/6). We compute the "synset tag sum"
%% (see next predicate) of the concepts subsumed by Synset_ID adding them to obtain the
%% Frequency of the concept Synset_ID.
%%
%% Note that the concepts subsumed by Synset_ID are their hyponyms

%%% Frequency of use for the root concept "entity" (synset_ID = 100001740) -with_repetitions
frequency_of_use(100000000, 125518) :- !.
frequency_of_use(100001740, 125518) :- !.
%%% Frequency of use for the root concept "entity" (synset_ID = 100001740) - without_repetitions
%% rootFrequency(100001740, 94941).

%%% Frequency of use for the virtual root concept for verbs (Root = 200000000)
frequency_of_use(200000000, 95651) :- !.

frequency_of_use(Synset_ID, Frequency) :-
        frequency_of_use(with_repetitions, Synset_ID, Frequency).

%%% frequency_of_use(+Repetitions, +Synset_ID, -Frequency)
%%%
frequency_of_use(with_repetitions, Synset_ID, Frequency) :-
        wn_gen_all_hyponyms_of(Synset_ID, List_all_Hyponym_IDs),
        sum_synset_tag_num([Synset_ID|List_all_Hyponym_IDs], Frequency).

frequency_of_use(without_repetitions, Synset_ID, Frequency) :-
        wn_gen_all_hyponyms_of(Synset_ID, List_all_Hyponym_IDs),
        setof(X,member(X,List_all_Hyponym_IDs),List_all_Hyponym_IDs_NR),
        sum_synset_tag_num([Synset_ID|List_all_Hyponym_IDs_NR], Frequency).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% synset_tag_num(+Synset_ID, -Frequency),
%%
%% The operator wn_s/6 stores the synset information of the WordNet database. Each word has
%% an entry:
%%            s(synset_ID,w_num,‘word’,ss_type,sense_number, tag_number).
%% where tag_number indicates how common a word is in relation to a text. The number is equal
%% to the times the word was found in a test corpus. Therefore the higher the number, the more
%% common the word. This parameter can be employed to obtain the frequency of use of a word
%% and if we sum the tag_number of all words in a synsent, we can obtain the specific frequency of
%% use associated to the whole synset.
%%
synset_tag_num(Synset_ID, Frequency) :-
    integer(Synset_ID),
    Synset_ID > 100000000,
    Synset_ID < 500000000,
    findall(Tag_number, wordnet:wn_s(Synset_ID, _, _, _, _, Tag_number), Word_frequencies),
    sum(Word_frequencies, Frequency).

%%% sum(L, S), S is the sum of the list of numbers L.
%%%
sum(L, S) :- sum(L, 0, S).

sum([], A, A).
sum([N|Nums], A, S) :- NewA is N + A, sum(Nums, NewA, S).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sum_synset_tag_num(+List_Synset_ID, -Frequency),
%%
sum_synset_tag_num([],0).
sum_synset_tag_num([Synset_ID|List_Synset_ID], Frequency) :-
        synset_tag_num(Synset_ID, FS),
        sum_synset_tag_num(List_Synset_ID, FLS),
        Frequency is FS + FLS.



