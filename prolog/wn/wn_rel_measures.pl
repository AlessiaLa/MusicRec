%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT source v1.2 : wn_rel_measures module
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julián-Iranzo (Universidad de Castilla-La Mancha, Spain)
         Fernando Sáenz-Pérez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.
*******************************************************************************/

:- module(wn_rel_measures, [
        wn_yarm/3           %% (+Word1, +Word2, -Degree)
     %% experiment_yarm/4   %% (+Word1, +Word2, -Degree, -NormalizedDegree)
   ]).

:- use_module(etu).               %%% Michael A. Covington's Efficient Tokenizer
:- use_module(wn_synsets).
:- use_module(library(snowball)). %%% The Snowball multi-lingual stemmer library


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_yarm(+Word1, +Word2, -Degree)
%%% YARM (Yet Another Relatedness Measure) compares the gloses SGL_W1 and SGL_W2
%%% of two words (after removing stop words and stemming) by computing the Jaccard index
%%% for them as the relatednes Degree:
%%%  Degree = |SGL_W1 intersect SGL_W2| / |SGL_W1 union SGL_W2|
%%%         = |SGL_W1 intersect SGL_W2| / (|SGL_W1| + |SGL_W2| - |SGL_W1 intersect SGL_W2|)
%%%
wn_yarm(W1:W1_SS_type:W1_Sense_num, W2:W2_SS_type:W2_Sense_num, Degree) :-
        gloss_List_of(W1:W1_SS_type:W1_Sense_num, SGL_W1),
        gloss_List_of(W2:W2_SS_type:W2_Sense_num, SGL_W2),
        intersect(SGL_W1, SGL_W2, I), length(I,CardI),
        length(SGL_W1,CardGL_W1), length(SGL_W2,CardGL_W2),
        Degree is CardI/(CardGL_W1 + CardGL_W2 -CardI).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% gloss_List_of(+Word, -Gloss_list)
%%%
gloss_List_of(Word, Gloss_list) :-
        listify_gloss_of(Word, GL),
        remove_stop_words(GL, RGL),
        stemming_list_of_words(RGL, SGL),
        clear_repeated_elements(SGL, Gloss_list).


%%% listify_gloss_of(+Word, -Gloss_list)
%%%
listify_gloss_of(Word, Gloss_list) :-
        wn_gloss_of(Word, Gloss),
        atom_string(Gloss, SGloss),
        open_string(SGloss, Stream),
        tokenize_line(Stream,TGloss),
        tokens_words(TGloss,Gloss_list).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% remove_stop_words(+Gloss_list, -Pruned_Gloss_list)
%%%
remove_stop_words([],[]).
remove_stop_words([Word|Words], Pruned_Gloss_list) :-
        stopWords(StopWords_list),
        (member(Word, StopWords_list) ->
            remove_stop_words(Words, Pruned_Gloss_list)
            ;
            remove_stop_words(Words, Tail_Pruned_Gloss_list),
            Pruned_Gloss_list = [Word|Tail_Pruned_Gloss_list]
        ).

%%% stemming_list_of_words(+Words, -SWords)
%%% Takes a list of words 'Words' and returns the list of the stems of the words in 'Words'.
%%% The stemmed list is returned in 'SWords'.
%%% It uses the stemmer library Snowball to perform that function.
%%%
stemming_list_of_words([],[]).
stemming_list_of_words([W|Words], [S|SWords]) :-
        snowball(english, W, S),
        stemming_list_of_words(Words, SWords).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%        PARAMETERS and AXILIARY PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% stopWords(List)
%%% English Stop Words (CSV)
%%% FROM WIKIPEDIA: https://en.wikipedia.org/wiki/Stop_words
%%%
stopWords([a, able, about, across, after, all, almost, also, am, among, an, and, any, are, as, at, be, because, been, but, by, can, cannot, could, dear, did, do, does, either, else, ever, every, for, from, get, got, had, has, have, he, her, hers, him, his, how, however, i, if, in, into, is, it, its, just, least, let, like, likely, may, me, might, most, must, my, neither, no, nor, not, of, off, often, on, only, or, other, our, own, rather, said, say, says, she, should, since, so, some, than, that, the, their, them, then, there, these, they, this, tis, to, too, twas, us, wants, was, we, were, what, when, where, which, while, who, whom, why, will, with, would, yet, you, your]).


%%% clear_repeated_elements(+MultiSet, -Set)
clear_repeated_elements(L1, L2) :- clear_repeated_elements(L1, [], L2).

clear_repeated_elements([], A, A).
clear_repeated_elements([X|R], A, L):-
    (member(X,A) ->
        clear_repeated_elements(R, A, L)
        ;
        clear_repeated_elements(R, [X|A], L)
    ).


%%% Clocksin & Mellish
intersect([], _, []).
intersect([X|Resto], C2, [X|I]) :- member(X, C2), !, intersect(Resto, C2, I).
intersect([_|Resto], C2, I) :- intersect(Resto, C2, I).

%%% Clocksin & Mellish
union([], U, U).
union([X|Resto], C2, U) :- member(X, C2), !, union(Resto, C2, U).
union([X|Resto], C2, [X|U]) :- union(Resto, C2, U).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ONLY FOR THE EXPERIMENTS (remove comment in the exportation list)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% As part of an investigation into “the relationship between similarity of context and
%%% similarity of meaning (synonymy)”, Rubenstein and Goodenough (1965) obtained “synonymy
%%% judgements” from 51 human subjects on 65 pairs of words. The pairs ranged from “highly
%%% synonymous” to “semantically unrelated”, and the subjects were asked to rate them, on
%%% the scale of 0.0 to 4.0, according to their “similarity of meaning”.
%%%
%%% We want to test our YARM measure againts a subset of 20 of these pairs of words, in order
%%% to investigate how well the YARM measure reflects human judgments of semantic relatedness.
%%%
%%% In the experiments we obtained relatedness scores for the human-rated pairs.
%%% Where either or both of the words had more than one synset in WordNet, we took the
%%% most-related pair of synsets.

%%%%%%%%%%%%%%%%%%%
%%% experiment_yarm(+Word1, +Word2, -Degree, -NormalizedDegree)
%%% Obtains the best relatedness degree between Word1 and Word2.
%%% Degree is a number in [0,1].
%%% NormalizedDegree is a number in [0,4].
%%%
experiment_yarm(W1, W2, D, ND) :-
findall(Degree, yarm_nondet(W1, W2, Degree), LDegrees),
max_list(LDegrees, D),
ND is D * 4.


%%% yarm_nondet(+Word1, +Word2, -Degree)
%%% As wn_yarm but it obtains the relation between the different senses of the words
%%% non determinitically.
%%% It is introduced to be used in the experiments. It will not be at disposal of the
%%% users.
%%%
yarm_nondet(W1, W2, Degree) :-
gloss_List_of(W1, SGL_W1),
gloss_List_of(W2, SGL_W2),
intersect(SGL_W1, SGL_W2, I), length(I,CardI),
union(SGL_W1, SGL_W2, U), length(U,CardU),
Degree is CardI/CardU.


