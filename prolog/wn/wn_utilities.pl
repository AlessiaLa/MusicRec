%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT source v1.3 : wn_utilities module 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
AUTHORS: Pascual Julián-Iranzo (Universidad de Castilla-La Mancha, Spain)
Fernando Sáenz-Pérez  (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.
*******************************************************************************/

:- module(wn_utilities, [
  	wn_word/1,              % ?Word
  	wn_measure/1,           % ?Measure
  	check_wn_words/2,       % +Words, -Word
  	wn_display_graph/1,     % +Graph
  	wn_maxDepth/2,          % (+Type, -MaxDepth)
  	wn_max_wordnet_sense/3, % (+Word, +Type, -MaxSense)
  	wn_virtual_root/2,      % (+List_HyperNymSynSets, -Virtual_Root_ID)
  	wn_convert_synsetID_to_representative/2,   % (+SynSet_ID, -Word_string)
  	wn_convert_synsetIDs_to_representatives/2, % (+List_SynSet_IDs, -List_representatives)
		atoms_functors_in_term/3, % +Term, -Atoms, -Functors
		bpl_predicates/3        % +Functors, -NonPredicates, -Predicates
	]
	).
	
%:- use_module(library(ordsets)).
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PREDICATES USED IN THE AUTOMATIC GENERATION OF PROXIMITY EQUATIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% wn_word(?Word)
%
%     Unifies Word with one of the words in WordNet.
%

wn_word(Word) :-
  atom(Word),
  !,
  wordnet:wn_s(_, _, Word, _, _, _).
 
wn_word(Word:Type:Sense) :-
  wordnet:wn_s(_, _, Word, Type, Sense, _).


%% wn_measure(?Measure)
%
%     Unifies Measure with one of the supported WordNet measures.
%

wn_measure(path).
wn_measure(wup).
wn_measure(lch).


%% check_wn_words(+Words, -Word)
%
%     Return in Word the first word in the list Words which is not
%     found in WordNet. If all words are found, Word will not be 
%     unified.
%

check_wn_words([], _).

check_wn_words([Word|Words], WordNotFound) :-
  wn_word(Word),
  !,
  check_wn_words(Words, WordNotFound).
  
check_wn_words([Word|_Words], Word).

  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PREDICATES FOR GRAPHICAL DISPLAY OF GRAPHS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Use:
%   wn_display_graph(Graph), where Graph=[arc(v1,v2),...,arc(vn-1,vn)]
%
% Examples:
%   ?- findall(arc(X,Y),(wn_hypernyms(man,List),append(_,[X,Y|_],List)),Graph), wn_display_graph(Graph).
%   ?- setof(arc(X,Y),List^H^T^(wn_hypernyms(man,List),append(H,[X,Y|T],List)),Graph), wn_display_graph(Graph).

% Requires:
% - PDF displayer (as indicated in pdf_displayer/1 fact and accesible in the path).
% - dot (part of Graphviz, accesible in the path)
% - dot2tex (for generating a LaTeX version of the graph). If LaTeX output is enabled (disabled by default)


%%% 'PDFViewer' is a user defined environment variable that must be exported to
%%% be accesible by the son process that executes SWI-Prolog. It stores the command
%%% to launch the specific PDF viewer of the operating system we are using.
pdf_displayer(PDFViewer) :-
    getenv('PDFViewer', PDFViewer), %% Set by the user
    !.

pdf_displayer('open -a Preview') :-
    current_prolog_flag(apple, true), % MacOS system
    !.

pdf_displayer('xpdf') :-
    current_prolog_flag(unix, true), % Linux system
    !.

pdf_displayer('acrobat.exe /A "view=Fit"') :-
    current_prolog_flag(windows, true), % Windows system
    !.

:- if((getenv('OSTYPE',OSystem), OSystem = darwin16)).
    %% The system is MacOS
    pdf_displayer('open -a Preview').
:- elif((getenv('OSTYPE',OSystem), OSystem = linux-gnu)).
    %% The system is Linux GNU
    pdf_displayer('xpdf').
:- else.
    %% The system is Windows
    pdf_displayer('acrobat.exe /A "view=Fit"').
:- endif.

% wn_display_graph(+Graph)
%   Graph is a list of arc(From,To)
%   Displays a PDF containing the graphical representation of Graph
%   Creates the files:
%   - out.dot: A file with the graph in DOT format (graph description language)
%   - out.pdf: The PDF document with the graph representation
%   - out.tex: The LaTeX document with the graph representation.
%              Disabled for now (just uncomment it below for enabling)
%
wn_display_graph(Graph) :-
    open('out.dot', write, Handle),
    write(Handle, 'digraph G { size="1,1";'),
    nl(Handle),
    write_arcs(Handle, Graph),
    write(Handle,'}'),
    close(Handle),
    % shell('dot2tex out.dot > out.tex'),
    display_dot_in_pdf.

display_dot_in_pdf :-
  (  write('Displaying graph...'),
     nl,
     shell('dot out.dot -Tpdf -o out.pdf'),
     pdf_displayer(PDFViewer),
     atom_concat(PDFViewer, ' out.pdf', PDFViewerCommand),
     %% (shell(PDFViewerCommand) -> true ; true),
     (shell(PDFViewerCommand) -> true
       ; write('ERROR: Cannot start PDF viewer. Check the environment variable PDFViewer')
    ),
    !
    ;
    write('ERROR: Cannot generate PDF output file. Check that the dot program is accesible')
  ).

write_arcs(_Handle,[]).
write_arcs(Handle,[arc(A,B)|R]):-
    write(Handle,A),
    write(Handle,' -> '),
    write(Handle,B),
    write(Handle,';'),
    nl(Handle),
    write_arcs(Handle,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%        PARAMETERS AND OTHER AUXILIARY PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_maxDepth(+Type, -MaxDepth)
%%% Returns the maximum depth of a concept in the hierachies of nouns (Type=n) and verbs (Type=v).
%%%
wn_maxDepth(n, 20).     % (max depth for nouns in WordNet HyperTrees is 20)
wn_maxDepth(v, 14).     % (max depth for verbs in WordNet HyperTrees is 14)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_max_wordnet_sense(+Word, +Type, -MaxSense)
%%% Returns the maximum number of senses, 'MaxSense', of a word, 'Word', of type, 'Type'.
%%% If the parameters 'Word' and 'Type' are not instantiated then it returns the maximum
%%% number of senses for a word in the WordNet data base.
%%%
wn_max_wordnet_sense(Word, WType, MaxSense) :-
    findall(WSense, wordnet:wn_s(_Synset_id, _WNum, Word, WType, WSense, _Tag_count), WSenseList),
    max_list(WSenseList, MaxSense).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% wn_virtual_root(SynSet_ID, -Virtual_Root_ID)
%%% Given a SynSet_ID and, acording to it, generates a virtual Root ID
%%%
%%% NOTE 1 (PROBLEMS WITH the Root of the hierarchy):
%%% Verbs do not have an explicit root. It may be that two verbs
%%% do not share a Less Common Subsumer. In this case LCS=Root and Root is assigned
%%% to a virtual root for verbs (Synset_ID = 200000000). Note that in this case, the
%%% information content of the LCS is 0 and the similarity of these two verbs should
%%% be around 0 also.
%%% On the other hand, nouns have a unique root hierarchy which is "entity" (synset_ID =
%%% 100001740). However, by uniformity of treatment we introduce a virtual root for names
%%% (Synset_ID = 100000000).
%%%
%%% NOTE 2: This predicate is useful when computing similarity measures and information
%%% content of words.
%%%
wn_virtual_root(SynSet_ID, Virtual_Root_ID) :-
    ((SynSet_ID > 100000000, SynSet_ID < 200000000) ->
        Virtual_Root_ID = 100000000
        ;
        ((SynSet_ID > 200000000, SynSet_ID < 300000000) ->
            Virtual_Root_ID = 200000000
            ;
            fail
        )
    ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% wn_convert_synsetID_to_representative(+SynSet_ID, -Word_string)
%%%
wn_convert_synsetID_to_representative(SynSet_ID, Word_string) :-
    wordnet:wn_s(SynSet_ID, 1, Word, SS_type, Sense_num, _),
    word_term_to_string(Word:SS_type:Sense_num, Word_string).


%%% wn_convert_synsetIDs_to_representatives(+List_SynSet_IDs, -List_representatives)
%%% List_SynSet_IDs is a list of synset_IDs (which usually are hypernyms or hyponyms of a given word).
%%% This predicate converts List_SynSet_IDs into a list of representative words.
%%%
%%% We mean by "representative word" the first word of the synset. The one with W_num=1. This is
%%% usually the most representative word of the sysntet.
%%%
wn_convert_synsetIDs_to_representatives([],[]).
wn_convert_synsetIDs_to_representatives([SynSet_ID|SynSet_IDs],[Word_string|Representatives]):-
        wn_convert_synsetID_to_representative(SynSet_ID, Word_string),
        wn_convert_synsetIDs_to_representatives(SynSet_IDs, Representatives).


%%% word_term_to_string(+Word:SS_type:Sense_num, -Word_string)
%%%
%%% Converts a word term Word:SS_type:Sense_num into a string "<Word>_<SS_type>_<Sense_num>".
%%% For instance, the word term 'psychological feature':n:1 is converted into "psychological_feature_n_1".
%%%
word_term_to_string(Word:SS_type:Sense_num, Word_string):-
    %% adapting Word, which may be a composed atom (e.g. 'psychological feature'
    %% or 'Grimes\' golden')
    atom_string(Word, S), split_string(S, "\' -", " ", L1), %% delimiters: \' and blank space character
    list_strings_to_string(L1, S1),
    string_concat(S1, "_", S2),
    string_concat(S2, SS_type, S3),
    string_concat(S3, "_", S4),
    number_string(Sense_num, SN),
    string_concat(S4, SN, Word_string).


%%% list_strings_to_string(+Lis_of_strings, +String)
%%% Concatenates the Lis_of_strings into one String.
%%%
list_strings_to_string([],"").
list_strings_to_string([Str],Str):- !.
list_strings_to_string([Str|StrLists], String) :-
        (Str="" ->
            S=Str
            ;
            string_concat(Str, "_", S)
        ),
        list_strings_to_string(StrLists, SS),
        string_concat(S, SS, String).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% atoms_functors_in_term(+Term, -Atoms,-Functors) 
%   Returns all the atoms in Term
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

atoms_functors_in_term(Term, Atoms, Functors) :-
  atoms_functors_in_term(Term, [], DupAtoms, [], DupFunctors),
  ordsets:list_to_ord_set(DupAtoms, Atoms),
  ordsets:list_to_ord_set(DupFunctors, Functors).

  
atoms_functors_in_term(Var, Atoms, Atoms, Functors, Functors):-
  var(Var),
  !.
  
atoms_functors_in_term(Number, Atoms, Atoms, Functors, Functors):-
  number(Number),
  !.
  
atoms_functors_in_term(Atom, Atoms, [Atom|Atoms], Functors, Functors):-
  atom(Atom),
  !.
  
atoms_functors_in_term([], Atoms, Atoms, Functors, Functors) :-
  !.
  
atoms_functors_in_term([Term|Terms], AtomsIn, AtomsOut, FunctorsIn, FunctorsOut) :- 
  !,
  atoms_functors_in_term_list([Term|Terms], AtomsIn, AtomsOut, FunctorsIn, FunctorsOut).
  
atoms_functors_in_term(Term, AtomsIn, AtomsOut, FunctorsIn, FunctorsOut) :- 
  Term =.. [Functor|Terms],
  atoms_functors_in_term_list(Terms, AtomsIn, AtomsOut, [Functor|FunctorsIn], FunctorsOut).

  
atoms_functors_in_term_list([], Atoms, Atoms, Functors, Functors).

atoms_functors_in_term_list([Term|Terms], AtomsIn, AtomsOut, FunctorsIn, FunctorsOut):-
  atoms_functors_in_term(Term, AtomsIn, AtomsOut1, FunctorsIn, FunctorsOut1),
  atoms_functors_in_term_list(Terms, AtomsOut1, AtomsOut, FunctorsOut1, FunctorsOut).

  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% bpl_predicates(+Functors, -NonPredicates, -Predicates)
%   Returns non-predicate functors in NonPredicates,
%   and predicate functors in Predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

bpl_predicates([], [], []).

bpl_predicates([Functor|Functors], NonPredicates, [Predicate|Predicates]) :-
  remove_program_prefix(Functor, Predicate),
  Functor\==Predicate,
  !,
  bpl_predicates(Functors, NonPredicates, Predicates).
  
bpl_predicates([NonPredicate|Functors], [NonPredicate|NonPredicates], Predicates) :-
  bpl_predicates(Functors, NonPredicates, Predicates).

%% remove_program_prefix(+Atom, -Result)
%
%     Removes the current program prefix from Atom and returns in Result.
%     If Atom does not include the program prefix, just returns Atom.
%

remove_program_prefix(Atom, Result) :-
  parser:program_prefix(Prefix),
  atom_concat(Prefix, '_', PrefixUS),
  atom_concat(PrefixUS, Result, Atom),
  !.
  
remove_program_prefix(Atom, Atom).


