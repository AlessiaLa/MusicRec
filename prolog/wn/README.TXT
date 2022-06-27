
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% WN_CONNECT v1.3 : 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

AUTHORS: Pascual Juli'an-Iranzo (Universidad de Castilla-La Mancha, Spain)
         Fernando S'aenz-P'erez (Universidad Complutense de Madrid, Spain)

WN_CONNECT is licensed for research and educational purposes only and it is
distributed with NO WARRANTY OF ANY KIND. You are freely allowed to use, copy
and distribute WN_CONNECT provided that you make no modifications to any of its
files and give credit to its original authors.


WN_CONNECT is a software application prototype that aims to access the lexical 
database WordNet. One of its main features is that it has been fully implemented 
using Prolog.

========================================
REQUIREMENTS:

- SWI-Prolog must be pre-installed. Goto to http://www.swi-prolog.org to obtain the
last version of this Prolog interpreter.

- In order to use certain predicates for graphical representation of graphs, you
have to install Graphviz and make it accesible in the PATH environment variable.

 

========================================
INSTALLATION STEPS:

** In a Unix-like system, follow these steps:
_____________________________________________


1) Download WordNet 3.0 Prolog version from 
   http://wordnetcode.princeton.edu/3.0/WNprolog-3.0.tar.gz 
   and unzip it in a directory of your choice. For example: 
                    /usr/local/WordNet-3.0/wn\_prologDB

2) Set the environment variable WNDB to this newly created directory.  
   For example, in a Bourne-like Shell, write:
                    export WNDB=/usr/local/WordNet-3.0/wn\_prologDB

3) Download the modules of WN_CONNECT from 
   https://dectau.uclm.es/bousi-prolog/applications and unzip 
   it in a directory of your choice. For example:
                    /home/myuser/wn

4) Extend the environment variable PATH with the previous directory. 
   For example, write:  
                    export PATH=/home/myuser/wn:\$PATH

5) Open a terminal and execute the shell script:
                    wn.sh



** In a Windows system, follow these steps:
___________________________________________


1) Download WordNet 3.0 Prolog version from
   http://wordnetcode.princeton.edu/3.0/WNprolog-3.0.tar.gz 
   and unzip it in a directory of your choice. For example: 
                     c:\wn_prologDB

2) Set the environment variable WNDB to this newly created directory.  
   For example, use either the system dialog box in Control Panel 
   (Environment Variables), or write in a Command Prompt (cmd.exe):
                     set WNDB=c:\wn_prologDB

3) Download the modules of WN_CONNECT from
   https://dectau.uclm.es/bousi-prolog/applications and unzip 
   it in a directory of your choice. For example:
                     c:\wn

4) Add to the PATH environment variable the directory where this tool 
   is located (similar to step 2 above):
                     set PATH=c:\wn;%PATH%

5) Open a terminal and execute the shell script:
                    wn.sh
                    
   Alternatively you can simply double click this file from the
   file explorer.

