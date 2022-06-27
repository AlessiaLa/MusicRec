#!/bin/bash
# ***************************************************************************
# Batch script for starting SWI-Prolog with connection to the WN_CONNECT tool

# If you want to use this tools from any directory:
#   Add to the PATH environment variable where is located this tool
#   For this, use either the system dialog box or in Control Panel 
#   (Environment Variables), or in a terminal:
#   export PATH=/usr/local/wn:$PATH
#   Replace /usr/local/wn with the actual folder you have installed WN_CONNECT
#   Ensure that you have defined the environment variable WNDB, as, e.g.:
#   export WNDB=/usr/local/WordNet3.0
# ***************************************************************************
folder=$(dirname "$(which wn_connect.pl)")
if [ "$folder" = "" ];
then
  echo ERROR: File wn_connect.pl not found. Check that its location has been added to the environment variable PATH
else
  pushd $folder
  swipl wn_connect.pl
  popd
fi
