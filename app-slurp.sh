#!/bin/bash

echo "Doing Joshua"
cd ~/joshua-dist/
git pull mit master


echo "Doing ATTACK PLANNER"
FILE=~/Research-Projects/attack-planning
if test -e "$FILE";
   then echo "Pulling ATTACK-PLANNER"
	cd ~/Research-Projects/attack-planning/
	git pull mit master
else echo "Attack Planner Directory doesn't exist.  Skipping"
     fi

echo "Doing McClim"
FILE=~/quicklisp/local-projects/mcclim
if test -e "$FILE";
   then echo "Pulling McClim"
	cd ~/quicklisp/local-projects/mcclim
	git pull mit master
else "MccClim doesn't exist.  Skipping"
     fi

echo "Doing McClim Additions"
FILE=~/quicklisp/local-projects/mcclim-additions
if test -e "$FILE";
   then echo "Pulling McClim Additions"
	cd ~/quicklisp/local-projects/mcclim-additions/
	git pull mit master
else "MccClim Additions doesn't exist.  Skipping"
     fi
