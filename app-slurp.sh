#!/bin/bash

echo "Doing Joshua"
cd ~/joshua-dist/
git pull mit master

echo "Doing AWDRAT"
FILE=~/Research-Projects/awdrat
if test -e "$FILE";
   then echo "Pulling AWDRAT"
	cd ~/Research-Projects/awdrat/
	git pull mit master
else echo "AWDRAT Directory doesn't exist.  Skipping"
     fi

echo "Doing ATTACK PLANNER"
FILE=~/Research-Projects/attack-planning
if test -e "$FILE";
   then echo "Pulling ATTACK-PLANNER"
	cd ~/Research-Projects/attack-planning/
	git pull mit master
else echo "Attack Planner Directory doesn't exist.  Skipping"
     fi

echo "Doing Control System"
FILE=~/Research-Projects/control-system
if test -e "$FILE";
   then echo "Pulling Control System"
	cd ~/Research-Projects/control-system/
	git pull mit master
else "Control System Directory doesn't exist.  Skipping"
     fi


echo "Doing Natural Software"
FILE=~/Research-Projects/natural-software
if test -e "$FILE";
   then echo "Pulling Natural Software"
	cd ~/Research-Projects/natural-software/
	git pull mit master
else "Natural Software Directory doesn't exist.  Skipping"
fi

echo "Doing Start Interface"
FILE=~/Research-Projects/start-interface
if test -e "$FILE";
   then echo "Pulling Start Interface"
	cd ~/Research-Projects/start-interface/
	git pull mit master
else "Start Interface Directory doesn't exist.  Skipping"
     fi

echo "Doing Recipes"
FILE=~/Research-Projects/recipes
if test -e "$FILE";
   then echo "Pulling Recipes"
	cd ~/Research-Projects/recipes/
	git pull mit master
else "Recipes Directory doesn't exist.  Skipping"
     fi

echo "Doing Guide"
FILE=~/Research-Projects/ASIST/guide
if test -e "$FILE";
   then echo "Pulling Guide"
	cd ~/Research-Projects/ASIST/guide/
	git pull mit master
else "Guide Directory doesn't exist.  Skipping"
     fi

echo "Doing McClim Additions"
FILE=~/quicklisp/local-projects/mcclim-additions
if test -e "$FILE";
   then echo "Pulling Guide"
	cd ~/quicklisp/local-projects/mcclim-additions/
	git pull mit master
else "MccClim Additions doesn't exist.  Skipping"
     fi
