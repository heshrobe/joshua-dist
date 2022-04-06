#!/bin/bash

echo "Doing Joshua"
cd ~/joshua-dist/
git pull mit master


echo "Doing Start-Interface"
FILE=~/Research-Projects/start-interface
if test -e "$FILE";
   then echo "Pulling Start-Interface"
	cd ~/Research-Projects/start-interface/
	git pull mit master
else echo "Start Interface Directory doesn't exist.  Skipping"
fi


echo "Doing Planning Core"
FILE=~/Research-Projects/planning-core
if test -e "$FILE";
   then echo "Pulling Planning core"
	cd ~/Research-Projects/planning-core/
	git pull mit master
else echo "Planning Core Directory doesn't exist.  Skipping"
     fi

echo "Doing Recipes"
FILE=~/Research-Projects/recipes
if test -e "$FILE";
   then echo "Pulling Recipes"
	cd ~/Research-Projects/recipes/
	git pull mit master
else echo "Start Interface Directory doesn't exist.  Skipping"
     fi
     
echo "Doing Guide"
FILE=~/Research-Projects/guide
if test -e "$FILE";
   then echo "Pulling Guide"
	cd ~/Research-Projects/guide/
	git pull mit master
else echo "Start Interface Directory doesn't exist.  Skipping"
     fi

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
