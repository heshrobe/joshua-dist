#!/bin/bash

echo "Doing Joshua"
cd ~/joshua-dist/
git pull

echo "Doing AWDRAT"
FILE=~/Research-Projects/awdrat
if test -e "$FILE";
   then echo "Pulling AWDRAT"
	cd ~/Research-Projects/awdrat/
	git pull
else echo "AWDRAT Directory doesn't exist.  Skipping"
     fi

echo "Doing ATTACK PLANNER"
FILE=~/Research-Projects/attack-planning
if test -e "$FILE";
   then echo "Pulling ATTACK-PLANNER"
	cd ~/Research-Projects/attack-planning/
	git pull
else echo "Attack Planner Directory doesn't exist.  Skipping"
     fi

echo "Doing Control System"
FILE=~/Research-Projects/control-system
if test -e "$FILE";
   then echo "Pulling Control System"
	cd ~/Research-Projects/control-system/
	git pull
else "Control System Directory doesn't exist.  Skipping"
     fi


echo "Doing Natural Software"
FILE=~/Research-Projects/natural-software
if test -e "$FILE";
   then echo "Pulling Natural Software"
	cd ~/Research-Projects/natural-software/
	git pull
else "Natural Software Directory doesn't exist.  Skipping"
     fi
