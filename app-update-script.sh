#!/bin/bash

cd ~/josh-dist/
echo "\nDoing Joshua Distribution\n"
git commit -a -m "Updating all code"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit

FILE=~/Research-Projects/awdrat
if test -d $FILE; then
echo "\nDoing AWDRAT\n"
cd ~/Research-Projects/awdrat/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit
else echo "\nAWDRAT not present"
fi

FILE=~/Research-Projects/attack-planning
if test -e $FILE; then
echo "\nDoing Attack Planner\n"
cd ~/Research-Projects/attack-planning/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit master
else echo "\nAttack Planner not present"
fi

FILE=~/Research-Projects/control-system
if test -e $FILE; then
echo "\nDoing Control System Demo\n"
cd ~/Research-Projects/control-system/
git commit -a -m "Updating all apps"
echo "\npushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit
else echo "\nControl System not present"
fi

FILE=~/Research-Projects/natural-software
if test -e $FILE; then
echo "\nDoing NatSoft\n"
cd ~/Research-Projects/natural-software/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit
else echo "\nNatural Software not present"
fi
