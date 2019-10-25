#!/bin/bash

cd ~/josh-dist/
echo "\nDoing Joshua Distribution\n"
git commit -a -m "Updating all code"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit

if test -e "~/Research-Projects/awdrat"; then
echo "\nDoing AWDRAT\n"
cd ~/Research-Projects/awdrat/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit
else echo "\nAWDRAT not present"
fi

if test -e "~/Research-Projects/attack-planning"; then
echo "\nDoing Attack Planner\n"
cd ~/Research-Projects/attack-planning/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Githu"
git push mit
fi

if test -e "~/Research-Projects/control-system"; then
echo "\nDoing Control System Demo\n"
cd ~/Research-Projects/control-system/
git commit -a -m "Updating all apps"
echo "\npushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit
fi

if test -e "~/Research-Projects/natural-software"; then
echo "\nDoing NatSoft\n"
cd ~/Research-Projects/natural-software/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit
fi
