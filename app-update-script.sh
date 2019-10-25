#!/bin/bash

cd ~/josh-dist/
echo "\nDoing Joshua Distribution\n"
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit

if test -e "~/Research-Projects/awdrat
echo "\nDoing AWDRAT\n"
cd ~/Research-Projects/awdrat/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit
fi

echo "\nDoing Attack Planner\n"
cd ~/Research-Projects/attack-planning/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Githu"b
git push mit

echo "\nDoing Control System Demo\n"
cd ~/Research-Projects/control-system/
git commit -a -m "Updating all apps"
echo "\nshing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit

echo "\nDoing NatSoft\n"
cd ~/Research-Projects/natural-software/
git commit -a -m "Updating all apps"
echo "\nPushing to Github.com"
git push origin
echo "\nPushing to MIT Github"
git push mit
