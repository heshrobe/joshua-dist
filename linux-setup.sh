#!/bin/bash

echo "Updating Apt-Get"
sudo apt-get update
echo "Installing libxm4"
sudo apt-get install libxm4
echo "Installing wget"
sudo apt-get install wget
echo "Installng multiarch-support"
wget http://archive.ubuntu.com/ubuntu/pool/main/g/glibc/multiarch-support_2.27-3ubuntu1.2_amd64.deb
sudo dpkg -i multiarch-support_2.27-3ubuntu1.2_amd64.deb
echo "Installing libxp6"
sudo wget -O  libxp6_1.0.2-2_amd64.deb https://www.dropbox.com/s/cuov7kx1jpl1exs/libxp6_1.0.2-2_amd64.deb?dl=0
sudo dpkg -i libxp6_1.0.2-2_amd64.deb




