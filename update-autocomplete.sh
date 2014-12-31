#!/bin/bash

TEMP=$(mktemp -d)
BASE=$PWD

cd $TEMP
git clone https://github.com/cask/cask.git cask
git clone https://github.com/auto-complete/auto-complete.git 

cd auto-complete
PATH=$PATH:$TEMP/cask/bin make byte-compile

cd $BASE
if [ -d auto-complete ]; then
    rm -rf auto-complete
fi

mkdir auto-complete
cp $TEMP/auto-complete/*.el* auto-complete/
cp -r $TEMP/auto-complete/dict auto-complete/

rm -rf $TEMP
