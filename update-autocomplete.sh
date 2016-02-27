#!/bin/bash

TEMPARG=
if [ $(uname) != 'Linux' ]; then
    TEMPARG="-t tmp"
fi

TEMP=$(mktemp -d $TEMPARG)
BASE=$PWD

# target directory
if [ -d $1 ]; then
    rm -rf $1
fi
mkdir -p $1
cd $1
TARGET=$PWD

# download auto-complete and compile.
cd $TEMP
git clone https://github.com/cask/cask.git cask
git clone https://github.com/auto-complete/auto-complete.git 

cd auto-complete
PATH=$PATH:$TEMP/cask/bin make byte-compile

cp $TEMP/auto-complete/*.el* $TARGET/
cp -r $TEMP/auto-complete/dict $TARGET/

rm -rf $TEMP
