#!/bin/bash

INST_CEDET=0
TEMPDIR=/tmp/emacsconf_$$

mkdir -p $TEMPDIR
cp pkg/* $TEMPDIR
pushd $TEMPDIR

## CEDET
if [ $INST_CEDET -eq 1 ]; then
echo "Installing CEDET..."
tar zxf cedet-1.0.tar.gz
mkdir -p ~/local/cedet
mv cedet-1.0 ~/local/
pushd ~/local/cedet-1.0
make EMACS=emacs
popd
echo "Done"
fi

## Clear temp
popd
rm -rf $TEMPDIR

## emacs.d and init script
if [ -d ~/.emacs.d ]; then
    rm -rf ~/.emacs.d 
fi
cp -r emacs.d ~/.emacs.d
cp emacs ~/.emacs