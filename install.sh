#!/bin/bash

EMACSD=$HOME/.emacs.d
INITEL=$EMACSD/init.el
LOCALPACKAGE=$EMACSD/local_packages
TEMPDIR=/tmp/emacsconf_$$

## emacs.d and init script
if [ -d ~/.emacs.d ]; then
  echo "Delete original emacs.d folder" 
  rm -rf $EMACSD
fi

## Build new emacs.d for user, put init.el, local package into it.
echo "Create new emacs.d"
mkdir -p $EMACSD

echo "Load local packages"
cp -r local_packages $LOCALPACKAGE

echo "Building init.el"
echo "(add-to-list 'load-path \"$LOCALPACKAGE\")" >> $INITEL
cat init.el >> $INITEL

