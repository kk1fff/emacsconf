#!/bin/bash

EMACSD=$HOME/.emacs.d
INITEL=$EMACSD/init.el
LOCALPACKAGE=$EMACSD/local_packages
COLORTHEME=$EMACSD/color-theme
TEMPDIR=/tmp/emacsconf_$$

##
## Build temp dir.
##
echo "Temp directory is $TEMPDIR..."
mkdir -p $TEMPDIR

##
## emacs.d and init script
##
if [ -d ~/.emacs.d ]; then
  echo "Delete original emacs.d folder" 
  rm -rf $EMACSD
fi

##
## Build new emacs.d for user, put init.el, local package into it.
##
echo "Create new emacs.d"
mkdir -p $EMACSD

echo "Load local packages"
cp -r local_packages $LOCALPACKAGE
cp -r color-theme $COLORTHEME

##
## Install Mozilla's coding style.
##
echo "Install Mozilla's developing environment"
pushd $TEMPDIR >> /dev/null
wget http://hg.mozilla.org/users/jblandy_mozilla.com/mozilla-elisp/archive/tip.tar.gz
tar zxf tip.tar.gz
mv mozilla-elisp* mozilla-elisp
cd mozilla-elisp
cp *.el $LOCALPACKAGE
popd >> /dev/null

##
## Write init.el for loading local packages.
##
echo "Building init.el"
echo "(add-to-list 'load-path \"$LOCALPACKAGE\")" >> $INITEL
echo "(add-to-list 'load-path \"$COLORTHEME\")" >> $INITEL
echo "(add-to-list 'load-path \"$COLORTHEME/themes\")" >> $INITEL
cat init.el >> $INITEL

##
## Cleanup temp directory.
##
rm -r $TEMPDIR
