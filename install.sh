#!/bin/bash

EMACSD=$HOME/.emacs.d
INITEL=$EMACSD/init.el
LOCALPACKAGE=$EMACSD/local_packages
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

##
## Install Mozilla's coding style.
##
echo "Install Mozilla's developing environment"
pushd $TEMPDIR >> /dev/null
wget http://hg.mozilla.org/users/jblandy_mozilla.com/mozilla-elisp/archive/tip.tar.gz
tar zxf tip.tar.gz
mv mozilla-elisp* mozilla-elisp
cd mozilla-elisp
echo "== Mozilla coding style files =="
ls
echo "============= End =============="
cp *.el $LOCALPACKAGE
popd >> /dev/null

##
## Install helm
##
echo "Install helm"
pushd $TEMPDIR >> /dev/null
git clone https://github.com/emacs-helm/helm.git
mv helm $LOCALPACKAGE/
popd >> /dev/null

##
## Install helm-etags+
##
# echo "Install helm-etags+"
# pushd $TEMPDIR >> /dev/null
# git clone https://github.com/jixiuf/helm-etags-plus
# mv helm-etags-plus/*.el $LOCALPACKAGE/helm
# popd >> /dev/null

##
## Install helm-gtags
##
echo "Install helm-gtags"
pushd $TEMPDIR >> /dev/null
git clone git://github.com/syohex/emacs-helm-gtags.git
mv emacs-helm-gtags/*.el $LOCALPACKAGE/helm
popd >> /dev/null

##
## Install powerline
##
echo "Install power line"
pushd $TEMPDIR >> /dev/null
git clone git://github.com/kk1fff/emacs-package-powerline.git
mv emacs-package-powerline $LOCALPACKAGE
popd >> /dev/null

##
## Install theme
##
echo "Install theme"
pushd $TEMPDIR >> /dev/null
git clone git://github.com/kk1fff/emacs-themes.git
mv emacs-themes $LOCALPACKAGE
popd >> /dev/null

##
## Write init.el for loading local packages.
##
echo "Building init.el"
echo "(add-to-list 'load-path \"$LOCALPACKAGE\")"                     >> $INITEL
echo "(add-to-list 'load-path \"$LOCALPACKAGE/emacs-nav-49\")"        >> $INITEL
echo "(add-to-list 'load-path \"$LOCALPACKAGE/helm\")"                >> $INITEL
echo "(add-to-list 'load-path \"$LOCALPACKAGE/nxhtml/autostart.el\")" >> $INITEL
echo "(add-to-list 'load-path \"$LOCALPACKAGE/emacs-package-powerline\")"   >> $INITEL
echo "(add-to-list 'custom-theme-load-path \"$LOCALPACKAGE/emacs-themes\")" >> $INITEL
cat init.el                                                           >> $INITEL

##
## Cleanup temp directory.
##
rm -rf $TEMPDIR
