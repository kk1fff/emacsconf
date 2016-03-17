#!/bin/bash

TEMPARG=
if [ $(uname) != 'Linux' ]; then
    TEMPARG="-t tmp"
fi

TEMP=$(mktemp -d $TEMPARG)
ORIGINALDIR=$PWD
TARGET=$PWD/emacs.d

function push-load-path {
    echo "(add-to-list 'load-path \"~/.emacs.d/$1\")" >> $TEMP/load_path
}

function clone-and-add {
    echo "Download $2 from $1"
    git clone "$1" "$2" 2>&1 > /dev/null
    rm -rf "$2/.git"
    find "$2/" -name .gitignore -print0 | xargs -0 rm
    push-load-path "loc_pkg/$2"
}

function clone-make-and-add {
    clone-and-add $1 $2
    cd $2
    make
    cd -
}

# Download from git
cd $TEMP
pwd

mkdir loc_pkg
cd loc_pkg
clone-and-add "https://github.com/kk1fff/emacs-package-multi-web-mode.git" "multi-web-mode"
clone-and-add "https://github.com/antonj/Highlight-Indentation-for-Emacs.git" "highlight-indentation"
clone-make-and-add "https://github.com/emacs-helm/helm.git" "helm"
cd ..

if [ -d $TARGET ]; then
    rm -rf $TARGET
fi
mkdir -p $TARGET
mv loc_pkg $TARGET
cd $ORIGINALDIR

# Local misc script
cp -r misc_scripts $TARGET/loc_pkg/misc

# Themes
cp -r themes $TARGET/loc_themes

# autocomplete
./update-autocomplete.sh $TARGET/loc_pkg/auto-complete
push-load-path "loc_pkg/auto-complete"

# clean up
cat $TEMP/load_path init.template.el > $TARGET/init.el
rm -rf $TEMP
