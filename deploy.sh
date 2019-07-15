#!/usr/bin/env bash

echo "Please update the user in HOST and comment this line." && exit 1;

HOST="shiwei@pl.cs.jhu.edu"
DST="${HOST}:/var/www/pl.cs.jhu.edu/pl2";
SITE="_site";
REPO="www-pl2";

if [ -d "$SITE" ]; then
  cd $SITE && git pull;
else
  mkdir -p $SITE;
  cd $SITE && git clone git@pl.cs.jhu.edu:"$REPO";
fi;

# `cd $SITE` is inherited before rsync ..

rsync -ahr --exclude=.git --exclude=deploy.sh --stats "${REPO}/" "$DST";