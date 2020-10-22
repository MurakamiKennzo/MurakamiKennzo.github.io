#!/bin/bash
stack run -- html > index.html
stack run -- style > index.css
if [ ! $0 ]; then
    $0='publish'
fi
git checkout master
git add .
git commit -m $0
git push origin master
