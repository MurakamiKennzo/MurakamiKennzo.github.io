#!/bin/bash
if [ ! $0 ]; then
    $0='publish'
fi
stack run -- html > index.html &&
stack run -- style > index.css
git checkout master &&
git add . &&
git commit -m $0 &&
git push origin master &&
git checkout hs-page
