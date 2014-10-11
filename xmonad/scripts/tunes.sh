#!/bin/bash

if pidof cmus >/dev/null; then
  artist=$(cmus-remote -Q |grep 'tag artist' |cut -d' ' -f3- |cut -c-50)
  title=$(cmus-remote -Q |grep title |cut -d' ' -f3- |cut -c-50)
  out=$artist::$title
else
  out=''
fi

if [[ $(cmus-remote -Q | grep status| cut -d' ' -f2) == 'paused' ]]; then
  out=:$(echo $out| cut -c-80):
fi

echo $out

