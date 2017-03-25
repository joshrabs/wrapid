#!/bin/bash
set -e

if [ "$1" = 'run' ]; then
    sleep 2
    stack exec -- wrapid-app -p 3004 
else
    exec "$@"
fi
