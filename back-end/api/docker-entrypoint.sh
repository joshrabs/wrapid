#!/bin/bash
set -e

if [ "$1" = 'run' ]; then
    sleep 2
    stack exec -- wrapid-api -p 3001
else
    exec "$@"
fi
