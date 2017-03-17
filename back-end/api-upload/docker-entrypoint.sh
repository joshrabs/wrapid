#!/bin/bash
set -e

if [ "$1" = 'run' ]; then
    sleep 2
    stack exec -- wrapid-api-upload -p 3002
else
    exec "$@"
fi
