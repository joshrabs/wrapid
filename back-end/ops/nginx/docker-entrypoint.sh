#!/bin/sh
set -e

if [ "$1" = 'run' ]; then
  echo "Stating server"
  service nginx start
else
    exec "$@"
fi

