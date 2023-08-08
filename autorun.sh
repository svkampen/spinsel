#!/bin/bash

inotifywait --exclude 'root' -q -m -r -e modify -e create -e close_write . | \
    while read -r path; do
        timeout 3 cat
        stack run
    done
