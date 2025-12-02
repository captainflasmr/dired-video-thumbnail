#!/bin/bash
# test-interactive.sh - Open test in vanilla Emacs interactively

SCRIPT_DIR=..

echo $SCRIPT_DIR

emacs -Q \
      --eval "(load-file \"$SCRIPT_DIR/dired-video-thumbnail.el\")" \
      --eval "(dired \".\")" \
      --eval "(message \"Loaded dired-video-thumbnail. Press M-x dired-video-thumbnail to test.\")"
