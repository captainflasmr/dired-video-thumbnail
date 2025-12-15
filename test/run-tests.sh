#!/bin/bash

# Run unit tests for dired-video-thumbnail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPT_DIR")"

echo "Running dired-video-thumbnail unit tests..."
echo "Project directory: $PROJECT_DIR"
echo "Test directory: $SCRIPT_DIR"
echo ""

# Run Emacs in batch mode with the tests
emacs -Q --batch \
    --eval "(add-to-list 'load-path \"$PROJECT_DIR\")" \
    --eval "(add-to-list 'load-path \"$SCRIPT_DIR\")" \
    -l "$PROJECT_DIR/dired-video-thumbnail.el" \
    -l "$SCRIPT_DIR/dired-video-thumbnail-test.el" \
    -f ert-run-tests-batch-and-exit

EXIT_CODE=$?

echo ""
if [ $EXIT_CODE -eq 0 ]; then
    echo "✓ All tests passed!"
else
    echo "✗ Some tests failed (exit code: $EXIT_CODE)"
fi

exit $EXIT_CODE
