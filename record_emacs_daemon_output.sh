#!/bin/bash

# Define the log file name
LOG_FILE="emacs_daemon_first_run_output.log"

echo "--- Emacs Daemon First Run Output Recorder ---"
echo ""

# Check if an Emacs daemon is already running
echo "Checking for existing Emacs daemon process..."
# Attempt to evaluate a simple Emacs Lisp expression.
# If emacsclient connects successfully, a daemon is running.
# Redirect output to /dev/null to keep the terminal clean.
if emacsclient -e "(message \"Check\")" &>/dev/null; then
  echo "An Emacs daemon is already running."
  echo "This script is designed to capture the *initial* output when Emacs builds packages."
  echo "To get the 'first run' output, please stop the existing daemon first."
  echo "You can usually do this by running: emacsclient -e '(kill-emacs)'"
  echo ""
  echo "Exiting."
  exit 1
else
  echo "No Emacs daemon detected. Proceeding with first run capture."
  echo ""
fi

echo "Running 'emacs --daemon' and recording all output to: $LOG_FILE"
echo "This process will wait until Emacs has fully completed its initial startup,"
echo "including building and installing any packages. This might take a while."
echo ""

# Run emacs --daemon in the background and redirect all output (stdout and stderr)
# to the specified log file. The '&' puts the command in the background.
# We capture the Process ID ($!) of the background command.
emacs --daemon --init-directory . &>"$LOG_FILE" &
EMACS_DAEMON_PID=$!

echo "Emacs daemon started in the background (PID: $EMACS_DAEMON_PID)."
echo "Waiting for the Emacs daemon process to finish its initial setup..."
echo "Please be patient, as this can take a long time for the first run."
echo "(You can monitor the log file in another terminal: tail -f \"$LOG_FILE\")"
echo ""

# Wait for the emacs --daemon process to complete.
# The 'emacs --daemon' command typically exits after it has successfully
# initialized and started the daemon process in the background.
wait $EMACS_DAEMON_PID

echo ""
echo "------------------------------------------------"
echo "Emacs daemon initial setup complete."
echo "The full output from 'emacs --daemon' has been saved to:"
echo "  $LOG_FILE"
echo ""
echo "You can view the captured output using:"
echo "  less \"$LOG_FILE\""
echo "  cat \"$LOG_FILE\""
echo ""
echo "The Emacs daemon should now be running in the background."
echo "You can connect to it using:"
echo "  emacsclient -c   (for a new graphical frame)"
echo "  emacsclient -t   (for a new terminal frame)"
echo ""
echo "To stop the daemon later, you can run:"
echo "  emacsclient -e '(kill-emacs)'"
echo "------------------------------------------------"
