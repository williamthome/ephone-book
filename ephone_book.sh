#!/bin/sh

set -e

case "$1" in
  run)
    case "$2" in
      dev)
        echo "\n---------------------------------------------------------------"
        echo "Live reload will be enabled."
        echo "Important:"
        echo "- xdotool is required $ sudo apt-get install xdotool"
        echo "- The command refreshes the active tab in the browser"
        echo "- Browser name is hardcoded at dev.escript as \"Navigator\""
                # see https://stackoverflow.com/a/47704119/14166584
        echo "p.s: Tested and working well using Firefox"
        echo "---------------------------------------------------------------\n"

        rebar3 shell --script dev.escript
        ;;
      *)
        echo "Please chose a run option in [dev]."

        exit 1
        ;;
    esac
    ;;
  *)
    echo "Please chose a command in [run]"

    exit 1
    ;;
esac

exit 0
