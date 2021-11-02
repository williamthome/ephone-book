#!/bin/sh

set -e

case "$1" in
  release)
    rebar3 as prod release
    ;;
  run)
    case "$2" in
      dev)
        echo "
        ---------------------------------------------------------------

        Live reload will be enabled.

        Tip:
        Split the terminal and run the watch command to
        reload assets in priv folder.

        Important:
        - xdotool is required $ sudo apt-get install xdotool
        - The command refreshes the active tab in the browser
        - Browser name is hardcoded at dev.escript as \"Navigator\".
          See https://stackoverflow.com/a/47704119/14166584

        p.s: Tested and working well using Firefox

        ---------------------------------------------------------------
        "

        rebar3 as dev shell --script dev.escript
        ;;
      *)
        echo "Please chose a run option in [dev]."

        exit 1
        ;;
    esac
    ;;
  watch)
    # Requires inotify-tools
    # See https://unix.stackexchange.com/questions/323901/how-to-use-inotifywait-to-watch-a-directory-for-creation-of-files-of-a-specific/606213#606213

    # Could use multiple folders
    # e.g. WatchDir="src priv"
    WatchDir=priv
    BuildDir=./_build/default/lib/ephone_book/

    echo "Watching $WatchDir for file changes..."

    inotifywait \
      $WatchDir \
      -qrm \
      -e close_write \
      --format './%w%f %w' \
      --include "(.*?)\.(html|css|js)$" \
    | while read file dir; do
      outdir="$BuildDir$dir"
      echo "Copying $file to $outdir"

      set +e
      cp $file $outdir
      set -e

      export CurrentWindowId=$(xdotool getactivewindow)
      xdotool search --onlyvisible --classname Navigator windowactivate --sync key F5
      xdotool windowactivate $CurrentWindowId
    done
    ;;
  dev-watch)
    x-terminal-emulator -e ./ephone_book.sh run dev &
    x-terminal-emulator -e ./ephone_book.sh watch &
    ;;
  deploy)
    ./docker.sh build
    flyctl deploy
    ;;
  *)
    echo "Please chose a command in [release, run, watch, dev-watch, deploy]"

    exit 1
    ;;
esac

exit 0
