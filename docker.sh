#!/bin/sh

set -e

IMAGE=williamthome/ephone_book
PUBLIC_PORT=2938
EXPOSED_PORT=8080

case "$1" in
    build)
      ./ephone_book.sh release
      docker build -t $IMAGE .
      ;;
    run)
      case "$2" in
        it)
          docker run -it --rm -p $PUBLIC_PORT:$EXPOSED_PORT --init $IMAGE
          ;;
        d)
          docker run -d -p $PUBLIC_PORT:$EXPOSED_PORT --init $IMAGE
          ;;
        *)
          echo "Please chose a run option in [it, d]."
          echo "it for iterable and d for detached"

          exit 1
          ;;
      esac
      ;;
    *)
      echo "Please chose a command in [build, run]"

      exit 1
      ;;
esac

exit 0
