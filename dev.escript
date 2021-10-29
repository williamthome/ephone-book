#!/usr/bin/env escript

main(_) ->
  sync:go(),
  sync:onsync(fun(_Mods) ->
    os:cmd("
      export CurrentWindowId=$(xdotool getactivewindow) &&
      xdotool search --onlyvisible --classname Navigator windowactivate --sync key F5 &&
      xdotool windowactivate $CurrentWindowId"
    )
  end).
