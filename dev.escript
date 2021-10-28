#!/usr/bin/env escript

main(_) ->
  sync:go(),
  sync:onsync(fun(_Mods) ->
    os:cmd("xdotool search --onlyvisible --classname Navigator windowactivate --sync key F5")
  end).
