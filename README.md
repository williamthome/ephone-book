# ephone_book

An OTP application

## Dev

> Make sure to open your browser and the url from the current page
be the app page.

Live reload to watch changes in erlang modules and assets files

    $ ./ephone_book.sh dev-watch

Live reload for erlang modules

    $ ./ephone_book.sh run dev

Live reload for assets files

    $ ./ephone_book.sh watch

## Rebar3


### Compile

    $ rebar3 compile

### Run in shell

    $ rebar3 shell

### Build a release

    $ rebar3 as prod release

### Run the release in shell

    $ rebar3 as prod shell

## Docker

### Build

    $ ./docker.sh build

### Run

Attached to the console (interactive)

    $ ./docker.sh run it

or detached

    $ ./docker.sh run d

> navigate to http://0.0.0.0:2938

## Deploying to fly.io

Command used to create the app:\
$ flyctl launch --image williamthome/ephone_book --name ephone-book

    $ ./ephone_book.sh deploy
