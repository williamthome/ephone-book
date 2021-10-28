# ephone_book

An OTP application

## Rebar3

## Dev live reload

    $ ./ephone_book.sh run dev

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

Console iterable

    $ ./docker.sh run it

or detached

    $ ./docker.sh run d

> navigate to http://0.0.0.0:2938

## Deploy to fly.io

### Command used to create the app:

    $ flyctl launch --image williamthome/ephone_book --name ephone-book

### Deploying

> Ensure to first build the image before deploy because flyctl looks for an existing local image.

    $ ./docker.sh build
    $ flyctl deploy
