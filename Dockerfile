###
### Fist Stage - Building the Release
###

FROM erlang:24-alpine AS build

# install build dependencies
# git is to install rebar3 deps from git
RUN apk add git

# prepare build dir
WORKDIR /usr/src/app

COPY src src/
COPY priv priv/
COPY rebar.config .
RUN rebar3 as prod release

###
### Second Stage - Setup the Runtime Environment
###

FROM alpine AS app

RUN apk add --no-cache libstdc++ openssl ncurses-libs

WORKDIR /usr/src/app

COPY --from=build /usr/src/app/_build/prod/rel/ephone_book /prod

ENV HOST=0.0.0.0
ENV PORT=8080

EXPOSE $PORT

# run by erlang console, or
# to run at background use "foreground" instead
CMD ["/prod/bin/ephone_book", "foreground"]
