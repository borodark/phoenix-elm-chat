# Elm and Phoenix Chat Client

This is a chat application that we build out in episodes on the
[Elm](http://www.dailydrip.com/topics/elm) and
[Elixir](http://www.dailydrip.com/topics/elixir) tracks for
[DailyDrip](http://www.dailydrip.com).

Optionaly install missing npm deps:
``` npm install file
npm install webpack
npm install elm-webpack-loader elm-hot-loader

```

To run this chat:

In one terminal:

```sh
cd presence_chat
mix deps.get
mix ecto.create
mix ecto.migrate
iex -S mix phoenix.server
```

In another terminal:

```sh
cd elm-client
./build
servedir # this is just an alias I have for serving this directory on port 9091, do whatever to get the directory served as http
```

Then visit <http://localhost:9091>.
