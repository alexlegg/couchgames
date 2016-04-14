all: server client

.PHONY: server
server:
	stack install

.PHONY: types
types: server
	couchgames-exe -e

.PHONY: client
client: types
	elm-make clientsrc/Main.elm --output=static/app.js
