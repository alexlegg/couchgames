all: server client

.PHONY: server
server:
	stack install

.PHONY: bridge
bridge: server
	couchgames-elmbridge > clientsrc/Types.elm

.PHONY: client
client: bridge
	elm-make clientsrc/Main.elm --output=static/index.html
