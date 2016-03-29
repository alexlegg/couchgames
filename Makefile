FAY=stack exec fay --
FAYFILES=src/Client/Test.hs \
		 src/Client/SocketIO.hs \
		 src/Client/VirtualDOM.hs \
		 src/CouchGames/Player.hs \
		 src/CouchGames/Session.hs
FAYMAIN=src/Client/Test.hs
FAYJS=static/Client.js
FAYPKGS=fay-text

all: $(FAYJS)
	stack install

static/%.js: $(FAYFILES)
	$(FAY) --package $(FAYPKGS) --include src/Client/,src/ -o $@ $(FAYMAIN)

clean:
	rm -rf $(FAYJS)

