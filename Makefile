FAY=stack exec fay --
FAYFILES=src/Client/Test.hs src/Client/SocketIO.hs src/Client/VirtualDOM.hs
FAYMAIN=src/Client/Test.hs
FAYJS=static/Client.js

all: $(FAYJS)
	stack install

static/%.js: $(FAYFILES)
	$(FAY) --include src/Client/,src/ -o $@ $(FAYMAIN)

clean:
	rm -rf $(FAYJS)

