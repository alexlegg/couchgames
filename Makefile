FAY=stack exec fay --
FAYFILES=src/Client/Test.hs src/Client/SocketIO.hs
FAYJS=$(addprefix static/,$(notdir $(FAYFILES:.hs=.js)))

all: $(FAYJS)
	stack install

static/%.js: src/Client/%.hs
	$(FAY) --include src/Client/,src/ -o $@ $<

clean:
	rm -rf $(FAYJS)

