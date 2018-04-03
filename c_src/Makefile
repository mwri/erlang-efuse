all: efuse

efuse.o: efuse.c efuse_defs.h
	gcc -c -Wall -std=gnu99 -D_FILE_OFFSET_BITS=64 -I/usr/include/fuse -shared -g -Wall -fPIC -MMD  -I"/usr/lib/erlang/lib/erl_interface-3.10.1/include" -I"/usr/lib/erlang/erts-9.2/include" efuse.c -o efuse.o

efuse: efuse.o
	gcc efuse.o  -lerl_interface -lei -pthread -lnsl -lfuse -lrt -ldl -o efuse
	cp efuse ../priv/

clean:
	rm -f efuse.o efuse
