all: efuse

efuse.o: efuse.c efuse_defs.h
	gcc -c -Wall -std=gnu99 -D_FILE_OFFSET_BITS=64 -I/usr/include/fuse -shared -g -Wall -fPIC -MMD efuse.c -o efuse.o

efuse: efuse.o
	gcc efuse.o -pthread -lfuse -ldl -o efuse
	cp efuse ../priv/

clean:
	rm -f efuse.o efuse
