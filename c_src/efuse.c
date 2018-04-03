// Author: Michael Wright <mjw@methodanalysis.com>
// Copyright 2015 Michael Wright <mjw@methodanalysis.com>
// 
// This file is part of the Erlang FUSE (Filesystem in Userspace)
// interface called 'efuse'.
// 
// 'efuse' is free software, licensed under the MIT license.


// This port provides an interface between Erlang and FUSE. It works
// by registering appropriate callbacks for handing file system
// functions, each of which sends the request on to the Erlang process
// that started the port. The Erlang process must respond appropriately
// and the port, upon receiving the response, provides it to FUSE.
// 
// Not all the possible FUSE callbacks are implemented.


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <syslog.h>

#define FUSE_USE_VERSION 26

#include <fuse.h>
#include <errno.h>
#include <fcntl.h>


#include "efuse_defs.h"


#define BUFFER_SIZE 20480


static unsigned char erlmsg[BUFFER_SIZE];


static int fusecb_getattr(const char *, struct stat *);
static int fusecb_readdir(const char *, void *, fuse_fill_dir_t, off_t, struct fuse_file_info *);
static int fusecb_read(const char *, char *, size_t, off_t, struct fuse_file_info *);
static int fusecb_readlink(const char * path, char * buf, size_t);

int read_from_erlang(unsigned char *, int);
int write_to_erlang(const unsigned int, const unsigned char *);


static struct fuse_operations efuse_oper = {
	.getattr  = fusecb_getattr,
	.readlink = fusecb_readlink,
	.readdir  = fusecb_readdir,
	.read     = fusecb_read,
};


// Port entry. Nothing to do really other than pass control to FUSE.

int main (int argc, char ** argv) {

	// report my PID back to Erlang
	((uint32_t*)erlmsg)[0] = htonl(EFUSE_STATUS_DATA);
	((uint32_t*)erlmsg)[1] = htonl(getpid());
	if (write_to_erlang(8, erlmsg) != 8) {
		syslog(
			LOG_ERR,
			"efuse[%d]: failure initialising (error writing pid to erlang VM)",
			getpid()
			);
		exit(1);
	}

	// pass control to FUSE until it is done
	syslog(LOG_NOTICE, "efuse[%d]: fuse mount %s", getpid(), argv[1]);
	fuse_main(argc, argv, &efuse_oper, NULL);
	syslog(LOG_NOTICE, "efuse[%d]: fuse umount %s", getpid(), argv[1]);

	exit(0);

}


// Write data to Erlang, with a 32 bit header declaring the length of
// the data (excluding the header).

int write_to_erlang(
		const unsigned int datalen,
		const unsigned char * data) {

	int writelen;

	uint32_t dataheader = htonl(datalen + sizeof(uint32_t));
	if ((writelen = write(4, (unsigned char *) &dataheader, 4)) != 4) {
		syslog(LOG_WARNING, "efuse[%d]: write: port error writing data header (wrote %d)",
				getpid(), writelen);
		return -1;
	}

	uint32_t magiccookie1 = htonl(EFUSE_MAGICCOOKIE);
	if ((writelen = write(4, (unsigned char *) &magiccookie1, 4)) != 4) {
		syslog(LOG_CRIT, "efuse[%d]: write: port error writing magic cookie 1 (wrote %d)",
				getpid(), writelen);
		exit(1);
	}

	if (datalen > 0) {
		int writelen;
		if ((writelen = write(4, data, datalen)) != datalen) {
			syslog(LOG_WARNING, "efuse[%d]: write: port error writing data (wrote %d)",
					getpid(), writelen);
			return -1;
		}
	}

	return datalen;

}


// Read data from Erlang, formed of a 32 bit header declaring the length
// of the data (after the header).

int read_from_erlang(unsigned char * buf, int maxdatalen) {

	uint32_t datalen;

	int readlen;
	if ((readlen = read(3, (unsigned char *) &datalen, 4)) != 4) {
		syslog(LOG_WARNING, "efuse[%d]: read: port error reading data header (read %d)",
				getpid(), readlen);
		return -1;
	}
	datalen = ntohl(datalen);

	uint32_t magiccookie1;
	if ((readlen = read(3, (unsigned char *) &magiccookie1, 4)) != 4) {
		syslog(LOG_CRIT, "efuse[%d]: read: port error reading data header (read %d)",
			getpid(), readlen);
		exit(1);
	}
	magiccookie1 = ntohl(magiccookie1);
	if (magiccookie1 != EFUSE_MAGICCOOKIE) {
		syslog(LOG_CRIT, "efuse[%d]: read: port read invalid magic cookie %u",
				getpid(), magiccookie1);
		exit(1);
	}
	datalen -= sizeof(uint32_t);

	uint32_t readtotal = 0;
	while (readtotal < datalen) {
		readlen = read(3, buf+readtotal, datalen-readtotal);
		if (readlen < 0) {
			syslog(LOG_ERR, "efuse[%d]: read: port read %d (expected %d)",
					getpid(), readtotal, datalen);
			return -1;
		}
		readtotal += readlen;
		if (readtotal < datalen)
			syslog(LOG_WARNING, "efuse[%d]: read: port short read (%d of %d)",
					getpid(), readtotal, datalen);
	}

	return datalen;

}


// Implement the 'getattr' FUSE callback.

static int fusecb_getattr(
		const char * path,
		struct stat * stbuf) {

	// send request to Erlang
	((uint32_t*)erlmsg)[0] = htonl(EFUSE_REQUEST_GETATTR);
	strcpy((char *) erlmsg+4, path);
	write_to_erlang(strlen(path)+4, erlmsg);

	int replylen;
	if ((replylen = read_from_erlang(erlmsg, BUFFER_SIZE)) < 0) {
		syslog(LOG_ERR, "efuse[%d]: no response from FS implementation (getattr %s)",
				getpid(), path);
		return -ENOENT;
	}

	// check response correct code
	unsigned int replycode = ntohl(((uint32_t*)erlmsg)[0]);
	if (replycode != EFUSE_REQUEST_GETATTR) {
		syslog(LOG_ERR,
				"efuse[%d]: unexpected response %d (expected %d) from FS implementation (getattr %s)",
				getpid(), replycode, EFUSE_REQUEST_GETATTR, path);
		return -ENOENT;
	}

	// check response not an error
	unsigned int replyresult = ntohl(((uint32_t*)erlmsg)[1]);
	if (replyresult != 0) {
		syslog(LOG_WARNING,
				"efuse[%d]: response result code %d (error) from FS implementation (getattr %s)",
				getpid(), replyresult, path);
		return -ENOENT;
	}

	// pass data in response back to FUSE
	unsigned int mode = ntohl(((uint32_t*)erlmsg)[2]);
	unsigned int type = ntohl(((uint32_t*)erlmsg)[3]);
	unsigned int size = ntohl(((uint32_t*)erlmsg)[4]);
	memset(stbuf, 0, sizeof(struct stat));
	stbuf->st_mode = mode | (
			type == EFUSE_ATTR_DIR ?
				S_IFDIR :
			type == EFUSE_ATTR_FILE ?
				S_IFREG :
			type == EFUSE_ATTR_SYMLINK ?
				S_IFLNK :
			0);
	stbuf->st_nlink = type == EFUSE_ATTR_DIR ? 2 : 1;
	stbuf->st_size = size;

	return 0;

}


// Implement the 'readdir' FUSE callback.

static int fusecb_readdir(
		const char *path,
		void *buf,
		fuse_fill_dir_t filler,
		off_t offset,
		struct fuse_file_info *fi) {

	// send request to Erlang
	((uint32_t*)erlmsg)[0] = htonl(EFUSE_REQUEST_READDIR);
	strcpy((char *) erlmsg+4, path);
	write_to_erlang(strlen(path)+4, erlmsg);

	int replylen;
	if ((replylen = read_from_erlang(erlmsg, BUFFER_SIZE)) < 0) {
		syslog(LOG_ERR, "efuse[%d]: no response from FS implementation (readdir %s)",
				getpid(), path);
		return -ENOENT;
	}

	// check response correct code
	unsigned int replycode = ntohl(((uint32_t*)erlmsg)[0]);
	if (replycode != EFUSE_REQUEST_READDIR) {
		syslog(LOG_ERR,
				"efuse[%d]: unexpected response %d (expected %d) from FS implementation (readdir %s)",
				getpid(), replycode, EFUSE_REQUEST_READDIR, path);
		return -ENOENT;
	}

	// check response not an error
	unsigned int replyresult = ntohl(((uint32_t*)erlmsg)[1]);
	if (replyresult != 0) {
		syslog(LOG_WARNING,
				"efuse[%d]: response result code %d (error) from FS implementation (readdir %s)",
				getpid(), replyresult, path);
		return -ENOENT;
	}

	// pass data in response back to FUSE
	filler(buf, ".", NULL, 0);
	filler(buf, "..", NULL, 0);
	for (int i = 8; i < replylen; i += strlen((char *) erlmsg+i)+1) {
		filler(buf, (char *) erlmsg+i, NULL, 0);
	}

	return 0;

}


// Implement the 'read' FUSE callback.

static int fusecb_read(
		const char *            path,
		char *                  buf,
		size_t                  size,
		off_t                   offset,
		struct fuse_file_info * fi) {

	// send request to Erlang
	((uint32_t*)erlmsg)[0] = htonl(EFUSE_REQUEST_READ);
	strcpy((char *) erlmsg+4, path);
	write_to_erlang(strlen(path)+4, erlmsg);

	int replylen;

	if ((replylen = read_from_erlang(erlmsg, BUFFER_SIZE)) < 0) {
		syslog(LOG_ERR, "efuse[%d]: no response from FS implementation (read %s)",
				getpid(), path);
		return -ENOENT;
	}

	// check response correct code
	unsigned int replycode = ntohl(((uint32_t*)erlmsg)[0]);
	if (replycode != EFUSE_REQUEST_READ) {
		syslog(LOG_ERR,
				"efuse[%d]: unexpected response %d (expected %d) from FS implementation (read %s)",
				getpid(), replycode, EFUSE_REQUEST_READ, path);
		return -ENOENT;
	}

	// check response not an error
	unsigned int replyresult = ntohl(((uint32_t*)erlmsg)[1]);
	if (replyresult != 0) {
		syslog(LOG_WARNING,
				"efuse[%d]: response result code %d (error) from FS implementation (read %s)",
				getpid(), replyresult, path);
		return -ENOENT;
	}

	// pass data in response back to FUSE
	replylen -= 8;
	if (offset < replylen) {
		int returneddatalen =
			replylen < offset + size
			? replylen - offset
			: size;
		memcpy(buf, ((char *) erlmsg+8) + offset, size);
		return returneddatalen;
	} else {
		return 0;
	}

}


// Implement the 'readlink' FUSE callback.

static int fusecb_readlink(
		const char * path,
		char       * buf,
		size_t     size
		) {

	// send request to Erlang
	((uint32_t*)erlmsg)[0] = htonl(EFUSE_REQUEST_READLINK);
	strcpy((char *) erlmsg+4, path);
	write_to_erlang(strlen(path)+4, erlmsg);

	int replylen;
	if ((replylen = read_from_erlang(erlmsg, BUFFER_SIZE)) < 0) {
		syslog(LOG_ERR, "efuse[%d]: no response from FS implementation (readlink %s)",
				getpid(), path);
		return -ENOENT;
	}

	// check response correct code
	unsigned int replycode = ntohl(((uint32_t*)erlmsg)[0]);
	if (replycode != EFUSE_REQUEST_READLINK) {
		syslog(LOG_ERR,
				"efuse[%d]: unexpected response %d (expected %d) from FS implementation (readlink %s)",
				getpid(), replycode, EFUSE_REQUEST_READLINK, path);
		return -ENOENT;
	}

	// check response not an error
	unsigned int replyresult = ntohl(((uint32_t*)erlmsg)[1]);
	if (replyresult != 0) {
		syslog(LOG_WARNING,
				"efuse[%d]: response result code %d (error) from FS implementation (readlink %s)",
				getpid(), replyresult, path);
		return -ENOENT;
	}

	// pass data in response back to FUSE
	strcpy(buf, (char *) erlmsg+8);

	return 0;

}


