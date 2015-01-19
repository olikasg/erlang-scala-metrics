# Makefile for SSL on Unix
# 
# Placed in obj directory. 
#
CC = gcc
	
BINDIR = ../../bin/i386-apple-darwin14.0.0
LIBS = -lutil -ldl -lm  
SSL_LIBDIR = /usr/lib 
OBJS = ../priv/obj/i386-apple-darwin14.0.0/esock.o ../priv/obj/i386-apple-darwin14.0.0/debuglog.o ../priv/obj/i386-apple-darwin14.0.0/esock_poll.o ../priv/obj/i386-apple-darwin14.0.0/esock_osio.o ../priv/obj/i386-apple-darwin14.0.0/esock_utils.o ../priv/obj/i386-apple-darwin14.0.0/esock_posix_str.o ../priv/obj/i386-apple-darwin14.0.0/esock_openssl.o%

$(BINDIR)/ssl_esock: $(OBJS)
	$(CC) -L$(SSL_LIBDIR) -Wl,-R$(SSL_LIBDIR) -o $@ $^ \
		$(LIBS) -lssl -lcrypto
