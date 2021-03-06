
/*
 * A *very* limited socket interface exported by inet_drv.c.
 * Used by the erl_mtrace.c. 
 */

#ifndef ERL_SOCK_H_
#define ERL_SOCK_H_

#ifdef __WIN32__
#include <winsock2.h>
typedef SOCKET erts_sock_t;
#else
typedef int erts_sock_t;
#endif

#define ERTS_SOCK_INVALID_SOCKET	-1

erts_sock_t erts_sock_open(void);
void erts_sock_close(erts_sock_t);
int erts_sock_connect(erts_sock_t, byte *, int, Uint16);
Sint erts_sock_send(erts_sock_t, const void *, Sint);
int erts_sock_gethostname(char *, int);
int erts_sock_errno(void);

#endif
