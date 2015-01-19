/* ``The contents of this file are subject to the Erlang Public License,
 * Version 1.1, (the "License"); you may not use this file except in
 * compliance with the License. You should have received a copy of the
 * Erlang Public License along with this software. If not, it can be
 * retrieved via the world wide web at http://www.erlang.org/.
 * 
 * Software distributed under the License is distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
 * the License for the specific language governing rights and limitations
 * under the License.
 * 
 * The Initial Developer of the Original Code is Ericsson Utvecklings AB.
 * Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
 * AB. All Rights Reserved.''
 * 
 *     $Id$
 */

/* 
 * Interface functions to the dynamic linker using dl* functions.
 * (As far as I know it works on SunOS 4, 5, Linux and FreeBSD. /Seb) 
 */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_vm.h"
#include "global.h"
#ifdef HAVE_DLFCN_H
#include <dlfcn.h>
#endif


/* some systems do not have RTLD_NOW defined, and require the "mode"
 * argument to dload() always be 1.
 */
#ifndef RTLD_NOW
#  define RTLD_NOW 1
#endif

#define MAX_NAME_LEN 255      /* XXX should we get the system path size? */
#define EXT_LEN      3
#define FILE_EXT     ".so"    /* extension appended to the filename */

static char **errcodes = NULL;
static int num_errcodes = 0;
static int num_errcodes_allocated = 0;


static char *my_strdup(char *what)
{
    char *res = erts_alloc(ERTS_ALC_T_DDLL_ERRCODES, strlen(what) + 1);
    strcpy(res, what);
    return res;
}

static int find_errcode(char *string) 
{
    int i;

    for(i=0;i<num_errcodes;++i) {
	if (!strcmp(string, errcodes[i])) {
	    return i;
	}
    }
    if (num_errcodes_allocated == num_errcodes) {
	errcodes = (num_errcodes_allocated == 0) 
	    ? erts_alloc(ERTS_ALC_T_DDLL_ERRCODES, 
			 (num_errcodes_allocated = 10) * sizeof(char *)) 
	    : erts_realloc(ERTS_ALC_T_DDLL_ERRCODES, errcodes,
			   (num_errcodes_allocated += 10) * sizeof(char *));
    }
    errcodes[num_errcodes++] = my_strdup(string);
    return (num_errcodes - 1);
}

void erl_sys_ddll_init(void) {
#if defined(HAVE_DLOPEN) && defined(ERTS_NEED_DLOPEN_BEFORE_DLERROR)
    /*
     * dlopen() needs to be called before we make the first call to
     * dlerror(); otherwise, dlerror() might dump core. At least
     * some versions of linuxthread suffer from this bug.
     */
    void *handle = dlopen("/nonexistinglib", RTLD_NOW);
    if (handle)
	dlclose(handle);
#endif    
    return;
}

/* 
 * Open a shared object
 */
int erts_sys_ddll_open(char *full_name, void **handle)
{
#if defined(HAVE_DLOPEN)
    char dlname[MAX_NAME_LEN+1];
    int len;
    
    if ((len = sys_strlen(full_name)) >= MAX_NAME_LEN-EXT_LEN) {
	return ERL_DE_LOAD_ERROR_NAME_TO_LONG;
    }
    sys_strcpy(dlname, full_name);
    sys_strcpy(dlname+len, FILE_EXT);
    
    return erts_sys_ddll_open_noext(dlname, handle);
#else
    return ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY;
#endif
}
int erts_sys_ddll_open_noext(char *dlname, void **handle)
{
    int ret = ERL_DE_NO_ERROR;
    char *str;
    dlerror();
    *handle = dlopen(dlname, RTLD_NOW);
    if ( (str = dlerror())) {
	/* Remove prefix filename to avoid exploading number of errorcodes on extreme usage */
	if (strstr(str,dlname) == str) {
	    char *save_str = str;
	    str += strlen(dlname);
	    while (*str == ':' || *str == ' ') {
		++str;
	    }
	    if (*str == '\0') { /* Better with filename than nothing... */
		str = save_str;
	    }
	}
	ret = ERL_DE_DYNAMIC_ERROR_OFFSET - find_errcode(str);
    }
    return ret;
}

/* 
 * Find a symbol in the shared object
 */
int erts_sys_ddll_sym(void *handle, char *func_name, void **function)
{
#if defined(HAVE_DLOPEN)
    void *sym;
    char *e;
    int ret;
    dlerror();
    sym = dlsym(handle, func_name);
    if ((e = dlerror()) != NULL) {
	ret = ERL_DE_DYNAMIC_ERROR_OFFSET - find_errcode(e);
    } else {
	*function = sym;
	ret = ERL_DE_NO_ERROR;
    }
    return ret;
#else
    return ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY;
#endif
}

/* XXX:PaN These two will be changed with new driver interface! */

/* 
 * Load the driver init function, might appear under different names depending on object arch... 
 */

int erts_sys_ddll_load_driver_init(void *handle, void **function)
{
    void *fn;
    int res;
    if ((res = erts_sys_ddll_sym(handle, "driver_init", &fn)) != ERL_DE_NO_ERROR) {
	res = erts_sys_ddll_sym(handle, "_driver_init", &fn);
    }
    if (res == ERL_DE_NO_ERROR) {
	*function = fn;
    }
    return res;
}

/* 
 * Call the driver_init function, whatever it's really called, simple on unix... 
*/
void *erts_sys_ddll_call_init(void *function) {
    void *(*initfn)(void) = function;
    return (*initfn)();
}


/* 
 * Close a chared object
 */
int erts_sys_ddll_close(void *handle)
{
#if defined(HAVE_DLOPEN)
    int ret;
    char *s;
    dlerror();
    if (dlclose(handle) == 0) {
	ret = ERL_DE_NO_ERROR;
    } else {
	if ((s = dlerror()) == NULL) {
	    ret = ERL_DE_ERROR_UNSPECIFIED;
	} else {
	    ret = ERL_DE_DYNAMIC_ERROR_OFFSET - find_errcode(s);
	}
    }
    return ret;
#else
    return ERL_DE_ERROR_NO_DDLL_FUNCTIONALITY;
#endif
}


/*
 * Return string that describes the (current) error
 */
char *erts_sys_ddll_error(int code)
{
    int actual_code;

    if (code > ERL_DE_DYNAMIC_ERROR_OFFSET) {
	return "Unspecified error";
    }
    actual_code = -1*(code - ERL_DE_DYNAMIC_ERROR_OFFSET);
#if defined(HAVE_DLOPEN)
    {
	char *msg;

	if (actual_code >= num_errcodes) {
	    msg = "Unknown dlload error";
	} else {
	    msg = errcodes[actual_code];
	}
	return msg;
    }
#endif
    return "no error";
}
