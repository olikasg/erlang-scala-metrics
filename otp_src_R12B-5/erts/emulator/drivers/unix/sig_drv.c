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

/* Purpose: demonstrate how to include interupt handlers in erlang */

#ifdef HAVE_CONFIG_H
#  include "config.h"
#endif

#include "sys.h"
#include "erl_driver.h"
#include <signal.h>
#include <stdio.h>

static ErlDrvData sig_start(ErlDrvPort, char*);
static int sig_init(void);
static void sig_stop(ErlDrvData), doio(ErlDrvData, ErlDrvEvent);

ErlDrvEntry sig_driver_entry = {
    sig_init,
    sig_start,
    sig_stop,
    NULL,
    doio,
    NULL,
    "sig_test"
};

static ErlDrvPort this_port;

static int sig_init(void)
{
    this_port = (ErlDrvPort)-1;
    return 0;
}

static sigc(int ino)
{
    driver_interrupt(this_port, ino);
}

static ErlDrvData sig_start(ErlDrvPort port, char* buf)
{
    if (this_port != (ErlDrvPort)-1)
	return ERL_DRV_ERROR_GENERAL;
    this_port = port;
    signal(SIGUSR1, sigc);
    return (ErlDrvData)port;
}

static void sig_stop(ErlDrvData port)
{
    this_port = (ErlDrvPort)-1;
    signal(SIGUSR1, SIG_DFL);
}

doio(ErlDrvData port, ErlDrvEvent ino)
{
    /* First go get the io, unless we already did that */
    /* In the sighandler */

    /* Then send it to erlang */

    driver_output(this_port, "y", 1);
}
