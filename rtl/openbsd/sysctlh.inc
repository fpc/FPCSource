{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Marco van de Voort

    The OS dependant sysctl constants.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{
 * Copyright (c) 1989;1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software contributed to Berkeley by
 * Mike Karels at Berkeley Software Design, Inc.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (Name:INCLUDING;CtlType: BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY;CtlType: OR TORT (Name:INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      @(Name:#)sysctl.h (and others)  8.2 (Berkeley) 3/30/95
}

CONST

{
 * Definitions for sysctl call.  The sysctl call uses a hierarchical name
 * for objects that can be examined or modified.  The name is expressed as
 * a sequence of integers.  Like a file path name, the meaning of each
 * component depends on its place in the hierarchy.  The top-level and kern
 * identifiers are defined here, and other identifiers are defined in the
 * respective subsystem header files.
}

        CTL_MAXNAME     = 12;   { largest number of components supported }

        CTLTYPE_NODE    = 1;    { name is a node }
        CTLTYPE_INT     = 2;    { name describes an integer }
        CTLTYPE_STRING  = 3;    { name describes a string }
        CTLTYPE_QUAD    = 4;    { name describes a 64-bit number }
        CTLTYPE_STRUCT  = 5;    { name describes a structure }

{
 * Top-level identifiers
}
        CTL_UNSPEC      = 0;            { unused }
        CTL_KERN        = 1;            { 'high kernel': proc, limits }
        CTL_VM          = 2;            { virtual memory }
        CTL_FS          = 3;            { file system, mount type is next }
        CTL_NET         = 4;            { network, see socket.h }
        CTL_DEBUG       = 5;            { debugging parameters }
        CTL_HW          = 6;            { generic cpu/io }
        CTL_MACHDEP     = 7;            { machine dependent }
        CTL_USER        = 8;            { user-level }
        CTL_DDB         = 9;            { DDB user interface, see db_var.h }
        CTL_VFS         = 10;           { VFS sysctl's }
        CTL_MAXID       = 11;           { number of valid top-level ids }

        CTL_NAMES   : array [0..10] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'kern';CtlType: CTLTYPE_NODE ),
        (Name: 'vm';CtlType: CTLTYPE_NODE ),
        (Name: 'fs';CtlType: CTLTYPE_NODE ),
        (Name: 'net';CtlType: CTLTYPE_NODE ),
        (Name: 'debug';CtlType: CTLTYPE_NODE ),
        (Name: 'hw';CtlType: CTLTYPE_NODE ),
        (Name: 'machdep';CtlType: CTLTYPE_NODE ),
        (Name: 'user';CtlType: CTLTYPE_NODE ),
        (Name: 'ddb';CtlType: CTLTYPE_NODE ),
        (Name: 'vfs';CtlType: CTLTYPE_NODE ));

{
 * CTL_KERN identifiers
}
        KERN_OSTYPE              = 1;   { string: system version }
        KERN_OSRELEASE           = 2;   { string: system release }
        KERN_OSREV               = 3;   { int: system revision }
        KERN_VERSION             = 4;   { string: compile time info }
        KERN_MAXVNODES           = 5;   { int: max vnodes }
        KERN_MAXPROC             = 6;   { int: max processes }
        KERN_MAXFILES            = 7;   { int: max open files }
        KERN_ARGMAX              = 8;   { int: max arguments to exec }
        KERN_SECURELVL           = 9;   { int: system security level }
        KERN_HOSTNAME           = 10;   { string: hostname }
        KERN_HOSTID             = 11;   { int: host identifier }
        KERN_CLOCKRATE          = 12;   { struct: struct clockinfo }
        KERN_VNODE              = 13;   { struct: vnode structures }
        KERN_PROC               = 14;   { struct: process entries }
        KERN_FILE               = 15;   { struct: file entries }
        KERN_PROF               = 16;   { node: kernel profiling info }
        KERN_POSIX1             = 17;   { int: POSIX.1 version }
        KERN_NGROUPS            = 18;   { int: # of supplemental group ids }
        KERN_JOB_CONTROL        = 19;   { int: is job control available }
        KERN_SAVED_IDS          = 20;   { int: saved set-user/group-ID }
        KERN_BOOTTIME           = 21;   { struct: time kernel was booted }
        KERN_DOMAINNAME         = 22;   { string: (Name:YP) domainname }
        KERN_MAXPARTITIONS      = 23;   { int: number of partitions/disk }
        KERN_RAWPARTITION       = 24;   { int: raw partition number }
{ define gap                    25      }
{ define gap                    26      }
        KERN_OSVERSION          = 27;   { string: kernel build version }
        KERN_SOMAXCONN          = 28;   { int: listen queue maximum }
        KERN_SOMINCONN          = 29;   { int: half-open controllable param }
        KERN_USERMOUNT          = 30;   { int: users may mount filesystems }
        KERN_RND                = 31;   { struct: rnd(Name:4) statistics }
        KERN_NOSUIDCOREDUMP     = 32;   { int: no setuid coredumps ever }
        KERN_FSYNC              = 33;   { int: file synchronization support }
        KERN_SYSVMSG            = 34;   { int: SysV message queue suppoprt }
        KERN_SYSVSEM            = 35;   { int: SysV semaphore support }
        KERN_SYSVSHM            = 36;   { int: SysV shared memory support }
        KERN_ARND               = 37;   { int: random integer from arc4rnd }
        KERN_MSGBUFSIZE         = 38;   { int: size of message buffer }
        KERN_MALLOCSTATS        = 39;   { node: malloc statistics }
        KERN_CPTIME             = 40;   { array: cp_time }
        KERN_NCHSTATS           = 41;   { struct: vfs cache statistics }
        KERN_FORKSTAT           = 42;   { struct: fork statistics }
        KERN_NSELCOLL           = 43;   { int: select(Name:2) collisions }
        KERN_TTY                = 44;   { node: tty information }
        KERN_CCPU               = 45;   { int: ccpu }
        KERN_FSCALE             = 46;   { int: fscale }
        KERN_NPROCS             = 47;   { int: number of processes }
        KERN_MSGBUF             = 48;   { message buffer, KERN_MSGBUFSIZE }
        KERN_POOL               = 49;   { struct: pool information }
        KERN_STACKGAPRANDOM     = 50;   { int: stackgap_random }
        KERN_SYSVIPC_INFO       = 51;   { struct: SysV sem/shm/msg info }
        KERN_USERCRYPTO         = 52;   { int: usercrypto }
        KERN_CRYPTODEVALLOWSOFT = 53;   { int: cryptodevallowsoft }
        KERN_SPLASSERT          = 54;   { int: splassert }
        KERN_PROC_ARGS          = 55;   { node: proc args and env }
        KERN_NFILES             = 56;   { int: number of open files }
        KERN_TTYCOUNT           = 57;   { int: number of tty devices }
        KERN_NUMVNODES          = 58;   { int: number of vnodes in use }
        KERN_MBSTAT             = 59;   { struct: mbuf statistics }
        KERN_USERASYMCRYPTO     = 60;   { int: usercrypto }
        KERN_MAXID              = 61;   { number of valid kern ids }

        CTL_KERN_NAMES   : array [0..60] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'ostype';CtlType: CTLTYPE_STRING ),
        (Name: 'osrelease';CtlType: CTLTYPE_STRING ),
        (Name: 'osrevision';CtlType: CTLTYPE_INT ),
        (Name: 'version';CtlType: CTLTYPE_STRING ),
        (Name: 'maxvnodes';CtlType: CTLTYPE_INT ),
        (Name: 'maxproc';CtlType: CTLTYPE_INT ),
        (Name: 'maxfiles';CtlType: CTLTYPE_INT ),
        (Name: 'argmax';CtlType: CTLTYPE_INT ),
        (Name: 'securelevel';CtlType: CTLTYPE_INT ),
        (Name: 'hostname';CtlType: CTLTYPE_STRING ),
        (Name: 'hostid';CtlType: CTLTYPE_INT ),
        (Name: 'clockrate';CtlType: CTLTYPE_STRUCT ),
        (Name: 'vnode';CtlType: CTLTYPE_STRUCT ),
        (Name: 'proc';CtlType: CTLTYPE_STRUCT ),
        (Name: 'file';CtlType: CTLTYPE_STRUCT ),
        (Name: 'profiling';CtlType: CTLTYPE_NODE ),
        (Name: 'posix1version';CtlType: CTLTYPE_INT ),
        (Name: 'ngroups';CtlType: CTLTYPE_INT ),
        (Name: 'job_control';CtlType: CTLTYPE_INT ),
        (Name: 'saved_ids';CtlType: CTLTYPE_INT ),
        (Name: 'boottime';CtlType: CTLTYPE_STRUCT ),
        (Name: 'domainname';CtlType: CTLTYPE_STRING ),
        (Name: 'maxpartitions';CtlType: CTLTYPE_INT ),
        (Name: 'rawpartition';CtlType: CTLTYPE_INT ),
        (Name: 'gap';CtlType: 0 ),
        (Name: 'gap';CtlType: 0 ),
        (Name: 'osversion';CtlType: CTLTYPE_STRING ),
        (Name: 'somaxconn';CtlType: CTLTYPE_INT ),
        (Name: 'sominconn';CtlType: CTLTYPE_INT ),
        (Name: 'usermount';CtlType: CTLTYPE_INT ),
        (Name: 'random';CtlType: CTLTYPE_STRUCT ),
        (Name: 'nosuidcoredump';CtlType: CTLTYPE_INT ),
        (Name: 'fsync';CtlType: CTLTYPE_INT ),
        (Name: 'sysvmsg';CtlType: CTLTYPE_INT ),
        (Name: 'sysvsem';CtlType: CTLTYPE_INT ),
        (Name: 'sysvshm';CtlType: CTLTYPE_INT ),
        (Name: 'arandom';CtlType: CTLTYPE_INT ),
        (Name: 'msgbufsize';CtlType: CTLTYPE_INT ),
        (Name: 'malloc';CtlType: CTLTYPE_NODE ),
        (Name: 'cp_time';CtlType: CTLTYPE_STRUCT ),
        (Name: 'nchstats';CtlType: CTLTYPE_STRUCT ),
        (Name: 'forkstat';CtlType: CTLTYPE_STRUCT ),
        (Name: 'nselcoll';CtlType: CTLTYPE_INT ),
        (Name: 'tty';CtlType: CTLTYPE_NODE ),
        (Name: 'ccpu';CtlType: CTLTYPE_INT ),
        (Name: 'fscale';CtlType: CTLTYPE_INT ),
        (Name: 'nprocs';CtlType: CTLTYPE_INT ),
        (Name: 'msgbuf';CtlType: CTLTYPE_STRUCT ),
        (Name: 'pool';CtlType: CTLTYPE_NODE ),
        (Name: 'stackgap_random';CtlType: CTLTYPE_INT ),
        (Name: 'sysvipc_info';CtlType: CTLTYPE_INT ),
        (Name: 'usercrypto';CtlType: CTLTYPE_INT ),
        (Name: 'cryptodevallowsoft';CtlType: CTLTYPE_INT ),
        (Name: 'splassert';CtlType: CTLTYPE_INT ),
        (Name: 'procargs';CtlType: CTLTYPE_NODE ),
        (Name: 'nfiles';CtlType: CTLTYPE_INT ),
        (Name: 'ttycount';CtlType: CTLTYPE_INT ),
        (Name: 'numvnodes';CtlType: CTLTYPE_INT ),
        (Name: 'mbstat';CtlType: CTLTYPE_STRUCT ),
        (Name: 'userasymcrypto';CtlType: CTLTYPE_INT ));

{
 * KERN_PROC subtypes
}
        KERN_PROC_ALL           = 0;    { everything but kernel threads }
        KERN_PROC_PID           = 1;    { by process id }
        KERN_PROC_PGRP          = 2;    { by process group id }
        KERN_PROC_SESSION       = 3;    { by session of pid }
        KERN_PROC_TTY           = 4;    { by controlling tty }
        KERN_PROC_UID           = 5;    { by effective uid }
        KERN_PROC_RUID          = 6;    { by real uid }
        KERN_PROC_KTHREAD       = 7;    { also return kernel threads }

{
 * KERN_SYSVIPC_INFO subtypes
}
        KERN_SYSVIPC_MSG_INFO   = 1;    { msginfo and msqid_ds }
        KERN_SYSVIPC_SEM_INFO   = 2;    { seminfo and semid_ds }
        KERN_SYSVIPC_SHM_INFO   = 3;    { shminfo and shmid_ds }

{
 * KERN_PROC_ARGS subtypes
}
        KERN_PROC_ARGV          = 1;
        KERN_PROC_NARGV         = 2;
        KERN_PROC_ENV           = 3;
        KERN_PROC_NENV          = 4;

{
 * CTL_FS identifiers
}
        FS_POSIX        = 1;            { POSIX flags }
        FS_MAXID        = 2;

        CTL_FS_NAMES   : array [0..1] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'posix';CtlType: CTLTYPE_NODE ));

{
 * CTL_FS identifiers
}
        FS_POSIX_SETUID = 1;            { int: always clear SGID/SUID bit when owner change }
        FS_POSIX_MAXID  = 2;

        CTL_FS_POSIX_NAMES   : array [0..1] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'setuid';CtlType: CTLTYPE_INT ));

{
 * CTL_HW identifiers
}
        HW_MACHINE       = 1;           { string: machine class }
        HW_MODEL         = 2;           { string: specific machine model }
        HW_NCPU          = 3;           { int: number of cpus }
        HW_BYTEORDER     = 4;           { int: machine byte order }
        HW_PHYSMEM       = 5;           { int: total memory }
        HW_USERMEM       = 6;           { int: non-kernel memory }
        HW_PAGESIZE      = 7;           { int: software page size }
        HW_DISKNAMES     = 8;           { strings: disk drive names }
        HW_DISKSTATS     = 9;           { struct: diskstats[] }
        HW_DISKCOUNT    = 10;           { int: number of disks }
        HW_MAXID        = 11;           { number of valid hw ids }

        CTL_HW_NAMES   : array [0..10] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'machine';CtlType: CTLTYPE_STRING ),
        (Name: 'model';CtlType: CTLTYPE_STRING ),
        (Name: 'ncpu';CtlType: CTLTYPE_INT ),
        (Name: 'byteorder';CtlType: CTLTYPE_INT ),
        (Name: 'physmem';CtlType: CTLTYPE_INT ),
        (Name: 'usermem';CtlType: CTLTYPE_INT ),
        (Name: 'pagesize';CtlType: CTLTYPE_INT ),
        (Name: 'disknames';CtlType: CTLTYPE_STRING ),
        (Name: 'diskstats';CtlType: CTLTYPE_STRUCT ),
        (Name: 'diskcount';CtlType: CTLTYPE_INT ));

{
 * CTL_USER definitions
}
        USER_CS_PATH             = 1;   { string: _CS_PATH }
        USER_BC_BASE_MAX         = 2;   { int: BC_BASE_MAX }
        USER_BC_DIM_MAX          = 3;   { int: BC_DIM_MAX }
        USER_BC_SCALE_MAX        = 4;   { int: BC_SCALE_MAX }
        USER_BC_STRING_MAX       = 5;   { int: BC_STRING_MAX }
        USER_COLL_WEIGHTS_MAX    = 6;   { int: COLL_WEIGHTS_MAX }
        USER_EXPR_NEST_MAX       = 7;   { int: EXPR_NEST_MAX }
        USER_LINE_MAX            = 8;   { int: LINE_MAX }
        USER_RE_DUP_MAX          = 9;   { int: RE_DUP_MAX }
        USER_POSIX2_VERSION     = 10;   { int: POSIX2_VERSION }
        USER_POSIX2_C_BIND      = 11;   { int: POSIX2_C_BIND }
        USER_POSIX2_C_DEV       = 12;   { int: POSIX2_C_DEV }
        USER_POSIX2_CHAR_TERM   = 13;   { int: POSIX2_CHAR_TERM }
        USER_POSIX2_FORT_DEV    = 14;   { int: POSIX2_FORT_DEV }
        USER_POSIX2_FORT_RUN    = 15;   { int: POSIX2_FORT_RUN }
        USER_POSIX2_LOCALEDEF   = 16;   { int: POSIX2_LOCALEDEF }
        USER_POSIX2_SW_DEV      = 17;   { int: POSIX2_SW_DEV }
        USER_POSIX2_UPE         = 18;   { int: POSIX2_UPE }
        USER_STREAM_MAX         = 19;   { int: POSIX2_STREAM_MAX }
        USER_TZNAME_MAX         = 20;   { int: POSIX2_TZNAME_MAX }
        USER_MAXID              = 21;   { number of valid user ids }

        CTL_USER_NAMES   : array [0..20] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'cs_path';CtlType: CTLTYPE_STRING ),
        (Name: 'bc_base_max';CtlType: CTLTYPE_INT ),
        (Name: 'bc_dim_max';CtlType: CTLTYPE_INT ),
        (Name: 'bc_scale_max';CtlType: CTLTYPE_INT ),
        (Name: 'bc_string_max';CtlType: CTLTYPE_INT ),
        (Name: 'coll_weights_max';CtlType: CTLTYPE_INT ),
        (Name: 'expr_nest_max';CtlType: CTLTYPE_INT ),
        (Name: 'line_max';CtlType: CTLTYPE_INT ),
        (Name: 're_dup_max';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_version';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_c_bind';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_c_dev';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_char_term';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_fort_dev';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_fort_run';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_localedef';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_sw_dev';CtlType: CTLTYPE_INT ),
        (Name: 'posix2_upe';CtlType: CTLTYPE_INT ),
        (Name: 'stream_max';CtlType: CTLTYPE_INT ),
        (Name: 'tzname_max';CtlType: CTLTYPE_INT ));

{
 * CTL_DEBUG definitions
 *
 * Second level identifier specifies which debug variable.
 * Third level identifier specifies which stucture component.
}
        CTL_DEBUG_NAME          = 0;    { string: variable name }
        CTL_DEBUG_VALUE         = 1;    { int: variable value }
        CTL_DEBUG_MAXID         = 20;

        POOLWORDS = 1024;       { Power of 2 - note that this is 32-bit words }

        RND_RND         = 0;    { real randomness like nuclear chips }
        RND_SRND        = 1;    { strong random source }
        RND_URND        = 2;    { less strong random source }
        RND_PRND        = 3;    { pseudo random source }
        RND_ARND        = 4;    { aRC4 based random number generator }
        RND_NODEV       = 5;    { First invalid minor device number }

        RND_SRC_TRUE    = 0;
        RND_SRC_TIMER   = 1;
        RND_SRC_MOUSE   = 2;
        RND_SRC_TTY     = 3;
        RND_SRC_DISK    = 4;
        RND_SRC_NET     = 5;
        RND_SRC_AUDIO   = 6;
        RND_SRC_VIDEO   = 7;
        RND_SRC_NUM     = 8;


{
 * Types
}
        SOCK_STREAM     = 1;            { stream socket }
        SOCK_DGRAM      = 2;            { datagram socket }
        SOCK_RAW        = 3;            { raw-protocol interface }
        SOCK_RDM        = 4;            { reliably-delivered message }
        SOCK_SEQPACKET  = 5;            { sequenced packet stream }

{
 * Option flags per-socket.
}
        SO_DEBUG        = $00001;               { turn on debugging info recording }
        SO_ACCEPTCONN   = $00002;               { socket has had listen(Name:) }
        SO_REUSEADDR    = $00004;               { allow local address reuse }
        SO_KEEPALIVE    = $00008;               { keep connections alive }
        SO_DONTROUTE    = $00010;               { just use interface addresses }
        SO_BROADCAST    = $00020;               { permit sending of broadcast msgs }
        SO_USELOOPBACK  = $00040;               { bypass hardware when possible }
        SO_LINGER       = $00080;               { linger on close if data present }
        SO_OOBINLINE    = $00100;               { leave received OOB data in line }
        SO_REUSEPORT    = $00200;               { allow local address & port reuse }

{
 * Additional options, not kept in so_options.
}
        SO_SNDBUF       = $01001;               { send buffer size }
        SO_RCVBUF       = $01002;               { receive buffer size }
        SO_SNDLOWAT     = $01003;               { send low-water mark }
        SO_RCVLOWAT     = $01004;               { receive low-water mark }
        SO_SNDTIMEO     = $01005;               { send timeout }
        SO_RCVTIMEO     = $01006;               { receive timeout }
        SO_ERROR        = $01007;               { get error status and clear }
        SO_TYPE         = $01008;               { get socket type }
        SO_NETPROC      = $01020;               { multiplex; network processing }


{
 * Level number for (Name:get/set)sockopt() to apply to socket itself.
}
        SOL_SOCKET      = $0ffff;               { options for socket level }

{
 * Address families.
}
        AF_UNSPEC       = 0;            { unspecified }
        AF_LOCAL        = 1;            { local to host (Name:pipes;CtlType: portals) }
        AF_UNIX         = AF_LOCAL;     { backward compatibility }
        AF_INET         = 2;            { internetwork: UDP, TCP, etc. }
        AF_IMPLINK      = 3;            { arpanet imp addresses }
        AF_PUP          = 4;            { pup protocols: e.g. BSP }
        AF_CHAOS        = 5;            { mit CHAOS protocols }
        AF_NS           = 6;            { XEROX NS protocols }
        AF_ISO          = 7;            { ISO protocols }
        AF_OSI          = AF_ISO;
        AF_ECMA         = 8;            { european computer manufacturers }
        AF_DATAKIT      = 9;            { datakit protocols }
        AF_CCITT        = 10;           { CCITT protocols, X.25 etc }
        AF_SNA          = 11;           { IBM SNA }
        AF_DECnet       = 12;           { DECnet }
        AF_DLI          = 13;           { DEC Direct data link interface }
        AF_LAT          = 14;           { LAT }
        AF_HYLINK       = 15;           { NSC Hyperchannel }
        AF_APPLETALK    = 16;           { Apple Talk }
        AF_ROUTE        = 17;           { Internal Routing Protocol }
        AF_LINK         = 18;           { Link layer interface }
        pseudo_AF_XTP   = 19;           { eXpress Transfer Protocol (Name:no AF) }
        AF_COIP         = 20;           { connection-oriented IP, aka ST II }
        AF_CNT          = 21;           { Computer Network Technology }
        pseudo_AF_RTIP  = 22;           { Help Identify RTIP packets }
        AF_IPX          = 23;           { Novell Internet Protocol }
        AF_INET6        = 24;           { IPv6 }
        pseudo_AF_PIP   = 25;           { Help Identify PIP packets }
        AF_ISDN         = 26;           { Integrated Services Digital Network}
        AF_E164         = AF_ISDN;              { CCITT E.164 recommendation }
        AF_NATM         = 27;           { native ATM access }
        AF_ENCAP        = 28;
        AF_SIP          = 29;           { Simple Internet Protocol }
        AF_KEY          = 30;
        pseudo_AF_HDRCMPLT = 31;                { Used by BPF to not rewrite headers
                                           in interface output routine }
        AF_MAX          = 32;

{
 * Protocol families, same as address families for now.
}
        PF_UNSPEC       = AF_UNSPEC  ;
        PF_LOCAL        = AF_LOCAL   ;
        PF_UNIX         = PF_LOCAL   ;  { backward compatibility }
        PF_INET         = AF_INET    ;
        PF_IMPLINK      = AF_IMPLINK ;
        PF_PUP          = AF_PUP     ;
        PF_CHAOS        = AF_CHAOS   ;
        PF_NS           = AF_NS      ;
        PF_ISO          = AF_ISO     ;
        PF_OSI          = AF_ISO     ;
        PF_ECMA         = AF_ECMA    ;
        PF_DATAKIT      = AF_DATAKIT ;
        PF_CCITT        = AF_CCITT   ;
        PF_SNA          = AF_SNA     ;
        PF_DECnet       = AF_DECnet  ;
        PF_DLI          = AF_DLI     ;
        PF_LAT          = AF_LAT     ;
        PF_HYLINK       = AF_HYLINK  ;
        PF_APPLETALK    = AF_APPLETALK ;
        PF_ROUTE        = AF_ROUTE     ;
        PF_LINK         = AF_LINK      ;
        PF_XTP          = pseudo_AF_XTP;        { really just proto family, no AF }
        PF_COIP         = AF_COIP      ;
        PF_CNT          = AF_CNT       ;
        PF_IPX          = AF_IPX          ;             { same format as AF_NS }
        PF_INET6        = AF_INET6  ;
        PF_PIP          = pseudo_AF_PIP;
        PF_ISDN         = AF_ISDN        ;
        PF_NATM         = AF_NATM        ;
        PF_ENCAP        = AF_ENCAP       ;
        PF_SIP          = AF_SIP         ;
        PF_KEY          = AF_KEY         ;
        PF_MAX          = AF_MAX         ;

{
 * These are the valid values for the 'how' field used by shutdown(Name:2).
}
        SHUT_RD         = 0;
        SHUT_WR         = 1;
        SHUT_RDWR       = 2;

{
 * Definitions for network related sysctl, CTL_NET.
 *
 * Second level is protocol family.
 * Third level is protocol number.
 *
 * Further levels are defined by the individual families below.
}
        NET_MAXID       = AF_MAX;

        CTL_NET_NAMES   : array [0..30] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'unix';CtlType: CTLTYPE_NODE ),
        (Name: 'inet';CtlType: CTLTYPE_NODE ),
        (Name: 'implink';CtlType: CTLTYPE_NODE ),
        (Name: 'pup';CtlType: CTLTYPE_NODE ),
        (Name: 'chaos';CtlType: CTLTYPE_NODE ),
        (Name: 'xerox_ns';CtlType: CTLTYPE_NODE ),
        (Name: 'iso';CtlType: CTLTYPE_NODE ),
        (Name: 'emca';CtlType: CTLTYPE_NODE ),
        (Name: 'datakit';CtlType: CTLTYPE_NODE ),
        (Name: 'ccitt';CtlType: CTLTYPE_NODE ),
        (Name: 'ibm_sna';CtlType: CTLTYPE_NODE ),
        (Name: 'decnet';CtlType: CTLTYPE_NODE ),
        (Name: 'dec_dli';CtlType: CTLTYPE_NODE ),
        (Name: 'lat';CtlType: CTLTYPE_NODE ),
        (Name: 'hylink';CtlType: CTLTYPE_NODE ),
        (Name: 'appletalk';CtlType: CTLTYPE_NODE ),
        (Name: 'route';CtlType: CTLTYPE_NODE ),
        (Name: 'link_layer';CtlType: CTLTYPE_NODE ),
        (Name: 'xtp';CtlType: CTLTYPE_NODE ),
        (Name: 'coip';CtlType: CTLTYPE_NODE ),
        (Name: 'cnt';CtlType: CTLTYPE_NODE ),
        (Name: 'rtip';CtlType: CTLTYPE_NODE ),
        (Name: 'ipx';CtlType: CTLTYPE_NODE ),
        (Name: 'inet6';CtlType: CTLTYPE_NODE ),
        (Name: 'pip';CtlType: CTLTYPE_NODE ),
        (Name: 'isdn';CtlType: CTLTYPE_NODE ),
        (Name: 'natm';CtlType: CTLTYPE_NODE ),
        (Name: 'encap';CtlType: CTLTYPE_NODE ),
        (Name: 'sip';CtlType: CTLTYPE_NODE ),
        (Name: 'key';CtlType: CTLTYPE_NODE ));

{
 * PF_ROUTE - Routing table
 *
 * Three additional levels are defined:
 *      Fourth: address family, 0 is wildcard
 *      Fifth: type of info, defined below
 *      Sixth: flag(Name:s) to mask with for NET_RT_FLAGS
}
        NET_RT_DUMP     = 1;            { dump; may limit to a.f. }
        NET_RT_FLAGS    = 2;            { by flags, e.g. RESOLVING }
        NET_RT_IFLIST   = 3;            { survey interface list }
        NET_RT_MAXID    = 4;

        CTL_NET_RT_NAMES   : array [0..3] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'dump';CtlType: CTLTYPE_STRUCT ),
        (Name: 'flags';CtlType: CTLTYPE_STRUCT ),
        (Name: 'iflist';CtlType: CTLTYPE_STRUCT ));

        MSG_OOB         = $01;          { process out-of-band data }
        MSG_PEEK        = $02;          { peek at incoming message }
        MSG_DONTROUTE   = $04;          { send without using routing tables }
        MSG_EOR         = $08;          { data completes record }
        MSG_TRUNC       = $010;         { data discarded before delivery }
        MSG_CTRUNC      = $020;         { control data lost before delivery }
        MSG_WAITALL     = $040;         { wait for full request or error }
        MSG_DONTWAIT    = $080;         { this message should be nonblocking }
        MSG_BCAST       = $0100;                { this message rec'd as broadcast }
        MSG_MCAST       = $0200;                { this message rec'd as multicast }

{
 * Possible states of profiling.
}
        GMON_PROF_ON    = 0;
        GMON_PROF_BUSY  = 1;
        GMON_PROF_ERROR = 2;
        GMON_PROF_OFF   = 3;

{
 * Sysctl definitions for extracting profiling information from the kernel.
}
        GPROF_STATE     = 0;    { int: profiling enabling variable }
        GPROF_COUNT     = 1;    { struct: profile tick count buffer }
        GPROF_FROMS     = 2;    { struct: from location hash bucket }
        GPROF_TOS       = 3;    { struct: destination/count structure }
        GPROF_GMONPARAM = 4;    { struct: profiling parameters (Name:see above) }

{
 * CTL_VM identifiers
}
        VM_METER        = 1;            { struct vmmeter }
        VM_LOADAVG      = 2;            { struct loadavg }
        VM_PSSTRINGS    = 3;            { PSSTRINGS }
        VM_UVMEXP       = 4;            { struct uvmexp }
        VM_SWAPENCRYPT  = 5;            { int }
        VM_NKMEMPAGES   = 6;            { int - # kmem_map pages }
        VM_ANONMIN      = 7;
        VM_VTEXTMIN     = 8;
        VM_VNODEMIN     = 9;
        VM_MAXSLP       = 10;
        VM_USPACE       = 11;
        VM_MAXID        = 12;           { number of valid vm ids }

        CTL_VM_NAMES   : array [0..11] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'vmmeter';CtlType: CTLTYPE_STRUCT ),
        (Name: 'loadavg';CtlType: CTLTYPE_STRUCT ),
        (Name: 'psstrings';CtlType: CTLTYPE_STRUCT ),
        (Name: 'uvmexp';CtlType: CTLTYPE_STRUCT ),
        (Name: 'swapencrypt';CtlType: CTLTYPE_NODE ),
        (Name: 'nkmempages';CtlType: CTLTYPE_INT ),
        (Name: 'anonmin';CtlType: CTLTYPE_INT ),
        (Name: 'vtextmin';CtlType: CTLTYPE_INT ),
        (Name: 'vnodemin';CtlType: CTLTYPE_INT ),
        (Name: 'maxslp';CtlType: CTLTYPE_INT ),
        (Name: 'uspace';CtlType: CTLTYPE_INT ));

        SWPENC_ENABLE   = 0;
        SWPENC_CREATED  = 1;
        SWPENC_DELETED  = 2;
        SWPENC_MAXID    = 3;

        CTL_SWPENC_NAMES   : array [0..2] OF CtlNameRec = (
        (Name: 'enable';CtlType: CTLTYPE_INT ),
        (Name: 'keyscreated';CtlType: CTLTYPE_INT ),
        (Name: 'keysdeleted';CtlType: CTLTYPE_INT ));

{
 * Protocols
}
        IPPROTO_IP              = 0;            { dummy for IP }
        IPPROTO_HOPOPTS         = IPPROTO_IP;   { Hop-by-hop option header }
        IPPROTO_ICMP            = 1;            { control message protocol }
        IPPROTO_IGMP            = 2;            { group mgmt protocol }
        IPPROTO_GGP             = 3;            { gateway^2 (Name:deprecated) }
        IPPROTO_IPIP            = 4;            { IP inside IP }
        IPPROTO_IPV4            = IPPROTO_IPIP; { IP inside IP }
        IPPROTO_TCP             = 6;            { tcp }
        IPPROTO_EGP             = 8;            { exterior gateway protocol }
        IPPROTO_PUP             = 12;           { pup }
        IPPROTO_UDP             = 17;           { user datagram protocol }
        IPPROTO_IDP             = 22;           { xns idp }
        IPPROTO_TP              = 29;           { tp-4 w/ class negotiation }
        IPPROTO_IPV6            = 41;           { IPv6 in IPv6 }
        IPPROTO_ROUTING         = 43;           { Routing header }
        IPPROTO_FRAGMENT        = 44;           { Fragmentation/reassembly header }
        IPPROTO_RSVP            = 46;           { resource reservation }
        IPPROTO_GRE             = 47;           { GRE encap, RFCs 1701/1702 }
        IPPROTO_ESP             = 50;           { Encap. Security Payload }
        IPPROTO_AH              = 51;           { Authentication header }
        IPPROTO_MOBILE          = 55;           { IP Mobility, RFC 2004 }
        IPPROTO_ICMPV6          = 58;           { ICMP for IPv6 }
        IPPROTO_NONE            = 59;           { No next header }
        IPPROTO_DSTOPTS         = 60;           { Destination options header }
        IPPROTO_EON             = 80;           { ISO cnlp }
        IPPROTO_ETHERIP         = 97;           { Ethernet in IPv4 }
        IPPROTO_ENCAP           = 98;           { encapsulation header }
        IPPROTO_PIM             = 103;          { Protocol indep. multicast }
        IPPROTO_IPCOMP          = 108;          { IP Payload Comp. Protocol }
        IPPROTO_RAW             = 255;          { raw IP packet }

        IPPROTO_MAX             = 256;


{
 * Options for use with [gs]etsockopt at the IP level.
 * First word of comment is data type; bool is stored in int.
}
        IP_OPTIONS              = 1;    { buf/ip_opts; set/get IP options }
        IP_HDRINCL              = 2;    { int; header is included with data }
        IP_TOS                  = 3;    { int; IP type of service and preced. }
        IP_TTL                  = 4;    { int; IP time to live }
        IP_RECVOPTS             = 5;    { bool; receive all IP opts w/dgram }
        IP_RECVRETOPTS          = 6;    { bool; receive IP opts for response }
        IP_RECVDSTADDR          = 7;    { bool; receive IP dst addr w/dgram }
        IP_RETOPTS              = 8;    { ip_opts; set/get IP options }
        IP_MULTICAST_IF         = 9;    { in_addr; set/get IP multicast i/f  }
        IP_MULTICAST_TTL        = 10;   { u_char; set/get IP multicast ttl }
        IP_MULTICAST_LOOP       = 11;   { u_char; set/get IP multicast loopback }
        IP_ADD_MEMBERSHIP       = 12;   { ip_mreq; add an IP group membership }
        IP_DROP_MEMBERSHIP      = 13;   { ip_mreq; drop an IP group membership }

{ 14-17 left empty for future compatibility with FreeBSD }

        IP_PORTRANGE            = 19;   { int; range to choose for unspec port }
        IP_AUTH_LEVEL           = 20;   { int; authentication used }
        IP_ESP_TRANS_LEVEL      = 21;   { int; transport encryption }
        IP_ESP_NETWORK_LEVEL    = 22;   { int; full-packet encryption }
        IP_IPSEC_LOCAL_ID       = 23;   { buf; IPsec local ID }
        IP_IPSEC_REMOTE_ID      = 24;   { buf; IPsec remote ID }
        IP_IPSEC_LOCAL_CRED     = 25;   { buf; IPsec local credentials }
        IP_IPSEC_REMOTE_CRED    = 26;   { buf; IPsec remote credentials }
        IP_IPSEC_LOCAL_AUTH     = 27;   { buf; IPsec local auth material }
        IP_IPSEC_REMOTE_AUTH    = 28;   { buf; IPsec remote auth material }
        IP_IPCOMP_LEVEL         = 29;   { int; compression used }

{
 * Security levels - IPsec, not IPSO
}

        IPSEC_LEVEL_BYPASS      = $000;    { Bypass policy altogether }
        IPSEC_LEVEL_NONE        = $000;    { Send clear, accept any }
        IPSEC_LEVEL_AVAIL       = $001;    { Send secure if SA available }
        IPSEC_LEVEL_USE         = $002;    { Send secure, accept any }
        IPSEC_LEVEL_REQUIRE     = $003;    { Require secure inbound, also use }
        IPSEC_LEVEL_UNIQUE      = $004;    { Use outbound SA that is unique }
        IPSEC_LEVEL_DEFAULT     = IPSEC_LEVEL_AVAIL;


{
 * Defaults and limits for options
}
        IP_DEFAULT_MULTICAST_TTL  = 1;  { normally limit m'casts to 1 hop  }
        IP_DEFAULT_MULTICAST_LOOP = 1;  { normally hear sends if a member  }
        IP_MAX_MEMBERSHIPS      = 20;   { per socket; must fit in one mbuf }

{
 * Argument for IP_PORTRANGE:
 * - which range to search when port is unspecified at bind(Name:) or connect()
}
        IP_PORTRANGE_DEFAULT    = 0;    { default range }
        IP_PORTRANGE_HIGH       = 1;    { 'high' - request firewall bypass }
        IP_PORTRANGE_LOW        = 2;    { 'low' - vouchsafe security }

{
 * Buffer lengths for strings containing printable IP addresses
}
        INET_ADDRSTRLEN         = 16;

{
 * Definitions for inet sysctl operations.
 *
 * Third level is protocol number.
 * Fourth level is desired variable within that protocol.
}
        IPPROTO_MAXID   = (IPPROTO_IPCOMP + 1); { don't list to IPPROTO_MAX }

        CTL_IPPROTO_NAMES   : array [0..108] OF CtlNameRec = (
        (Name: 'ip';CtlType: CTLTYPE_NODE ),
        (Name: 'icmp';CtlType: CTLTYPE_NODE ),
        (Name: 'igmp';CtlType: CTLTYPE_NODE ),
        (Name: 'ggp';CtlType: CTLTYPE_NODE ),
        (Name: 'ipip';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: 'tcp';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: 'egp';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'pup';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'udp';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'gre';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'esp';CtlType: CTLTYPE_NODE ),
        (Name: 'ah';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'mobileip';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'etherip';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'ipcomp';CtlType: CTLTYPE_NODE ));

{
 * Names for IP sysctl objects
}
        IPCTL_FORWARDING        = 1;    { act as router }
        IPCTL_SENDREDIRECTS     = 2;    { may send redirects when forwarding }
        IPCTL_DEFTTL            = 3;    { default TTL }
{$ifdef notyet}
        IPCTL_DEFMTU            = 4;    { default MTU }
{$endif}
        IPCTL_SOURCEROUTE       = 5;    { may perform source routes }
        IPCTL_DIRECTEDBCAST     = 6;    { default broadcast behavior }
        IPCTL_IPPORT_FIRSTAUTO  = 7;
        IPCTL_IPPORT_LASTAUTO   = 8;
        IPCTL_IPPORT_HIFIRSTAUTO = 9;
        IPCTL_IPPORT_HILASTAUTO = 10;
        IPCTL_IPPORT_MAXQUEUE   = 11;
        IPCTL_ENCDEBUG          = 12;
{$ifdef obssolete}
        IPCTL_GIF_TTL           = 13;   { default TTL for gif encap packet }
{$endif}
        IPCTL_IPSEC_EXPIRE_ACQUIRE = 14;   { How long to wait for key mgmt. }
        IPCTL_IPSEC_EMBRYONIC_SA_TIMEOUT        = 15; { new SA lifetime }
        IPCTL_IPSEC_REQUIRE_PFS = 16;
        IPCTL_IPSEC_SOFT_ALLOCATIONS            = 17;
        IPCTL_IPSEC_ALLOCATIONS = 18;
        IPCTL_IPSEC_SOFT_BYTES  = 19;
        IPCTL_IPSEC_BYTES       = 20;
        IPCTL_IPSEC_TIMEOUT     = 21;
        IPCTL_IPSEC_SOFT_TIMEOUT = 22;
        IPCTL_IPSEC_SOFT_FIRSTUSE = 23;
        IPCTL_IPSEC_FIRSTUSE    = 24;
        IPCTL_IPSEC_ENC_ALGORITHM = 25;
        IPCTL_IPSEC_AUTH_ALGORITHM = 26;
        IPCTL_MTUDISC           = 27;   { allow path MTU discovery }
        IPCTL_MTUDISCTIMEOUT    = 28;   { allow path MTU discovery }
        IPCTL_IPSEC_IPCOMP_ALGORITHM    = 29;
        IPCTL_MAXID             = 30;

        IPCTL_NAMES   : array [0..29] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'forwarding';CtlType: CTLTYPE_INT ),
        (Name: 'redirect';CtlType: CTLTYPE_INT ),
        (Name: 'ttl';CtlType: CTLTYPE_INT ),
        {(Name: 'mtu';CtlType: CTLTYPE_INT ), }
        (Name: ''; CTLTYPE:0 ),
        (Name: 'sourceroute';CtlType: CTLTYPE_INT ),
        (Name: 'directed-broadcast';CtlType: CTLTYPE_INT ),
        (Name: 'portfirst';CtlType: CTLTYPE_INT ),
        (Name: 'portlast';CtlType: CTLTYPE_INT ),
        (Name: 'porthifirst';CtlType: CTLTYPE_INT ),
        (Name: 'porthilast';CtlType: CTLTYPE_INT ),
        (Name: 'maxqueue';CtlType: CTLTYPE_INT ),
        (Name: 'encdebug';CtlType: CTLTYPE_INT ),
        (Name: '';CtlType: 0 ),
        (Name: 'ipsec-expire-acquire';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-invalid-life';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-pfs';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-soft-allocs';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-allocs';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-soft-bytes';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-bytes';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-timeout';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-soft-timeout';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-soft-firstuse';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-firstuse';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-enc-alg';CtlType: CTLTYPE_STRING ),
        (Name: 'ipsec-auth-alg';CtlType: CTLTYPE_STRING ),
        (Name: 'mtudisc';CtlType: CTLTYPE_INT ),
        (Name: 'mtudisctimeout';CtlType: CTLTYPE_INT ),
        (Name: 'ipsec-comp-alg';CtlType: CTLTYPE_STRING ));

{
 * Names for ICMP sysctl objects
}
        ICMPCTL_MASKREPL        = 1;    { allow replies to netmask requests }
        ICMPCTL_BMCASTECHO      = 2;    { reply to icmps to broadcast/mcast }
        ICMPCTL_ERRPPSLIMIT     = 3;    { ICMP error pps limitation }
        ICMPCTL_REDIRACCEPT     = 4;    { Accept redirects from routers }
        ICMPCTL_REDIRTIMEOUT    = 5;    { Remove routes added via redirects }
        ICMPCTL_TSTAMPREPL      = 6;    { allow replies to timestamp requests }
        ICMPCTL_MAXID           = 7;

        ICMPCTL_NAMES   : array [0..6] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'maskrepl';CtlType: CTLTYPE_INT ),
        (Name: 'bmcastecho';CtlType: CTLTYPE_INT ),
        (Name: 'errppslimit';CtlType: CTLTYPE_INT ),
        (Name: 'rediraccept';CtlType: CTLTYPE_INT ),
        (Name: 'redirtimeout';CtlType: CTLTYPE_INT ),
        (Name: 'tstamprepl';CtlType: CTLTYPE_INT ));

{
 * Names for ICMP sysctl objects
}
        ICMPV6CTL_STATS         = 1;
        ICMPV6CTL_REDIRACCEPT   = 2;    { accept/process redirects }
        ICMPV6CTL_REDIRTIMEOUT  = 3;    { redirect cache time }
{$ifdef obsolete_false}
        ICMPV6CTL_ERRRATELIMIT  = 5;    { ICMPv6 error rate limitation }
{$endif}
        ICMPV6CTL_ND6_PRUNE     = 6;
        ICMPV6CTL_ND6_DELAY     = 8;
        ICMPV6CTL_ND6_UMAXTRIES = 9;
        ICMPV6CTL_ND6_MMAXTRIES         = 10;
        ICMPV6CTL_ND6_USELOOPBACK       = 11;
{ #define ICMPV6CTL_ND6_PROXYALL        12;     obsoleted, do not reuse here }
        ICMPV6CTL_NODEINFO      = 13;
        ICMPV6CTL_ERRPPSLIMIT   = 14;   { ICMPv6 error pps limitation }
        ICMPV6CTL_ND6_MAXNUDHINT        = 15;
        ICMPV6CTL_MTUDISC_HIWAT = 16;
        ICMPV6CTL_MTUDISC_LOWAT = 17;
        ICMPV6CTL_ND6_DEBUG     = 18;
        ICMPV6CTL_ND6_DRLIST    = 19;
        ICMPV6CTL_ND6_PRLIST    = 20;
        ICMPV6CTL_MAXID         = 21;

        ICMPV6CTL_NAMES   : array [0..20] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'rediraccept';CtlType: CTLTYPE_INT ),
        (Name: 'redirtimeout';CtlType: CTLTYPE_INT ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ),
        (Name: 'nd6_prune';CtlType: CTLTYPE_INT ),
        (Name: '';CtlType: 0 ),
        (Name: 'nd6_delay';CtlType: CTLTYPE_INT ),
        (Name: 'nd6_umaxtries';CtlType: CTLTYPE_INT ),
        (Name: 'nd6_mmaxtries';CtlType: CTLTYPE_INT ),
        (Name: 'nd6_useloopback';CtlType: CTLTYPE_INT ),
        (Name: '';CtlType: 0 ),
        (Name: 'nodeinfo';CtlType: CTLTYPE_INT ),
        (Name: 'errppslimit';CtlType: CTLTYPE_INT ),
        (Name: 'nd6_maxnudhint';CtlType: CTLTYPE_INT ),
        (Name: 'mtudisc_hiwat';CtlType: CTLTYPE_INT ),
        (Name: 'mtudisc_lowat';CtlType: CTLTYPE_INT ),
        (Name: 'nd6_debug';CtlType: CTLTYPE_INT ),
        (Name: '';CtlType: 0 ),
        (Name: '';CtlType: 0 ));

{
 * Names for TCP sysctl objects.
}

        TCPCTL_RFC1323          = 1; { enable/disable RFC1323 timestamps/scaling }
        TCPCTL_KEEPINITTIME     = 2; { TCPT_KEEP value }
        TCPCTL_KEEPIDLE         = 3; { allow tcp_keepidle to be changed }
        TCPCTL_KEEPINTVL        = 4; { allow tcp_keepintvl to be changed }
        TCPCTL_SLOWHZ           = 5; { return kernel idea of PR_SLOWHZ }
        TCPCTL_BADDYNAMIC       = 6; { return bad dynamic port bitmap }
        TCPCTL_RECVSPACE        = 7; { receive buffer space }
        TCPCTL_SENDSPACE        = 8; { send buffer space }
        TCPCTL_IDENT            = 9; { get connection owner }
        TCPCTL_SACK            = 10; { selective acknowledgement, rfc 2018 }
        TCPCTL_MSSDFLT         = 11; { Default maximum segment size }
        TCPCTL_RSTPPSLIMIT     = 12; { RST pps limit }
        TCPCTL_ACK_ON_PUSH     = 13; { ACK immediately on PUSH }
        TCPCTL_ECN             = 14; { RFC3168 ECN }
        TCPCTL_MAXID           = 15;

        TCPCTL_NAMES  : array [0..14] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'rfc1323';CtlType:       CTLTYPE_INT ),
        (Name: 'keepinittime';CtlType:  CTLTYPE_INT ),
        (Name: 'keepidle';CtlType:      CTLTYPE_INT ),
        (Name: 'keepintvl';CtlType:     CTLTYPE_INT ),
        (Name: 'slowhz';CtlType:        CTLTYPE_INT ),
        (Name: 'baddynamic';CtlType: CTLTYPE_STRUCT ),
        (Name: 'recvspace';CtlType:     CTLTYPE_INT ),
        (Name: 'sendspace';CtlType:     CTLTYPE_INT ),
        (Name: 'ident';CtlType:         CTLTYPE_STRUCT ),
        (Name: 'sack';CtlType:  CTLTYPE_INT ),
        (Name: 'mssdflt';CtlType:       CTLTYPE_INT ),
        (Name: 'rstppslimit';CtlType:   CTLTYPE_INT ),
        (Name: 'ackonpush';CtlType:     CTLTYPE_INT ),
        (Name: 'ecn';CtlType:   CTLTYPE_INT ));

{
 * Names for UDP sysctl objects
}
        UDPCTL_CHECKSUM         = 1; { checksum UDP packets }
        UDPCTL_BADDYNAMIC       = 2; { return bad dynamic port bitmap }
        UDPCTL_RECVSPACE        = 3; { receive buffer space }
        UDPCTL_SENDSPACE        = 4; { send buffer space }
        UDPCTL_MAXID            = 5;

        UDPCTL_NAMES  : array [0..4] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'checksum';CtlType: CTLTYPE_INT ),
        (Name: 'baddynamic';CtlType: CTLTYPE_STRUCT ),
        (Name: 'recvspace';CtlType:  CTLTYPE_INT ),
        (Name: 'sendspace';CtlType:  CTLTYPE_INT ));

{
 * Names for IPX sysctl objects.
}

        IPXCTL_CHECKSUM         = 1;
        IPXCTL_FORWARDING       = 2;
        IPXCTL_NETBIOS          = 3;
        IPXCTL_RECVSPACE        = 4;
        IPXCTL_SENDSPACE        = 5;
        IPXCTL_MAXID            = 6;

        IPXCTL_NAMES  : array [0..5] OF CtlNameRec = (
        (Name: '';CtlType: 0),
        (Name: 'checksum';CtlType: CTLTYPE_INT ),
        (Name: 'forwarding';CtlType: CTLTYPE_INT ),
        (Name: 'netbios';CtlType: CTLTYPE_INT ),
        (Name: 'recvspace';CtlType: CTLTYPE_INT ),
        (Name: 'sendspace';CtlType: CTLTYPE_INT ));

        DBCTL_RADIX     = 1;
        DBCTL_MAXWIDTH  = 2;
        DBCTL_MAXLINE   = 3;
        DBCTL_TABSTOP   = 4;
        DBCTL_PANIC     = 5;
        DBCTL_CONSOLE   = 6;
        DBCTL_MAXID     = 7;

        CTL_DDB_NAMES  : array [0..6] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'radix';CtlType: CTLTYPE_INT ),
        (Name: 'max_width';CtlType: CTLTYPE_INT ),
        (Name: 'max_line';CtlType: CTLTYPE_INT ),
        (Name: 'tab_stop_width';CtlType: CTLTYPE_INT ),
        (Name: 'panic';CtlType: CTLTYPE_INT ),
        (Name: 'console';CtlType: CTLTYPE_INT ));

{
 * Sysctl CTL_VFS definitions.
 *
 * Second level identifier specifies which filesystem. Second level
 * identifier VFS_GENERIC returns information about all filesystems.
}
        VFS_GENERIC     = 0;    { generic filesystem information }
{
 * Third level identifiers for VFS_GENERIC are given below; third
 * level identifiers for specific filesystems are given in their
 * mount specific header files.
}
        VFS_MAXTYPENUM  = 1;    { int: highest defined filesystem type }
        VFS_CONF        = 2;    { struct: vfsconf for filesystem given
                                   as next argument }
        VFSGEN_MAXID    = 3;    { max number of vfs.generic ids }

        CTL_VFSGENCTL_NAMES  : array [0..2] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'maxtypenum';CtlType: CTLTYPE_INT ),
        (Name: 'conf';CtlType: CTLTYPE_NODE ));

{
 * fs.nfs sysctl(Name:3) identifiers
}
        NFS_NFSSTATS    = 1;    { struct: struct nfsstats }
        NFS_NIOTHREADS  = 2;    { number of i/o threads }
        NFS_MAXID       = 3;

        FS_NFS_NAMES  : array [0..2] OF CtlNameRec = (
                        (Name: '';CtlType: 0 ),
                        (Name: 'nfsstats';CtlType: CTLTYPE_STRUCT ),
                        (Name: 'iothreads';CtlType: CTLTYPE_INT ));

        FFS_CLUSTERREAD         = 1;    { cluster reading enabled }
        FFS_CLUSTERWRITE        = 2;    { cluster writing enabled }
        FFS_REALLOCBLKS         = 3;    { block reallocation enabled }
        FFS_ASYNCFREE           = 4;    { asynchronous block freeing enabled }
        FFS_MAX_SOFTDEPS        = 5;    { maximum structs before slowdown }
        FFS_SD_TICKDELAY        = 6;    { ticks to pause during slowdown }
        FFS_SD_WORKLIST_PUSH    = 7;    { # of worklist cleanups }
        FFS_SD_BLK_LIMIT_PUSH   = 8;    { # of times block limit neared }
        FFS_SD_INO_LIMIT_PUSH   = 9;    { # of times inode limit neared }
        FFS_SD_BLK_LIMIT_HIT    = 10;   { # of times block slowdown imposed }
        FFS_SD_INO_LIMIT_HIT    = 11;   { # of times inode slowdown imposed }
        FFS_SD_SYNC_LIMIT_HIT   = 12;   { # of synchronous slowdowns imposed }
        FFS_SD_INDIR_BLK_PTRS   = 13;   { bufs redirtied as indir ptrs not written }
        FFS_SD_INODE_BITMAP     = 14;   { bufs redirtied as inode bitmap not written }
        FFS_SD_DIRECT_BLK_PTRS  = 15;   { bufs redirtied as direct ptrs not written }
        FFS_SD_DIR_ENTRY        = 16;   { bufs redirtied as dir entry cannot write }
        FFS_MAXID               = 17;   { number of valid ffs ids }

        FFS_NAMES  : array [0..16] OF CtlNameRec = (
        (Name: '';CtlType: 0 ),
        (Name: 'doclusterread';CtlType: CTLTYPE_INT ),
        (Name: 'doclusterwrite';CtlType: CTLTYPE_INT ),
        (Name: 'doreallocblks';CtlType: CTLTYPE_INT ),
        (Name: 'doasyncfree';CtlType: CTLTYPE_INT ),
        (Name: 'max_softdeps';CtlType: CTLTYPE_INT ),
        (Name: 'sd_tickdelay';CtlType: CTLTYPE_INT ),
        (Name: 'sd_worklist_push';CtlType: CTLTYPE_INT ),
        (Name: 'sd_blk_limit_push';CtlType: CTLTYPE_INT ),
        (Name: 'sd_ino_limit_push';CtlType: CTLTYPE_INT ),
        (Name: 'sd_blk_limit_hit';CtlType: CTLTYPE_INT ),
        (Name: 'sd_ino_limit_hit';CtlType: CTLTYPE_INT ),
        (Name: 'sd_sync_limit_hit';CtlType: CTLTYPE_INT ),
        (Name: 'sd_indir_blk_ptrs';CtlType: CTLTYPE_INT ),
        (Name: 'sd_inode_bitmap';CtlType: CTLTYPE_INT ),
        (Name: 'sd_direct_blk_ptrs';CtlType: CTLTYPE_INT ),
        (Name: 'sd_dir_entry';CtlType: CTLTYPE_INT ));

{


  Revision 1.3  2005/02/14 17:13:31  peter
    * truncate log

}
