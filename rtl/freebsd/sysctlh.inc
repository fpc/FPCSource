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


CONST

{
 * Definitions for sysctl call.  The sysctl call uses a hierarchical name
 * for objects that can be examined or modified.  The name is expressed as
 * a sequence of integers.  Like a file path name, the meaning of each
 * component depends on its place in the hierarchy.  The top-level and kern
 * identifiers are defined here, and other identifiers are defined in the
 * respective subsystem header files.
}

        CTL_MAXNAME     = 12;   {  largest number of components supported }

{
 * Each subsystem defined by sysctl defines a list of variables
 * for that subsystem. Each name is either a node with further
 * levels defined below it, or it is a leaf of some particular
 * type given below. Each sysctl level defines a set of name/type
 * pairs to be used by sysctl(1) in manipulating the subsystem.
}

Type
    ctlname = record
                ctl_name : pchar;       {  subsystem name }
                ctl_type : cint         {  type of name }
               End;

Const
        CTLTYPE         = $f;   {  Mask for the type }
        CTLTYPE_NODE    = 1;    {  name is a node }
        CTLTYPE_INT     = 2;    {  name describes an integer }
        CTLTYPE_STRING  = 3;    {  name describes a string }
        CTLTYPE_QUAD    = 4;    {  name describes a 64-bit number }
        CTLTYPE_OPAQUE  = 5;    {  name describes a structure }
        CTLTYPE_STRUCT  = CTLTYPE_OPAQUE;       {  name describes a structure }
        CTLTYPE_UINT    = 6;    {  name describes an unsigned integer }
        CTLTYPE_LONG    = 7;    {  name describes a long }
        CTLTYPE_ULONG   = 8;    {  name describes an unsigned long }

        CTLFLAG_RD      = $80000000;    {  Allow reads of variable }
        CTLFLAG_WR      = $40000000;    {  Allow writes to the variable }
        CTLFLAG_RW      = (CTLFLAG_RD OR CTLFLAG_WR);
        CTLFLAG_NOLOCK  = $20000000;    {  XXX Don't Lock }
        CTLFLAG_ANYBODY = $10000000;    {  All users can set this var }
        CTLFLAG_SECURE  = $8000000;     {  Permit set only if securelevel<=0 }
        CTLFLAG_PRISON  = $4000000;     {  Prisoned roots can fiddle }
        CTLFLAG_DYN     = $2000000;     {  Dynamic oid - can be freed }

{
 * USE THIS instead of a hardwired number from the categories below
 * to get dynamically assigned sysctl entries using the linker-set
 * technology. This is the way nearly all new sysctl variables should
 * be implemented.
 * e.g. SYSCTL_INT(_parent, OID_AUTO, name, CTLFLAG_RW, &variable, 0, "");
 }
        OID_AUTO        = (-1);

{
 * Top-level identifiers
 }
        CTL_UNSPEC      = 0;
        CTL_KERN        = 1;            {  "high kernel": proc, limits }
        CTL_VM          = 2;            {  virtual memory }
        CTL_VFS         = 3;            {  file system, mount type is next }
        CTL_NET         = 4;            {  network, see socket.h }
        CTL_DEBUG       = 5;            {  debugging parameters }
        CTL_HW          = 6;            {  generic cpu/io }
        CTL_MACHDEP     = 7;            {  machine dependent }
        CTL_USER        = 8;            {  user-level }
        CTL_P1003_1B    = 9;            {  POSIX 1003.1B }
        CTL_MAXID       = 10;           {  number of valid top-level ids }


{
 * CTL_KERN identifiers
 }
        KERN_OSTYPE             =  1;   {  string: system version }
        KERN_OSRELEASE          =  2;   {  string: system release }
        KERN_OSREV              =  3;   {  int: system revision }
        KERN_VERSION            =  4;   {  string: compile time info }
        KERN_MAXVNODES          =  5;   {  int: max vnodes }
        KERN_MAXPROC            =  6;   {  int: max processes }
        KERN_MAXFILES           =  7;   {  int: max open files }
        KERN_ARGMAX             =  8;   {  int: max arguments to exec }
        KERN_SECURELVL          =  9;   {  int: system security level }
        KERN_HOSTNAME           = 10;   {  string: hostname }
        KERN_HOSTID             = 11;   {  int: host identifier }
        KERN_CLOCKRATE          = 12;   {  struct: struct clockrate }
        KERN_VNODE              = 13;   {  struct: vnode structures }
        KERN_PROC               = 14;   {  struct: process entries }
        KERN_FILE               = 15;   {  struct: file entries }
        KERN_PROF               = 16;   {  node: kernel profiling info }
        KERN_POSIX1             = 17;   {  int: POSIX.1 version }
        KERN_NGROUPS            = 18;   {  int: # of supplemental group ids }
        KERN_JOB_CONTROL        = 19;   {  int: is job control available }
        KERN_SAVED_IDS          = 20;   {  int: saved set-user/group-ID }
        KERN_BOOTTIME           = 21;   {  struct: time kernel was booted }
        KERN_NISDOMAINNAME      = 22;   {  string: YP domain name }
        KERN_UPDATEINTERVAL     = 23;   {  int: update process sleep time }
        KERN_OSRELDATE          = 24;   {  int: OS release date }
        KERN_NTP_PLL            = 25;   {  node: NTP PLL control }
        KERN_BOOTFILE           = 26;   {  string: name of booted kernel }
        KERN_MAXFILESPERPROC    = 27;   {  int: max open files per proc }
        KERN_MAXPROCPERUID      = 28;   {  int: max processes per uid }
        KERN_DUMPDEV            = 29;   {  dev_t: device to dump on }
        KERN_IPC                = 30;   {  node: anything related to IPC }
        KERN_DUMMY              = 31;   {  unused }
        KERN_PS_STRINGS         = 32;   {  int: address of PS_STRINGS }
        KERN_USRSTACK           = 33;   {  int: address of USRSTACK }
        KERN_LOGSIGEXIT         = 34;   {  int: do we log sigexit procs? }
        KERN_MAXID              = 35;      {  number of valid kern ids }



{
 * KERN_PROC subtypes
 }
        KERN_PROC_ALL           = 0;    {  everything }
        KERN_PROC_PID           = 1;    {  by process id }
        KERN_PROC_PGRP          = 2;    {  by process group id }
        KERN_PROC_SESSION       = 3;    {  by session of pid }
        KERN_PROC_TTY           = 4;    {  by controlling tty }
        KERN_PROC_UID           = 5;    {  by effective uid }
        KERN_PROC_RUID          = 6;    {  by real uid }
        KERN_PROC_ARGS          = 7;    {  get/set arguments/proctitle }
 	KERN_PROC_PROC          = 8;    {  only return procs }
        KERN_PROC_SV_NAME       = 9;    {  get syscall vector name }
        KERN_PROC_RGID          = 10;   {  by real group id }
        KERN_PROC_GID           = 11;   {  by effective group id }
        KERN_PROC_PATHNAME      = 12;   {  path to executable }
        KERN_PROC_OVMMAP        = 13;   {  Old VM map entries for process }
        KERN_PROC_OFILEDESC     = 14;   {  Old file descriptors for process }
        KERN_PROC_KSTACK        = 15;   {  Kernel stacks for process }
        KERN_PROC_INC_THREAD    = $10;  {
                                         * modifier for pid, pgrp, tty,
                                         * uid, ruid, gid, rgid and proc
                                         * This effectively uses 16-31
                                         }
        KERN_PROC_VMMAP         = 32;   { VM map entries for process }
        KERN_PROC_FILEDESC      = 33;   { File descriptors for process }
        KERN_PROC_GROUPS        = 34;   { process groups }

{
 * KERN_IPC identifiers
 }
        KIPC_MAXSOCKBUF         = 1;    {  int: max size of a socket buffer }
        KIPC_SOCKBUF_WASTE      = 2;    {  int: wastage factor in sockbuf }
        KIPC_SOMAXCONN          = 3;    {  int: max length of connection q }
        KIPC_MAX_LINKHDR        = 4;    {  int: max length of link header }
        KIPC_MAX_PROTOHDR       = 5;    {  int: max length of network header }
        KIPC_MAX_HDR            = 6;    {  int: max total length of headers }
        KIPC_MAX_DATALEN        = 7;    {  int: max length of data? }
        KIPC_MBSTAT             = 8;    {  struct: mbuf usage statistics }
        KIPC_NMBCLUSTERS        = 9;    {  int: maximum mbuf clusters }

{
 * CTL_HW identifiers
 }
        HW_MACHINE      =  1;           {  string: machine class }
        HW_MODEL        =  2;           {  string: specific machine model }
        HW_NCPU         =  3;           {  int: number of cpus }
        HW_BYTEORDER    =  4;           {  int: machine byte order }
        HW_PHYSMEM      =  5;           {  int: total memory }
        HW_USERMEM      =  6;           {  int: non-kernel memory }
        HW_PAGESIZE     =  7;           {  int: software page size }
        HW_DISKNAMES    =  8;           {  strings: disk drive names }
        HW_DISKSTATS    =  9;           {  struct: diskstats[] }
        HW_FLOATINGPT   = 10;           {  int: has HW floating point? }
        HW_MACHINE_ARCH = 11;           {  string: machine architecture }
        HW_MAXID        = 12;           {  number of valid hw ids }


{
 * CTL_USER definitions
 }
        USER_CS_PATH            =  1;   {  string: _CS_PATH }
        USER_BC_BASE_MAX        =  2;   {  int: BC_BASE_MAX }
        USER_BC_DIM_MAX         =  3;   {  int: BC_DIM_MAX }
        USER_BC_SCALE_MAX       =  4;   {  int: BC_SCALE_MAX }
        USER_BC_STRING_MAX      =  5;   {  int: BC_STRING_MAX }
        USER_COLL_WEIGHTS_MAX   =  6;   {  int: COLL_WEIGHTS_MAX }
        USER_EXPR_NEST_MAX      =  7;   {  int: EXPR_NEST_MAX }
        USER_LINE_MAX           =  8;   {  int: LINE_MAX }
        USER_RE_DUP_MAX         =  9;   {  int: RE_DUP_MAX }
        USER_POSIX2_VERSION     = 10;   {  int: POSIX2_VERSION }
        USER_POSIX2_C_BIND      = 11;   {  int: POSIX2_C_BIND }
        USER_POSIX2_C_DEV       = 12;   {  int: POSIX2_C_DEV }
        USER_POSIX2_CHAR_TERM   = 13;   {  int: POSIX2_CHAR_TERM }
        USER_POSIX2_FORT_DEV    = 14;   {  int: POSIX2_FORT_DEV }
        USER_POSIX2_FORT_RUN    = 15;   {  int: POSIX2_FORT_RUN }
        USER_POSIX2_LOCALEDEF   = 16;   {  int: POSIX2_LOCALEDEF }
        USER_POSIX2_SW_DEV      = 17;   {  int: POSIX2_SW_DEV }
        USER_POSIX2_UPE         = 18;   {  int: POSIX2_UPE }
        USER_STREAM_MAX         = 19;   {  int: POSIX2_STREAM_MAX }
        USER_TZNAME_MAX         = 20;   {  int: POSIX2_TZNAME_MAX }
        USER_MAXID              = 21;   {  number of valid user ids }


        CTL_P1003_1B_ASYNCHRONOUS_IO            = 1 ;   {  boolean }
        CTL_P1003_1B_MAPPED_FILES               = 2 ;   {  boolean }
        CTL_P1003_1B_MEMLOCK                    = 3 ;   {  boolean }
        CTL_P1003_1B_MEMLOCK_RANGE              = 4 ;   {  boolean }
        CTL_P1003_1B_MEMORY_PROTECTION          = 5 ;   {  boolean }
        CTL_P1003_1B_MESSAGE_PASSING            = 6 ;   {  boolean }
        CTL_P1003_1B_PRIORITIZED_IO             = 7 ;   {  boolean }
        CTL_P1003_1B_PRIORITY_SCHEDULING        = 8 ;   {  boolean }
        CTL_P1003_1B_REALTIME_SIGNALS           = 9 ;   {  boolean }
        CTL_P1003_1B_SEMAPHORES                 = 10;   {  boolean }
        CTL_P1003_1B_FSYNC                      = 11;   {  boolean }
        CTL_P1003_1B_SHARED_MEMORY_OBJECTS      = 12;   {  boolean }
        CTL_P1003_1B_SYNCHRONIZED_IO            = 13;   {  boolean }
        CTL_P1003_1B_TIMERS                     = 14;   {  boolean }
        CTL_P1003_1B_AIO_LISTIO_MAX             = 15;   {  int }
        CTL_P1003_1B_AIO_MAX                    = 16;   {  int }
        CTL_P1003_1B_AIO_PRIO_DELTA_MAX         = 17;   {  int }
        CTL_P1003_1B_DELAYTIMER_MAX             = 18;   {  int }
        CTL_P1003_1B_MQ_OPEN_MAX                = 19;   {  int }
        CTL_P1003_1B_PAGESIZE                   = 20;   {  int }
        CTL_P1003_1B_RTSIG_MAX                  = 21;   {  int }
        CTL_P1003_1B_SEM_NSEMS_MAX              = 22;   {  int }
        CTL_P1003_1B_SEM_VALUE_MAX              = 23;   {  int }
        CTL_P1003_1B_SIGQUEUE_MAX               = 24;   {  int }
        CTL_P1003_1B_TIMER_MAX                  = 25;   {  int }

        CTL_P1003_1B_MAXID              = 26;

{       LongestStringInCtlNames = 21;}


Const

        CTL_NAMES : Array[0..9] OF CtlNameRec =  (
        ( Name: '';  CtlType: 0 ),
        ( Name: 'kern';  CtlType : CTLTYPE_NODE ),
        ( Name: 'vm';  CtlType : CTLTYPE_NODE ),
        ( Name: 'vfs';  CtlType : CTLTYPE_NODE ),
        ( Name: 'net';  CtlType : CTLTYPE_NODE ),
        ( Name: 'debug';  CtlType : CTLTYPE_NODE ),
        ( Name: 'hw';  CtlType : CTLTYPE_NODE ),
        ( Name: 'machdep';  CtlType : CTLTYPE_NODE ),
        ( Name: 'user';  CtlType : CTLTYPE_NODE ),
        ( Name: 'p1003_1b';  CtlType : CTLTYPE_NODE ));

        CTL_KERN_NAME : Array[0..34] OF CtlNameRec = (
        ( Name: ''; CtlType: 0 ),
        ( Name: 'ostype';  CtlType : CTLTYPE_STRING ),
        ( Name: 'osrelease';  CtlType : CTLTYPE_STRING ),
        ( Name: 'osrevision';  CtlType : CTLTYPE_INT ),
        ( Name: 'version';  CtlType : CTLTYPE_STRING ),
        ( Name: 'maxvnodes';  CtlType : CTLTYPE_INT ),
        ( Name: 'maxproc';  CtlType : CTLTYPE_INT ),
        ( Name: 'maxfiles';  CtlType : CTLTYPE_INT ),
        ( Name: 'argmax';  CtlType : CTLTYPE_INT ),
        ( Name: 'securelevel';  CtlType : CTLTYPE_INT ),
        ( Name: 'hostname';  CtlType : CTLTYPE_STRING ),
        ( Name: 'hostid';  CtlType : CTLTYPE_UINT ),
        ( Name: 'clockrate';  CtlType : CTLTYPE_STRUCT ),
        ( Name: 'vnode';  CtlType : CTLTYPE_STRUCT ),
        ( Name: 'proc';  CtlType : CTLTYPE_STRUCT ),
        ( Name: 'file';  CtlType : CTLTYPE_STRUCT ),
        ( Name: 'profiling';  CtlType : CTLTYPE_NODE ),
        ( Name: 'posix1version';  CtlType : CTLTYPE_INT ),
        ( Name: 'ngroups';  CtlType : CTLTYPE_INT ),
        ( Name: 'job_control';  CtlType : CTLTYPE_INT ),
        ( Name: 'saved_ids';  CtlType : CTLTYPE_INT ),
        ( Name: 'boottime';  CtlType : CTLTYPE_STRUCT ),
        ( Name: 'nisdomainname';  CtlType : CTLTYPE_STRING ),
        ( Name: 'update';  CtlType : CTLTYPE_INT ),
        ( Name: 'osreldate';  CtlType : CTLTYPE_INT ),
        ( Name: 'ntp_pll';  CtlType : CTLTYPE_NODE ),
        ( Name: 'bootfile';  CtlType : CTLTYPE_STRING ),
        ( Name: 'maxfilesperproc';  CtlType : CTLTYPE_INT ),
        ( Name: 'maxprocperuid';  CtlType : CTLTYPE_INT ),
        ( Name: 'dumpdev';  CtlType : CTLTYPE_STRUCT ), {  we lie; don't print as int }
        ( Name: 'ipc';  CtlType : CTLTYPE_NODE ),
        ( Name: 'dummy';  CtlType : CTLTYPE_INT ),
        ( Name: 'ps_strings';  CtlType : CTLTYPE_INT ),
        ( Name: 'usrstack';  CtlType : CTLTYPE_INT ),
        ( Name: 'logsigexit';  CtlType : CTLTYPE_INT ));

{
 * CTL_VFS identifiers
}
        CTL_VFS_NAMES : array[0..0] of CTLNameRec = (
        ( Name: 'vfsconf';  CtlType : CTLTYPE_STRUCT ));


        CTL_HW_NAMES : array[0..10] of CTLNameRec = (
        ( Name: ''; CtlType: 0 ),
        ( Name: 'machine';  CtlType : CTLTYPE_STRING ),
        ( Name: 'model';  CtlType : CTLTYPE_STRING ),
        ( Name: 'ncpu';  CtlType : CTLTYPE_INT ),
        ( Name: 'byteorder';  CtlType : CTLTYPE_INT ),
        ( Name: 'physmem';  CtlType : CTLTYPE_UINT ),
        ( Name: 'usermem';  CtlType : CTLTYPE_UINT ),
        ( Name: 'pagesize';  CtlType : CTLTYPE_INT ),
        ( Name: 'disknames';  CtlType : CTLTYPE_STRUCT ),
        ( Name: 'diskstats';  CtlType : CTLTYPE_STRUCT ),
        ( Name: 'floatingpoint';  CtlType : CTLTYPE_INT ));


        CTL_USER_NAMES  : array[0..20] of CTLNameRec = (
        ( Name :''; CtlType: 0 ),
        ( Name: 'cs_path';  CtlType : CTLTYPE_STRING ),
        ( Name: 'bc_base_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'bc_dim_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'bc_scale_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'bc_string_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'coll_weights_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'expr_nest_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'line_max';  CtlType : CTLTYPE_INT ),
        ( Name: 're_dup_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_version';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_c_bind';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_c_dev';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_char_term';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_fort_dev';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_fort_run';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_localedef';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_sw_dev';  CtlType : CTLTYPE_INT ),
        ( Name: 'posix2_upe';  CtlType : CTLTYPE_INT ),
        ( Name: 'stream_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'tzname_max';  CtlType : CTLTYPE_INT ));

        CTL_P1003_1B_NAMES  : array[0..25] of CTLNameRec = (
        ( Name: ''; CtlType: 0 ),
        ( Name: 'asynchronous_io';  CtlType : CTLTYPE_INT ),
        ( Name: 'mapped_files';  CtlType : CTLTYPE_INT ),
        ( Name: 'memlock';  CtlType : CTLTYPE_INT ),
        ( Name: 'memlock_range';  CtlType : CTLTYPE_INT ),
        ( Name: 'memory_protection';  CtlType : CTLTYPE_INT ),
        ( Name: 'message_passing';  CtlType : CTLTYPE_INT ),
        ( Name: 'prioritized_io';  CtlType : CTLTYPE_INT ),
        ( Name: 'priority_scheduling';  CtlType : CTLTYPE_INT ),
        ( Name: 'realtime_signals';  CtlType : CTLTYPE_INT ),
        ( Name: 'semaphores';  CtlType : CTLTYPE_INT ),
        ( Name: 'fsync';  CtlType : CTLTYPE_INT ),
        ( Name: 'shared_memory_objects';  CtlType : CTLTYPE_INT ),
        ( Name: 'synchronized_io';  CtlType : CTLTYPE_INT ),
        ( Name: 'timers';  CtlType : CTLTYPE_INT ),
        ( Name: 'aio_listio_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'aio_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'aio_prio_delta_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'delaytimer_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'mq_open_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'pagesize';  CtlType : CTLTYPE_INT ),
        ( Name: 'rtsig_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'nsems_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'sem_value_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'sigqueue_max';  CtlType : CTLTYPE_INT ),
        ( Name: 'timer_max';  CtlType : CTLTYPE_INT ));


const
{
 * Types
}
        SOCK_STREAM     = 1;            { stream socket }
        SOCK_DGRAM      = 2;            { datagram socket }
        SOCK_RAW        = 3;            { raw-protocol interface }
        SOCK_RDM        = 4;            { reliably-delivered message }
        SOCK_SEQPACKET  = 5;            { sequenced packet stream }

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
        AF_ECMA         = 8;            { European computer manufacturers }
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
        AF_SIP          = 24;           { Simple Internet Protocol }
        pseudo_AF_PIP   = 25;           { Help Identify PIP packets }
        AF_ISDN         = 26;           { Integrated Services Digital Network}
        AF_E164         = AF_ISDN;      { CCITT E.164 recommendation }
        pseudo_AF_KEY   = 27;           { Internal key-management function }
        AF_INET6        = 28;           { IPv6 }
        AF_NATM         = 29;           { native ATM access }
        AF_ATM          = 30;           { ATM }
        pseudo_AF_HDRCMPLT = 31;                { Used by BPF to not rewrite headers
                                         * in interface output routine
                                         }
        AF_NETGRAPH     = 32;           { Netgraph sockets }

        AF_MAX          = 33;

{
 * Protocol families, same as address families for now.
}

{
 * Definitions for network related sysctl, CTL_NET.
 *
 * Second level is protocol family.
 * Third level is protocol number.
 *
 * Further levels are defined by the individual families below.
}
        NET_MAXID       = AF_MAX;

        CTL_NET_NAMES  : Array[0..32] OF CtlNameRec =  (
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
        (Name: 'sip';CtlType: CTLTYPE_NODE ),
        (Name: 'pip';CtlType: CTLTYPE_NODE ),
        (Name: 'isdn';CtlType: CTLTYPE_NODE ),
        (Name: 'key';CtlType: CTLTYPE_NODE ),
        (Name: 'inet6';CtlType: CTLTYPE_NODE ),
        (Name: 'natm';CtlType: CTLTYPE_NODE ),
        (Name: 'atm';CtlType: CTLTYPE_NODE ),
        (Name: 'hdrcomplete';CtlType: CTLTYPE_NODE ),
        (Name: 'netgraph';CtlType: CTLTYPE_NODE ));

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

        CTL_NET_RT_NAMES  : Array[0..3] OF CtlNameRec =  (
        (Name: '';CtlType: 0 ),
        (Name: 'dump';CtlType: CTLTYPE_STRUCT ),
        (Name: 'flags';CtlType: CTLTYPE_STRUCT ),
        (Name: 'iflist';CtlType: CTLTYPE_STRUCT ));

{
 * Possible states of profiling.
}
        GMON_PROF_ON    = 0;
        GMON_PROF_BUSY  = 1;
        GMON_PROF_ERROR = 2;
        GMON_PROF_OFF   = 3;
        GMON_PROF_HIRES = 4;

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
        VM_METER                = 1;    { struct vmmeter }
        VM_LOADAVG              = 2;    { struct loadavg }
        VM_V_FREE_MIN           = 3;    { cnt.v_free_min }
        VM_V_FREE_TARGET        = 4;    { cnt.v_free_target }
        VM_V_FREE_RESERVED      = 5;    { cnt.v_free_reserved }
        VM_V_INACTIVE_TARGET    = 6;    { cnt.v_inactive_target }
        VM_V_CACHE_MIN          = 7;    { cnt.v_cache_max }
        VM_V_CACHE_MAX          = 8;    { cnt.v_cache_min }
        VM_V_PAGEOUT_FREE_MIN   = 9;    { cnt.v_pageout_free_min }
        VM_PAGEOUT_ALGORITHM    = 10;   { pageout algorithm }
        VM_SWAPPING_ENABLED     = 11;   { swapping enabled }
        VM_MAXID                = 12;   { number of valid vm ids }

        CTL_VM_NAMES  : Array[0..11] OF CtlNameRec =  (
        (Name: '';CtlType: 0 ),
        (Name: 'vmmeter';CtlType: CTLTYPE_STRUCT ),
        (Name: 'loadavg';CtlType: CTLTYPE_STRUCT ),
        (Name: 'v_free_min';CtlType: CTLTYPE_INT ),
        (Name: 'v_free_target';CtlType: CTLTYPE_INT ),
        (Name: 'v_free_reserved';CtlType: CTLTYPE_INT ),
        (Name: 'v_inactive_target';CtlType: CTLTYPE_INT ),
        (Name: 'v_cache_min';CtlType: CTLTYPE_INT ),
        (Name: 'v_cache_max';CtlType: CTLTYPE_INT ),
        (Name: 'v_pageout_free_min';CtlType: CTLTYPE_INT),
        (Name: 'pageout_algorithm';CtlType: CTLTYPE_INT),
        (Name: 'swapping_enabled';CtlType: CTLTYPE_INT));

{
 * Protocols (Name:RFC 1700)
}
        IPPROTO_IP              = 0;            { dummy for IP }
        IPPROTO_HOPOPTS         = 0;            { IP6 hop-by-hop options }
        IPPROTO_ICMP            = 1;            { control message protocol }
        IPPROTO_IGMP            = 2;            { group mgmt protocol }
        IPPROTO_GGP             = 3;            { gateway^2 (Name:deprecated) }
        IPPROTO_IPV4            = 4;            { IPv4 encapsulation }
        IPPROTO_IPIP            = IPPROTO_IPV4; { for compatibility }
        IPPROTO_TCP             = 6;            { tcp }
        IPPROTO_ST              = 7;            { Stream protocol II }
        IPPROTO_EGP             = 8;            { exterior gateway protocol }
        IPPROTO_PIGP            = 9;            { private interior gateway }
        IPPROTO_RCCMON          = 10;           { BBN RCC Monitoring }
        IPPROTO_NVPII           = 11;           { network voice protocol}
        IPPROTO_PUP             = 12;           { pup }
        IPPROTO_ARGUS           = 13;           { Argus }
        IPPROTO_EMCON           = 14;           { EMCON }
        IPPROTO_XNET            = 15;           { Cross Net Debugger }
        IPPROTO_CHAOS           = 16;           { Chaos}
        IPPROTO_UDP             = 17;           { user datagram protocol }
        IPPROTO_MUX             = 18;           { Multiplexing }
        IPPROTO_MEAS            = 19;           { DCN Measurement Subsystems }
        IPPROTO_HMP             = 20;           { Host Monitoring }
        IPPROTO_PRM             = 21;           { Packet Radio Measurement }
        IPPROTO_IDP             = 22;           { xns idp }
        IPPROTO_TRUNK1          = 23;           { Trunk-1 }
        IPPROTO_TRUNK2          = 24;           { Trunk-2 }
        IPPROTO_LEAF1           = 25;           { Leaf-1 }
        IPPROTO_LEAF2           = 26;           { Leaf-2 }
        IPPROTO_RDP             = 27;           { Reliable Data }
        IPPROTO_IRTP            = 28;           { Reliable Transaction }
        IPPROTO_TP              = 29;           { tp-4 w/ class negotiation }
        IPPROTO_BLT             = 30;           { Bulk Data Transfer }
        IPPROTO_NSP             = 31;           { Network Services }
        IPPROTO_INP             = 32;           { Merit Internodal }
        IPPROTO_SEP             = 33;           { Sequential Exchange }
        IPPROTO_3PC             = 34;           { Third Party Connect }
        IPPROTO_IDPR            = 35;           { InterDomain Policy Routing }
        IPPROTO_XTP             = 36;           { XTP }
        IPPROTO_DDP             = 37;           { Datagram Delivery }
        IPPROTO_CMTP            = 38;           { Control Message Transport }
        IPPROTO_TPXX            = 39;           { TP++ Transport }
        IPPROTO_IL              = 40;           { IL transport protocol }
        IPPROTO_IPV6            = 41;           { IP6 header }
        IPPROTO_SDRP            = 42;           { Source Demand Routing }
        IPPROTO_ROUTING         = 43;           { IP6 routing header }
        IPPROTO_FRAGMENT        = 44;           { IP6 fragmentation header }
        IPPROTO_IDRP            = 45;           { InterDomain Routing}
        IPPROTO_RSVP            = 46;           { resource reservation }
        IPPROTO_GRE             = 47;           { General Routing Encap. }
        IPPROTO_MHRP            = 48;           { Mobile Host Routing }
        IPPROTO_BHA             = 49;           { BHA }
        IPPROTO_ESP             = 50;           { IP6 Encap Sec. Payload }
        IPPROTO_AH              = 51;           { IP6 Auth Header }
        IPPROTO_INLSP           = 52;           { Integ. Net Layer Security }
        IPPROTO_SWIPE           = 53;           { IP with encryption }
        IPPROTO_NHRP            = 54;           { Next Hop Resolution }
{ 55-57: Unassigned }
        IPPROTO_ICMPV6          = 58;           { ICMP6 }
        IPPROTO_NONE            = 59;           { IP6 no next header }
        IPPROTO_DSTOPTS         = 60;           { IP6 destination option }
        IPPROTO_AHIP            = 61;           { any host internal protocol }
        IPPROTO_CFTP            = 62;           { CFTP }
        IPPROTO_HELLO           = 63;           { 'hello' routing protocol }
        IPPROTO_SATEXPAK        = 64;           { SATNET/Backroom EXPAK }
        IPPROTO_KRYPTOLAN       = 65;           { Kryptolan }
        IPPROTO_RVD             = 66;           { Remote Virtual Disk }
        IPPROTO_IPPC            = 67;           { Pluribus Packet Core }
        IPPROTO_ADFS            = 68;           { Any distributed FS }
        IPPROTO_SATMON          = 69;           { Satnet Monitoring }
        IPPROTO_VISA            = 70;           { VISA Protocol }
        IPPROTO_IPCV            = 71;           { Packet Core Utility }
        IPPROTO_CPNX            = 72;           { Comp. Prot. Net. Executive }
        IPPROTO_CPHB            = 73;           { Comp. Prot. HeartBeat }
        IPPROTO_WSN             = 74;           { Wang Span Network }
        IPPROTO_PVP             = 75;           { Packet Video Protocol }
        IPPROTO_BRSATMON        = 76;           { BackRoom SATNET Monitoring }
        IPPROTO_ND              = 77;           { Sun net disk proto (Name:temp.) }
        IPPROTO_WBMON           = 78;           { WIDEBAND Monitoring }
        IPPROTO_WBEXPAK         = 79;           { WIDEBAND EXPAK }
        IPPROTO_EON             = 80;           { ISO cnlp }
        IPPROTO_VMTP            = 81;           { VMTP }
        IPPROTO_SVMTP           = 82;           { Secure VMTP }
        IPPROTO_VINES           = 83;           { Banyon VINES }
        IPPROTO_TTP             = 84;           { TTP }
        IPPROTO_IGP             = 85;           { NSFNET-IGP }
        IPPROTO_DGP             = 86;           { dissimilar gateway prot. }
        IPPROTO_TCF             = 87;           { TCF }
        IPPROTO_IGRP            = 88;           { Cisco/GXS IGRP }
        IPPROTO_OSPFIGP         = 89;           { OSPFIGP }
        IPPROTO_SRPC            = 90;           { Strite RPC protocol }
        IPPROTO_LARP            = 91;           { Locus Address Resoloution }
        IPPROTO_MTP             = 92;           { Multicast Transport }
        IPPROTO_AX25            = 93;           { AX.25 Frames }
        IPPROTO_IPEIP           = 94;           { IP encapsulated in IP }
        IPPROTO_MICP            = 95;           { Mobile Int.ing control }
        IPPROTO_SCCSP           = 96;           { Semaphore Comm. security }
        IPPROTO_ETHERIP         = 97;           { Ethernet IP encapsulation }
        IPPROTO_ENCAP           = 98;           { encapsulation header }
        IPPROTO_APES            = 99;           { any private encr. scheme }
        IPPROTO_GMTP            = 100;          { GMTP}
        IPPROTO_IPCOMP          = 108;          { payload compression (Name:IPComp) }
{ 101-254: Partly Unassigned }
        IPPROTO_PIM             = 103;          { Protocol Independent Mcast }
        IPPROTO_PGM             = 113;          { PGM }
{ 255: Reserved }
{ BSD Private, local use, namespace incursion }
        IPPROTO_DIVERT          = 254;          { divert pseudo-protocol }
        IPPROTO_RAW             = 255;          { raw IP packet }
        IPPROTO_MAX             = 256;

{ last return value of *_input(Name:);CtlType: meaning 'all job for this pkt is done'.  }
        IPPROTO_DONE            = 257;


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
        IP_MULTICAST_IF         = 9;    { u_char; set/get IP multicast i/f  }
        IP_MULTICAST_TTL        = 10;   { u_char; set/get IP multicast ttl }
        IP_MULTICAST_LOOP       = 11;   { u_char; set/get IP multicast loopback }
        IP_ADD_MEMBERSHIP       = 12;   { ip_mreq; add an IP group membership }
        IP_DROP_MEMBERSHIP      = 13;   { ip_mreq; drop an IP group membership }
        IP_MULTICAST_VIF        = 14;   { set/get IP mcast virt. iface }
        IP_RSVP_ON              = 15;   { enable RSVP in kernel }
        IP_RSVP_OFF             = 16;   { disable RSVP in kernel }
        IP_RSVP_VIF_ON          = 17;   { set RSVP per-vif socket }
        IP_RSVP_VIF_OFF         = 18;   { unset RSVP per-vif socket }
        IP_PORTRANGE            = 19;   { int; range to choose for unspec port }
        IP_RECVIF               = 20;   { bool; receive reception if w/dgram }
{ for IPSEC }
        IP_IPSEC_POLICY         = 21;   { int; set/get security policy }
        IP_FAITH                = 22;   { bool; accept FAITH'ed connections }

        IP_FW_ADD               = 50;   { add a firewall rule to chain }
        IP_FW_DEL               = 51;   { delete a firewall rule from chain }
        IP_FW_FLUSH             = 52;   { flush firewall rule chain }
        IP_FW_ZERO              = 53;   { clear single/all firewall counter(Name:s) }
        IP_FW_GET               = 54;   { get entire firewall rule chain }
        IP_FW_RESETLOG          = 55;   { reset logging counters }

        IP_DUMMYNET_CONFIGURE   = 60;   { add/configure a dummynet pipe }
        IP_DUMMYNET_DEL         = 61;   { delete a dummynet pipe from chain }
        IP_DUMMYNET_FLUSH       = 62;   { flush dummynet }
        IP_DUMMYNET_GET         = 64;   { get entire dummynet pipes }

{
 * Defaults and limits for options
}
        IP_DEFAULT_MULTICAST_TTL  = 1;  { normally limit m'casts to 1 hop  }
        IP_DEFAULT_MULTICAST_LOOP = 1;  { normally hear sends if a member  }
        IP_MAX_MEMBERSHIPS        = 20; { per socket }

{
 * Argument for IP_PORTRANGE:
 * - which range to search when port is unspecified at bind(Name:) or connect()
}
        IP_PORTRANGE_DEFAULT    = 0;    { default range }
        IP_PORTRANGE_HIGH       = 1;    { 'high' - request firewall bypass }
        IP_PORTRANGE_LOW        = 2;    { 'low' - vouchsafe security }

{
 * Definitions for inet sysctl operations.
 *
 * Third level is protocol number.
 * Fourth level is desired variable within that protocol.
}
        IPPROTO_MAXID = (IPPROTO_AH + 1);       { don't list to IPPROTO_MAX }

        CTL_IPPROTO_NAMES  : Array[0..51] OF CtlNameRec =  (
        (Name: 'ip';CtlType: CTLTYPE_NODE ),
        (Name: 'icmp';CtlType: CTLTYPE_NODE ),
        (Name: 'igmp';CtlType: CTLTYPE_NODE ),
        (Name: 'ggp';CtlType: CTLTYPE_NODE ),
        (Name: '';CtlType: 0 ),
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
        (Name: 'idp';CtlType: CTLTYPE_NODE ),
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
        (Name: 'ipsec';CtlType: CTLTYPE_NODE ));

{
 * Names for IP sysctl objects
}
        IPCTL_FORWARDING        = 1;    { act as router }
        IPCTL_SENDREDIRECTS     = 2;    { may send redirects when forwarding }
        IPCTL_DEFTTL            = 3;    { default TTL }
{$ifdef notyettunable}
        IPCTL_DEFMTU            = 4;    { default MTU }
{$endif}
        IPCTL_RTEXPIRE          = 5;    { cloned route expiration time }
        IPCTL_RTMINEXPIRE       = 6;    { min value for expiration time }
        IPCTL_RTMAXCACHE        = 7;    { trigger level for dynamic expire }
        IPCTL_SOURCEROUTE       = 8;    { may perform source routes }
        IPCTL_DIRECTEDBROADCAST = 9;    { may re-broadcast received packets }
        IPCTL_INTRQMAXLEN       = 10;   { max length of netisr queue }
        IPCTL_INTRQDROPS        = 11;   { number of netisr q drops }
        IPCTL_STATS             = 12;   { ipstat structure }
        IPCTL_ACCEPTSOURCEROUTE = 13;   { may accept source routed packets }
        IPCTL_FASTFORWARDING    = 14;   { use fast IP forwarding code }
        IPCTL_KEEPFAITH         = 15;   { FAITH IPv4->IPv6 translater ctl }
        IPCTL_GIF_TTL           = 16;   { default TTL for gif encap packet }
        IPCTL_MAXID             = 17;

        IPCTL_NAMES  : Array[0..14] OF CtlNameRec =  (
        (Name: '';CtlType: 0 ),
        (Name: 'forwarding';CtlType: CTLTYPE_INT ),
        (Name: 'redirect';CtlType: CTLTYPE_INT ),
        (Name: 'ttl';CtlType: CTLTYPE_INT ),
        (Name: 'mtu';CtlType: CTLTYPE_INT ),
        (Name: 'rtexpire';CtlType: CTLTYPE_INT ),
        (Name: 'rtminexpire';CtlType: CTLTYPE_INT ),
        (Name: 'rtmaxcache';CtlType: CTLTYPE_INT ),
        (Name: 'sourceroute';CtlType: CTLTYPE_INT ),
        (Name: 'directed-broadcast';CtlType: CTLTYPE_INT ),
        (Name: 'intr-queue-maxlen';CtlType: CTLTYPE_INT ),
        (Name: 'intr-queue-drops';CtlType: CTLTYPE_INT ),
        (Name: 'stats';CtlType: CTLTYPE_STRUCT ),
        (Name: 'accept_sourceroute';CtlType: CTLTYPE_INT ),
        (Name: 'fastforwarding';CtlType: CTLTYPE_INT ));

{
 * Names for ICMP sysctl objects
}
        ICMPCTL_MASKREPL        = 1;    { allow replies to netmask requests }
        ICMPCTL_STATS           = 2;    { statistics (Name:read-only) }
        ICMPCTL_ICMPLIM         = 3;
        ICMPCTL_MAXID           = 4;

        ICMPCTL_NAMES  : Array[0..3] OF CtlNameRec =  (
        (Name: '';CtlType: 0 ),
        (Name: 'maskrepl';CtlType: CTLTYPE_INT ),
        (Name: 'stats';CtlType: CTLTYPE_STRUCT ),
        (Name: 'icmplim';CtlType: CTLTYPE_INT ));

{
 * Names for ICMP sysctl objects
}
        ICMPV6CTL_STATS         = 1;
        ICMPV6CTL_REDIRACCEPT   = 2;    { accept/process redirects }
        ICMPV6CTL_REDIRTIMEOUT  = 3;    { redirect cache time }
{$ifdef obsolete}       {obsoleted}
        ICMPV6CTL_ERRRATELIMIT  = 5;    { ICMPv6 error rate limitation }
{$endif}
        ICMPV6CTL_ND6_PRUNE     = 6;
        ICMPV6CTL_ND6_DELAY     = 8;
        ICMPV6CTL_ND6_UMAXTRIES = 9;
        ICMPV6CTL_ND6_MMAXTRIES         = 10;
        ICMPV6CTL_ND6_USELOOPBACK       = 11;
//define ICMPV6CTL_ND6_PROXYALL = 12;   obsoleted, do not reuse here
        ICMPV6CTL_NODEINFO      = 13;
        ICMPV6CTL_ERRPPSLIMIT   = 14;   { ICMPv6 error pps limitation }
        ICMPV6CTL_ND6_MAXNUDHINT= 15;
        ICMPV6CTL_MTUDISC_HIWAT = 16;
        ICMPV6CTL_MTUDISC_LOWAT = 17;
        ICMPV6CTL_ND6_DEBUG     = 18;
        ICMPV6CTL_ND6_DRLIST    = 19;
        ICMPV6CTL_ND6_PRLIST    = 20;
        ICMPV6CTL_MAXID         = 21;

        ICMPV6CTL_NAMES  : Array[0..20] OF CtlNameRec =  (
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
 * Names for UDP sysctl objects
}
        UDPCTL_CHECKSUM         = 1;    { checksum UDP packets }
        UDPCTL_STATS            = 2;    { statistics (Name:read-only) }
        UDPCTL_MAXDGRAM         = 3;    { max datagram size }
        UDPCTL_RECVSPACE        = 4;    { default receive buffer space }
        UDPCTL_PCBLIST          = 5;    { list of PCBs for UDP sockets }
        UDPCTL_MAXID            = 6;

        UDPCTL_NAMES  : Array[0..5] OF CtlNameRec =  (
        (Name: '';CtlType: 0 ),
        (Name: 'checksum';CtlType: CTLTYPE_INT ),
        (Name: 'stats';CtlType: CTLTYPE_STRUCT ),
        (Name: 'maxdgram';CtlType: CTLTYPE_INT ),
        (Name: 'recvspace';CtlType: CTLTYPE_INT ),
        (Name: 'pcblist';CtlType: CTLTYPE_STRUCT ));

{


  Revision 1.3  2005/02/14 17:13:22  peter
    * truncate log

}
