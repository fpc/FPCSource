{
    NetBSD sysctl constants, from sys/sysctl.h, uvm/uvm_param.h, sys/gmon.h,
    sys/socket.h, netinet/in.h, netinet/icmp_var.h, netinet/icmp6.h and netinet/udp_var.h.
    See the file COPYING.FPC, included in this distribution, for details about the copyright.
}

Const
        CTL_MAXNAME     = 12;   { largest number of components supported }
        SYSCTL_NAMELEN  = 32;   { longest name allowed for a node }

Type
    ctlname = record
                ctl_name : PAnsiChar;   { subsystem name }
                ctl_type : cint         { type of name }
              End;

    { Node description returned by CTL_QUERY; pointer and size fields are padded to 64 bits }
    sysctlnode = record
                sysctl_flags  : cuint32;        { flags and type }
                sysctl_num    : cint32;         { mib number }
                sysctl_name   : array[0..SYSCTL_NAMELEN-1] of AnsiChar; { node name }
                sysctl_ver    : cuint32;        { node's version vs. rest of tree }
                __rsvd        : cuint32;
                sysctl_un     : array[0..1] of cuint64; { child array, data pointer or immediate value }
                _sysctl_size  : cuint64;        { size of instrumented data }
                _sysctl_func  : cuint64;        { access helper function }
                _sysctl_parent: cuint64;        { parent of this node }
                _sysctl_desc  : cuint64;        { description of node }
              End;
    psysctlnode = ^sysctlnode;

Const
        CTLTYPE_NODE    = 1;    { name is a node }
        CTLTYPE_INT     = 2;    { name describes an integer }
        CTLTYPE_STRING  = 3;    { name describes a string }
        CTLTYPE_QUAD    = 4;    { name describes a 64-bit number }
        CTLTYPE_STRUCT  = 5;    { name describes a structure }
        CTLTYPE_BOOL    = 6;    { name describes a bool }
{$ifdef cpu64}
        CTLTYPE_LONG    = CTLTYPE_QUAD;
{$else}
        CTLTYPE_LONG    = CTLTYPE_INT;
{$endif}

{ Flags that apply to each node }
        CTLFLAG_READONLY  = $00000000;
        CTLFLAG_READWRITE = $00000070;
        CTLFLAG_ANYWRITE  = $00000080;
        CTLFLAG_PRIVATE   = $00000100;
        CTLFLAG_PERMANENT = $00000200;
        CTLFLAG_OWNDATA   = $00000400;
        CTLFLAG_IMMEDIATE = $00000800;
        CTLFLAG_HEX       = $00001000;
        CTLFLAG_ROOT      = $00002000;
        CTLFLAG_ANYNUMBER = $00004000;
        CTLFLAG_HIDDEN    = $00008000;
        CTLFLAG_ALIAS     = $00010000;
        CTLFLAG_MMAP      = $00020000;
        CTLFLAG_OWNDESC   = $00040000;
        CTLFLAG_UNSIGNED  = $00080000;

{ sysctl API version }
        SYSCTL_VERS_MASK  = $ff000000;
        SYSCTL_VERS_0     = $00000000;
        SYSCTL_VERS_1     = $01000000;
        SYSCTL_VERSION    = SYSCTL_VERS_1;

{ Meta-identifiers }
        CTL_EOL         = -1;   { end of createv/destroyv list }
        CTL_QUERY       = -2;   { enumerates children of a node }
        CTL_CREATE      = -3;   { node create request }
        CTL_CREATESYM   = -4;   { node create request with symbol }
        CTL_DESTROY     = -5;   { node destroy request }
        CTL_MMAP        = -6;   { mmap request }
        CTL_DESCRIBE    = -7;   { get node descriptions }

{ Top-level identifiers }
        CTL_UNSPEC      = 0;    { unused }
        CTL_KERN        = 1;    { "high kernel": proc, limits }
        CTL_VM          = 2;    { virtual memory }
        CTL_VFS         = 3;    { file system, mount type is next }
        CTL_NET         = 4;    { network, see socket.h }
        CTL_DEBUG       = 5;    { debugging parameters }
        CTL_HW          = 6;    { generic CPU/io }
        CTL_MACHDEP     = 7;    { machine dependent }
        CTL_USER        = 8;    { user-level }
        CTL_DDB         = 9;    { in-kernel debugger }
        CTL_PROC        = 10;   { per-proc attr }
        CTL_VENDOR      = 11;   { vendor-specific data }
        CTL_EMUL        = 12;   { emulation-specific data }
        CTL_SECURITY    = 13;   { security }

{ CTL_KERN identifiers }
        KERN_OSTYPE             =  1;   { string: system version }
        KERN_OSRELEASE          =  2;   { string: system release }
        KERN_OSREV              =  3;   { int: system revision }
        KERN_VERSION            =  4;   { string: compile time info }
        KERN_MAXVNODES          =  5;   { int: max vnodes }
        KERN_MAXPROC            =  6;   { int: max processes }
        KERN_MAXFILES           =  7;   { int: max open files }
        KERN_ARGMAX             =  8;   { int: max arguments to exec }
        KERN_SECURELVL          =  9;   { int: system security level }
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
        KERN_OBOOTTIME          = 21;   { struct: time kernel was booted }
        KERN_DOMAINNAME         = 22;   { string: (YP) domainname }
        KERN_MAXPARTITIONS      = 23;   { int: number of partitions/disk }
        KERN_RAWPARTITION       = 24;   { int: raw partition number }
        KERN_NTPTIME            = 25;   { struct: extended-precision time }
        KERN_TIMEX              = 26;   { struct: ntp timekeeping state }
        KERN_AUTONICETIME       = 27;   { int: proc time before autonice }
        KERN_AUTONICEVAL        = 28;   { int: auto nice value }
        KERN_RTC_OFFSET         = 29;   { int: offset of rtc from gmt }
        KERN_ROOT_DEVICE        = 30;   { string: root device }
        KERN_MSGBUFSIZE         = 31;   { int: max # of chars in msg buffer }
        KERN_FSYNC              = 32;   { int: file synchronization support }
        KERN_OLDSYSVMSG         = 33;   { old: SysV message queue support }
        KERN_OLDSYSVSEM         = 34;   { old: SysV semaphore support }
        KERN_OLDSYSVSHM         = 35;   { old: SysV shared memory support }
        KERN_OLDSHORTCORENAME   = 36;   { old, unimplemented }
        KERN_SYNCHRONIZED_IO    = 37;   { int: POSIX synchronized I/O }
        KERN_IOV_MAX            = 38;   { int: max iovec's for readv(2) etc. }
        KERN_MBUF               = 39;   { node: mbuf parameters }
        KERN_MAPPED_FILES       = 40;   { int: POSIX memory mapped files }
        KERN_MEMLOCK            = 41;   { int: POSIX memory locking }
        KERN_MEMLOCK_RANGE      = 42;   { int: POSIX memory range locking }
        KERN_MEMORY_PROTECTION  = 43;   { int: POSIX memory protections }
        KERN_LOGIN_NAME_MAX     = 44;   { int: max length login name + NUL }
        KERN_DEFCORENAME        = 45;   { old: sort core name format }
        KERN_LOGSIGEXIT         = 46;   { int: log signaled processes }
        KERN_PROC2              = 47;   { struct: process entries }
        KERN_PROC_ARGS          = 48;   { struct: process argv/env }
        KERN_FSCALE             = 49;   { int: fixpt FSCALE }
        KERN_CCPU               = 50;   { old: fixpt ccpu }
        KERN_CP_TIME            = 51;   { struct: CPU time counters }
        KERN_OLDSYSVIPC_INFO    = 52;   { old: number of valid kern ids }
        KERN_MSGBUF             = 53;   { kernel message buffer }
        KERN_CONSDEV            = 54;   { dev_t: console terminal device }
        KERN_MAXPTYS            = 55;   { int: maximum number of ptys }
        KERN_PIPE               = 56;   { node: pipe limits }
        KERN_MAXPHYS            = 57;   { int: kernel value of MAXPHYS }
        KERN_SBMAX              = 58;   { int: max socket buffer size }
        KERN_TKSTAT             = 59;   { tty in/out counters }
        KERN_MONOTONIC_CLOCK    = 60;   { int: POSIX monotonic clock }
        KERN_URND               = 61;   { int: random integer from urandom }
        KERN_LABELSECTOR        = 62;   { int: disklabel sector }
        KERN_LABELOFFSET        = 63;   { int: offset of label within sector }
        KERN_LWP                = 64;   { struct: lwp entries }
        KERN_FORKFSLEEP         = 65;   { int: sleep length on failed fork }
        KERN_POSIX_THREADS      = 66;   { int: POSIX Threads option }
        KERN_POSIX_SEMAPHORES   = 67;   { int: POSIX Semaphores option }
        KERN_POSIX_BARRIERS     = 68;   { int: POSIX Barriers option }
        KERN_POSIX_TIMERS       = 69;   { int: POSIX Timers option }
        KERN_POSIX_SPIN_LOCKS   = 70;   { int: POSIX Spin Locks option }
        KERN_POSIX_READER_WRITER_LOCKS = 71; { int: POSIX R/W Locks option }
        KERN_DUMP_ON_PANIC      = 72;   { int: dump on panic }
        KERN_SOMAXKVA           = 73;   { int: max socket kernel virtual mem }
        KERN_ROOT_PARTITION     = 74;   { int: root partition }
        KERN_DRIVERS            = 75;   { struct: driver names and majors #s }
        KERN_BUF                = 76;   { struct: buffers }
        KERN_FILE2              = 77;   { struct: file entries }
        KERN_VERIEXEC           = 78;   { node: verified exec }
        KERN_CP_ID              = 79;   { struct: cpu id numbers }
        KERN_HARDCLOCK_TICKS    = 80;   { int: number of hardclock ticks }
        KERN_ARND               = 81;   { void *buf, size_t siz random }
        KERN_SYSVIPC            = 82;   { node: SysV IPC parameters }
        KERN_BOOTTIME           = 83;   { struct: time kernel was booted }
        KERN_EVCNT              = 84;   { struct: evcnts }
        KERN_SOFIXEDBUF         = 85;   { bool: fixed socket buffer sizes }
        KERN_ENTROPY            = 86;   { node: entropy(9) subsystem }

{ KERN_PROC subtypes }
        KERN_PROC_ALL           = 0;    { everything }
        KERN_PROC_PID           = 1;    { by process id }
        KERN_PROC_PGRP          = 2;    { by process group id }
        KERN_PROC_SESSION       = 3;    { by session of pid }
        KERN_PROC_TTY           = 4;    { by controlling tty }
        KERN_PROC_UID           = 5;    { by effective uid }
        KERN_PROC_RUID          = 6;    { by real uid }
        KERN_PROC_GID           = 7;    { by effective gid }
        KERN_PROC_RGID          = 8;    { by real gid }

{ KERN_PROC_ARGS subtypes }
        KERN_PROC_ARGV          = 1;    { argv }
        KERN_PROC_NARGV         = 2;    { number of strings in above }
        KERN_PROC_ENV           = 3;    { environ }
        KERN_PROC_NENV          = 4;    { number of strings in above }
        KERN_PROC_PATHNAME      = 5;    { path to executable }
        KERN_PROC_CWD           = 6;    { current working dir }

{ KERN_SYSVIPC subtypes }
        KERN_SYSVIPC_INFO       = 1;    { struct: number of valid kern ids }
        KERN_SYSVIPC_MSG        = 2;    { int: SysV message queue support }
        KERN_SYSVIPC_SEM        = 3;    { int: SysV semaphore support }
        KERN_SYSVIPC_SHM        = 4;    { int: SysV shared memory support }
        KERN_SYSVIPC_SHMMAX     = 5;    { int: max shared memory segment size (bytes) }
        KERN_SYSVIPC_SHMMNI     = 6;    { int: max number of shared memory identifiers }
        KERN_SYSVIPC_SHMSEG     = 7;    { int: max shared memory segments per process }
        KERN_SYSVIPC_SHMMAXPGS  = 8;    { int: max amount of shared memory (pages) }
        KERN_SYSVIPC_SHMUSEPHYS = 9;    { int: physical memory usage }

{ KERN_SYSVIPC_INFO subtypes }
        KERN_SYSVIPC_MSG_INFO   = 4;    { msginfo and msgid_ds }
        KERN_SYSVIPC_SEM_INFO   = 5;    { seminfo and semid_ds }
        KERN_SYSVIPC_SHM_INFO   = 6;    { shminfo and shmid_ds }

{ KERN_TKSTAT subtypes }
        KERN_TKSTAT_NIN         = 1;    { total input character }
        KERN_TKSTAT_NOUT        = 2;    { total output character }
        KERN_TKSTAT_CANCC       = 3;    { canonical input character }
        KERN_TKSTAT_RAWCC       = 4;    { raw input character }

        KERN_BUF_ALL            = 0;    { all buffers }
        KERN_BUFSLOP            = 20;

        KERN_FILE_BYFILE        = 1;
        KERN_FILE_BYPID         = 2;
        KERN_FILESLOP           = 10;

        KERN_EVCNT_COUNT_ANY     = 0;
        KERN_EVCNT_COUNT_NONZERO = 1;

        KERN_ENTROPY_EPOCH      = 1;    { int: PRNG reseed epoch }

{ CTL_HW identifiers }
        HW_MACHINE      =  1;   { string: machine class }
        HW_MODEL        =  2;   { string: specific machine model }
        HW_NCPU         =  3;   { int: number of cpus }
        HW_BYTEORDER    =  4;   { int: machine byte order }
        HW_PHYSMEM      =  5;   { int: total memory (bytes) }
        HW_USERMEM      =  6;   { int: non-kernel memory (bytes) }
        HW_PAGESIZE     =  7;   { int: software page size }
        HW_DISKNAMES    =  8;   { string: disk drive names }
        HW_IOSTATS      =  9;   { struct: iostats[] }
        HW_MACHINE_ARCH = 10;   { string: machine architecture }
        HW_ALIGNBYTES   = 11;   { int: ALIGNBYTES for the kernel }
        HW_CNMAGIC      = 12;   { string: console magic sequence(s) }
        HW_PHYSMEM64    = 13;   { quad: total memory (bytes) }
        HW_USERMEM64    = 14;   { quad: non-kernel memory (bytes) }
        HW_IOSTATNAMES  = 15;   { string: iostat names }
        HW_NCPUONLINE   = 16;   { number CPUs online }

{ CTL_USER definitions }
        USER_CS_PATH            =  1;   { string: _CS_PATH }
        USER_BC_BASE_MAX        =  2;   { int: BC_BASE_MAX }
        USER_BC_DIM_MAX         =  3;   { int: BC_DIM_MAX }
        USER_BC_SCALE_MAX       =  4;   { int: BC_SCALE_MAX }
        USER_BC_STRING_MAX      =  5;   { int: BC_STRING_MAX }
        USER_COLL_WEIGHTS_MAX   =  6;   { int: COLL_WEIGHTS_MAX }
        USER_EXPR_NEST_MAX      =  7;   { int: EXPR_NEST_MAX }
        USER_LINE_MAX           =  8;   { int: LINE_MAX }
        USER_RE_DUP_MAX         =  9;   { int: RE_DUP_MAX }
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
        USER_TZNAME_MAX         = 20;   { int: _POSIX_TZNAME_MAX }
        USER_ATEXIT_MAX         = 21;   { int: ATEXIT_MAX }
        USER_OSTYPE             = 22;   { str: "NetBSD" }
        USER_OSREVISION         = 23;   { int: __NetBSD_Version__ }
        USER_OSRELEASE          = 24;   { str: release string }
        USER_BUILDTIME          = 25;   { str: YYYY-MM-DD hh:mm:ss UTC }

{ CTL_DEBUG definitions }
        CTL_DEBUG_NAME          = 0;    { string: variable name }
        CTL_DEBUG_VALUE         = 1;    { int: variable value }

{ CTL_PROC subtypes }
        PROC_PID_CORENAME       = 1;
        PROC_PID_LIMIT          = 2;
        PROC_PID_STOPFORK       = 3;
        PROC_PID_STOPEXEC       = 4;
        PROC_PID_STOPEXIT       = 5;
        PROC_PID_PAXFLAGS       = 6;

{ PROC_PID_LIMIT subtypes: RLIMIT_* + 1 }
        PROC_PID_LIMIT_CPU      = 1;
        PROC_PID_LIMIT_FSIZE    = 2;
        PROC_PID_LIMIT_DATA     = 3;
        PROC_PID_LIMIT_STACK    = 4;
        PROC_PID_LIMIT_CORE     = 5;
        PROC_PID_LIMIT_RSS      = 6;
        PROC_PID_LIMIT_MEMLOCK  = 7;
        PROC_PID_LIMIT_NPROC    = 8;
        PROC_PID_LIMIT_NOFILE   = 9;
        PROC_PID_LIMIT_SBSIZE   = 10;
        PROC_PID_LIMIT_AS       = 11;
        PROC_PID_LIMIT_NTHR     = 12;

        PROC_PID_LIMIT_TYPE_SOFT = 1;
        PROC_PID_LIMIT_TYPE_HARD = 2;

        CTL_PROC_PAXFLAGS_ASLR     = $01;
        CTL_PROC_PAXFLAGS_MPROTECT = $02;
        CTL_PROC_PAXFLAGS_GUARD    = $04;

{ CTL_VM identifiers }
        VM_METER                = 1;    { struct vmmeter }
        VM_LOADAVG              = 2;    { struct loadavg }
        VM_UVMEXP               = 3;    { struct uvmexp }
        VM_NKMEMPAGES           = 4;    { kmem_map pages }
        VM_UVMEXP2              = 5;    { struct uvmexp_sysctl }
        VM_ANONMIN              = 6;
        VM_EXECMIN              = 7;
        VM_FILEMIN              = 8;
        VM_MAXSLP               = 9;
        VM_USPACE               = 10;
        VM_ANONMAX              = 11;
        VM_EXECMAX              = 12;
        VM_FILEMAX              = 13;
        VM_MINADDRESS           = 14;
        VM_MAXADDRESS           = 15;
        VM_PROC                 = 16;   { process information }
        VM_GUARD_SIZE           = 17;   { guard size for main thread }
        VM_THREAD_GUARD_SIZE    = 18;   { default guard size for new threads }

        VM_PROC_MAP             = 1;    { struct kinfo_vmentry }

{ Profiling states and KERN_PROF identifiers }
        GMON_PROF_ON    = 0;
        GMON_PROF_BUSY  = 1;
        GMON_PROF_ERROR = 2;
        GMON_PROF_OFF   = 3;

        GPROF_STATE     = 0;    { int: profiling enabling variable }
        GPROF_COUNT     = 1;    { struct: profile tick count buffer }
        GPROF_FROMS     = 2;    { struct: from location hash bucket }
        GPROF_TOS       = 3;    { struct: destination/count structure }
        GPROF_GMONPARAM = 4;    { struct: profiling parameters }
        GPROF_PERCPU    = 5;    { per cpu node }

{ CTL_NET second level: address families }
        AF_UNSPEC       = 0;            { unspecified }
        AF_LOCAL        = 1;            { local to host }
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
        AF_OROUTE       = 17;           { Internal Routing Protocol, old message format }
        AF_LINK         = 18;           { Link layer interface }
        pseudo_AF_XTP   = 19;           { eXpress Transfer Protocol (no AF) }
        AF_COIP         = 20;           { connection-oriented IP, aka ST II }
        AF_CNT          = 21;           { Computer Network Technology }
        pseudo_AF_RTIP  = 22;           { Help Identify RTIP packets }
        AF_IPX          = 23;           { Novell Internet Protocol }
        AF_INET6        = 24;           { IP version 6 }
        pseudo_AF_PIP   = 25;           { Help Identify PIP packets }
        AF_ISDN         = 26;           { Integrated Services Digital Network}
        AF_E164         = AF_ISDN;      { CCITT E.164 recommendation }
        AF_NATM         = 27;           { native ATM access }
        AF_ARP          = 28;           { (rev.) addr. res. prot. (RFC 826) }
        pseudo_AF_KEY   = 29;           { Internal key management protocol }
        pseudo_AF_HDRCMPLT = 30;        { Used by BPF to not rewrite hdrs in interface output routine }
        AF_BLUETOOTH    = 31;           { Bluetooth: HCI, SCO, L2CAP, RFCOMM }
        AF_IEEE80211    = 32;           { IEEE80211 }
        AF_MPLS         = 33;           { MultiProtocol Label Switching }
        AF_ROUTE        = 34;           { Internal Routing Protocol }
        AF_CAN          = 35;
        AF_ETHER        = 36;
        AF_MAX          = 37;

{ PF_ROUTE identifiers }
        NET_RT_DUMP      = 1;   { dump; may limit to a.f. }
        NET_RT_FLAGS     = 2;   { by flags, e.g. RESOLVING }
        NET_RT_OOOIFLIST = 3;   { old NET_RT_IFLIST (pre 1.5) }
        NET_RT_OOIFLIST  = 4;   { old NET_RT_IFLIST (pre-64bit time) }
        NET_RT_OIFLIST   = 5;   { old NET_RT_IFLIST (pre 8.0) }
        NET_RT_IFLIST    = 6;   { survey interface list }

{ PF_INET third level: protocols }
        IPPROTO_IP              = 0;    { dummy for IP }
        IPPROTO_HOPOPTS         = 0;    { IP6 hop-by-hop options }
        IPPROTO_ICMP            = 1;    { control message protocol }
        IPPROTO_IGMP            = 2;    { group mgmt protocol }
        IPPROTO_GGP             = 3;    { gateway^2 (deprecated) }
        IPPROTO_IPV4            = 4;    { IP header }
        IPPROTO_IPIP            = 4;    { IP inside IP }
        IPPROTO_TCP             = 6;    { tcp }
        IPPROTO_EGP             = 8;    { exterior gateway protocol }
        IPPROTO_PUP             = 12;   { pup }
        IPPROTO_UDP             = 17;   { user datagram protocol }
        IPPROTO_IDP             = 22;   { xns idp }
        IPPROTO_TP              = 29;   { tp-4 w/ class negotiation }
        IPPROTO_DCCP            = 33;   { DCCP }
        IPPROTO_IPV6            = 41;   { IP6 header }
        IPPROTO_ROUTING         = 43;   { IP6 routing header }
        IPPROTO_FRAGMENT        = 44;   { IP6 fragmentation header }
        IPPROTO_RSVP            = 46;   { resource reservation }
        IPPROTO_GRE             = 47;   { GRE encaps RFC 1701 }
        IPPROTO_ESP             = 50;   { encap. security payload }
        IPPROTO_AH              = 51;   { authentication header }
        IPPROTO_MOBILE          = 55;   { IP Mobility RFC 2004 }
        IPPROTO_IPV6_ICMP       = 58;   { IPv6 ICMP }
        IPPROTO_ICMPV6          = 58;   { ICMP6 }
        IPPROTO_NONE            = 59;   { IP6 no next header }
        IPPROTO_DSTOPTS         = 60;   { IP6 destination option }
        IPPROTO_EON             = 80;   { ISO cnlp }
        IPPROTO_ETHERIP         = 97;   { Ethernet-in-IP }
        IPPROTO_ENCAP           = 98;   { encapsulation header }
        IPPROTO_PIM             = 103;  { Protocol indep. multicast }
        IPPROTO_IPCOMP          = 108;  { IP Payload Comp. Protocol }
        IPPROTO_VRRP            = 112;  { VRRP RFC 2338 }
        IPPROTO_CARP            = 112;  { Common Address Resolution Protocol }
        IPPROTO_L2TP            = 115;  { L2TPv3 }
        IPPROTO_SCTP            = 132;  { SCTP }
        IPPROTO_PFSYNC          = 240;  { PFSYNC }
        IPPROTO_RAW             = 255;  { raw IP packet }
        IPPROTO_MAX             = 256;
        IPPROTO_DONE            = 257;  { last return value of *_input() }

{ IPPROTO_IP identifiers }
        IPCTL_FORWARDING        = 1;    { act as router }
        IPCTL_SENDREDIRECTS     = 2;    { may send redirects when forwarding }
        IPCTL_DEFTTL            = 3;    { default TTL }
        IPCTL_FORWSRCRT         = 5;    { forward source-routed packets }
        IPCTL_DIRECTEDBCAST     = 6;    { default broadcast behavior }
        IPCTL_ALLOWSRCRT        = 7;    { allow/drop all source-routed pkts }
        IPCTL_SUBNETSARELOCAL   = 8;    { treat subnets as local addresses }
        IPCTL_MTUDISC           = 9;    { allow path MTU discovery }
        IPCTL_ANONPORTMIN       = 10;   { minimum ephemeral port }
        IPCTL_ANONPORTMAX       = 11;   { maximum ephemeral port }
        IPCTL_MTUDISCTIMEOUT    = 12;   { allow path MTU discovery }
        IPCTL_MAXFLOWS          = 13;   { maximum ip flows allowed }
        IPCTL_HOSTZEROBROADCAST = 14;   { is host zero a broadcast addr? }
        IPCTL_GIF_TTL           = 15;   { default TTL for gif encap packet }
        IPCTL_LOWPORTMIN        = 16;   { minimum reserved port }
        IPCTL_LOWPORTMAX        = 17;   { maximum reserved port }
        IPCTL_MAXFRAGPACKETS    = 18;   { max packets reassembly queue }
        IPCTL_GRE_TTL           = 19;   { default TTL for gre encap packet }
        IPCTL_CHECKINTERFACE    = 20;   { drop pkts in from 'wrong' iface }
        IPCTL_IFQ               = 21;   { IP packet input queue }
        IPCTL_RANDOMID          = 22;   { use random IP ids (if configured) }
        IPCTL_LOOPBACKCKSUM     = 23;   { do IP checksum on loopback }
        IPCTL_STATS             = 24;   { IP statistics }
        IPCTL_DAD_COUNT         = 25;   { DAD packets to send }

{ IPPROTO_ICMP identifiers }
        ICMPCTL_MASKREPL        = 1;    { allow replies to netmask requests }
        ICMPCTL_ERRRATELIMIT    = 2;    { error rate limit }
        ICMPCTL_RETURNDATABYTES = 3;    { # of bytes to include in errors }
        ICMPCTL_ERRPPSLIMIT     = 4;    { ICMP error pps limitation }
        ICMPCTL_REDIRACCEPT     = 5;    { Accept redirects from routers }
        ICMPCTL_REDIRTIMEOUT    = 6;    { Remove routes added via redirects }
        ICMPCTL_STATS           = 7;    { ICMP statistics }
        ICMPCTL_BMCASTECHO      = 8;    { allow broad/mult-cast echo }
        ICMPCTL_DYNAMIC_RT_MSG  = 9;    { send routing message for RTM_DYNAMIC }

{ IPPROTO_ICMPV6 identifiers }
        ICMPV6CTL_STATS           = 1;
        ICMPV6CTL_REDIRACCEPT     = 2;  { accept/process redirects }
        ICMPV6CTL_REDIRTIMEOUT    = 3;  { redirect cache time }
        ICMPV6CTL_ERRRATELIMIT    = 5;  { ICMPv6 error rate limitation }
        ICMPV6CTL_ND6_PRUNE       = 6;
        ICMPV6CTL_ND6_DELAY       = 8;
        ICMPV6CTL_ND6_UMAXTRIES   = 9;
        ICMPV6CTL_ND6_MMAXTRIES   = 10;
        ICMPV6CTL_ND6_USELOOPBACK = 11;
        ICMPV6CTL_NODEINFO        = 13;
        ICMPV6CTL_ERRPPSLIMIT     = 14; { ICMPv6 error pps limitation }
        ICMPV6CTL_ND6_MAXNUDHINT  = 15;
        ICMPV6CTL_MTUDISC_HIWAT   = 16;
        ICMPV6CTL_MTUDISC_LOWAT   = 17;
        ICMPV6CTL_ND6_DEBUG       = 18;
        ICMPV6CTL_ND6_MAXQLEN     = 24;
        ICMPV6CTL_REFLECT_PMTU    = 25;
        ICMPV6CTL_DYNAMIC_RT_MSG  = 26;
        ICMPV6CTL_ND6_GCTIMER     = 27;

{ IPPROTO_UDP identifiers }
        UDPCTL_CHECKSUM         = 1;    { checksum UDP packets }
        UDPCTL_SENDSPACE        = 2;    { default send buffer }
        UDPCTL_RECVSPACE        = 3;    { default recv buffer }
        UDPCTL_LOOPBACKCKSUM    = 4;    { do UDP checksum on loopback }
        UDPCTL_STATS            = 5;    { UDP statistics }
