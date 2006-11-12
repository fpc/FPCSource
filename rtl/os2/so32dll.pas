{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000, 2001 by madded2 (madded@vao.udmnet.ru).
    Copyright (c) 2002, 2004 Yuri Prokushev (prokushev@freemail.ru).
    Copyright (c) 2005 Soren Ager

    Interface to OS/2 32-bit sockets library

 **********************************************************************

  Inet & Sockets Unit v1.04.
  /c/ 2000, 2001 by madded2 (madded@vao.udmnet.ru).
  based on units from SIBYL & infos from Toolkit 4.0.

  for help use tcppr.inf and C samples from toolkit.

  without res_* and dh_* funcs, and have very
  bad support for select() and ioctl() funcs

  new in ver 1.04 : little ioctl() & iptrace support + errors SOCE* constants
  new in ver 1.03 : used inet_lib.lib file for fixing VP linker bug
  new in ver 1.02 : $saves sections, need for correct registers operations
  new in ver 1.01 : ip header struct
}
{
@abstract(a unit to handle sockets)
@author(Yuri Prokushev (prokushev@freemail.ru))
@author(madded2 (madded@vao.udmnet.ru))
@created(3 Sep 2002)
@lastmod(23 Sep 2002)
@todo(sys/ioctl.h, sys/ioctlos2.h, sys/itypes.h)
This is functions from SO32DLL.DLL. These functions allows to use
protocol-independed sockets. Equal to SYS\SOCKET.H, NERRNO.H, SYS\SYSCTL.H.
}
unit SO32Dll;

interface

{$MODE ObjFPC}
{$ASMMODE Intel}
{$PACKRECORDS 1}

(***************************************************************************)
(*                                                                         *)
(*                                     Types                               *)
(*                                                                         *)
(***************************************************************************)
const
  // stream socket
  SOCK_STREAM    = 1;
  // datagram socket
  SOCK_DGRAM     = 2;
  // raw-protocol interface
  SOCK_RAW       = 3;
  // reliably-delivered message
  SOCK_RDM       = 4;
  // sequenced packet stream
  SOCK_SEQPACKET = 5;

(***************************************************************************)
(*                                                                         *)
(*                            Option flags per-socket                      *)
(*                                                                         *)
(***************************************************************************)
const
  // turn on debugging info recording
  SO_DEBUG        = $0001;
  // socket has had listen()
  SO_ACCEPTCONN   = $0002;
  // allow local address reuse
  SO_REUSEADDR    = $0004;
  // keep connections alive
  SO_KEEPALIVE    = $0008;
  // just use interface addresses
  SO_DONTROUTE    = $0010;
  // permit sending of broadcast msgs
  SO_BROADCAST    = $0020;
  // bypass hardware when possible
  SO_USELOOPBACK  = $0040;
  // linger on close if data present
  SO_LINGER       = $0080;
  // leave received OOB data in line
  SO_OOBINLINE    = $0100;
  // limited broadcast sent on all IFs
  SO_L_BROADCAST  = $0200;
  // set if shut down called for rcv
  SO_RCV_SHUTDOWN = $0400;
  // set if shutdown called for send
  SO_SND_SHUTDOWN = $0800;
  // allow local address & port reuse
  SO_REUSEPORT    = $1000;
  // allow t/tcp on socket
  SO_TTCP         = $2000;

(***************************************************************************)
(*                                                                         *)
(*                  Additional options, not kept in so_options             *)
(*                                                                         *)
(***************************************************************************)
const
  // send buffer size
  SO_SNDBUF   = $1001;
  // receive buffer size
  SO_RCVBUF   = $1002;
  // send low-water mark
  SO_SNDLOWAT = $1003;
  // receive low-water mark
  SO_RCVLOWAT = $1004;
  // send timeout
  SO_SNDTIMEO = $1005;
  // receive timeout
  SO_RCVTIMEO = $1006;
  // get error status and clear
  SO_ERROR    = $1007;
  // get socket type
  SO_TYPE     = $1008;
  // get socket options
  SO_OPTIONS  = $1010;

(***************************************************************************)
(*                                                                         *)
(*               Structure used for manipulating linger option             *)
(*                                                                         *)
(***************************************************************************)
type
  //Structure used for manipulating linger option
  linger = record
     l_onoff      :  Longint; // option on/off
     l_linger     : Longint; // linger time
  end;

(***************************************************************************)
(*                                                                         *)
(*      Level number for (get/set)sockopt() to apply to socket itself      *)
(*                                                                         *)
(***************************************************************************)
const
  // options for socket level
  SOL_SOCKET = $ffff;

(***************************************************************************)
(*                                                                         *)
(*                              Address families                           *)
(*                                                                         *)
(***************************************************************************)
const
  // unspecified
  AF_UNSPEC   = 0;
  // local to host (pipes, portals)
  AF_LOCAL    = 1;
  // backward compatibility
  AF_UNIX     = AF_LOCAL;
  AF_OS2      = AF_UNIX;
  // internetwork: UDP, TCP, etc.
  AF_INET     = 2;
  // arpanet imp addresses
  AF_IMPLINK  = 3;
  // pup protocols: e.g. BSP
  AF_PUP      = 4;
  // mit CHAOS protocols
  AF_CHAOS    = 5;
  // XEROX NS protocols
  AF_NS       = 6;
  // ISO protocols
  AF_ISO      = 7;
  // ISO protocols
  AF_OSI      = AF_ISO;
  // european computer manufacturers
  AF_ECMA     = 8;
  // datakit protocols
  AF_DATAKIT  = 9;
  // CCITT protocols, X.25 etc
  AF_CCITT    = 10;
  // IBM SNA
  AF_SNA      = 11;
  // DECnet
  AF_DECnet   = 12;
  // DEC Direct data link interface
  AF_DLI      = 13;
  // LAT
  AF_LAT      = 14;
  // NSC Hyperchannel
  AF_HYLINK   = 15;
  // Apple Talk
  AF_APPLETALK = 16;
  // Netbios
  AF_NB        = 17;
  // Netbios
  AF_NETBIOS   = AF_NB;
  // Link layer interface
  AF_LINK      = 18;
  // eXpress Transfer Protocol (no AF)
  pseudo_AF_XTP = 19;
  // connection-oriented IP, aka ST II
  AF_COIP      = 20;
  // Computer Network Technology
  AF_CNT       = 21;
  // Help Identify RTIP packets
  pseudo_AF_RTIP = 22;
  // Novell Internet Protocol
  AF_IPX       = 23;
  // Simple Internet Protocol
  AF_SIP       = 24;
  AF_INET6     = 24;
  // Help Identify PIP packets
  pseudo_AF_PIP = 25;
  // Internal Routing Protocol
  AF_ROUTE     = 39;
  // firewall support
  AF_FWIP      = 40;
  // IPSEC and encryption techniques
  AF_IPSEC     = 41;
  // DES
  AF_DES       = 42;
  AF_MD5       = 43;
  AF_CDMF      = 44;

  AF_MAX       = 45;

(***************************************************************************)
(*                                                                         *)
(*             Structure used by kernel to store most addresses            *)
(*                                                                         *)
(***************************************************************************)
type
  // Structure used by kernel to store most addresses
  sockaddr = record
    sa_len:    Byte;                     // total length
    sa_family: Byte;                     // address family
    sa_data:   array [0..13] of Byte; // up to 14 bytes of direct address
  end;
  psockaddr = ^sockaddr;

(***************************************************************************)
(*                                                                         *)
(*  Structure used by kernel to pass protocol information in raw sockets   *)
(*                                                                         *)
(***************************************************************************)
type
  // Structure used by kernel to pass protocol information in raw sockets
  sockproto = record
    sp_family:   Word; // address family
    sp_protocol: Word; // protocol
  end;


(***************************************************************************)
(*                                                                         *)
(*             Protocol families, same as address families for now         *)
(*                                                                         *)
(***************************************************************************)
const
  PF_UNSPEC    = AF_UNSPEC;
  PF_LOCAL     = AF_LOCAL;
  PF_UNIX      = AF_UNIX;
  PF_OS2       = AF_OS2;
  PF_INET      = AF_INET;
  PF_IMPLINK   = AF_IMPLINK;
  PF_PUP       = AF_PUP;
  PF_CHAOS     = AF_CHAOS;
  PF_NS        = AF_NS;
  PF_ISO       = AF_ISO;
  PF_OSI       = AF_OSI;
  PF_ECMA      = AF_ECMA;
  PF_DATAKIT   = AF_DATAKIT;
  PF_CCITT     = AF_CCITT;
  PF_SNA       = AF_SNA;
  PF_DECnet    = AF_DECnet;
  PF_DLI       = AF_DLI;
  PF_LAT       = AF_LAT;
  PF_HYLINK    = AF_HYLINK;
  PF_APPLETALK = AF_APPLETALK;
  PF_NETBIOS   = AF_NB;
  PF_NB        = AF_NB;
  PF_ROUTE     = AF_ROUTE;
  PF_LINK      = AF_LINK;
  // really just proto family, no AF
  PF_XTP       = pseudo_AF_XTP;
  PF_COIP      = AF_COIP;
  PF_CNT       = AF_CNT;
  PF_SIP       = AF_SIP;
  PF_INET6     = AF_INET6;
  // same format as AF_NS
  PF_IPX       = AF_IPX;
  // same format as AF_INET
  PF_RTIP      = pseudo_AF_RTIP;
  PF_PIP       = pseudo_AF_PIP;

  PF_MAX       = AF_MAX;


(***************************************************************************)
(*                                                                         *)
(*  Definitions for sysctl call. The sysctl call uses a hierarchical name  *)
(* for objects that can be examined or modified.  The name is expressed as *)
(* a sequence of integers.  Like a file path name, the meaning of each     *)
(* component depends on its place in the hierarchy. The top-level and kern *)
(* identifiers are defined here, and other identifiers are defined in the  *)
(* respective subsystem header files.                                      *)
(*                                                                         *)
(***************************************************************************)

const
// largest number of components supported
  CTL_MAXNAME    = 12;

(***************************************************************************)
(*                                                                         *)
(* Each subsystem defined by sysctl defines a list of variables            *)
(* for that subsystem. Each name is either a node with further             *)
(* levels defined below it, or it is a leaf of some particular             *)
(* type given below. Each sysctl level defines a set of name/type          *)
(* pairs to be used by sysctl(1) in manipulating the subsystem.            *)
(*                                                                         *)
(***************************************************************************)

type
  ctlname=record
    ctl_name: PChar;      // subsystem name
    ctl_type: Longint;    // type of name
  end;

const
  // name is a node
  CTLTYPE_NODE    =1;
  // name describes an integer
  CTLTYPE_INT     =2;
  // name describes a string
  CTLTYPE_STRING  =3;
  // name describes a 64-bit number
  CTLTYPE_QUAD    =4;
  // name describes a structure
  CTLTYPE_STRUCT  =5;
  // inetcfg sysctl code
  CTLTYPE_INETCFG =6;
  // inetver sysctl code
  CTLTYPE_INEVER  =7;

(*
 * Top-level identifiers
 *)
const
  // "high kernel": proc, limits
  CTL_KERN       = 1;
  // network, see socket.h
  CTL_NET        = 4;
  // OS/2 specific codes
  CTL_OS2        = 9;

(*

#define CTL_NAMES { \
        { 0, 0 }, \
        { "kern", CTLTYPE_NODE }, \
        { "net", CTLTYPE_NODE }, \
        { "os2", CTLTYPE_NODE }, \
}

/*
 * CTL_KERN identifiers
 */
#define KERN_MAXFILES            7      /* int: max open files */
#define KERN_HOSTNAME           10      /* string: hostname */
#define KERN_HOSTID             11      /* int: host identifier */

#define CTL_KERN_NAMES { \
        { 0, 0 }, \
        { "ostype", CTLTYPE_STRING }, \
        { "osrelease", CTLTYPE_STRING }, \
        { "osrevision", CTLTYPE_INT }, \
        { "version", CTLTYPE_STRING }, \
        { "maxvnodes", CTLTYPE_INT }, \
        { "maxproc", CTLTYPE_INT }, \
        { "maxfiles", CTLTYPE_INT }, \
        { "argmax", CTLTYPE_INT }, \
        { "securelevel", CTLTYPE_INT }, \
        { "hostname", CTLTYPE_STRING }, \
        { "hostid", CTLTYPE_INT }, \
        { "clockrate", CTLTYPE_STRUCT }, \
        { "vnode", CTLTYPE_STRUCT }, \
        { "proc", CTLTYPE_STRUCT }, \
        { "file", CTLTYPE_STRUCT }, \
        { "profiling", CTLTYPE_NODE }, \
        { "posix1version", CTLTYPE_INT }, \
        { "ngroups", CTLTYPE_INT }, \
        { "job_control", CTLTYPE_INT }, \
        { "saved_ids", CTLTYPE_INT }, \
        { "boottime", CTLTYPE_STRUCT }, \
}

/*
 * KERN_SYSCTL objects
 */
#define KERNCTL_INETVER      70          /* Sysctl code for sockets Inetversion */
#define OS2_MEMMAPIO         1           /* memory map io */
#define OS2_QUERY_MEMMAPIO   2           /* Query if mapped memory usable */

/* Generic Structure for Inetcfg calls */
struct inetcfg_ctl{
          unsigned long var_name;
          unsigned long var_cur_val;
          unsigned long var_max_val;
          unsigned long var_def_val;
          unsigned long var_min_val;
};

/* Inetversion */
struct inetvers_ctl {
         float version;
         char  versionstr[10];           /* Less than 10 chars in version string */
};

#include <sys/cdefs.h>
#ifndef KERNEL
__BEGIN_DECLS
int _System sysctl __TCPPROTO((int *, u_int, void *, size_t *, void *, size_t));
__END_DECLS
#endif
*)

(* !!TODO!! Not finished yet!!
/*
 * Definitions for network related sysctl, CTL_NET.
 *
 * Second level is protocol family.
 * Third level is protocol number.
 *
 * Further levels are defined by the individual families below.
 */
const
  NET_MAXID     = AF_MAX;

#define CTL_NET_NAMES { \
        { 0, 0 }, \
        { "local", CTLTYPE_NODE }, \
        { "inet", CTLTYPE_NODE }, \
        { "implink", CTLTYPE_NODE }, \
        { "pup", CTLTYPE_NODE }, \
        { "chaos", CTLTYPE_NODE }, \
        { "xerox_ns", CTLTYPE_NODE }, \
        { "iso", CTLTYPE_NODE }, \
        { "emca", CTLTYPE_NODE }, \
        { "datakit", CTLTYPE_NODE }, \
        { "ccitt", CTLTYPE_NODE }, \
        { "ibm_sna", CTLTYPE_NODE }, \
        { "decnet", CTLTYPE_NODE }, \
        { "dec_dli", CTLTYPE_NODE }, \
        { "lat", CTLTYPE_NODE }, \
        { "hylink", CTLTYPE_NODE }, \
        { "appletalk", CTLTYPE_NODE }, \
        { "netbios", CTLTYPE_NODE }, \
        { "route", CTLTYPE_NODE }, \
        { "link_layer", CTLTYPE_NODE }, \
        { "xtp", CTLTYPE_NODE }, \
        { "coip", CTLTYPE_NODE }, \
        { "cnt", CTLTYPE_NODE }, \
        { "rtip", CTLTYPE_NODE }, \
        { "ipx", CTLTYPE_NODE }, \
        { "sip", CTLTYPE_NODE }, \
        { "pip", CTLTYPE_NODE }, \
}

/*
 * PF_ROUTE - Routing table
 *
 * Three additional levels are defined:
 *      Fourth: address family, 0 is wildcard
 *      Fifth: type of info, defined below
 *      Sixth: flag(s) to mask with for NET_RT_FLAGS
 */
const
  // dump; may limit to a.f.
  NET_RT_DUMP   = 1;
  // by flags, e.g. RESOLVING
  NET_RT_FLAGS  = 2;
  // survey interface list
  NET_RT_IFLIST = 3;
  NET_RT_MAXID  = 4;

#define CTL_NET_RT_NAMES { \
        { 0, 0 }, \
        { "dump", CTLTYPE_STRUCT }, \
        { "flags", CTLTYPE_STRUCT }, \
        { "iflist", CTLTYPE_STRUCT }, \
}

*)

(***************************************************************************)
(*                                                                         *)
(*             Maximum queue length specifiable by listen                  *)
(*                                                                         *)
(***************************************************************************)
const
  // Maximum queue length specifiable by listen
  SOMAXCONN = 1024;

(***************************************************************************)
(*                                                                         *)
(*               Message header for recvmsg and sendmsg calls              *)
(*          Used value-result for recvmsg, value only for sendmsg          *)
(*                                                                         *)
(***************************************************************************)
type
  iovec = record
    iov_base  :  Pointer;
    iov_len   :  Longint;
  end;

  // Message header for recvmsg and sendmsg calls
  msghdr = record
    msg_name:       pChar;     // optional address
    msg_namelen:    Longint;   // size of address
    msg_iov:        ^iovec;    // scatter/gather array
    msg_iovlen:     Longint;   // # elements in msg_iov (max 1024)
    msg_control:    pChar;     // ancillary data, see below
    msg_controllen: Longint;   // ancillary data buffer len
    msg_flags:      Longint;   // flags on received message
  end;

const
  // process out-of-band data
  MSG_OOB       = $1;
  // peek at incoming message
  MSG_PEEK      = $2;
  // send without using routing tables
  MSG_DONTROUTE = $4;
  // send without using routing tables
  MSG_FULLREAD   = $8;
  // data completes record
  MSG_EOR        = $10;
  // data discarded before delivery
  MSG_TRUNC      = $20;
  // control data lost before delivery
  MSG_CTRUNC     = $40;
  // wait for full request or error
  MSG_WAITALL    = $80;
  // this message should be nonblocking
  MSG_DONTWAIT   = $100;
  MSG_EOF        = $200;
  // mem mapped io
  MSG_MAPIO      = $400;

(***************************************************************************)
(*                                                                         *)
(*        Header for ancillary data objects in msg_control buffer          *)
(*         Used for additional information with/about a datagram           *)
(*          not expressible by flags.   The format is a sequence         *)
(*           of message elements headed by cmsghdr structures              *)
(*                                                                         *)
(***************************************************************************)
type
  // Header for ancillary data objects in msg_control buffer
  cmsghdr = record
    cmsg_len:   Longint; // data byte count, including hdr
    cmsg_level: Longint; // originating protocol
    cmsg_type:  Longint; // protocol-specific type
  end;

  cmsg = record
    cmsg_hdr:  cmsghdr;
    cmsg_data: array [0..0] of Byte;
  end;

(***************************************************************************)
(*                                                                         *)
(*                     "Socket"-level control message types                *)
(*                                                                         *)
(***************************************************************************)
const
  // access rights (array of int)
  SCM_RIGHTS = $01;

(***************************************************************************)
(*                                                                         *)
(*              4.3 compat sockaddr, move to compat file later             *)
(*                                                                         *)
(***************************************************************************)
type
  // 4.3 compat sockaddr
  osockaddr = record
    sa_family: Word;                // address family
    sa_data: array [0..13] of Byte; // up to 14 bytes of direct address
  end;

(***************************************************************************)
(*                                                                         *)
(*             4.3-compat message header (move to compat file later)       *)
(*                                                                         *)
(***************************************************************************)
type
  // 4.3-compat message header
  omsghdr = record
    msg_name:         pChar;   // optional address
    msg_namelen:      Longint; // size of address
    msg_iov:          ^iovec;  // scatter/gather array
    msg_iovlen:       Longint; // # elements in msg_iov
    msg_accrights:    pChar;   // access rights sent/received
    msg_accrightslen: Longint;
  end;


(* !!TODO
/*
 * send_file parameter structure
 */
struct sf_parms {
        void   *header_data;      /* ptr to header data */
        size_t header_length;     /* size of header data */
        int    file_handle;       /* file handle to send from */
        size_t file_size;         /* size of file */
        int    file_offset;       /* byte offset in file to send from */
        size_t file_bytes;        /* bytes of file to be sent */
        void   *trailer_data;     /* ptr to trailer data */
        size_t trailer_length;    /* size of trailer data */
        size_t bytes_sent;        /* bytes sent in this send_file call */
};
*)

{ !!TODO Check is all this functions defined
__BEGIN_DECLS
int _System accept_and_recv __TCPPROTO((long, long*, struct sockaddr *, long*, struct sockaddr*, long*, caddr_t, size_t));
ssize_t _System recvfrom __TCPPROTO((int, void *, size_t, int, struct sockaddr *, int *));
ssize_t _System recvmsg __TCPPROTO((int, struct msghdr *, int));
ssize_t _System send __TCPPROTO((int, const void *, size_t, int));
ssize_t _System sendto __TCPPROTO((int, const void *, size_t, int, const struct sockaddr *, int));
ssize_t _System sendmsg __TCPPROTO((int, const struct msghdr *, int));
ssize_t _System send_file __TCPPROTO((int *, struct sf_parms *, int ));
int _System setsockopt __TCPPROTO((int, int, int, const void *, int));
int _System shutdown __TCPPROTO((int, int));
int _System socket __TCPPROTO((int, int, int));
int _System socketpair __TCPPROTO((int, int, int, int *));

/* OS/2 additions */
int _System sock_init __TCPPROTO((void));
int _System sock_errno __TCPPROTO((void));
void _System psock_errno __TCPPROTO((const char *));
char * _System sock_strerror __TCPPROTO((int));
int _System soabort __TCPPROTO((int));
int _System so_cancel __TCPPROTO((int));
int _System getinetversion __TCPPROTO((char *));
void _System addsockettolist __TCPPROTO((int));
int _System removesocketfromlist __TCPPROTO((int));
/*int _System removesocketfromlist __TCPPROTO((long *));*/  /*changed on 09-30-98 for corresponding change in sockets.c file*/

/* SOCKS additions */
int _System Raccept __TCPPROTO((int, struct sockaddr *, int *));
int _System Rbind __TCPPROTO((int, struct sockaddr *, int, struct sockaddr *));
int _System Rconnect __TCPPROTO((int, const struct sockaddr *, int));
int _System Rgetsockname __TCPPROTO((int, struct sockaddr *, int *));
int _System Rlisten __TCPPROTO((int, int));
__END_DECLS


/* more OS/2 stuff */

const
  // should be on free list
  MT_FREE      =  0;
  // dynamic (data) allocation
  MT_DATA      =  1;
  // packet header
  MT_HEADER    =  2;
  // socket structure
  MT_SOCKET    =  3;
  // protocol control block
  MT_PCB       =  4;
  // routing tables
  MT_RTABLE    =  5;
  // IMP host tables
  MT_HTABLE    =  6;
  // address resolution tables
  MT_ATABLE    =  7;
  // socket name
  MT_SONAME    =  8;
  // zombie proc status
  MT_ZOMBIE    =  9;
  // socket options
  MT_SOOPTS    =  10;
  // fragment reassembly header
  MT_FTABLE    =  11;
  // access rights
  MT_RIGHTS    =  12;
  // interface address
  MT_IFADDR    =  13;

Type
  sostats=record
    count: integer;
    socketdata: array[0..13*MAXSOCKETS-1] of integer;
  end;

}

(***************************************************************************)
(*                                                                         *)
(*          SOCE* constants - socket errors from NERRNO.H                  *)
(*  All OS/2 SOCKET API error constants are biased by SOCBASEERR from the  *)
(*                                 "normal"                                *)
(*                                                                         *)
(***************************************************************************)

const
  SOCBASEERR         = 10000;

  // Not owner
  SOCEPERM           = (SOCBASEERR+1);
  // No such file or directory
  SOCENOENT          = (SOCBASEERR+2);
  // No such process
  SOCESRCH           = (SOCBASEERR+3);
  // Interrupted system call
  SOCEINTR           = (SOCBASEERR+4);
  // Input/output error
  SOCEIO             = (SOCBASEERR+5);
  SOCENXIO           = (SOCBASEERR+6);      // No such device or address
  SOCE2BIG           = (SOCBASEERR+7);      // Argument list too long
  SOCENOEXEC         = (SOCBASEERR+8);      // Exec format error
  SOCEBADF           = (SOCBASEERR+9);      // Bad file number
  SOCECHILD          = (SOCBASEERR+10);     // No child processes
  SOCEDEADLK         = (SOCBASEERR+11);     // Resource deadlock avoided
  SOCENOMEM          = (SOCBASEERR+12);     // Cannot allocate memory
  SOCEACCES          = (SOCBASEERR+13);     // Permission denied
  SOCEFAULT          = (SOCBASEERR+14);     // Bad address
  SOCENOTBLK         = (SOCBASEERR+15);     // Block device required
  SOCEBUSY           = (SOCBASEERR+16);     // Device busy
  SOCEEXIST          = (SOCBASEERR+17);     // File exists
  SOCEXDEV           = (SOCBASEERR+18);     // Cross-device link
  SOCENODEV          = (SOCBASEERR+19);     // Operation not supported by device
  SOCENOTDIR         = (SOCBASEERR+20);     // Not a directory
  SOCEISDIR          = (SOCBASEERR+21);     // Is a directory
  SOCEINVAL          = (SOCBASEERR+22);     // Invalid argument
  SOCENFILE          = (SOCBASEERR+23);     // Too many open files in system
  SOCEMFILE          = (SOCBASEERR+24);     // Too many open files
  SOCENOTTY          = (SOCBASEERR+25);     // Inappropriate ioctl for device
  SOCETXTBSY         = (SOCBASEERR+26);     // Text file busy
  SOCEFBIG           = (SOCBASEERR+27);     // File too large
  SOCENOSPC          = (SOCBASEERR+28);     // No space left on device
  SOCESPIPE          = (SOCBASEERR+29);     // Illegal seek
  SOCEROFS           = (SOCBASEERR+30);     // Read-only file system
  SOCEMLINK          = (SOCBASEERR+31);     // Too many links
  SOCEPIPE           = (SOCBASEERR+32);     // Broken pipe

// math software
  SOCEDOM            = (SOCBASEERR+33);     // Numerical argument out of domain
  SOCERANGE          = (SOCBASEERR+34);     // Result too large

// non-blocking and interrupt i/o
  SOCEAGAIN          = (SOCBASEERR+35);     // Resource temporarily unavailable
  SOCEWOULDBLOCK     = SOCEAGAIN;           // Operation would block
  SOCEINPROGRESS     = (SOCBASEERR+36);     // Operation now in progress
  SOCEALREADY        = (SOCBASEERR+37);     // Operation already in progress

// ipc/network software -- argument errors
  SOCENOTSOCK        = (SOCBASEERR+38);     // Socket operation on non-socket
  SOCEDESTADDRREQ    = (SOCBASEERR+39);     // Destination address required
  SOCEMSGSIZE        = (SOCBASEERR+40);     // Message too long
  SOCEPROTOTYPE      = (SOCBASEERR+41);     // Protocol wrong type for socket
  SOCENOPROTOOPT     = (SOCBASEERR+42);     // Protocol not available
  SOCEPROTONOSUPPORT = (SOCBASEERR+43);     // Protocol not supported
  SOCESOCKTNOSUPPORT = (SOCBASEERR+44);     // Socket type not supported
  SOCEOPNOTSUPP      = (SOCBASEERR+45);     // Operation not supported
  SOCEPFNOSUPPORT    = (SOCBASEERR+46);     // Protocol family not supported
  SOCEAFNOSUPPORT    = (SOCBASEERR+47);     // Address family not supported by protocol family
  SOCEADDRINUSE      = (SOCBASEERR+48);     // Address already in use
  SOCEADDRNOTAVAIL   = (SOCBASEERR+49);     // Can't assign requested address

// ipc/network software -- operational errors
  SOCENETDOWN        = (SOCBASEERR+50);     // Network is down
  SOCENETUNREACH     = (SOCBASEERR+51);     // Network is unreachable
  SOCENETRESET       = (SOCBASEERR+52);     // Network dropped connection on reset
  SOCECONNABORTED    = (SOCBASEERR+53);     // Software caused connection abort
  SOCECONNRESET      = (SOCBASEERR+54);     // Connection reset by peer
  SOCENOBUFS         = (SOCBASEERR+55);     // No buffer space available
  SOCEISCONN         = (SOCBASEERR+56);     // Socket is already connected
  SOCENOTCONN        = (SOCBASEERR+57);     // Socket is not connected
  SOCESHUTDOWN       = (SOCBASEERR+58);     // Can't send after socket shutdown
  SOCETOOMANYREFS    = (SOCBASEERR+59);     // Too many references: can't splice
  SOCETIMEDOUT       = (SOCBASEERR+60);     // Operation timed out
  SOCECONNREFUSED    = (SOCBASEERR+61);     // Connection refused

  SOCELOOP           = (SOCBASEERR+62);     // Too many levels of symbolic links
  SOCENAMETOOLONG    = (SOCBASEERR+63);     // File name too long

// should be rearranged
  SOCEHOSTDOWN       = (SOCBASEERR+64);      // Host is down
  SOCEHOSTUNREACH    = (SOCBASEERR+65);      // No route to host
  SOCENOTEMPTY       = (SOCBASEERR+66);      // Directory not empty

// quotas & mush
  SOCEPROCLIM        = (SOCBASEERR+67);      // Too many processes
  SOCEUSERS          = (SOCBASEERR+68);      // Too many users
  SOCEDQUOT          = (SOCBASEERR+69);      // Disc quota exceeded

// Network File System
  SOCESTALE          = (SOCBASEERR+70);      // Stale NFS file handle
  SOCEREMOTE         = (SOCBASEERR+71);      // Too many levels of remote in path
  SOCEBADRPC         = (SOCBASEERR+72);      // RPC struct is bad
  SOCERPCMISMATCH    = (SOCBASEERR+73);      // RPC version wrong
  SOCEPROGUNAVAIL    = (SOCBASEERR+74);      // RPC prog. not avail
  SOCEPROGMISMATCH   = (SOCBASEERR+75);      // Program version wrong
  SOCEPROCUNAVAIL    = (SOCBASEERR+76);      // Bad procedure for program

  SOCENOLCK          = (SOCBASEERR+77);      // No locks available
  SOCENOSYS          = (SOCBASEERR+78);      // Function not implemented

  SOCEFTYPE          = (SOCBASEERR+79);      // Inappropriate file type or format
  SOCEAUTH           = (SOCBASEERR+80);      // Authentication error
  SOCENEEDAUTH       = (SOCBASEERR+81);      // Need authenticator

  SOCEOS2ERR         = (SOCBASEERR+100);     // OS/2 Error
  SOCELAST           = (SOCBASEERR+100);     // Must be equal largest errno

(* !!TODO Add this consts
/*
 * OS/2 SOCKET API errors redefined as regular BSD error constants
 */

#ifndef ENOENT
#define ENOENT                  SOCENOENT
#endif

#ifndef EFAULT
#define EFAULT                  SOCEFAULT
#endif

#ifndef EBUSY
#define EBUSY                   SOCEBUSY
#endif

#ifndef ENXIO
#define ENXIO                   SOCENXIO
#endif

#ifndef EACCES
#define EACCES                  SOCEACCES
#endif

#ifndef ENOMEM
#define ENOMEM                  SOCENOMEM
#endif

#ifndef ENOTDIR
#define ENOTDIR                 SOCENOTDIR
#endif

#ifndef EPERM
#define EPERM                   SOCEPERM
#endif

#ifndef ESRCH
#define ESRCH                   SOCESRCH
#endif

#ifndef EDQUOT
#define EDQUOT                  SOCEDQUOT
#endif

#ifndef EEXIST
#define EEXIST                  SOCEEXIST
#endif

#ifndef EBUSY
#define EBUSY                   SOCEBUSY
#endif

#ifndef EWOULDBLOCK
#define EWOULDBLOCK             SOCEWOULDBLOCK
#endif

#ifndef EINPROGRESS
#define EINPROGRESS             SOCEINPROGRESS
#endif

#ifndef EALREADY
#define EALREADY                SOCEALREADY
#endif

#ifndef ENOTSOCK
#define ENOTSOCK                SOCENOTSOCK
#endif

#ifndef EDESTADDRREQ
#define EDESTADDRREQ            SOCEDESTADDRREQ
#endif

#ifndef EMSGSIZE
#define EMSGSIZE                SOCEMSGSIZE
#endif

#ifndef EPROTOTYPE
#define EPROTOTYPE              SOCEPROTOTYPE
#endif

#ifndef ENOPROTOOPT
#define ENOPROTOOPT             SOCENOPROTOOPT
#endif

#ifndef EPROTONOSUPPORT
#define EPROTONOSUPPORT         SOCEPROTONOSUPPORT
#endif

#ifndef ESOCKTNOSUPPORT
#define ESOCKTNOSUPPORT         SOCESOCKTNOSUPPORT
#endif

#ifndef EOPNOTSUPP
#define EOPNOTSUPP              SOCEOPNOTSUPP
#endif

#ifndef EPFNOSUPPORT
#define EPFNOSUPPORT            SOCEPFNOSUPPORT
#endif

#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT            SOCEAFNOSUPPORT
#endif

#ifndef EADDRINUSE
#define EADDRINUSE              SOCEADDRINUSE
#endif

#ifndef EADDRNOTAVAIL
#define EADDRNOTAVAIL           SOCEADDRNOTAVAIL
#endif

#ifndef ENETDOWN
#define ENETDOWN                SOCENETDOWN
#endif

#ifndef ENETUNREACH
#define ENETUNREACH             SOCENETUNREACH
#endif

#ifndef ENETRESET
#define ENETRESET               SOCENETRESET
#endif

#ifndef ECONNABORTED
#define ECONNABORTED            SOCECONNABORTED
#endif

#ifndef ECONNRESET
#define ECONNRESET              SOCECONNRESET
#endif

#ifndef ENOBUFS
#define ENOBUFS                 SOCENOBUFS
#endif

#ifndef EISCONN
#define EISCONN                 SOCEISCONN
#endif

#ifndef ENOTCONN
#define ENOTCONN                SOCENOTCONN
#endif

#ifndef ESHUTDOWN
#define ESHUTDOWN               SOCESHUTDOWN
#endif

#ifndef ETOOMANYREFS
#define ETOOMANYREFS            SOCETOOMANYREFS
#endif

#ifndef ETIMEDOUT
#define ETIMEDOUT               SOCETIMEDOUT
#endif

#ifndef ECONNREFUSED
#define ECONNREFUSED            SOCECONNREFUSED
#endif

#ifndef ELOOP
#define ELOOP                   SOCELOOP
#endif

#ifndef ENAMETOOLONG            /* Borland and Watcom define this */
#define ENAMETOOLONG            SOCENAMETOOLONG
#endif

#ifndef EHOSTDOWN
#define EHOSTDOWN               SOCEHOSTDOWN
#endif

#ifndef EHOSTUNREACH
#define EHOSTUNREACH            SOCEHOSTUNREACH
#endif

#ifndef ENOTEMPTY               /* Watcom defines this */
#define ENOTEMPTY               SOCENOTEMPTY
#endif

#ifndef EINVAL
#define EINVAL                  SOCEINVAL
#endif

#ifndef EINTR
#define EINTR                   SOCEINTR
#endif

#ifndef EMFILE
#define EMFILE                  SOCEMFILE
#endif

#ifndef EPIPE
#define EPIPE                   SOCEPIPE
#endif
*)

// * bsd select definitions

const
{
 * Select uses bit masks of file descriptors in longs.  These macros
 * manipulate such bit fields (the filesystem macros use chars).
 * FD_SETSIZE may be defined by the user, but the default here should
 * be enough for most uses.
}
  FD_SETSIZE = 64;

type

  fd_set = record
    fd_count  :  Word;                        // how many are SET?
    fd_array  :  array[0..FD_SETSIZE-1] of Longint;   // an array of SOCKETs
  end;
  TFDSet = FD_Set;
  PFDSet = ^TFDSet;

  timeval = record
    tv_sec   :  Longint; // Number of seconds
    tv_usec  :  Longint; // Number of microseconds
  end;
  TTimeVal = TimeVal;
  PTimeVal = ^TTimeVal;

{
 * ioctl & ip trace support
}
const
  FIONREAD      = (Ord('f') SHL 8) OR 127;
  FIONBIO       = (Ord('f') SHL 8) OR 126;

  FIOASYNC      = (Ord('f') SHL 8) OR 125;
  FIOTCPCKSUM   = (Ord('f') SHL 8) OR 128;
  FIONSTATUS    = (Ord('f') SHL 8) OR 120;
  FIONURG       = (Ord('f') SHL 8) OR 121;

  SIOCSHIWAT    = (Ord('s') SHL 8) OR  0;
  SIOCGHIWAT    = (Ord('s') SHL 8) OR  1;
  SIOCSLOWAT    = (Ord('s') SHL 8) OR  2;
  SIOCGLOWAT    = (Ord('s') SHL 8) OR  3;
  SIOCATMARK    = (Ord('s') SHL 8) OR  7;
  SIOCSPGRP     = (Ord('s') SHL 8) OR  8;
  SIOCGPGRP     = (Ord('s') SHL 8) OR  9;
  SIOCSHOSTID   = (Ord('s') SHL 8) OR 10;

  SIOCADDRT     = (Ord('r') SHL 8) OR 10;
  SIOCDELRT     = (Ord('r') SHL 8) OR 11;
  SIOMETRIC1RT  = (Ord('r') SHL 8) OR 12;
  SIOMETRIC2RT  = (Ord('r') SHL 8) OR 13;
  SIOMETRIC3RT  = (Ord('r') SHL 8) OR 14;
  SIOMETRIC4RT  = (Ord('r') SHL 8) OR 15;

  SIOCREGADDNET = (Ord('r') SHL 8) OR 12;
  SIOCREGDELNET = (Ord('r') SHL 8) OR 13;
  SIOCREGROUTES = (Ord('r') SHL 8) OR 14;
  SIOCFLUSHROUTES=(Ord('r') SHL 8) OR 15;

  SIOCSIFADDR   = (Ord('i') SHL 8) OR 12;
  SIOCGIFADDR   = (Ord('i') SHL 8) OR 13;
  SIOCSIFDSTADDR= (Ord('i') SHL 8) OR 14;
  SIOCGIFDSTADDR= (Ord('i') SHL 8) OR 15;
  SIOCSIFFLAGS  = (Ord('i') SHL 8) OR 16;
  SIOCGIFFLAGS  = (Ord('i') SHL 8) OR 17;
  SIOCGIFBRDADDR= (Ord('i') SHL 8) OR 18;
  SIOCSIFBRDADDR= (Ord('i') SHL 8) OR 19;
  SIOCGIFCONF   = (Ord('i') SHL 8) OR 20;
  SIOCGIFNETMASK= (Ord('i') SHL 8) OR 21;
  SIOCSIFNETMASK= (Ord('i') SHL 8) OR 22;
  SIOCGIFMETRIC = (Ord('i') SHL 8) OR 23;
  SIOCSIFMETRIC = (Ord('i') SHL 8) OR 24;
  SIOCSIFSETSIG = (Ord('i') SHL 8) OR 25;
  SIOCSIFCLRSIG = (Ord('i') SHL 8) OR 26;
  SIOCSIFBRD    = (Ord('i') SHL 8) OR 27; { SINGLE-rt bcst. using old # for bkw cmpt }
  SIOCSIFALLRTB = (Ord('i') SHL 8) OR 63; { added to configure all-route broadcst }

  SIOCGIFLOAD     =(Ord('i') SHL 8) OR 27;
  SIOCSIFFILTERSRC=(Ord('i') SHL 8) OR 28;
  SIOCGIFFILTERSRC=(Ord('i') SHL 8) OR 29;

  SIOCSARP      = (Ord('i') SHL 8) OR 30;
  SIOCGARP      = (Ord('i') SHL 8) OR 31;
  SIOCDARP      = (Ord('i') SHL 8) OR 32;
  SIOCSIFSNMPSIG= (Ord('i') SHL 8) OR 33;
  SIOCSIFSNMPCLR= (Ord('i') SHL 8) OR 34;
  SIOCSIFSNMPCRC= (Ord('i') SHL 8) OR 35;
  SIOCSIFPRIORITY=(Ord('i') SHL 8) OR 36;
  SIOCGIFPRIORITY=(Ord('i') SHL 8) OR 37;
  SIOCSIFFILTERDST=(Ord('i') SHL 8) OR 38;
  SIOCGIFFILTERDST=(Ord('i') SHL 8) OR 39;
  SIOCSIF802_3  =  (Ord('i') SHL 8) OR 40;
  SIOCSIFNO802_3=  (Ord('i') SHL 8) OR 41;
  SIOCSIFNOREDIR=  (Ord('i') SHL 8) OR 42;
  SIOCSIFYESREDIR= (Ord('i') SHL 8) OR 43;

  SIOCSIFMTU    = (Ord('i') SHL 8) OR 45;
  SIOCSIFFDDI   = (Ord('i') SHL 8) OR 46;
  SIOCSIFNOFDDI = (Ord('i') SHL 8) OR 47;
  SIOCSRDBRD    = (Ord('i') SHL 8) OR 48;
  SIOCSARP_TR   = (Ord('i') SHL 8) OR 49;
  SIOCGARP_TR   = (Ord('i') SHL 8) OR 50;

{ multicast ioctls }
  SIOCADDMULTI  = (Ord('i') SHL 8) OR 51;    { add m'cast addr }
  SIOCDELMULTI  = (Ord('i') SHL 8) OR 52;    { del m'cast addr }
  SIOCMULTISBC  = (Ord('i') SHL 8) OR 61;    { use broadcast to send IP multicast }
  SIOCMULTISFA  = (Ord('i') SHL 8) OR 62;    { use functional addr to send IP multicast }


{$IFDEF SLBOOTP}
  SIOCGUNIT     = (Ord('i') SHL 8) OR 70;    { Used to retreive unit number on }
                                             { serial interface }
{$ENDIF}

  SIOCSIFSPIPE   = (Ord('i') SHL 8) OR 71;   { used to set pipe size on interface }
                                             { this is used as tcp send buffer size }
  SIOCSIFRPIPE   = (Ord('i') SHL 8) OR 72;   { used to set pipe size on interface }
                                             { this is used as tcp recv buffer size }
  SIOCSIFTCPSEG = (Ord('i') SHL 8) OR 73;    { set the TCP segment size on interface }
  SIOCSIFUSE576 = (Ord('i') SHL 8) OR 74;    { enable/disable the automatic change of mss to 576 }
                                             { if going through a router }
  SIOCGIFVALID  = (Ord('i') SHL 8) OR 75;    { to check if the interface is Valid or not }
                                             { sk June 14 1995 }
  SIOCGIFBOUND  = (Ord('i') SHL 8) OR 76;    { ioctl to return bound/shld bind ifs }
{ Interface Tracing Support }
  SIOCGIFEFLAGS = (Ord('i') SHL 8) OR 150;
  SIOCSIFEFLAGS = (Ord('i') SHL 8) OR 151;
  SIOCGIFTRACE  = (Ord('i') SHL 8) OR 152;
  SIOCSIFTRACE  = (Ord('i') SHL 8) OR 153;

{$IFDEF SLSTATS}
  SIOCSSTAT    = (Ord('i') SHL 8) OR 154;
  SIOCGSTAT    = (Ord('i') SHL 8) OR 155;
{$ENDIF}

{ NETSTAT stuff }
  SIOSTATMBUF   = (Ord('n') SHL 8) OR 40;
  SIOSTATTCP    = (Ord('n') SHL 8) OR 41;
  SIOSTATUDP    = (Ord('n') SHL 8) OR 42;
  SIOSTATIP     = (Ord('n') SHL 8) OR 43;
  SIOSTATSO     = (Ord('n') SHL 8) OR 44;
  SIOSTATRT     = (Ord('n') SHL 8) OR 45;
  SIOFLUSHRT    = (Ord('n') SHL 8) OR 46;
  SIOSTATICMP   = (Ord('n') SHL 8) OR 47;
  SIOSTATIF     = (Ord('n') SHL 8) OR 48;
  SIOSTATAT     = (Ord('n') SHL 8) OR 49;
  SIOSTATARP    = (Ord('n') SHL 8) OR 50;
  SIOSTATIF42   = (Ord('n') SHL 8) OR 51;

(* From fpc 2.0.0
  SIOCGIFFLAGS          =  $6900 + 17;  // get interface flags

  { Interface Tracing Support }
  SIOCGIFEFLAGS         =  $6900 + 150; // get interface enhanced flags
  SIOCSIFEFLAGS         =  $6900 + 151; // set interface enhanced flags
  SIOCGIFTRACE          =  $6900 + 152; // get interface trace data
  SIOCSIFTRACE          =  $6900 + 153; // set interface trace data
  { sorry, i skip other ioctl commands, see SYS\ioctl.h from toolkit for it.. }
*)


(* !!TODO Check all macros from sys/itypes.h
function  LSwap(a:Longint):Longint;
function  WSwap(a:Word):Word;

{ host -> network for long (4 bytes) }
function  htonl(a:Longint):Longint;

{ network -> host for long (4 bytes) }
function  ntohl(a:Longint):Longint;

{ host -> network for small (2 bytes) }
function  htons(a:Word):Word;

{ network -> host for small (2 bytes) }
function  ntohs(a:Word):Word;

*)

{ * init / misc funcs }

{ init sockets system }
function  sock_init:Longint; cdecl;

{ get inet version. version - buffer of ?? size for returned string. }
function  getinetversion(var version):Longint; cdecl;


{ * sockets errors reporting funcs }

{ last err code for this thread }
function  sock_errno:Longint; cdecl;

{ print last err string + str if not NIL }
procedure psock_errno(var str:PChar); cdecl;


{ * sockets creation / close funcs }

{ create new socket }
function  socket(domain,stype,protocol:Longint):Longint; cdecl;

{ close socket }
function  soclose(sock:Longint):Longint; cdecl;

{ cancel socket }
function  so_cancel(sock:Longint):Longint; cdecl;

{ shutdown socket. howto: 0/1/2 }
function  shutdown(sock,howto:Longint):Longint; cdecl;

{ abort socket. no docs found about it :( }
function  soabort(sock:Longint):Longint; cdecl;

(***************************************************************************)
(*                                                                         *)
(*                         sockets connection funcs                        *)
(*                                                                         *)
(***************************************************************************)

{ accept a connection from remote host. returns s_addr & s_addr_len if not nil }
function accept(sock:Longint; var s_addr:sockaddr; s_addr_len:Longint):Longint; cdecl;

{ bind a local name to the socket }
function bind(sock:Longint; var s_addr: sockaddr; s_addr_len:Longint):Longint; cdecl;

{ connect socket to remote host }
function connect(sock:Longint; var s_addr:sockaddr; s_addr_len:Longint):Longint; cdecl;

{ listen on socket. max_conn - queue size of listen. }
function listen(sock,max_conn:Longint):Longint; cdecl;

(***************************************************************************)
(*                                                                         *)
(*                       sockets read/write funcs                          *)
(*                                                                         *)
(***************************************************************************)

{ read data from socket. ! return N of readed bytes, or 0 (closed) or -1 }
function recv(sock:Longint; var buf; buf_len,flags:Longint):Longint; cdecl;

{ send data to socket. ! return N of sent bytes. -1 - err }
function  send(sock:Longint; var buf; buf_len,flags:Longint):Longint; cdecl;

{ read data from socket. ! return N of readed bytes, or 0 (closed) or -1 }
function  recvfrom(sock:Longint; var buf; buf_len,flags:Longint; var s_addr:sockaddr; var s_addr_len:Longint):Longint; cdecl;

{ send data to socket. ! return N of sent bytes. -1 - err }
function  sendto(sock:Longint; var buf; buf_len,flags:Longint; var s_addr:sockaddr; s_addr_len:Longint):Longint; cdecl;

{ read data into iov_count number of buffers iov.
  ! return N of readed bytes, or 0 (closed) or -1 }
function  readv(sock:Longint; var iov:iovec; iov_count:Longint):LONGINT; cdecl;

{ write data from iov_count number of buffers iov.
  ! return N of writed bytes, or -1 }
function  writev(sock:Longint; var iov:iovec; iov_count:Longint):LONGINT; cdecl;

{ read data + control info from socket
  ! return N of readed bytes, or 0 (closed) or -1 }
function  recvmsg(sock:Longint; var msgbuf:msghdr; flags:Longint):Longint; cdecl;

{ send data + control info to socket
  ! return N of sended bytes, or -1 }
function  sendmsg(sock:Longint; var msgbuf:msghdr; flags:Longint):Longint; cdecl;

(***************************************************************************)
(*                                                                         *)
(*                              select funcs                               *)
(*                                                                         *)
(***************************************************************************)

{ OS/2 select. 0 - timeout. -1 - err. XX - N of sockets worked. }
function  os2_select(var sockets; N_reads, N_writes, N_exepts, timeout:Longint):Longint; cdecl;

{ bsd select here. heavy voodoo.. }
function  select(nfds:Longint; var readfds,writefds,exceptfds:fd_set; var timeout:timeval):Longint; cdecl;

(***************************************************************************)
(*                                                                         *)
(*                                misc info                                *)
(*                                                                         *)
(***************************************************************************)

{ get host ip addr - addr of primary interface }
function gethostid:Longint; cdecl;

{ get connected to socket hostname }
function getpeername(sock:Longint; var s_addr:sockaddr; var s_addr_len:Longint):Longint; cdecl;

{ get local socket name }
function getsockname(sock:Longint; var s_addr:sockaddr; var s_addr_len:Longint):Longint; cdecl;

(***************************************************************************)
(*                                                                         *)
(*                             options & ioctls                            *)
(*                                                                         *)
(***************************************************************************)

{ get socket options }
function getsockopt(sock,level,optname:Longint; var buf; var buf_len:Longint):Longint; cdecl;

{ set socket options }
function  setsockopt(sock,level,optname:Longint; var buf; buf_len:Longint):Longint; cdecl;

{ f@$king ioctl. use sys/ioctl.h }
function os2_ioctl(sock,cmd:Longint; var data; data_len:Longint):Longint; cdecl;

(***************************************************************************)
(*                                                                         *)
(*     functions only for 4.1+ ip stacks (but also found in 4.02w ;))      *)
(*                                                                         *)
(***************************************************************************)


function  addsockettolist(sock:Longint):Longint; cdecl;

function  removesocketfromlist(sock:Longint):Longint; cdecl;

implementation

function  LSwap(a:Longint):Longint; assembler;
asm
      mov   eax,a
      xchg  ah,al
      ror   eax,16
      xchg  ah,al
end;

function  WSwap(a:Word):Word; assembler;
asm
      mov   ax,a
      xchg  ah,al
end;

function accept(sock:Longint; var s_addr: sockaddr; s_addr_len:Longint):Longint; cdecl; external 'SO32DLL' index 1;
function bind(sock:Longint; var s_addr: sockaddr; s_addr_len:Longint):Longint; cdecl; external 'SO32DLL' index 2;
function connect(sock:Longint; var s_addr:sockaddr; s_addr_len:Longint):Longint; cdecl; external 'SO32DLL' index 3;
function gethostid: Longint; cdecl; external 'SO32DLL' index 4;
function getpeername(sock:Longint; var s_addr:sockaddr; var s_addr_len:Longint):Longint; cdecl; external 'SO32DLL' index 5;
function getsockname(sock:Longint; var s_addr:sockaddr; var s_addr_len:Longint):Longint; cdecl; external 'SO32DLL' index 6;
function getsockopt(sock,level,optname:Longint; var buf; var buf_len:Longint):Longint; cdecl; external 'SO32DLL' index 7;
function os2_ioctl(sock,cmd:Longint; var data; data_len:Longint):Longint; cdecl; external 'SO32DLL' index 8;
function listen(sock,max_conn:Longint):Longint; cdecl; external 'SO32DLL' index 9;
function recv(sock:Longint; var buf; buf_len,flags:Longint):Longint; cdecl; external 'SO32DLL' index 10;
function  recvfrom(sock:Longint; var buf; buf_len,flags:Longint;var s_addr:sockaddr; var s_addr_len:Longint):Longint; cdecl; external 'SO32DLL' index 11;
function  os2_select(var sockets; N_reads, N_writes, N_exepts, timeout:Longint):Longint; cdecl; external 'SO32DLL' index 12;
function  send(sock:Longint; var buf; buf_len,flags:Longint):Longint; cdecl; external 'SO32DLL' index 13;
function  sendto(sock:Longint; var buf; buf_len,flags:Longint;var s_addr:sockaddr; s_addr_len:Longint):Longint; cdecl; external 'SO32DLL' index 14;
function  setsockopt(sock,level,optname:Longint; var buf; buf_len:Longint):Longint; cdecl; external 'SO32DLL' index 15;
function  socket(domain,stype,protocol:Longint):Longint; cdecl; external 'SO32DLL' index 16;
function  soclose(sock:Longint):Longint; cdecl; external 'SO32DLL' index 17;
function  so_cancel(sock:Longint):Longint; cdecl; external 'SO32DLL' index 18;
function  soabort(sock:Longint):Longint; cdecl; external 'SO32DLL' index 19;
function  sock_errno:Longint; cdecl; external 'SO32DLL' index 20;
function  recvmsg(sock:Longint; var msgbuf:msghdr; flags:Longint):Longint; cdecl; external 'SO32DLL' index 21;
function  sendmsg(sock:Longint; var msgbuf:msghdr; flags:Longint):Longint; cdecl; external 'SO32DLL' index 22;
function  readv(sock:Longint; var iov:iovec; iov_count:Longint):LONGINT; cdecl; external 'SO32DLL' index 23;
function  writev(sock:Longint; var iov:iovec; iov_count:Longint):LONGINT; cdecl; external 'SO32DLL' index 24;
function  shutdown(sock,howto:Longint):Longint; cdecl; external 'SO32DLL' index 25;
function  sock_init:Longint; cdecl; external 'SO32DLL' index 26;
function  addsockettolist(sock:Longint):Longint; cdecl; external 'SO32DLL' index 27;
function  removesocketfromlist(sock:Longint):Longint; cdecl; external 'SO32DLL' index 28;
{ entry 29 not used }
procedure psock_errno(var str:PChar); cdecl; external 'SO32DLL' index 30;
function  getinetversion(var version):Longint; cdecl; external 'SO32DLL' index 31;
function  select(nfds:Longint;
                 var readfds,writefds,exceptfds:fd_set;
                 var timeout:timeval):Longint; cdecl; external 'SO32DLL' index 32;


function  htonl(a:Longint):Longint;
begin   htonl:=LSwap(a);   end;
{ host -> network for long (4 bytes) }

function  ntohl(a:Longint):Longint;
begin   ntohl:=LSwap(a);   end;
{ network -> host for long (4 bytes) }

function  htons(a:Word):Word;
begin   htons:=WSwap(a);   end;
{ host -> network for small (2 bytes) }

function  ntohs(a:Word):Word;
begin   ntohs:=WSwap(a);   end;
{ network -> host for small (2 bytes) }

end.

(* !!TODO   Following code not revised as yet

{*
 * User-settable options (used with setsockopt).
 *}
  TCP_NODELAY    = $01;    // don't delay send to coalesce packets
  TCP_MAXSEG     = $02;    // set maximum segment size
  TCP_MSL        = $03;    // MSL HACK
  TCP_TIMESTMP   = $04;    // RFC 1323 (RTTM TimeStamp)
  TCP_WINSCALE   = $05;    // RFC 1323 (Window Scale)
  TCP_CC         = $06;    // RFC 1644 (Connection Count)


{
 * Structures returned by network data base library.  All addresses are
 * supplied in host order, and returned in network order (suitable for
 * use in system calls).
}

type

  PLongint = ^Longint;

  { struct for gethostbyname() and gethostbyaddr() }
  hostent = record
    h_name       :  PChar;       // official name of host
    h_aliases    :  ^PChar;      // alias list
    h_addrtype   :  Longint;     // host address type
    h_length     :  Longint;     // length of address
    h_addr_list  :  ^PLongint;   // list of addresses from name server
  end;

  phostent = ^hostent;

{
 * Error return codes from gethostbyname(), gethostbyaddr() and res_* funcs
 * (left in extern int h_errno).
}

const

  NETDB_INTERNAL  = -1;       // see errno
  NETDB_SUCCESS   =  0;       // no problem
  HOST_NOT_FOUND  =  1;       // Authoritative Answer Host not found
  TRY_AGAIN       =  2;       // Non-Authoritive Host not found, or SERVERFAIL
  NO_RECOVERY     =  3;       // Non recoverable errors, FORMERR, REFUSED, NOTIMP
  NO_DATA         =  4;       // Valid name, no data record of requested type
  NO_ADDRESS      =  NO_DATA; // no address, look for MX record

type

  { struct for getprotobyname() and getprotobynumber() }
  protoent = record
    p_name     :  PChar;         // official protocol name
    p_aliases  :  ^PChar;        // alias list
    p_proto    :  Longint;       // protocol #
  end;

  pprotoent = ^protoent;

type

  { struct for getservbyname() and getservbyport() }
  servent = record
    s_name     :  PChar;         // official service name
    s_aliases  :  ^PChar;        // alias list
    s_port     :  Longint;       // port # (need ntohl() !!)
    s_proto    :  PChar;         // protocol to use
  end;

  pservent = ^servent;

const
  IFF_UP                =  $1;          // interface is up
  IFF_BROADCAST         =  $2;          // broadcast address valid
  IFF_DEBUG             =  $4;          // turn on debugging
  IFF_LOOPBACK          =  $8;          // is a loopback net
  IFF_POINTOPOINT       =  $10;         // interface is point-to-point link
  IFF_LINK2             =  $20;         // was trailers, not used
  IFF_NOTRAILERS        =  IFF_LINK2;
  IFF_RUNNING           =  $40;         // resources allocated
  IFF_NOARP             =  $80;         // no address resolution protocol
  IFF_PROMISC           =  $100;        // receive all packets
  IFF_ALLMULTI          =  $200;        // receive all multicast packets
  IFF_BRIDGE            =  $1000;       // support token ring routine field
  IFF_SNAP              =  $2000;       // support extended SAP header
  IFF_DEFMTU            =  $400;        // default mtu of 1500
  IFF_RFC1469_BC        =  1;           // using broadcast
  IFF_RFC1469_FA        =  2;           // using functional
  IFF_RFC1469_MA        =  3;           // using multicast
  IFF_ETHER             =  $4000;       // Ethernet interface
  IFF_LOOPBRD           =  $8000;       // loop back broadcasts
  IFF_MULTICAST         =  $800;        // supports multicast

  IFF_SIMPLEX           =  $10000;      // can't hear own transmissions
  IFF_OACTIVE           =  $20000;      // transmission in progress
  IFF_802_3             =  $40000;
  IFF_CANONICAL         =  $80000;
  IFF_RUNNINGBLK        =  $100000;     // threads waited for intf running

  { Interface enhanced flags }
  IFFE_PKTTRACE         =  $00000001;   // trace datalink where possible
  IFFE_IPTRACE          =  $00000002;   // trace ONLY IP packets

type
  { trace buffer struct }
  pkt_trace_hdr = record
     pt_htype           :  Word;        // header type
     pt_len             :  Word;        // in: pt_buf len, out: packet len
     pt_data            :  Pointer;     // packet
     pt_tstamp          :  Longint;     // time stamp in milliseconds
  end;

const
  { physical protocols IDs }
  HT_IP                 =  $01;  // IP
  HT_ETHER              =  $06;  // Ethernet
  HT_ISO88023           =  $07;  // CSMA CD
  HT_ISO88025           =  $09;  // Token Ring
  HT_SLIP               =  $1c;  // Serial Line IP
  HT_PPP                =  $18;  // PPP IP

const
  IFNAMSIZ              =  16;   // interface name length

type
{
* Interface request structure used for socket
* ioctl's.  All interface ioctl's must have parameter
* definitions which begin with ifr_name.  The
* remainder may be interface specific.
}
  ifreq = record
     ifr_name           :  array[0..IFNAMSIZ-1] of Char;
     case Byte of
     0: (ifr_addr       :  sockaddr);  // address
     1: (ifr_dstaddr    :  sockaddr);  // other end of p-to-p link
     2: (ifr_broadaddr  :  sockaddr);  // broadcast address
     3: (ifr_flags      :  Word); // flags
     4: (ifr_metric     :  Longint);   // metric
     5: (ifr_data       :  Pointer);   // for use by interface
     6: (ifr_eflags     :  Longint);   // eflags
  end;


{
 * Structure of an internet header, naked of options.
}
type
   ip = record
      hlen_ver                         :  Byte; { lo 4 bits = header len/4
                                                  hi 4 bits = ip ver (4) }
      ip_tos                           :  Byte;      { type of service }
      ip_len                           :  Word; { total packet length }
      ip_id                            :  Word; { identification }
      ip_off                           :  Word; { fragment offset field }
      ip_ttl                           :  Byte;      { time to live }
      ip_p                             :  Byte;      { protocol (see IPPROTO_* ) }
      ip_sum                           :  Word; { header checksum }
      ip_src, ip_dst                   :  Longint;   { ip from / to addr }
   end;


{ in.h / inet.h const & func }

{
 * Protocols
}
const
     IPPROTO_IP              = 0;               { dummy for IP }
     IPPROTO_ICMP            = 1;               { control message protocol }
     IPPROTO_GGP             = 3;               { gateway^2 (deprecated) }
     IPPROTO_TCP             = 6;               { tcp }
     IPPROTO_EGP             = 8;               { exterior gateway protocol }
     IPPROTO_PUP             = 12;              { pup }
     IPPROTO_UDP             = 17;              { user datagram protocol }
     IPPROTO_IDP             = 22;              { xns idp }

     IPPROTO_RAW             = 255;             { raw IP packet }
     IPPROTO_MAX             = 256;

{
 * Ports < IPPORT_RESERVED are reserved for
 * privileged processes (e.g. root).
 * Ports > IPPORT_USERRESERVED are reserved
 * for servers, not necessarily privileged.
}
const
    IPPORT_RESERVED         = 1024;
    IPPORT_USERRESERVED     = 5000;

{
 * Link numbers
}
const
    IMPLINK_IP              = 155;
    IMPLINK_LOWEXPER        = 156;
    IMPLINK_HIGHEXPER       = 158;

{
 * Definitions of bits in internet address integers.
 * On subnets, the decomposition of addresses to host and net parts
 * is done according to subnet mask, not the masks here.
}
const
     IN_CLASSA_NET           = $ff000000;
     IN_CLASSA_NSHIFT        = 24;
     IN_CLASSA_HOST          = $00ffffff;
     IN_CLASSA_MAX           = 128;

     IN_CLASSB_NET           = $ffff0000;
     IN_CLASSB_NSHIFT        = 16;
     IN_CLASSB_HOST          = $0000ffff;
     IN_CLASSB_MAX           = 65536;

     IN_CLASSC_NET           = $ffffff00;
     IN_CLASSC_NSHIFT        = 8;
     IN_CLASSC_HOST          = $000000ff;

     INADDR_ANY              = $00000000;
     INADDR_BROADCAST        = $ffffffff;     { must be masked }
     INADDR_NONE             = $ffffffff;     { -1 return }

     IN_LOOPBACKNET          = 127;           { official! }

{
 * Socket address, internet style.
}
type
   sockaddr_in = record
      sin_family                       :  Word;
      sin_port                         :  Word; { htons first!! }
      sin_addr                         :  Longint; {in_addr; hist reasons :)) }
      sin_zero                         :  array[0..7] of Byte; {must be zero}
   end;

{ * Internet address (a structure for historical reasons) }
type
   in_addr = record
      s_addr                           :  Longint;
   end;

{*
 * Options for use with [gs]etsockopt at the IP level.
 * }
const

  IP_OPTIONS            = 1;   // buf/ip_opts; set/get IP options
  IP_MULTICAST_IF       = 2;   // u_char; set/get IP multicast i/f
  IP_MULTICAST_TTL      = 3;   // u_char; set/get IP multicast ttl
  IP_MULTICAST_LOOP     = 4;   // u_char; set/get IP multicast loopback
  IP_ADD_MEMBERSHIP     = 5;   // ip_mreq; add an IP group membership
  IP_DROP_MEMBERSHIP    = 6;   // ip_mreq; drop an IP group membership
  IP_HDRINCL            = 7;   // int; header is included with data
  IP_TOS                = 8;   // int; IP type of service and preced.
  IP_TTL                = 9;   // int; IP time to live
  IP_RECVOPTS           = 10;  // bool; receive all IP opts w/dgram
  IP_RECVRETOPTS        = 11;  // bool; receive IP opts for response
  IP_RECVDSTADDR        = 12;  // bool; receive IP dst addr w/dgram
  IP_RETOPTS            = 13;  // ip_opts; set/get IP options
  IP_RECVTRRI           = 14;  // bool; receive token ring routing inf

{*
 * Defaults and limits for options
 * }
  IP_DEFAULT_MULTICAST_TTL  = 1;    // normally limit m'casts to 1 hop
  IP_DEFAULT_MULTICAST_LOOP = 1;    // normally hear sends if a member
  IP_MAX_MEMBERSHIPS        = 20;   // per socket; must fit in one mbuf
  MAX_IN_MULTI    = 16*IP_MAX_MEMBERSHIPS;     // 320 max per os2

*)


(* List of not checked functions from SO32DLL.DLL
³ 00011 ³ RECVFROM
³ 00012 ³ SELECT
³ 00013 ³ SEND
³ 00014 ³ SENDTO
³ 00015 ³ SETSOCKOPT
³ 00016 ³ SOCKET
³ 00017 ³ SOCLOSE
³ 00018 ³ SO_CANCEL
³ 00019 ³ SOABORT
³ 00020 ³ SOCK_ERRNO
³ 00021 ³ RECVMSG
³ 00022 ³ SENDMSG
³ 00023 ³ READV
³ 00024 ³ WRITEV
³ 00025 ³ SHUTDOWN
³ 00026 ³ SOCK_INIT
³ 00027 ³ ADDSOCKETTOLIST
³ 00028 ³ REMOVESOCKETFROMLIST
³ 00030 ³ PSOCK_ERRNO
³ 00031 ³ GETINETVERSION
³ 00032 ³ BSDSELECT
³ 00035 ³ SET_ERRNO
³ 00038 ³ WINSOCKCLEANUPSOCKETS
³ 00039 ³ GETSOCKETFROMLIST
À´Done
*)
