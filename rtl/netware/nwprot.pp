{
  Netware Server Imports for FreePascal, contains definitions for the
  netware server protocol library

  Initial Version 2003/02/23 Armin (diehl@nordrhein.de or armin@freepascal.org)

  The C-NDK and Documentation can be found here:
    http://developer.novell.com

  This program is distributed in the hope that it will be useful,but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE.

  Do not blame Novell if there are errors in this file, instead
  contact me and i will se what i can do.

  This module is untested, for the socket functions please use winsock
}

unit nwprot;

interface

{$mode objfpc}
{$packrecords C}

const
   O_RDONLY   = $0000;
   O_WRONLY   = $0001;
   O_RDWR     = $0002;
   O_ACCMODE  = $0003;
   O_APPEND   = $0010;
   O_CREAT    = $0020;
   O_TRUNC    = $0040;
   O_EXCL     = $0080;
   O_TEXT     = $0100;
   O_BINARY   = $0200;
   O_NDELAY   = $0400;
   O_NOCTTY   = $0800;
   O_NONBLOCK = O_NDELAY;


{-ip_route.h-------------------------------------------------------------------}
// dont know where the symbols came from, may be TCPIP.NLM, for now we
// define 'clib'

{ total size of an IP address in bytes  }

const
   IP_ADDR_SZ = 4;


type
   Pip_addr = ^Tip_addr;
   Tip_addr = record
     case longint of
       0 : ( ip_array : array[0..(IP_ADDR_SZ)-1] of byte );
       1 : ( ip_short : array[0..(IP_ADDR_SZ DIV 2)-1] of word );
       2 : ( ip_long  : dword );
     end;

const
   SNPA_MX = 10;   // maximum address mapping size is that largest we currently use

// Simple IP interface information block --
type
   Pip_if_info = ^Tip_if_info;
   Tip_if_info = record
     ifi_local_addr : Tip_addr;    // interface's IP address
     ifi_net_mask   : Tip_addr;    // Netmask
     ifi_broadcast  : Tip_addr;    // Broadcast
   end;

// Extended IP interface information block
   Pip_extended_if_info = ^Tip_extended_if_info;
   Tip_extended_if_info = record
     iex_signature    : dword;    // API signature
     iex_version      : dword;    // API version
     iex_length       : dword;    // bufsize
     iex_flags        : dword;
     iex_if_id        : dword;    // Interface-ID
     iex_timestamp    : dword;    // creation time
     iex_local_addr   : Tip_addr; // IP Address
     iex_net_mask     : Tip_addr; // Netmask
     iex_broadcast    : Tip_addr; // Broadcast Address
     iex_packet_mx    : dword;    // max out packet size
     iex_packet_opt   : dword;    // optimum packet size
     iex_reasm_mx     : dword;    // maximum reassembled packet
     iex_net_type     : longint;  // Network type
     iex_board_num    : dword;    // ODLI voardnumber
     iex_our_snpa     : array[0..(SNPA_MX)-1] of byte;  // SNPA for interface
   end;

function IPExtendedIFInfo(info_pt:Pip_extended_if_info):longint;cdecl;external 'clib' name 'IPExtendedIFInfo';
function IPExtendedIFInfo(var info_t:Tip_extended_if_info):longint;cdecl;external 'clib' name 'IPExtendedIFInfo';
function IPGetIFInfo(if_info_pt:Pip_if_info):longint;cdecl;external 'clib' name 'IPGetIFInfo';
function IPGetIFInfo(var if_info_t:Tip_if_info):longint;cdecl;external 'clib' name 'IPGetIFInfo';
function IPGetLocalAddr(last_addr:dword):dword;cdecl;external 'clib' name 'IPGetLocalAddr';function IPGetLocalAddrIncludingAux(last_addr:dword):dword;cdecl;external 'clib' name 'IPGetLocalAddrIncludingAux';

{-netdb.h----------------------------------------------------------------------}

// Macros mapping the standard 4.3BSD names are not implemented in pascal

{
   $Abstract:
   Standard definitions for accessing the socket interface's network
   database in Novell's NetWare 386 TCP/IP.  Since process context is
   limited in NetWare 386, we need to play some games to provide context
   to the database.
   $

   $Implementation Notes:
   The actual NetWare 386 TCP/IP routines take an additional parameter
   to provide them a block for maintaining context.  The normal routines
   are actually macros which call the context aware routines.

   One modification is required for porting an NLM the NetWare 386
   versions of the database routines: a context block must be defined.
   This is done by using the macro NETDB_DEFINE_CONTEXT in any one
   module linked into the NLM.

   If the preprocessor symbol NOT_NETWARE_386 is defined, this becomes
   the standard netdb.h from 4.3BSD for use in more typical environments.
   $

   The HOSTS database macros (i.e. gethostxxx) have the capability to
   evaluate either to the routines that access just the local /etc/hosts
   file (i.e. NWgethostxxx), or else the routines that automatically access
   a combination of local file, DNS, and NIS (i.e. NetDBgethostxxx).  The
   former case is the way previous SDK usage of the macro was implemented.
   The latter case is a newer option that utilizes network name services
   transparent to the NLM, but it requires NETDB.NLM (which is also provided
   in this SDK).  NETDB.NLM is an extension to TCP/IP and may be freely
   distributed with your product.

   The developer may choose which routines to use by directly calling the
   routines desired (either NWgethostxxx or NetDBgethostxxx).  If the macros
   are used, then the macros will call the local-file-only versions of the
   calls (i.e. NWgethostxxx) unless the symbol NETDB_USE_INTERNET is
   defined below.  If you wish to use the internet name services such as
   NIS or DNS in addition to the local hosts file for host access, then this
   symbol must be defined (either here or in your source file).
  }
{$define NETDB_USE_INTERNET}

const
   HOST_NOT_FOUND = 1;
   TRY_AGAIN      = 2;
   NO_RECOVERY    = 3;
   NO_ADDRESS     = 4;

{  Structures returned by network
   data base library.  All addresses
   are supplied in host order, and
   returned in network order (suitable
   for use in system calls). }

{  define  h_addr   h_addr_list[0] /* address, for backward compatiblity */ }
type
   Phostent = ^Thostent;
   Thostent = record
     h_name      : Pchar;    // official name of host
     h_aliases   : ^Pchar;   // alias list
     h_addrtype  : longint;
     h_length    : longint;  // length of address
     h_addr_list : ^Pchar;   // list of addresses from name server
   end;

// Assumption here is that a network number fits in 32 bits -- probably a poor one.
   Pnetent = ^Tnetent;
   Tnetent = record
     n_name     : Pchar;    // official name of net
     n_aliases  : ^Pchar;   // alias list
     n_addrtype : longint;
     n_net      : dword;
     n_mask     : dword;    // Netmask, Novell extension
   end;

   Pservent = ^Tservent;
   Tservent = record
     s_name    : Pchar;        // official service name
     s_aliases : ^Pchar;       // alias list
     s_port    : longint;      // portnumber
     s_proto   : Pchar;        // protocol to use
   end;

   Pprotoent = ^Tprotoent;
   Tprotoent = record
     p_name    : Pchar;     // official protocol name
     p_aliases : ^Pchar;
     p_proto   : longint;
   end;

// var h_errno : longint;cvar;external;

    const
       SCRATCHBUFSIZE = 1024;
       MAXALIASES = 10;
       MAXALIASSIZE = 64;
       MAXNAMESIZE = 64;
       MAXADDRSIZE = 19;
       MAXHOSTADDR = 1;

    { Special Novell structure for providing context in the otherwise
      context-free NetWare 386 environment. The applications SHOULD NOT
      access this structure ! }
    type
       Pnwsockent = ^Tnwsockent;
       Tnwsockent = record
            nse_hostctx  : pointer;  // PFILE;
            nse_netctx   : pointer;  // PFILE;
            nse_protoctx : pointer;  // PFILE;
            nse_servctx  : pointer;  // PFILE;
            nse_h_errno  : longint;
            nse_sockent_un : record
                case longint of
                   0 : ( nsu_hst   : Thostent );
                   1 : ( nsu_net   : Tnetent );
                   2 : ( nsu_proto : Tprotoent );
                   3 : ( nsu_serv  : Tservent );
                end;
            nse_scratch : array[0..(SCRATCHBUFSIZE)-1] of char;
         end;
    { Declare the context block.  The client must supply the actual
      block by placing NETDB_DEFINE_CONTEXT in one of the C modules
      in the link. }
//      var nwSocketCtx : longint;cvar;external;

    { ------------------------------------------------------------------------
                            Host file examination
       ------------------------------------------------------------------------  }
{ Local-file-only routines  }

function NWgethostbyname(nwsktctx:Pnwsockent; name:Pchar):Phostent;cdecl;external {'tcpip'} name 'NWgethostbyname';
function NWgethostbyname(var nwsktctx:Tnwsockent; name:Pchar):Phostent;cdecl;external {'tcpip'} name 'NWgethostbyname';

function NWgethostbyaddr(nwsktctx:Pnwsockent; addr:Pchar; length:longint; _type:longint):Phostent;cdecl;external {'tcpip'} name 'NWgethostbyaddr';
function NWgethostbyaddr(var nwsktctx:Tnwsockent; addr:Pchar; length:longint; _type:longint):Phostent;cdecl;external {'tcpip'} name 'NWgethostbyaddr';

function NWgethostent(nwsktctx:Pnwsockent):Phostent;cdecl;external {'tcpip'} name 'NWgethostent';
function NWgethostent(var nwsktctx:Tnwsockent):Phostent;cdecl;external {'tcpip'} name 'NWgethostent';

procedure NWsethostent(nwsktctx:Pnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NWsethostent';
procedure NWsethostent(var nwsktctx:Tnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NWsethostent';

procedure NWendhostent(nwsktctx:Pnwsockent);cdecl;external {'tcpip'} name 'NWendhostent';
procedure NWendhostent(var nwsktctx:Tnwsockent);cdecl;external {'tcpip'} name 'NWendhostent';
    { Internet Name Service routines  }
    {
       NetDBgethostbyname() -- returns the host entry (struct hostent  ) given
        the name of a host.

        The local file sys:/etc/hosts is consulted first to see if the entry
        exists there.  If so, then that is returned.  If not, then if DNS is
        installed on the machine, it will be consulted to perform the lookup.
        If the host still is not found, then NIS will be consulted if at all
        possible.

        This function returns NULL when an error occurs.  The integer
        nwsktent->nse_h_errno can be checked to determine the nature of the
        error.

        The integer nwsktent->nse_h_errno can have the following values:

          HOST_NOT_FOUND       No such host exists.

        If the NetDBgethostbyname function succeeds, it will return a pointer
        to a structure of type struct hostent.

        Syntax:
          struct hostent   NetDBgethostbyname(struct nwsockent  nwsktent,
                                              char  name);

             nwskent: Points to a context block.

             name:    Official name of the host.

        Returns:
          A pointer to the appropriate struct hostent if any that matches.
          NULL if no match found.
                                                                              }
function NetDBgethostbyname(nwskent:Pnwsockent; name:Pchar):Phostent;cdecl;external {'tcpip'} name 'NetDBgethostbyname';
function NetDBgethostbyname(var nwskent:Tnwsockent; name:Pchar):Phostent;cdecl;external {'tcpip'} name 'NetDBgethostbyname';
    {
       NetDBgethostbyaddr() -- returns the host entry (struct hostent  ) given
        the address of a host.

        The local file sys:/etc/hosts is consulted first to see if the entry
        exists there.  If so, then that is returned.  If not, then if DNS is
        installed on the machine, it will be consulted to perform the lookup.
        If the host still is not found, then NIS will be consulted if at all
        possible.

        This function returns NULL when an error occurs.  The integer
        nwsktent->nse_h_errno can be checked to determine the nature of the
        error.

        The integer nwsktent->nse_h_errno can have the following values:

          HOST_NOT_FOUND       No such host exists.

        If the NetDBgethostbyaddr function succeeds, it will return a pointer
        to a structure of type struct hostent.

        Syntax:
          struct hostent   NetDBgethostbyaddr(struct nwsockent  nwskent,
                                              char  addr, int len, int type);

             nwsktent: (Input) Points to a context block.

             addr:     (Input) Internet address of the host.

             len:      (Input) Length of the Internet address, in bytes.

             type:     (Input) Value corresponding to the type of Internet
                       address.  Currently, the type is always AF_INET.

        Returns:
          A pointer to the appropriate struct hostent if any that matches.
          NULL if no match found.
                                                                              }
function NetDBgethostbyaddr(nwsktent:Pnwsockent; addr:Pchar; len:longint; _type:longint):Phostent;cdecl;external {'tcpip'} name 'NetDBgethostbyaddr';
function NetDBgethostbyaddr(var nwsktent:Tnwsockent; addr:Pchar; len:longint; _type:longint):Phostent;cdecl;external {'tcpip'} name 'NetDBgethostbyaddr';
    {
       NetDBgethostent() -- returns the next sequential entry from the
        SYS:ETC/HOSTS file, opening the file it it is not already open.  Once
        the local file is depleted, all of the NIS host entries will be
        retrieved until those are depleted.

        Note that there may be duplicate entries in the local and NIS databases.
        The caller should handle these appropriately.

        This function returns NULL when an error occurs.  The integer
        nwsktent->nse_h_errno can be checked to determine the nature of the
        error.

        The integer nwsktent->nse_h_errno can have the following values:

          HOST_NOT_FOUND       No more hosts exist in either SYS:ETC/HOSTS or
                               NIS.

        Syntax:
          struct hostent   NetDBgethostent(struct nwsockent  nwsktent,
                                           short   ploc);

          nwsktent:  (Input) Points to a context block.

          ploc:      (Output) If non-NULL, this short will indicate if this
                     entry is from the local sys:etc/hosts file (NETDB_LOC_LOCAL)
                     or from the NIS database (NETDB_LOC_NIS).

                     Pass in NULL if you're not interested in this information.

        Returns:
          A pointer to the next host entry if the function is successful.
          NULL if no more entries or an error occurred.
                                                                              }
function NetDBgethostent(nwsktent:Pnwsockent; ploc:Psmallint):Phostent;cdecl;external {'tcpip'} name 'NetDBgethostent';
function NetDBgethostent(var nwsktent:Tnwsockent; ploc:Psmallint):Phostent;cdecl;external {'tcpip'} name 'NetDBgethostent';
    {
       NetDBsethostent() -- rewinds the SYS:ETC/HOSTS file if the file is
        already open.  This call guarantees that the next call to
        NetDBgethostent() will return the FIRST record in the local hosts file,
        regardless of whether the LAST call returned an entry from the local
        file or from NIS.

        If the stayopen flag is set (nonzero), the SYS:ETC/HOSTS file is NOT
        closed after each call made to NetDBgethostbyname() or
        NetDBgethostbyaddr().

        Syntax:
          void NetDBsethostent(struct nwsockent  nwsktent, int stayopen);

          nwsktent:  (Input) Points to a context block.

          stayopen:  (Input) If nonzero, causes SYS:ETC/HOSTS to remain open
                     after a call to NetDBgethostbyname() or
                     NetDBgethostbyaddr().

        Returns:
          Nothing.
                                                                              }
procedure NetDBsethostent(nwsktent:Pnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NetDBsethostent';
procedure NetDBsethostent(var nwsktent:Tnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NetDBsethostent';
    {
       NetDBendhostent() -- closes the SYS:ETC/HOSTS file.  Also ends access
        to the NIS database.  After this call, the next call to
        NetDBgethostent() will be from the beginning of the local file again.

        Syntax:
          void NetDBendhostent(struct nwsockent  nwsktent);

          nwsktent:  (Input) Points to a context block.

        Returns:
          Nothing.
                                                                              }
procedure NetDBendhostent(nwsktent:Pnwsockent);cdecl;external {'tcpip'} name 'NetDBendhostent';
procedure NetDBendhostent(var nwsktent:Tnwsockent);cdecl;external {'tcpip'} name 'NetDBendhostent';
    {
       NetDBgethostname() -- this gets the current machine's host name into the
        passed in buffer (if it is large enough).

        This will use the local hosts file if it exists, otherwise it will then
        try both DNS and NIS if available in order to get the official name of
        our own machine.

        Syntax:
          int NetDBgethostname(struct nwsockent  nwsktent, char  name,
                                                                int namelen);

          nwsktent: (Input)  Points to a context block.

          name:     (Output) Official name of the host.

          namelen:  (Input)  Specifies the size of the array pointed to by name.

        Returns:
           0: The call succeeded.
          -1: The call failed.
                                                                              }
function NetDBgethostname(nwsktent:Pnwsockent; name:Pchar; namelen:longint):longint;cdecl;external {'tcpip'} name 'NetDBgethostname';
function NetDBgethostname(var nwsktent:Tnwsockent; name:Pchar; namelen:longint):longint;cdecl;external {'tcpip'} name 'NetDBgethostname';

// Network file examination
function NWgetnetbyname(nwsktctx:Pnwsockent; name:Pchar):Pnetent;cdecl;external {'tcpip'} name 'NWgetnetbyname';
function NWgetnetbyname(var nwsktctx:Tnwsockent; name:Pchar):Pnetent;cdecl;external {'tcpip'} name 'NWgetnetbyname';

function NWgetnetbyaddr(nwsktctx:Pnwsockent; net:longint; _type:longint):Pnetent;cdecl;external {'tcpip'} name 'NWgetnetbyaddr';
function NWgetnetbyaddr(var nwsktctx:Tnwsockent; net:longint; _type:longint):Pnetent;cdecl;external {'tcpip'} name 'NWgetnetbyaddr';

function NWgetnetent(nwsktctx:Pnwsockent):Pnetent;cdecl;external {'tcpip'} name 'NWgetnetent';
function NWgetnetent(var nwsktctx:Tnwsockent):Pnetent;cdecl;external {'tcpip'} name 'NWgetnetent';

procedure NWsetnetent(nwsktctx:Pnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NWsetnetent';
procedure NWsetnetent(var nwsktctx:Tnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NWsetnetent';

procedure NWendnetent(nwsktctx:Pnwsockent);cdecl;external {'tcpip'} name 'NWendnetent';
procedure NWendnetent(var nwsktctx:Tnwsockent);cdecl;external {'tcpip'} name 'NWendnetent';

// Service file examination
function NWgetservbyname(nwsktctx:Pnwsockent; name:Pchar; protocol:Pchar):Pservent;cdecl;external {'tcpip'} name 'NWgetservbyname';
function NWgetservbyname(var nwsktctx:Tnwsockent; name:Pchar; protocol:Pchar):Pservent;cdecl;external {'tcpip'} name 'NWgetservbyname';

function NWgetservbyport(nwsktctx:Pnwsockent; port:longint; protocol:Pchar):Pservent;cdecl;external {'tcpip'} name 'NWgetservbyport';
function NWgetservbyport(var nwsktctx:Tnwsockent; port:longint; protocol:Pchar):Pservent;cdecl;external {'tcpip'} name 'NWgetservbyport';

function NWgetservent(nwsktctx:Pnwsockent):Pservent;cdecl;external {'tcpip'} name 'NWgetservent';
function NWgetservent(var nwsktctx:Tnwsockent):Pservent;cdecl;external {'tcpip'} name 'NWgetservent';

procedure NWsetservent(nwsktctx:Pnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NWsetservent';
procedure NWsetservent(var nwsktctx:Tnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NWsetservent';

procedure NWendservent(nwsktctx:Pnwsockent);cdecl;external {'tcpip'} name 'NWendservent';
procedure NWendservent(var nwsktctx:Tnwsockent);cdecl;external {'tcpip'} name 'NWendservent';

// Protocol file examination
function NWgetprotobyname(nwsktctx:Pnwsockent; name:Pchar):Pprotoent;cdecl;external {'tcpip'} name 'NWgetprotobyname';
function NWgetprotobyname(var nwsktctx:Tnwsockent; name:Pchar):Pprotoent;cdecl;external {'tcpip'} name 'NWgetprotobyname';

function NWgetprotobynumber(nwsktctx:Pnwsockent; protocol:longint):Pprotoent;cdecl;external {'tcpip'} name 'NWgetprotobynumber';
function NWgetprotobynumber(var nwsktctx:Tnwsockent; protocol:longint):Pprotoent;cdecl;external {'tcpip'} name 'NWgetprotobynumber';

function NWgetprotoent(nwsktctx:Pnwsockent):Pprotoent;cdecl;external {'tcpip'} name 'NWgetprotoent';
function NWgetprotoent(var nwsktctx:Tnwsockent):Pprotoent;cdecl;external {'tcpip'} name 'NWgetprotoent';

procedure NWsetprotoent(nwsktctx:Pnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NWsetprotoent';
procedure NWsetprotoent(var nwsktctx:Tnwsockent; stayopen:longint);cdecl;external {'tcpip'} name 'NWsetprotoent';

procedure NWendprotoent(nwsktctx:Pnwsockent);cdecl;external {'tcpip'} name 'NWendprotoent';
procedure NWendprotoent(var nwsktctx:Tnwsockent);cdecl;external {'tcpip'} name 'NWendprotoent';

function gethostname(name:Pchar; namelen:longint):longint;cdecl;external {'tcpip'} name 'gethostname';
function gethostid:longint;cdecl;external {'tcpip'} name 'gethostid';
{-tiuser.h---------------------------------------------------------------------}
const
   EAGAIN = -(1);
{ Error values  }
   TACCES = 1;
   TBADADDR = 2;
   TBADDATA = 3;
   TBADF = 4;
   TBADFLAG = 5;
   TBADOPT = 6;
   TBADSEQ = 7;
   TBUFOVFLW = 8;
   TFLOW = 9;
   TLOOK = 10;
   TNOADDR = 11;
   TNODATA = 12;
   TNOREL = 13;
   TNOTSUPPORT = 14;
   TOUTSTATE = 15;
   TSTATECHNG = 16;
   TSYSERR = 17;
   TNOUDERR = 18;
   TNODIS = 19;
   TNOSTRUCTYPE = 20;
   TBADNAME = 21;
   TBADQLEN = 22;
   TADDRBUSY = 23;
{ t_look events  }
   _T_LISTEN = 1;
   _T_CONNECT = 2;
   _T_DATA = 3;
   _T_EXDATA = 4;
   _T_DISCONNECT = 5;
   _T_ORDREL = 6;
   _T_ERROR = 7;
   _T_UDERR = 8;
   _T_GODATA = 9;
   _T_GOEXDATA = 10;
   _T_EVENTS = 11;
{ Flag definitions  }
   _T_EXPEDITED = $01;
   _T_MORE = $02;
   _T_NEGOTIATE = $04;
   _T_CHECK = $08;
   _T_DEFAULT = $10;
   _T_SUCCESS = $20;
   _T_FAILURE = $40;

var t_errno : longint;cvar;external;

    type
       Pt_info = ^Tt_info;
       Tt_info = record
         addr     : longint;
         options  : longint;
         tsdu     : longint;
         etsdu    : longint;
         connect  : longint;
         discon   : longint;
         servtype : longint;
       end;

    { Service types  }
    { Connection-mode service  }

    const
       T_COTS = 1;
    { Connection service with orderly release  }
       T_COTS_ORD = 2;
    { Connectionless-mode service  }
       T_CLTS = 3;
    type
       Pnetbuf = ^Tnetbuf;
       Tnetbuf = record
         maxlen : dword;
         len    : dword;
         buf    : Pchar;
       end;

       Pt_bind = ^Tt_bind;
       Tt_bind = record
         addr : Tnetbuf;
         qlen : dword;
       end;

       Pt_optmgmt = ^Tt_optmgmt;
       Tt_optmgmt = record
         opt   : Tnetbuf;
         flags : longint;
       end;

       Pt_discon = ^Tt_discon;
       Tt_discon = record
         udata    : Tnetbuf;
         reason   : longint;
         sequence : longint;
       end;

       Pt_call = ^Tt_call;
       Tt_call = record
         addr     : Tnetbuf;
         opt      : Tnetbuf;
         udata    : Tnetbuf;
         sequence : longint;
       end;

       Pt_unitdata = ^Tt_unitdata;
       Tt_unitdata = record
         addr  : Tnetbuf;
         opt   : Tnetbuf;
         udata : Tnetbuf;
       end;

       Pt_uderr = ^Tt_uderr;
       Tt_uderr = record
         addr  : Tnetbuf;
         opt   : Tnetbuf;
         error : longint;
       end;

    // t_alloc structure types, had to prefix with _ because some
    // names conflict with functions

    const
       _T_BIND = $1;
       _T_CALL = $2;
       _T_OPTMGMT = $4;
       _T_DIS = $8;
       _T_UNITDATA = $10;
       _T_UDERROR = $20;
       _T_INFO = $40;
    { XTI names for t_alloc structure types  }
       _T_BIND_STR = _T_BIND;
       _T_OPTMGMT_STR = _T_OPTMGMT;
       _T_CALL_STR = _T_CALL;
       _T_DIS_STR = _T_DIS;
       _T_UNITDATA_STR = _T_UNITDATA;
       _T_UDERROR_STR = _T_UDERROR;
       _T_INFO_STR = _T_INFO;
    { t_alloc field identifiers  }
       _T_ADDR = $1000;
       _T_OPT = $2000;
       _T_UDATA = $4000;
       _T_ALL = $8000;
    { State values  }
    { added to match xti state tables  }
       _T_UNINIT = 0;
    { unbound  }
       _T_UNBND = 1;
    { idle  }
       _T_IDLE = 2;
    { outgoing connection pending  }
       _T_OUTCON = 3;
    { incoming connection pending  }
       _T_INCON = 4;
    { data transfer  }
       _T_DATAXFER = 5;
    { outgoing orderly release  }
       _T_OUTREL = 6;
    { incoming orderly release  }
       _T_INREL = 7;
    { general purpose defines  }
       _T_YES = 1;
       _T_NO = 0;
       _T_UNUSED = -(1);
       _T_NULL = 0;
       _T_ABSREQ = $8000;
      var
         t_errlist : array of Pchar;cvar;external;
         t_nerr : longint;cvar;external;
    {---------------------TCP specific Options-------------------------- }
    { TCP Precedence Levels  }

    const
       _T_ROUTINE = 0;
       _T_PRIORITY = 1;
       _T_IMMEDIATE = 2;
       _T_FLASH = 3;
       _T_OVERRIDEFLASH = 4;
       _T_CRITIC_ECP = 5;
       _T_INETCONTROL = 6;
       _T_NETCONTROL = 7;

    type
       Psecoptions = ^Tsecoptions;
       Tsecoptions = record
         security    : smallint;
         compartment : smallint;
         handling    : smallint;
         tcc         : longint;
       end;

       Ptcp_options = ^Ttcp_options;
       Ttcp_options = record
         precedence   : smallint;      // TCP options
         timeout      : longint;       // abort timeout
         max_seg_size : longint;
         secopt       : Tsecoptions;   // security options
       end;


function t_accept(fd:longint; resfd:longint; call:Pt_call):longint;cdecl;external 'tli' name 't_accept';
function t_alloc(fd:longint; struct_type:longint; fields:longint):Pchar;cdecl;external 'tli' name 't_alloc';
function t_bind(fd:longint; req:Pt_bind; ret:Pt_bind):longint;cdecl;external 'tli' name 't_bind';
function t_blocking(fd:longint):longint;cdecl;external 'tli' name 't_blocking';
function t_close(fd:longint):longint;cdecl;external 'tli' name 't_close';
function t_connect(fd:longint; sndcall:Pt_call; rcvcall:Pt_call):longint;cdecl;external 'tli' name 't_connect';
procedure t_error(errmsg:Pchar);cdecl;external 'tli' name 't_error';
function t_free(ptr:Pchar; struct_type:longint):longint;cdecl;external 'tli' name 't_free';
function t_getinfo(fd:longint; info:Pt_info):longint;cdecl;external 'tli' name 't_getinfo';
function t_getstate(fd:longint):longint;cdecl;external 'tli' name 't_getstate';
function t_listen(fd:longint; call:Pt_call):longint;cdecl;external 'tli' name 't_listen';
function t_look(fd:longint):longint;cdecl;external 'tli' name 't_look';
function t_nonblocking(fd:longint):longint;cdecl;external 'tli' name 't_nonblocking';
function t_open(path:Pchar; oflag:longint; info:Pt_info):longint;cdecl;external 'tli' name 't_open';
function t_optmgmt(fd:longint; req:Pt_optmgmt; ret:Pt_optmgmt):longint;cdecl;external 'tli' name 't_optmgmt';
function t_rcv(fd:longint; buf:Pchar; nbytes:dword; flags:Plongint):longint;cdecl;external 'tli' name 't_rcv';
function t_rcvconnect(fd:longint; call:Pt_call):longint;cdecl;external 'tli' name 't_rcvconnect';
function t_rcvdis(fd:longint; discon:Pt_discon):longint;cdecl;external 'tli' name 't_rcvdis';
function t_rcvrel(fd:longint):longint;cdecl;external 'tli' name 't_rcvrel';
function t_rcvudata(fd:longint; unitdata:Pt_unitdata; flags:Plongint):longint;cdecl;external 'tli' name 't_rcvudata';
function t_rcvuderr(fd:longint; uderr:Pt_uderr):longint;cdecl;external 'tli' name 't_rcvuderr';
function t_snd(fd:longint; buf:Pchar; nbytes:dword; flags:longint):longint;cdecl;external 'tli' name 't_snd';
function t_snddis(fd:longint; call:Pt_call):longint;cdecl;external 'tli' name 't_snddis';
function t_sndrel(fd:longint):longint;cdecl;external 'tli' name 't_sndrel';
function t_sndudata(fd:longint; unitdata:Pt_unitdata):longint;cdecl;external 'tli' name 't_sndudata';
function t_sync(fd:longint):longint;cdecl;external 'tli' name 't_sync';
function t_unbind(fd:longint):longint;cdecl;external 'tli' name 't_unbind';

// havent found the declaration for __get_t_errno_ptr, hope that is correct:
function __get_t_errno_ptr:plongint; cdecl;external 'clib' name '__get_t_errno_ptr';

function t_error : longint;

{-ositli.h---------------------------------------------------------------------}
const
   TPDR_NORMAL = 128 + 0;
   TPDR_CRCONG = 128 + 1;
   TPDR_CONNEG = 128 + 2;
   TPDR_DUPSR = 128 + 3;
   TPDR_MMREF = 128 + 4;
   TPDR_PE = 128 + 5;
   TPDR_REOVFL = 128 + 7;
   TPDR_NWREF = 128 + 8;
   TPDR_INVHD = 128 + 10;
   TPDR_RNS = 0;
   TPDR_CONG = 1;
   TPDR_NOSESS = 2;
   TPDR_UNKADDR = 3;   // Address unknown

// Options management pre-defined values.
   T_YES = 1;
   T_NO = 0;
   T_UNUSED = -(1);
   T_NULL = 0;
   T_ABSREQ = $8000;
   T_PRIDFLT = 4;
   T_PRILOW = 3;
   T_PRIMID = 2;
   T_PRIHIGH = 1;
   T_PRITOP = 0;
   T_NOPROTECT = 1;
   T_PASSIVEPROTECT = 2;
   T_ACTIVEPROTECT = 4;
   T_LTPDUDFLT = 2048;
   T_CLASS0 = 0;
   T_CLASS1 = 1;
   T_CLASS2 = 2;
   T_CLASS3 = 3;
   T_CLASS4 = 4;

 // Options Management structures.
type
   Prate = ^Trate;
   Trate = record
     targetvalue    : longint;   // target value
     minacceptvalue : longint;   // minimum acceptable value
   end;

   Preqvalue = ^Treqvalue;
   Treqvalue = record
     called  : Trate;    // called rate
     calling : Trate;    // calling rate
   end;

   Pthrpt = ^Tthrpt;
   Tthrpt = record
     maxthrpt : Treqvalue;   // maximum throughput
     avgthrpt : Treqvalue;   // average throughput
   end;

   Pmanagement = ^Tmanagement;
   Tmanagement = record
     dflt      : smallint;
     ltpdu     : longint;
     reastime  : smallint;
     _class    : char;
     altclass  : char;
     extform   : char;
     flowctrl  : char;
     checksum  : char;
     netexp    : char;
     netrecptcf: char;
   end;

// Connection oriented options.
   Pisoco_options = ^Tisoco_options;
   Tisoco_options = record
     throughput     : Tthrpt;
     transdel       : Treqvalue;
     reserrorrate   : Trate;
     transffailprob : Trate;
     estfailprob    : Trate;
     relfailprob    : Trate;
     estdelay       : Trate;
     reldelay       : Trate;
     connresil      : Tnetbuf;
     protection     : word;
     priority       : smallint;
     mngmt          : Tmanagement;   // management parameters
     expd           : char;          // expedited data: T_YES or T_NO
   end;

// Connectionless options.
   Pisocl_options = ^Tisocl_options;
   Tisocl_options = record
     transdel     : Trate;     // transit delay
     reserrorrate : Trate;     // residual error rate
     protection   : word;
     priority     : smallint;
   end;

// Novell connectionless options.
   Pnovell_isocl_options = ^Tnovell_isocl_options;
   Tnovell_isocl_options = record
     transdel     : Trate;     // transit delay
     reserrorrate : Trate;     // residual error rate
     protection   : word;
     priority     : smallint;
     checksum     : longint;
   end;
{-param.h----------------------------------------------------------------------}
const
   HZ     = 18;
   NULL   = 0;
   PZERO  = 20;
   PCATCH = $8000;
{-poll.h-----------------------------------------------------------------------}
const
   NPOLLFILE = 65535;
   POLLIN    =  1;
   POLLPRI   =  2;
   POLLOUT   =  4;
   POLLERR   = 10;
   POLLHUP   = 20;
   POLLNVAL  = 40;
{ array of streams to poll  }
{ Internal "fd" for the benefit of the kernel  }
type
   Ppollfd = ^Tpollfd;
   Tpollfd = record
     fd      : longint;
     events  : smallint;
     revents : smallint;
     _ifd    : longint;
   end;

{ I_POLL structure for ioctl on non-5.3 systems  }
   Pstrpoll = ^Tstrpoll;
   Tstrpoll = record
     nfds    : dword;
     pollfdp : Ppollfd;
     timeout : longint;
   end;

function poll(const fds:array of Tpollfd; nfds:dword; timeout:longint):longint;cdecl;external 'clib' name 'poll';
{-proc.h-----------------------------------------------------------------------}
type
   Pproc = ^Tproc;
   Tproc = record
     p_pid  : smallint;
     p_pgrp : smallint;
   end;
{-strlog.h---------------------------------------------------------------------}
const
   SL_FATAL  = $1;
   SL_NOTIFY = $2;
   SL_ERROR  = $4;
   SL_TRACE  = $8;
   I_TRCLOG  = 1;
   I_ERRLOG  = 2;
   LOGMSGSZ  = 128;

type
   Plog_ctl = ^Tlog_ctl;
   Tlog_ctl = record
     mid   : smallint;
     sid   : smallint;
     level : char;
     flags : smallint;
     ltime : longint;
     ttime : longint;
     seq_no: longint;
   end;

   Ptrace_ids = ^Ttrace_ids;
   Ttrace_ids = record
     ti_mid   : smallint;
     ti_sid   : smallint;
     ti_level : char;
     ti_flags : smallint;
   end;
{-strstat.h--------------------------------------------------------------------}
type
   Pmodule_stat = ^Tmodule_stat;
   Tmodule_stat = record
     ms_pcnt : longint;
     ms_scnt : longint;
     ms_ocnt : longint;
     ms_ccnt : longint;
     ms_acnt : longint;
     ms_xptr : Pchar;
     ms_xsize: smallint;
   end;
{-user.h-----------------------------------------------------------------------}
type
   Puser = ^Tuser;
   Tuser = record
     u_error : longint;
     u_uid   : longint;
     u_gid   : longint;
     u_ruid  : longint;
     u_rgid  : longint;
     u_ttyp  : Psmallint;
     u_procp : Pproc;
   end;
{-stream.h---------------------------------------------------------------------}
type
   Pmodule_info = ^Tmodule_info;
   Tmodule_info = record
     mi_idnum  : word;
     mi_idname : Pchar;
     mi_minpsz : smallint;
     mi_maxpsz : smallint;
     mi_hiwat  : smallint;
     mi_lowat  : smallint;
   end;

   Pqinit = ^Tqinit;
   Tqinit = record
     qi_putp   : function :longint;cdecl;
     qi_srvp   : function :longint;
     qi_qopen  : function :longint;
     qi_qclose : function :longint;
     qi_qadmin : function :longint;
     qi_minfo  : Pmodule_info;
     qi_mstat  : Pmodule_stat;
   end;

   Pdatab = ^Tdatab;
   Tdatab = record
     db_freep  : Pdatab;
     db_base   : Pbyte;
     db_lim    : Pbyte;
     db_ref    : byte;
     db_type   : byte;
     db_class  : byte;
     db_pad    : array[0..0] of byte;
   end;

   Tdblk_t = Tdatab;

type
   Pmsgb = ^Tmsgb;
   Tmsgb = record
     b_next  : Pmsgb;   // next message on queue
     b_prev  : Pmsgb;   // previous message on queue
     b_cont  : Pmsgb;   // next message block of message
     b_rptr  : PChar;   // first unread data byte in buffer
     b_wptr  : PChar;   // first unwritten data byte
     b_datap : Pdatab;  // data block
   end;

   Tmblk_t = Tmsgb;
   Pmblk_t = Pmsgb;

   Pq_xtra = pointer;  // dont know where this is defined

   Pqueue = ^Tqueue;
   Tqueue = record
     q_qinfo : Pqinit;
     q_first : Pmsgb;
     q_last  : Pmsgb;
     q_next  : Pqueue;
     q_link  : Pqueue;
     q_ptr   : Pchar;
     q_count : byte;  //ushort;
     q_flag  : byte;  // ushort;
     q_minpsz: smallint;
     q_maxpsz: smallint;
     q_hiwat : byte;  // ushort;
     q_lowat : byte;  // ushort;
     q_osx   : Pq_xtra;
     q_ffcp  : Pqueue;
     q_bfcp  : Pqueue;
   end;

   Tqueue_t = Tqueue;
   Pqueue_t = Pqueue;
{ Q state defines  }

const
   F_Q_IS_WRITE_Q = $1;
   F_Q_DISABLED = $2;
   F_Q_FULL = $4;
   F_Q_TO_SCHEDULE = $8;
   F_Q_PUT_STOPPED = $10;
   F_Q_WELDED = $20;
   F_Q_SEQUENT_SYNCH = $40;
{ Q state defines for 5.4 compatibility  }
   QREADR = $80;
   QFULL = F_Q_FULL;
   QENAB = F_Q_TO_SCHEDULE;
{ Used in M_IOCTL mblks to muxes (ioc_cmd I_LINK)  }
{ lowest level write queue of upper stream  }
{ highest level write queue of lower stream  }
{ system-unique index for lower stream  }
type
   Plinkblk = ^Tlinkblk;
   Tlinkblk = record
     l_qtop  : Pqueue_t;
     l_qbot  : Pqueue_t;
     l_index : longint;
   end;

{ Message types  }

const
   QNORM = 0;
{ Ordinary data  }
   M_DATA = 0;
{ Internal control info and data  }
   M_PROTO = 1;
{ Request a driver to send a break  }
   M_BREAK = 010;
{ Used to pass a file pointer  }
   M_PASSFP = 011;
{ Requests a signal to be sent  }
   M_SIG = 013;
{ Request a real-time delay  }
   M_DELAY = 014;
{ For inter-module communication  }
   M_CTL = 015;
{ Used internally for I_STR requests  }
   M_IOCTL = 016;
{ Alters characteristics of stream head  }
   M_SETOPTS = 020;
{ Priority messages types  }
   QPCTL = 0200;
{ Positive ack of previous M_IOCTL  }
   M_IOCACK = 0201;
{ Previous M_IOCTL failed  }
   M_IOCNAK = 0202;
{ Same as M_PROTO except for priority }
   M_PCPROTO = 0203;
{ Priority signal  }
   M_PCSIG = 0204;
{ Requests modules to flush queues  }
   M_FLUSH = 0206;
{ Request drivers to stop output  }
   M_STOP = 0207;
{ Request drivers to start output  }
   M_START = 0210;
{ Driver can no longer produce data  }
   M_HANGUP = 0211;
{ Reports downstream error condition  }
   M_ERROR = 0212;
{ Reports client read at stream head  }
   M_READ = 0213;
{ PSE-private type; high priority data  }
   M_HPDATA = 0214;
   FLUSHALL = 1;
   FLUSHDATA = 0;

type
   Piocblk = ^Tiocblk;
   Tiocblk = record
     ioc_cmd   : longint;
     ioc_uid   : word;
     ioc_gid   : word;
     ioc_id    : dword;
     ioc_count : dword;
     ioc_error : longint;
     ioc_rval  : longint;
   end;

   Pstrpfp = ^Tstrpfp;
   Tstrpfp = record
     pass_file_cookie : dword;
     pass_uid : word;
     pass_gid : word;
     pass_sth : pointer;
   end;

   Pstroptions = ^Tstroptions;
   Tstroptions = record
     so_flags   : smallint;
     so_readopt : smallint;
     so_wroff   : word;
     so_minpsz  : smallint;
     so_maxpsz  : smallint;
     so_hiwat   : word;
     so_lowat   : word;
   end;

const
   SO_ALL = 0377;
   SO_READOPT = 01;
   SO_WROFF = 02;
   SO_MINPSZ = 04;
   SO_MAXPSZ = 010;
   SO_HIWAT = 020;
   SO_LOWAT = 040;
   SO_MREADON = 0100;
   SO_MREADOFF = 0200;
   BPRI_LO = 1;
   BPRI_MED = 2;
   BPRI_HI = 3;
   INFPSZ = -(1);

const
   MAXMSGSIZE = 4096;
   OPENFAIL = -(1);
   CLONEOPEN = $2;
   MODOPEN = $1;
   NSTREVENT = 40;
   STRMSGSZ = MAXMSGSIZE;
   STRCTLSZ = 1024;
   STRLOFRAC = 80;
   STRMEDFRAC = 90;
   MAXBSIZE = MAXMSGSIZE;

type TFuncLongCdecl = function : longint; cdecl;

function allocb(size:longint; pri:longint):Pmblk_t;cdecl;external 'streams' name 'allocb';
function allocq:Pqueue_t;cdecl;external 'streams' name 'allocq';
function adjmsg(mp:Pmblk_t; len_param:longint):longint;cdecl;external 'streams' name 'adjmsg';
function backq(q:Pqueue_t):Pqueue_t;cdecl;external 'streams' name 'backq';
function bufcall(size:longint; pri:longint; func:TFuncLongCdecl; arg:longint):longint;cdecl;external 'streams' name 'bufcall';
procedure bcopy(src:Pchar; dst:Pchar; len:longint);cdecl;external 'streams' name 'bcopy';
procedure bzero(buffer:Pchar; nbytes:longint);cdecl;external 'streams' name 'bzero';
function canput(q:Pqueue_t):longint;cdecl;external 'streams' name 'canput';
function copyb(mp:Pmblk_t):Pmblk_t;cdecl;external 'streams' name 'copyb';
function copymsg(mp:Pmblk_t):Pmblk_t;cdecl;external 'streams' name 'copymsg';
function dupb(bp:Pmblk_t):Pmblk_t;cdecl;external 'streams' name 'dupb';
function dupmsg(mp:Pmblk_t):Pmblk_t;cdecl;external 'streams' name 'dupmsg';
function flushq(q:Pqueue_t; flag:longint):longint;cdecl;external 'streams' name 'flushq';
function freeb(bp:Pmblk_t):longint;cdecl;external 'streams' name 'freeb';
function freemsg(mp:Pmblk_t):longint;cdecl;external 'streams' name 'freemsg';
function freeq(q:Pqueue_t):longint;cdecl;external 'streams' name 'freeq';
function getq(q:Pqueue_t):Pmblk_t;cdecl;external 'streams' name 'getq';
function insq(q:Pqueue_t; emp:Pmblk_t; nmp:Pmblk_t):longint;cdecl;external 'streams' name 'insq';
function linkb(mp1:Pmblk_t; mp2:Pmblk_t):longint;cdecl;external 'streams' name 'linkb';
function msgdsize(mp:Pmblk_t):longint;cdecl;external 'streams' name 'msgdsize';
function pullupmsg(mp:Pmblk_t; len:longint):longint;cdecl;external 'streams' name 'pullupmsg';
function putbq(q:Pqueue_t; mp:Pmblk_t):longint;cdecl;external 'streams' name 'putbq';
function putctl(q:Pqueue_t; _type:longint):longint;cdecl;external 'streams' name 'putctl';
function putctl1(q:Pqueue_t; _type:longint; c:longint):longint;cdecl;external 'streams' name 'putctl1';
function putq(q:Pqueue_t; mp:Pmblk_t):longint;cdecl;external 'streams' name 'putq';
function qenable(q:Pqueue_t):longint;cdecl;external 'streams' name 'qenable';
function qreply(q:Pqueue_t; mp:Pmblk_t):longint;cdecl;external 'streams' name 'qreply';
function qsize(q:Pqueue_t):longint;cdecl;external 'streams' name 'qsize';
function rmvb(mp:Pmblk_t; bp:Pmblk_t):Pmblk_t;cdecl;external 'streams' name 'rmvb';
function rmvq(q:Pqueue_t; mp:Pmblk_t):longint;cdecl;external 'streams' name 'rmvq';
function strlog(sid:smallint; mid:smallint; level:char; aflags:smallint; args:array of const):longint;cdecl;external 'streams' name 'strlog';
function strlog(sid:smallint; mid:smallint; level:char; aflags:smallint):longint;cdecl;external 'streams' name 'strlog';
function testb(size:longint; pri:longint):longint;cdecl;external 'streams' name 'testb';
function timeout(func:TFuncLongCdecl; arg:pointer; ticks:longint):longint;cdecl;external 'streams' name 'timeout';
function unlinkb(mp:Pmblk_t):Pmblk_t;cdecl;external 'streams' name 'unlinkb';
function unbufcall(id:longint):longint;cdecl;external 'streams' name 'unbufcall';
{-tispxipx.h-------------------------------------------------------------------}
type
   Pipxaddr_s = ^Tipxaddr_s;
   Tipxaddr_s = record
     ipxa_net    : array[0..3] of byte;
     ipxa_node   : array[0..5] of byte;
     ipxa_socket : array[0..1] of byte;
   end;
   TIPX_ADDR = Tipxaddr_s;
   PIPX_ADDR = ^TIPX_ADDR;

   Pipxopt_s = ^Tipxopt_s;
   Tipxopt_s = record
     ipx_type : byte;
     ipx_pad1 : array[0..2] of byte;
     ipx_hops : byte;
     ipx_pad2 : array[0..2] of byte;
   end;
   TIPX_OPTS = Tipxopt_s;
   PIPX_OPTS = ^TIPX_OPTS;

   Pspxopt_s = ^Tspxopt_s;
   Tspxopt_s = record
     spx_connectionID     : array[0..1] of byte;
     spx_allocationNumber : array[0..1] of byte;
     spx_pad1             : array[0..3] of byte;
   end;
   TSPX_OPTS = Tspxopt_s;
   PSPX_OPTS = ^TSPX_OPTS;

   Pspx_optmgmt = ^Tspx_optmgmt;
   Tspx_optmgmt = record
     spxo_retry_count     : byte;
     spxo_watchdog_flag   : byte;
     spxo_min_retry_delay : dword;
     spxo_pad2            : array[0..1] of byte;
   end;

const
   OPTIONS_VERSION = 1;

function OPTIONS_SIZE : longint;

type
   Pspx2_options = ^Tspx2_options;
   Tspx2_options = record
     versionNumber          : dword;
     spxIIOptionNegotiate   : dword;
     spxIIRetryCount        : dword;
     spxIIMinimumRetryDelay : dword;
     spxIIMaximumRetryDelta : dword;
     spxIIWatchdogTimeout   : dword;
     spxIIConnectTimeout    : dword;
     spxIILocalWindowSize   : dword;
     spxIIRemoteWindowSize  : dword;
     spxIIConnectionID      : dword;
     spxIIInboundPacketSize : dword;
     spxIIOutboundPacketSize: dword;
     spxIISessionFlags      : dword;
   end;

const
   SPX_WATCHDOG_OFF = 0;
   SPX_WATCHDOG_ON =  not (SPX_WATCHDOG_OFF);
   SPX_WATCHDOG_DEFAULT = SPX_WATCHDOG_ON;
   SPX_RETRY_MIN = 3;
   SPX_RETRY_MAX = 50;
   SPX_RETRY_DEFAULT = 10;
   SPX_WATCHDOG_TIMEOUT_MIN = 3000;
   SPX_WATCHDOG_TIMEOUT_MAX = 300000;
   SPX_WATCHDOG_TIMEOUT_DEFAULT = 60000;
   SPX_MIN_RETRY_DELAY_MIN = 1;
   SPX_MIN_RETRY_DELAY_MAX = 60000;
   SPX_MIN_RETRY_DELAY_DEFAULT = 0;
   SPX_MAX_RETRY_DELTA_MIN = 1000;
   SPX_MAX_RETRY_DELTA_MAX = 60000;
   SPX_MAX_RETRY_DELTA_DEFAULT = 5000;
   SPX_OPTION_NEGOTIATE_OFF = 0;
   SPX_OPTION_NEGOTIATE_ON =  not (SPX_OPTION_NEGOTIATE_OFF);
   SPX_OPTION_NEGOTIATE_DEFAULT = SPX_OPTION_NEGOTIATE_ON;
   SPX_CONNECT_TIMEOUT_MIN = 1000;
   SPX_CONNECT_TIMEOUT_MAX = 120000;
   SPX_CONNECT_TIMEOUT_DEFAULT = 0;
   SPX_LOCAL_WINDOW_SIZE_MIN = 1;
   SPX_LOCAL_WINDOW_SIZE_MAX = 8;
   SPX_LOCAL_WINDOW_SIZE_DEFAULT = 0;
   SPX2_SF_NONE = $00;
   SPX2_SF_IPX_CHECKSUM = $01;
   SPX2_SF_SPX2_SESSION = $02;
   TLI_SPX_CONNECTION_FAILED = $ed;
   TLI_SPX_CONNECTION_TERMINATED = $ec;
   TLI_SPX_MALFORMED_PACKET = $fe;
   TLI_SPX_PACKET_OVERFLOW = $fd;
   TLI_SPX_UNREACHABLE_DEST = $70;
   TLI_IPX_MALFORMED_ADDRESS = $fe;
   TLI_IPX_PACKET_OVERFLOW = $fd;

{-in.pp------------------------------------------------------------------------}
const
   IPPROTO_IP = 0;
   IPPROTO_ICMP = 1;
   IPPROTO_IGMP = 2;
   IPPROTO_GGP = 3;
   IPPROTO_TCP = 6;
   IPPROTO_EGP = 8;
   IPPROTO_PUP = 12;
   IPPROTO_UDP = 17;
   IPPROTO_IDP = 22;
   IPPROTO_ND = 77;
   IPPROTO_RAW = 255;
   IPPROTO_MAX = 256;

// Port/socket numbers: network standard functions
   IPPORT_ECHO = 7;
   IPPORT_DISCARD = 9;
   IPPORT_SYSTAT = 11;
   IPPORT_DAYTIME = 13;
   IPPORT_NETSTAT = 15;
   IPPORT_FTP = 21;
   IPPORT_TELNET = 23;
   IPPORT_SMTP = 25;
   IPPORT_TIMESERVER = 37;
   IPPORT_NAMESERVER = 42;
   IPPORT_WHOIS = 43;
   IPPORT_MTP = 57;

// Port/socket numbers: host specific functions
   IPPORT_TFTP = 69;
   IPPORT_RJE = 77;
   IPPORT_FINGER = 79;
   IPPORT_TTYLINK = 87;
   IPPORT_SUPDUP = 95;

// UNIX TCP sockets
   IPPORT_EXECSERVER = 512;
   IPPORT_LOGINSERVER = 513;
   IPPORT_CMDSERVER = 514;
   IPPORT_EFSSERVER = 520;

// UNIX UDP sockets
   IPPORT_BIFFUDP = 512;
   IPPORT_WHOSERVER = 513;
{ 520+1 also used  }
   IPPORT_ROUTESERVER = 520;

   IPPORT_RESERVED = 1024;
   IPPORT_USERRESERVED = 5000;

type
   Pin_addr = ^Tin_addr;
   Tin_addr = record
     s_addr : dword;
   end;

const
   IN_CLASSA_NET = $ff000000;
   IN_CLASSA_NSHIFT = 24;
   IN_CLASSA_HOST = $00ffffff;
   IN_CLASSA_MAX = 128;
   IN_CLASSB_NET = $ffff0000;
   IN_CLASSB_NSHIFT = 16;
   IN_CLASSB_HOST = $0000ffff;
   IN_CLASSB_MAX = 65536;
   IN_CLASSC_NET = $ffffff00;
   IN_CLASSC_NSHIFT = 8;
   IN_CLASSC_HOST = $000000ff;
   IN_LOOPBACKNET = 127;

// var sin_port : word;cvar;public;
//   sin_zero : array[0..7] of char;cvar;public;

    const
       IP_OPTIONS = 1;

function ntohs(value:word):word;cdecl;external {'tcpip'} name 'ntohs';
function htons(value:word):word;cdecl;external {'tcpip'} name 'htons';
function ntohl(value:dword):dword;cdecl;external {'tcpip'} name 'ntohl';
function htonl(value:dword):dword;cdecl;external {'tcpip'} name 'htonl';
{------------------------------------------------------------------------------}

implementation

function t_error : longint;
begin
  t_error := __get_t_errno_ptr^;
end;

function OPTIONS_SIZE : longint;
begin
  OPTIONS_SIZE:=13 * (sizeof(longint));
end;


end.
