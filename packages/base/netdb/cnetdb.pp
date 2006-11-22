unit cNetDB;

{*-
 * Copyright (c) 1980, 1983, 1988, 1993
 *	The Regents of the University of California.  All rights reserved.
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
 *	This product includes software developed by the University of
 *	California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * -
 * Portions Copyright (c) 1993 by Digital Equipment Corporation.
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies, and that
 * the name of Digital Equipment Corporation not be used in advertising or
 * publicity pertaining to distribution of the document or software without
 * specific, written prior permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND DIGITAL EQUIPMENT CORP. DISCLAIMS ALL
 * WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS.   IN NO EVENT SHALL DIGITAL EQUIPMENT
 * CORPORATION BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL
 * DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR
 * PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS
 * ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
 * SOFTWARE.
 * -
 * --Copyright--
 *}

{*
 *      @(#)netdb.h	8.1 (Berkeley) 6/2/93
 *      From: Id: netdb.h,v 8.9 1996/11/19 08:39:29 vixie Exp $
 * $FreeBSD: src/include/netdb.h,v 1.38.2.1 2005/07/22 20:17:30 ume Exp $
 *}

{$mode objfpc}{$H+}

{$packrecords C}

interface

uses
  BaseUnix, Sockets;
  
const
  LIB_C = 'c';

const
   NETDB_INTERNAL = -(1);
{ no problem  }
   NETDB_SUCCESS = 0;
{ Authoritative Answer Host not found  }
   HOST_NOT_FOUND = 1;
{ Non-Authoritative Host not found, or SERVERFAIL  }
   TRY_AGAIN = 2;
{ Non recoverable errors, FORMERR, REFUSED, NOTIMP  }
   NO_RECOVERY = 3;
{ Valid name, no data record of requested type  }
   NO_DATA = 4;
{ no address, look for MX record  }
   NO_ADDRESS = NO_DATA;
{
 * Error return codes from getaddrinfo()
  }

   EAI_AGAIN = 2;
{ invalid value for ai_flags  }
   EAI_BADFLAGS = 3;
{ non-recoverable failure in name resolution  }
   EAI_FAIL = 4;
{ ai_family not supported  }
   EAI_FAMILY = 5;
{ memory allocation failure  }
   EAI_MEMORY = 6;

{ hostname nor servname provided, or not known  }

   EAI_NONAME = 8;
{ servname not supported for ai_socktype  }
   EAI_SERVICE = 9;
{ ai_socktype not supported  }
   EAI_SOCKTYPE = 10;
{ system error returned in errno  }
   EAI_SYSTEM = 11;
   EAI_BADHINTS = 12;
   EAI_PROTOCOL = 13;
   EAI_MAX = 14;
{
 * Flag values for getaddrinfo()
  }
{ get address to use bind()  }
   AI_PASSIVE = $00000001;
{ fill ai_canonname  }
   AI_CANONNAME = $00000002;
{ prevent host name resolution  }
   AI_NUMERICHOST = $00000004;
{ prevent service name resolution  }
   AI_NUMERICSERV = $00000008;
{ IPv6 and IPv4-mapped (with AI_V4MAPPED)  }
   AI_ALL = $00000100;
{ accept IPv4-mapped if kernel supports  }
   AI_V4MAPPED_CFG = $00000200;
{ only if any address is assigned  }
   AI_ADDRCONFIG = $00000400;
{ accept IPv4-mapped IPv6 address  }
   AI_V4MAPPED = $00000800;
{ special recommended flags for getipnodebyname  }
   AI_DEFAULT = AI_V4MAPPED_CFG or AI_ADDRCONFIG;
{ valid flags for addrinfo (not a standard def, apps should not use it)  }
   AI_MASK = AI_PASSIVE or AI_CANONNAME or AI_NUMERICHOST or AI_NUMERICSERV or AI_ADDRCONFIG;

{
 * Constants for getnameinfo()
  }
   NI_MAXHOST = 1025;
   NI_MAXSERV = 32;
{
 * Flag values for getnameinfo()
  }
   NI_NOFQDN = $00000001;
   NI_NUMERICHOST = $00000002;
   NI_NAMEREQD = $00000004;
   NI_NUMERICSERV = $00000008;
   NI_DGRAM = $00000010;
{
 * Scope delimit character
  }

const
   SCOPE_DELIMITER = '%';
   
//#define	h_addr	h_addr_list[0]	/* address, for backward compatibility */

type
{*
 * Structures returned by network data base library.  All addresses are
 * supplied in host order, and returned in network order (suitable for
 * use in system calls).
 *}
  hostent = record
    h_name: PChar;      {/* official name of host *}
    h_aliases: PPChar;  {* alias list *}
    h_addrtype: cInt;   {* host address type *}
    h_length: cInt;     {* length of address *}
    h_addr_list: PPChar;{* list of addresses from name server *}
  end;
  THostEnt = hostent;
  PHostEnt = ^THostEnt;
  PPHostEnt = ^PHostEnt;

  netent = record
    n_name: PChar;      {* official name of net *}
    n_aliases: PPChar;  {* alias list *}
    n_addrtype: cInt;   {* net address type *}
    n_net: cuInt32;     {* network # *}
  end;
  TNetEnt = netent;
  PNetEnt = ^TNetEnt;
  PPNetEnt = ^PNetEnt;

  servent = record
    s_name: PChar;    {* official service name *}
    s_aliases: PPChar;{* alias list *}
    s_port: cInt;     {* port # *}
    s_proto: PChar;   {* protocol to use *}
  end;
  TServEnt = servent;
  PServEnt = ^TServEnt;
  PPServEnt = ^PServEnt;

  protoent = record
    p_name: PChar; {* official protocol name *}
    p_aliases: PPChar;  {* alias list *}
    p_proto: cInt;      {* protocol # *}
  end;
  TProtoEnt = protoent;
  PProtoEnt = ^TProtoEnt;
  PPProtoEnt = ^PProtoEnt;

  PAddrInfo = ^addrinfo;
  addrinfo = record
    ai_flags: cInt;     {* AI_PASSIVE, AI_CANONNAME, AI_NUMERICHOST *}
    ai_family: cInt;    {* PF_xxx *}
    ai_socktype: cInt;  {* SOCK_xxx *}
    ai_protocol: cInt;  {* 0 or IPPROTO_xxx for IPv4 and IPv6 *}
    {$ifdef BSD}
    ai_addrlen: socklen_t;  {* length of ai_addr *}
    {$else} // solaris and linux has this, fix if additional platforms added
    ai_addrlen: size_t;  {* length of ai_addr *}
    {$endif}
    ai_addr: psockaddr;	   {* binary address *}
    ai_canonname: PChar;   {* canonical name for hostname *}
    ai_next: PAddrInfo;	   {* next structure in linked list *}
  end;
  TAddrInfo = addrinfo;
  PPAddrInfo = ^PAddrInfo;
   

procedure EndHostent; cdecl; external LIB_C name 'endhostent';

procedure EndNetent; cdecl; external LIB_C name 'endnetent';

procedure EndNetgrent; cdecl; external LIB_C name 'endnetgrent';

procedure EndProtoent; cdecl; external LIB_C name 'endprotoent';

procedure Endservent; cdecl; external LIB_C name 'endservent';

procedure FreeHostent(ptr: PHostEnt); cdecl; external LIB_C name 'freehostent';

function GetHostByAddr(Addr: PChar; len, Typ: cInt): PHostEnt; cdecl; external LIB_C name 'gethostbyaddr';

function GetHostByName(Host: PChar): PHostEnt; cdecl; external LIB_C name 'gethostbyname';

function GetHostByHost2(Host: PChar; af: cInt): PHostEnt; cdecl; external LIB_C name 'gethostbyname2';

function GetHostent: PHostEnt; cdecl; external LIB_C name 'gethostent';

function GetIPNodeByAddr(src: Pointer; len: size_t; af: cInt; error_num: PcInt): PHostEnt; cdecl; external LIB_C name 'getipnodebyaddr';

function GetIPNodeByName(Name: PChar; af, flags: cInt; Error_num: PcInt): PHostEnt; cdecl; external LIB_C name 'getipnodebyname';

function GetNetByAddr(Net: cint32; Typ: cInt): PNetEnt; cdecl; external LIB_C name 'getnetbyaddr';

function GetNetByName(Name: PChar): PNetEnt; cdecl; external LIB_C name 'getnetbyname';

function GetNetent: PNetEnt; cdecl; external LIB_C name 'getnetent';

function GetNetgrent(Host, User, Domain: PPChar): cInt; cdecl; external LIB_C name 'getnetgrent';

function GetProtoByName(Name: PChar): PProtoEnt; cdecl; external LIB_C name 'getprotobyname';

function GetProtoBynumber(Proto: cInt): PProtoEnt; cdecl; external LIB_C name 'getprotobynumber';

function GetProtoent: PProtoEnt; cdecl; external LIB_C name 'getprotoent';

function GetServByName(Name, Proto: PChar): PServEnt; cdecl; external LIB_C name 'getservbyname';

function GetServByport(Port: cInt; Proto: PChar): PServEnt; cdecl; external LIB_C name 'getservbyport';

function GetServent: PServEnt; cdecl; external LIB_C name 'getservent';

procedure herror(erString: PChar); cdecl; external LIB_C name 'herror';

function hstrerror(err: cInt): PChar; cdecl; external LIB_C name 'hstrerror';

function innetgr(NetGroup, Host, User, Domain: PChar): cInt; cdecl; external LIB_C name 'innetgr';

procedure SetHostent(i: cInt); cdecl; external LIB_C name 'sethostent';

{* void	SetHostfile(const char *); *}

procedure SetNetent(StayOpen: cInt); cdecl; external LIB_C name 'setnetent';

procedure SetProtoent(StayOpen: cInt); cdecl; external LIB_C name 'setprotoent';

function GetAddrInfo(HostName, ServName: PChar;
                     Hints: PAddrInfo; res: PPAddrInfo): cInt; cdecl; external LIB_C name 'getaddrinfo';
       
function GetNameInfo(sa: PSockAddr; salen: TSockLen; Host: PChar; Hostlen: TSize;
                     Serv: PChar; Servlen: TSize; Flags: cInt): cInt; cdecl; external LIB_C name 'getnameinfo';

procedure FreeAddrInfo(ai: PAddrInfo); cdecl; external LIB_C name 'freeaddrinfo';

function gai_strerror(eCode: cInt): PChar; cdecl; external LIB_C name 'gai_strerror';

procedure SetNetgrent(NetGroup: PChar); cdecl; external LIB_C name 'setnetgrent';

procedure SetServent(StayOpen: cInt); cdecl; external LIB_C name 'setservent';


implementation

end.

