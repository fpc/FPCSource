{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Sockets;
Interface

{$ifdef Unix}
Uses UnixType;
{$endif}

{$macro on}
{$DEFINE FPC_NEW_SOCKETS_UNIT}
{$ifdef FPC_USE_LIBC}
{   define maybelibc:=cdecl;external;}	  // in future. Have to wrap now
{$endif}				  // because of !@$!@#% socketerror

{$define maybelibc:=}

{$i unxsockh.inc}
{$i socketsh.inc}

type
  TUnixSockAddr = packed Record
                 {$ifdef SOCK_HAS_SINLEN}
                    sa_len     : cuchar;
                 {$endif}
                  family       : sa_family_t;
	          path:array[0..107] of char;    //104 total for freebsd.
                  end;				 


{ unix socket specific functions }
Procedure Str2UnixSockAddr(const addr:string;var t:TUnixSockAddr;var len:longint);
Function Bind(Sock:longint;const addr:string):boolean;
Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:text):Boolean;
Function Connect(Sock:longint;const addr:string;var SockIn,SockOut:file):Boolean;
Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:text):Boolean;
Function Accept(Sock:longint;var addr:string;var SockIn,SockOut:File):Boolean;

//function  fpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint; maybelibc
//function  fpbind 	(s:cint; addrx : psockaddr; addrlen : tsocklen):cint;  maybelibc
//function  fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;  maybelibc

Implementation

Uses BaseUnix,{$ifndef FPC_USE_LIBC}SysCall{$else}initc{$endif};

{ Include filerec and textrec structures }
{$i filerec.inc}
{$i textrec.inc}
{******************************************************************************
                          Kernel Socket Callings
******************************************************************************}

{$ifndef FPC_USE_LIBC}
{$i unixsock.inc} 
{$else}
{$i stdsock.inc}
{$endif}
{$i sockovl.inc}
{$i sockets.inc}

end.

{
  $Log$
  Revision 1.9  2004-03-16 18:03:37  marco
   * first changes sockets units

  Revision 1.8  2003/11/25 15:13:28  marco
   * somebody added fields to socketsh.inc that were already under ifdef bsd

  Revision 1.7  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.6  2002/09/07 16:01:27  peter
    * old logs removed and tabs fixed

}
