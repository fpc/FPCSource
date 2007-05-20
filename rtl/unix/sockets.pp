{
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

{$ifdef FreeBSD}
{$DEFINE SOCK_HAS_SINLEN}               // BSD definition of scoketaddr
{$endif}

{$ifdef Darwin}
{$DEFINE SOCK_HAS_SINLEN}               // BSD definition of scoketaddr
{$endif}

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
//function  fpbind      (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;  maybelibc
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
