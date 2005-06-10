{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 Soren Ager

    Implementation of TCP/IP name resolution for OS/2.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  { Net type }
  socklib = 'TCP32DLL';
  AF_INET = 2;

  { Error constants. Returned by LastError method of THost, TNet}

  NETDB_INTERNAL= -1;       { see errno }
  NETDB_SUCCESS = 0;        { no problem }
  HOST_NOT_FOUND= 1;        { Authoritative Answer Host not found }
  TRY_AGAIN     = 2;        { Non-Authoritive Host not found, or SERVERFAIL }
  NO_RECOVERY   = 3;        { Non recoverable errors, FORMERR, REFUSED, NOTIMP }
  NO_DATA       = 4;        { Valid name, no data record of requested type }
  NO_ADDRESS    = NO_DATA;  { no address, look for MX record }


Type

  { THostEnt Object }
  THostEnt = record
    H_Name     : pchar;   { Official name }
    H_Aliases  : ppchar;  { Null-terminated list of aliases}
    H_Addrtype : longint;   { Host address type }
    H_length  : longint;   { Length of address }
    H_Addr : ppchar;    { null-terminated list of adresses }
  end;
  PHostEntry = ^THostEnt;

  { TNetEnt object }
  TNetEnt = record
    N_Name     : pchar;   { Official name }
    N_Aliases  : ppchar;  { Nill-terminated alias list }
    N_AddrType : longint; { Net address type }
    N_net      : Cardinal; { Network number }
  end;
  PNetEntry = ^TNetEnt;

  TServEnt = record
    s_name    : pchar;    { Service name }
    s_aliases : ppchar;   { Null-terminated alias list }
    s_port    : longint;  { Port number }
    s_proto   : pchar;    { Protocol to use }
  end;
  PServEntry = ^TServEnt;


function gethostent : PHostEntry; cdecl; external socklib index 30;
procedure sethostent (stayopen : longint); cdecl; external socklib index 28;
procedure endhostent; cdecl; external socklib index 29;

function getnetent : PNetEntry; cdecl; external socklib index 17;
procedure setnetent ( Stayopen : Longint);  cdecl; external socklib index 15;
procedure endnetent; cdecl; external socklib index 16;

function getservent : PServEntry; cdecl; external socklib index 27;
procedure setservent (StayOpen : longint); cdecl; external socklib index 25;
procedure endservent; cdecl; external socklib index 26;

function getnetbyaddr ( Net : Longint; nettype : Longint) : PNetEntry; cdecl; external socklib index 14;
function gethostbyname ( Name : Pchar) : PHostEntry; cdecl; external socklib index 11;
function gethostbyaddr ( Addr : PChar; Len : Longint; HType : Longint) : PHostentry ; cdecl; external socklib index 12;
function getnetbyname ( Name : pchar) : PNetEntry; cdecl; external socklib index 13;
function getservbyname (name : pchar  ; protocol : pchar) : PServEntry; cdecl; external socklib index 24;
function getservbyport (port : longint; protocol : pchar) : PServEntry; cdecl; external socklib index 23;

function  GetDNSError : LongInt;
begin
  GetDNSError:=0;   //!!! fpgetCerrno;
end;

Function InitResolve : Boolean;
begin
  Result:=True;
end;

Function FinalResolve : Boolean;
begin
  Result:=True;
end;
