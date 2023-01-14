{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2022 by Michael van Canney and other members of the
    Free Pascal development team

    *nix parts of the resolver

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
uses
  initc;

const
  { Net type }
  socklib = 'c';
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
    H_Name     : PAnsiChar;   { Official name }
    H_Aliases  : PPAnsiChar;  { Null-terminated list of aliases}
    H_Addrtype : longint;   { Host address type }
    H_length  : longint;   { Length of address }
    H_Addr : PPAnsiChar;    { null-terminated list of adresses }
  end;
  PHostEntry = ^THostEnt;

  { TNetEnt object }
  TNetEnt = record
    N_Name     : PAnsiChar;   { Official name }
    N_Aliases  : PPAnsiChar;  { Nill-terminated alias list }
    N_AddrType : longint; { Net address type }
    N_net      : Cardinal; { Network number }
  end;
  PNetEntry = ^TNetEnt;

  TServEnt = record
    s_name    : PAnsiChar;    { Service name }
    s_aliases : PPAnsiChar;   { Null-terminated alias list }
    s_port    : longint;  { Port number }
    s_proto   : PAnsiChar;    { Protocol to use }
  end;
  PServEntry = ^TServEnt;

{ C style calls, linked in from Libc }

function gethostent : PHostEntry; cdecl; external socklib;
procedure sethostent (stayopen : longint); cdecl; external socklib;
procedure endhostent; cdecl; external socklib;

function getnetent : PNetEntry; cdecl; external socklib;
procedure setnetent ( Stayopen : Longint);  cdecl; external socklib;
procedure endnetent; cdecl; external socklib;

function getservent : PServEntry; cdecl; external socklib;
procedure setservent (StayOpen : longint); cdecl; external socklib;
procedure endservent; cdecl; external socklib;

function getnetbyaddr ( Net : Longint; nettype : Longint) : PNetEntry; cdecl; external socklib;
function gethostbyname ( Name : PAnsiChar) : PHostEntry; cdecl; external socklib;
function gethostbyaddr ( Addr : PAnsiChar; Len : Longint; HType : Longint) : PHostentry ; cdecl; external socklib;
function getnetbyname ( Name : PAnsiChar) : PNetEntry; cdecl; external socklib;
function getservbyname (name : PAnsiChar  ; protocol : PAnsiChar) : PServEntry; cdecl; external socklib;
function getservbyport (port : longint; protocol : PAnsiChar) : PServEntry; cdecl; external socklib;

function  GetDNSError : libcint;

begin
  GetDNSError:=fpgetCerrno;
end;

Function InitResolve : Boolean;

begin
  Result:=True;
end;

Function FinalResolve : Boolean;

begin
  Result:=True;
end;
