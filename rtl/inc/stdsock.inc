{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{$define uselibc:=cdecl; external;}

function cfpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint; uselibc
function cfpbind 	(s:cint; addrx : psockaddr; addrlen : tsocklen):cint;  uselibc
function cfpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;  uselibc
function cfpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint; uselibc
function cfpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint; uselibc
function cfpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint; uselibc
function cfplisten      (s:cint; backlog : cint):cint;                          uselibc
function cfprecv 	(s:cint; buf: pointer; len: size_t; flags: cint):ssize_t; uselibc
function cfprecvfrom    (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t; uselibc
//function cfprecvmsg     (s:cint; msg: pmsghdr; flags:cint):ssize_t; uselibc
function cfpsend 	(s:cint; msg:pointer; len:size_t; flags:cint):ssize_t; uselibc
function cfpsendto 	(s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t; uselibc
//function cfpsendmsg 	(s:cint; hdr: pmsghdr; flags:cint):ssize; uselibc
function cfpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint; uselibc
function cfpshutdown 	(s:cint; how:cint):cint; uselibc
function cfpsocket 	(domain:cint; xtype:cint; protocol: cint):cint; uselibc
function cfpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint; uselibc


function fpaccept      (s:cint; addrx : psockaddr; addrlen : psocklen):cint; 

begin
 fpaccept:=cfpaccept(s,addrx,addrlen);
end;

function fpbind (s:cint; addrx : psockaddr; addrlen : tsocklen):cint;  
begin
  fpbind:=cfpbind (s,addrx,addrlen);
end;

function fpconnect     (s:cint; name  : psockaddr; namelen : tsocklen):cint;  
begin
  fpconnect:=cfpconnect (s,name,namelen);
end;

function fpgetpeername (s:cint; name  : psockaddr; namelen : psocklen):cint; 
begin
  fpgetpeername:=cfpgetpeername (s,name,namelen);
end;

function fpgetsockname (s:cint; name  : psockaddr; namelen : psocklen):cint; 
begin
  fpgetsockname:=cfpgetsockname(s,name,namelen);
end;

function fpgetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen : psocklen):cint; 
begin
  fpgetsockopt:=cfpgetsockopt(s,level,optname,optval,optlen);
end;

function fplisten      (s:cint; backlog : cint):cint;                          
begin
  fplisten:=cfplisten(s,backlog);
end;

function fprecv 	(s:cint; buf: pointer; len: size_t; flags:cint):ssize_t; 
begin
  fprecv:= cfprecv 	(s,buf,len,flags);
end;

function fprecvfrom    (s:cint; buf: pointer; len: size_t; flags: cint; from : psockaddr; fromlen : psocklen):ssize_t; 
begin
  fprecvfrom:= cfprecvfrom (s,buf,len,flags,from,fromlen);
end;

function fpsend 	(s:cint; msg:pointer; len:size_t; flags:cint):ssize_t; 
begin
  fpsend:=cfpsend (s,msg,len,flags);
end;

function fpsendto 	(s:cint; msg:pointer; len:size_t; flags:cint; tox :psockaddr; tolen: tsocklen):ssize_t; 
begin
  fpsendto:=cfpsendto (s,msg,len,flags,tox,tolen);
end;

function fpsetsockopt  (s:cint; level:cint; optname:cint; optval:pointer; optlen :tsocklen):cint; 
begin
  fpsetsockopt:=cfpsetsockopt(s,level,optname,optval,optlen);
end;

function fpshutdown 	(s:cint; how:cint):cint; 
begin
  fpshutdown:=cfpshutdown(s,how);
end;

function fpsocket 	(domain:cint; xtype:cint; protocol: cint):cint; 
begin
  fpsocket:=cfpsocket(domain,xtype,protocol);
end;

function fpsocketpair  (d:cint; xtype:cint; protocol:cint; sv:pcint):cint; 
begin
  fpsocketpair:=cfpsocketpair(d,xtype,protocol,sv);
end;

{
  $Log$
  Revision 1.1  2004-03-16 18:04:27  marco
   * new files sockets changes


}
