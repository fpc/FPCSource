{ %target=win32,linux,freebsd }

{ Source provided for Free Pascal Bug Report 2963 }
{ Submitted by "Claus Färber" on  2004-02-09 }
{ e-mail: claus@faerber.muc.de }
const
{$ifdef win32}
  dllname='ws2_32.dll';
{$else}
  dllname='c';
{$endif}

type
  addrinfo = array[0..255] of byte;
  paddrinfo = ^addrinfo;
  ppaddrinfo = ^paddrinfo;

function getaddrinfo(nodename, servname: PChar; hints: paddrinfo; res: ppaddrinfo): integer; overload; stdcall; external dllname;
function getaddrinfo(nodename, servname: PChar; hints: paddrinfo; var res: paddrinfo): integer; overload; stdcall; external dllname;
function getaddrinfo(nodename, servname: PChar; const hints: addrinfo; res: ppaddrinfo): integer; overload; stdcall; external dllname;
function getaddrinfo(nodename, servname: PChar; const hints: addrinfo; var res: paddrinfo): integer; overload; stdcall; external dllname;

procedure p;
var
  hints : addrinfo;
  res   : paddrinfo;
begin
  getaddrinfo('localhost','ftp',hints,res);
end;

begin
end.
