program ie200411013;

{$mode objfpc}{$H+}

uses
  ctypes;

type
  in_addr = record
    s_bytes : array[1..4] of byte;
  end;

  sockaddr = record
    sin_family: word;
    sin_port: word;
    sin_addr: in_addr;
  end;
  TSockAddr = sockaddr;

  { TSocketStream }

  TSocketStream = class
  private
    function GetRemoteAddress: TSockAddr;
  Public
    property RemoteAddress: TSockAddr read GetRemoteAddress;
  end;

function TSocketStream.GetRemoteAddress: TSockAddr;
var
  sa: sockaddr;
begin
  sa.sin_addr.s_bytes[2]:=4;
  result:=sa;
end;

var
  ss: TSocketStream;
  b: byte;
begin
  ss:=TSocketStream.create;
  b := ss.RemoteAddress.sin_addr.s_bytes[2];
  if b<>4 then
    halt(1);
end.

