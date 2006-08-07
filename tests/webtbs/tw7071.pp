{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

var
  err : boolean;

procedure Proc(var Buf);
begin
  writeln('Proc(var Buf)');
end;

procedure Proc(Stream: TStream);
begin
  writeln('Proc(Stream: TStream)');
  err:=false;
end;

var
  InputStream: TMemoryStream;
begin
  err:=true;
  InputStream:=TMemoryStream.Create;
  Proc(InputStream);
  if err then
    halt(1);
end.

