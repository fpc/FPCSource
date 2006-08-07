{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Pipes;

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
  InputStream: TInputPipeStream;
begin
  err:=true;
  InputStream:=TInputPipeStream.Create(0);
  Proc(InputStream);
  if err then
    halt(1);
end.

