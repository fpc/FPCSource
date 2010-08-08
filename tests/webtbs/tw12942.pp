{ %skiptarget=go32v2 }

{$mode objfpc}
{$s+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  classes;

type
  tmythread = class(tthread)
    procedure execute; override;
  end;

procedure tmythread.execute;
var
  a: array[0..1024*1024-1] of byte;
begin
  fillchar(a,sizeof(a),123);
end;

var
  t: tmythread;
begin
  t:=tmythread.create(false,1024*1024+128*1024);
  t.waitfor;
  if assigned(t.fatalexception) then
    halt(1);
  t.free;
end.
