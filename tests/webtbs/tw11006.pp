{$mode objfpc}

uses
{$ifdef unix}
  cthreads,
{$endif}
  sysutils,
  classes;

type
  tmythread = class(tthread)
    fs: ansistring;
    constructor create(const s: ansistring);
    procedure execute; override;
  end;

constructor tmythread.create(const s: ansistring);
begin
  fs:=s+'a';
  freeonterminate:=true;
  inherited create(true);
end;

procedure tmythread.execute;
begin
  sleep(60);
  writeln('done');
end;

var
  a: array[1..100] of tmythread;
  i: longint;
begin
  for i:=low(a) to high(a) do
    a[i]:=tmythread.create('b');
  for i:=low(a) to high(a) do
    a[i].resume;
  sleep(60);
end.
