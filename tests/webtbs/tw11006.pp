{%skiptarget=$nothread }

{$mode objfpc}

{$ifdef CPUWASM32}
  { This test runs out of memory, when using the default WebAssembly shared
    memory limit of 256 MiB, so we increase it to 512 MiB }
  {$M 1048576,536870912,536870912}
{$endif}

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
//  writeln('done');
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
