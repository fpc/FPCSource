{ Source provided for Free Pascal Bug Report 3467 }
{ Submitted by "Micha Nelissen" on  2004-12-24 }
{ e-mail: micha@neli.hopto.org }
program threadvartest;

{$mode objfpc}
{$H+}

uses
{$ifdef unix}
  cthreads,
{$endif}
  erroru,
  sysutils,
  classes
  ;

type
  tthread1 = class(tthread)
  public
    p : pointer;
    procedure execute; override;
  end;

  tthread2 = class(tthread)
  public
    p : pointer;
    procedure execute; override;
  end;

threadvar
  athreadvar: integer;

procedure tthread1.execute;
var
  i: integer;
begin
  writeln('thread 1 var is @', ptrint(@athreadvar));
    athreadvar := 1;
    p:=@athreadvar;
   Sleep(2000);
    for i := 0 to 100000 do
      if athreadvar <> 1 then
      begin
        writeln(athreadvar);
        error;
        break;
      end;
end;

procedure tthread2.execute;
var
  i: integer;
begin
  writeln('thread 2 var is @', ptrint(@athreadvar));
    athreadvar := 9;
    p:=@athreadvar;
   Sleep(2000);
    for i := 0 to 100000 do
      if athreadvar <> 9 then
      begin
        writeln('  ', athreadvar);
        error;
        break;
      end;
end;

var
  thread1: tthread1;
  thread2: tthread2;
begin
  thread1 := tthread1.create(false);
  thread2 := tthread2.create(false);
  thread1.waitfor;
  thread2.waitfor;
  if thread1.p=thread2.p then
    error;
end.
