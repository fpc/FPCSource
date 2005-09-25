{ Source provided for Free Pascal Bug Report 3499 }
{ Submitted by "Bart Tierens" on  2005-01-02 }
{ e-mail: svg@deds.nl }
program test;

{$ifdef fpc}{$mode delphi}{$endif}

uses SysUtils;

var
  err : boolean;

type
  TProcedure = procedure of object;
  Class1 = class
  public
    proc: TProcedure;
    procedure p(const aproc: TProcedure);
  end;
  Class2 = class
  public
    procedure d();
  end;

procedure Class1.p(const aproc: TProcedure);
begin
  proc := aproc;
end;

procedure Class2.d();
begin
  writeLn('procedure called');
  err:=false;
end;

var
  c: Class1;
  e: Class2;
begin
  err:=true;
  c := Class1.create();
  e := Class2.create();
  c.p(e.d);
  c.proc;
  if err then
    halt(1);
end.
