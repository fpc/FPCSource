{ Source provided for Free Pascal Bug Report 3101 }
{ Submitted by "Martin Schreiber" on  2004-05-15 }
{ e-mail:  }

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  Classes;

type
 ttestobj = class
  public
   constructor create1;
   constructor create2;
   destructor destroy; override;
   procedure afterconstruction; override;
   procedure beforedestruction; override;
 end;

var
 testobj: ttestobj;
 i : integer;
 err : boolean;

procedure ttestobj.afterconstruction;
begin
 writeln('afterconstruction');
 if i<>3 then
   err:=true;
 inc(i);
end;

procedure ttestobj.beforedestruction;
begin
 writeln('beforedestruction');
 if i<>4 then
   err:=true;
 inc(i);
end;

constructor ttestobj.create1;
begin
 writeln('create1');
 if i<>1 then
   err:=true;
 inc(i);
 self.create2;
end;

constructor ttestobj.create2;
begin
 writeln('create2');
 if i<>2 then
   err:=true;
 inc(i);
end;

destructor ttestobj.destroy;
begin
 writeln('destroy');
 inherited;
 if i<>5 then
   err:=true;
 inc(i);
end;

begin
 i:=1;
 testobj:= ttestobj(ttestobj.newinstance);
 testobj.create1;
 testobj.free;
 //expected: create,afterconstruction,beforedestruction,destroy
 //actual: create,beforedestruction,destroy
 //kylix shows the expected behavior
  if err then
    halt(1);
end.
