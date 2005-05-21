{ Source provided for Free Pascal Bug Report 3197 }
{ Submitted by "Martin Schreiber" on  2004-07-03 }
{ e-mail:  }
{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  Classes, SysUtils;

type
 ttestclass = class
  public
   teststring: ansistring;
   procedure test(const astring: ansistring);
 end;

{ ttestclass }

procedure ttestclass.test(const astring: ansistring);
begin
 teststring:= astring;
end;


var
 testclass: ttestclass;
begin
 testclass:= ttestclass.create;
 testclass.teststring:= 'abc';
 testclass.teststring:= testclass.teststring + '123'; //refcount 1
 testclass.test(testclass.teststring);
 if testclass.teststring = 'abc123' then begin
  writeln('ok');
 end
 else begin
  writeln('error');
  halt(1);
 end;
 testclass.free;
end.
