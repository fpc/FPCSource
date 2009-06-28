{ Source provided for Free Pascal Bug Report 4086 }
{ Submitted by "Martin Schreiber" on  2005-06-14 }
{ e-mail:  }
program project1;
{$ifdef FPC}
{$mode objfpc}{$H+}
{$else}
{$apptype console}
{$endif}

uses
 Classes,SysUtils;

type

 itest = interface
  procedure testproc;
 end;
 
 ttestclass1 = class(tobject,itest)
  public
   function queryinterface(const guid: tguid; out obj): hresult; stdcall;
   function _addref: integer; stdcall;
   function _release: integer; stdcall;
   procedure testproc;
 end;

 ttestclass2 = class
  public
   intf: pointer;
 end;
 
{ ttestclass1 }

function ttestclass1.queryinterface(const guid: tguid; out obj): hresult; stdcall;
begin
 result:= integer(e_nointerface);
end;

function ttestclass1._addref: integer; stdcall;
begin
 writeln('addref called');
// result:= inherited _addref;
 result:= -1;
end;

function ttestclass1._release: integer; stdcall;
begin
 writeln('release called');
// result:= inherited _release;
 result:= -1;
end;

procedure ttestclass1.testproc;
begin
 writeln('testproc called');
end;

var
 po1: pointer;
 test1: ttestclass1;
 test2: ttestclass2;

procedure test;
begin
  writeln('*** global variable');
  po1:= pointer(itest(test1));
  itest(po1).testproc;
  writeln('*** object field');
  test2.intf:= pointer(itest(test1));
  itest(test2.intf).testproc;

end;

begin
  test1:= ttestclass1.create;
  test2:= ttestclass2.create;
  test;
  test1.free;
  test2.free;
end.


