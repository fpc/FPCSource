{ %SKIPTARGET=$nothread }
program tclass16;

{$mode objfpc}

{$ifdef unix}
uses
  cthreads;
{$endif}

type
  TTest = class
  public class threadvar
    Test: LongInt;
  end;

function TestFunc(aData: Pointer): PtrInt;
var
  e: PRTLEvent;
begin
  e := PRTLEvent(aData);
  TTest.Test := 42;
  RTLeventSetEvent(e);
  Result := 0;
end;

var
  e: PRTLEvent;
begin
  TTest.Test := 21;
  e := RTLEventCreate;
  BeginThread(@TestFunc, e);
  RTLeventWaitFor(e);
  if TTest.Test <> 21 then
    Halt(1);
  Writeln('Ok');
end.

