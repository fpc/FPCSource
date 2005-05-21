{ %VERSION=1.1 }
{$MODE OBJFPC}
Unit ub0426;

interface

var
 z: integer platform;


procedure myroutine; platform;

procedure myroutine2; deprecated;

procedure myroutine3; unimplemented;


implementation

procedure myroutine; platform;
begin
end;

procedure myroutine2; deprecated;
begin
end;

procedure myroutine3;{$ifdef fpc}unimplemented;{$endif}
begin
end;

Begin
 myroutine;
 myroutine2;
 myroutine3;
 z:=0;
end.
