program openarrayoverload;
{$ifdef FPC}{$mode objfpc}{$h+}{$INTERFACES CORBA}{$endif}
{$ifdef mswindows}{$apptype console}{$endif}
uses
 {$ifdef FPC}{$ifdef linux}cthreads,{$endif}{$endif}
 sysutils;

type
 integerarty = array of integer;
 booleanarty = array of boolean;

function o2d(const values: array of integer): integerarty;
                                           overload;
begin
 result:= nil;
end;

function o2d(const values: array of boolean): booleanarty;
                                           overload;
begin
 result:= nil;
end;

var
 ar1: integerarty;

begin
 ar1:= o2d([127,2,3]);    // OK
 ar1:= o2d([128,2,3]);
  // openarrayoverload.pas(27,8) Error: Can't determine which overloaded function to call
end.
