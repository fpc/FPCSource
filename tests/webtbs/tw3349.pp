{ Source provided for Free Pascal Bug Report 3349 }
{ Submitted by "Martin Schreiber" on  2004-10-10 }
{ e-mail:  }
{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

uses
  Classes,sysutils;
type
 integerarty = array of integer;
var
 count: integer;

function getvalue: integerarty;
begin
 writeln(inttostr(count)+' getvalue called');
 inc(count);
 result:= nil;
end;

procedure proc(par: array of integer);
begin
end;

begin
  //getvalue is called twice, once for length and once for data
  proc(getvalue);
  if count<>1 then
    halt(1);
end.
