{ Source provided for Free Pascal Bug Report 3184 }
{ Submitted by "Martin Schreiber" on  2004-06-25 }
{ e-mail:  }
unit uw3184b;

{$ifdef fpc}{$mode objfpc}{$H+}{$endif}

interface
uses
 uw3184a;

type
 tclass2 = class(tclass1)
  public
   function proc2:longint;
 end;

implementation

function tclass2.proc2:longint;
begin
 result:=proc1;
end;

end.
