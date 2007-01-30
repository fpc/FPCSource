{ %FAIL }
{ Old file: tbf0320.pp }
{  }

{$ifdef fpc}{$mode delphi}{$endif}

{ These should give an error, as also done in tp,delphi.
  See tbs/tb0273.pp for a test with class which should compile in
  delphi mode }

type
  cl=object
    k : longint;
    procedure p1;
    procedure p2;
  end;

procedure cl.p1;
var
  k : longint;
begin
end;

procedure cl.p2;
var
  p1 : longint;
begin
end;

begin
end.
