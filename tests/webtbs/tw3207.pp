{ Source provided for Free Pascal Bug Report 3207 }
{ Submitted by "Marc Weustink" on  2004-07-11 }
{ e-mail: marc@freepascal.org }
program floatres;

{$mode objfpc}

function CheckReal: Real;
begin
  Result := 1/2;
end;

function CheckDouble: Double;
begin
  Result := 1/2;
end;

function CheckExtended: Extended;
begin
  Result := 1/2;
end;

var
  n: Integer;

begin
  for n := 1 to 1000 do CheckReal;

  for n := 1 to 1000 do CheckDouble;

  for n := 1 to 1000 do CheckExtended;
end.
