{ %fail }

{ Source provided for Free Pascal Bug Report 3450 }
{ Submitted by "Vincent Snijders" on  2004-12-11 }
{ e-mail: vsnijders@quicknet.nl }

{$mode objfpc}{$H+}

uses
  uw3450;

var
  A: TA;

begin
  A := TA.Create;
  // no error here
  with A do
    I := 9;
  // this fails, correctly
  A.I := 9;
end.
