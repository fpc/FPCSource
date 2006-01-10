{ %fail }
{ %opt=-S2 }

{ Source provided for Free Pascal Bug Report 4359 }
{ Submitted by "Wolfgang Ehrhardt (via News, submitted by Marco)" on  2005-09-12 }
{ e-mail: Wolfgang.Ehrhardt@munich.netsurf.de }
{. mode objfpc}
program test;

{$ifdef FPC_OBJFPC}
{$fatal Correctly stopped at position 1}      // not triggered by -S2, but is triggered by mode objfpc
{$endif}

var
  bug: integer;

begin
end.
