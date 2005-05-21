{ %recompile }

{ Source provided for Free Pascal Bug Report 2738 }
{ Submitted by "marco" on  2003-10-14 }
{ e-mail:  }
Program test87;
{$mode Delphi}


Uses uw2738;

procedure blaat;

var
  SaveExMask: T8087Exceptions;
begin
    SetMasked8087Exceptions(SaveExMask);
end;

begin
end.
