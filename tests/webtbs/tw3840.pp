{ Source provided for Free Pascal Bug Report 3840 }
{ Submitted by "Alexey Barkovoy" on  2005-03-27 }
{ e-mail: clootie@ixbt.com }

{$mode objfpc}

const
  sConst =  'X';

procedure Do1(const w: PWideChar);
begin
  writeln(Widestring(w));
end;

begin
  Do1(sConst); //Error: Incompatible type for arg no. 1: Got "Char", expected "PWideChar"
  Do1('W');    //Error: Incompatible type for arg no. 1: Got "Char", expected "PWideChar"
  Do1('WW');   // SUCCEEDED
end.
