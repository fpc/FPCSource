{ %OPT=-Sew }

{ Source provided for Free Pascal Bug Report 2438 }
{ Submitted by "Armin Diehl" on  2003-03-25 }
{ e-mail: armin@freepascal.org }
{$P+}

procedure int_str(var s : string);
begin
  s:='-2147483648';
end;

begin
end.
