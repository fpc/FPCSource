{ %fail }

{ Source provided for Free Pascal Bug Report 3158 }
{ Submitted by "Michalis Kamburelis" on  2004-06-11 }
{ e-mail: michalis@camelot.homedns.org }
{$R+}
{$Q+}

var A:Single;
begin
 A:=Exp(800);
 Writeln(A);
end.
