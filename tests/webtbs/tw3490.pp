{ %OPT=-Sew -vw }

{ Source provided for Free Pascal Bug Report 3490 }
{ Submitted by "Tomas Hajny" on  2004-12-29 }
{ e-mail:  }
var
 A: byte;
 B: cardinal;

begin
 A := 3;
 B := 0;
 B := B + A * A;
end.
