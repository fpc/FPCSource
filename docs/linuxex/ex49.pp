Program Example49;

{ Program to demonstrate the Glob and GlobFree functions. }

Uses BaseUnix,Unix;

Var G1,G2 : PGlob;

begin
  G1:=Glob ('*');
  if fpgeterrno=0 then
    begin
    G2:=G1;
    Writeln ('Files in this directory : ');
    While g2<>Nil do
      begin
      Writeln (g2^.name);
      g2:=g2^.next;
      end;
    GlobFree (g1);
    end;
end.
