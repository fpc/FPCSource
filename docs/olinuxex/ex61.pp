Program ex61;

{ Example program to demonstrate the CreateShellArgV function }

uses oldlinux;

Var
  S: String;
  PP : PPchar;
   I : longint;

begin
  S:='script -a -b -c -d -e fghijk';
  PP:=CreateShellArgV(S);
  I:=0;
  If PP<>Nil then
    While PP[i]<>Nil do
      begin
      Writeln ('Got : "',PP[i],'"');
      Inc(i);
      end;
end.