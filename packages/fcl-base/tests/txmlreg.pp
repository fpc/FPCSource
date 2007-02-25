{$mode objfpc}
{$h+}
program txmlreg;

uses xmlreg;

Var
  S : String;
  C : Cardinal;
  A : Array[0..15] of byte;

begin
  S:='SomeValue <>&';
  For C:=0 to 15 do
    A[C]:=C;
  With TXmlRegistry.Create('test.xml') do
    try
      If SetKey('/my/very/nice/key',true) then
        begin
        Writeln('Set key.');
        Writeln('Writing Carinal value');
        SetValueData('Cardinal',dtdword,C,SizeOf(C));
        Writeln('Writing string value');
        SetValueData('String',dtString,S[1],length(S));
        Writeln('Writing binary value');
        SetValueData('Binary',dtBinary,A,SizeOf(A));
        SetValueData('SecondCardinal',dtdword,C,SizeOf(C));
        DeleteValue('SecondCardinal');
        end;
    finally
      Free;
    end;
end.
