Program Example46;

{ Program to demonstrate the ParamCount and ParamStr functions. }
Var
  I : Longint;

begin
  Writeln (paramstr(0),' : Got ',ParamCount,' command-line parameters: ');
  For i:=1 to ParamCount do
    Writeln (ParamStr (i));
end.
