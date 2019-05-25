program demoio;

{$mode objfpc}
{$h+}
uses streamio, classes;

Var
  S : TStringStream;
  F : Text;
  a,b,c : Integer;

begin
  a:=1;
  b:=2;
  c:=a+b;
  S:=TStringStream.Create('');
  try
    AssignStream(F,S);
    Rewrite(F);
    Writeln(F,'Hello World !');
    Writeln(F,a:3,b:3,c:3);
    CloseFile(F);
    Writeln(S.DataString); 
  finally
    S.Free;
  end;
end.