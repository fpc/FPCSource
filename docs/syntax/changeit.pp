program changeit;

uses sysutils;

var
  S : String;

begin
  While not eof do
    begin
    ReadLn(S);
    S:=StringReplace(S,'\[','\begin{stack}\\',[rfReplaceAll]);
    S:=StringReplace(S,'\]','\end{stack}',[rfReplaceAll]);
    S:=StringReplace(S,'\(','\begin{stack}',[rfReplaceAll]);
    S:=StringReplace(S,'\)','\end{stack}',[rfReplaceAll]);
    S:=StringReplace(S,'\<','\begin{rep}',[rfReplaceAll]);
    S:=StringReplace(S,'\>','\end{rep}',[rfReplaceAll]);
    Writeln(S);
    end;
end.
 