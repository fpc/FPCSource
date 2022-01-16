program echoparams;

var
  I : Integer;
  
begin
  For I:=0 to ParamCount do
    Writeln(I:2,' : "',Paramstr(i),'"');
end.  