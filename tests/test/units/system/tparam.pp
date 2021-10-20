program tparam;

var
 i: integer;
Begin
  for i:=0 to ParamCount do
   Begin
     WriteLn(paramStr(i));
   End;
  for i:=-127 to -1 do
    if paramStr(i)<>'' then
      halt(1);
  writeln('ok')
end.
