{ %INTERACTIVE }
program tparam;

var
 i: integer;
Begin
  for i:=0 to ParamCount do
   Begin
     WriteLn(paramStr(i));
   End;
end.
