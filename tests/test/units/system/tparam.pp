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

{
 $Log$
 Revision 1.3  2002-09-07 15:40:56  peter
   * old logs removed and tabs fixed

 Revision 1.2  2002/03/05 21:52:34  carl
 + test is interactive

}
