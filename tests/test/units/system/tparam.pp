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
 Revision 1.2  2002-03-05 21:52:34  carl
 + test is interactive

 Revision 1.1  2001/07/14 04:24:20  carl
 + system unit tests : paramstr() + paramcount()

}
