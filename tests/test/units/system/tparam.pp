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
 $Log: tparam.pp,v $
 Revision 1.4  2005/02/14 17:13:37  peter
   * truncate log

}
