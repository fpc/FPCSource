{ Test program to be used by tdos for testing exec }
{ command.                                         }
var
 i: integer;
Begin
 WriteLn('Hello world. With parameters...');
 for i:=0 to paramcount do
   begin
     WriteLn(ParamStr(i));
   end;
 RunError(213);
end.

{

  $Log$
  Revision 1.2  2002-09-07 15:40:56  peter
    * old logs removed and tabs fixed

}
