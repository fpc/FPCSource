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

  $Log: hello.pp,v $
  Revision 1.3  2005/02/14 17:13:37  peter
    * truncate log

}
