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
  Revision 1.1  2001-12-10 02:25:33  carl
  + test file for tdos

}  