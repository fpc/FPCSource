{ %INTERACTIVE }
program targs;

var
 i: integer;
Begin
  for i:=0 to (argc-1) do
   Begin
      WriteLn(argv[i]);
   End;
end.

{
 $Log$
 Revision 1.1  2002-10-19 22:22:33  carl
   * small test for argv/argc checking

}
