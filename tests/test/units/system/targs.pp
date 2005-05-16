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
 $Log: targs.pp,v $
 Revision 1.3  2005/02/14 17:13:37  peter
   * truncate log

}
