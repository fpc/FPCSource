program targs;

var
 i: integer;
Begin
  for i:=0 to (argc-1) do
   Begin
      WriteLn(argv[i]);
   End;
end.
