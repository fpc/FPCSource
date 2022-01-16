program fpc_nestedtype_ice;

{$mode delphiunicode}

Type
 TRec = Record
  Type
   NestedType = Record
    Class Var
     FVar : Integer;
    Class Property Variable : Integer Read FVar Write FVar;
   End;
  End;

Begin
 TRec.NestedType.Variable := 1;
 if TRec.NestedType.Variable<>1 then
   halt(1);
End.
