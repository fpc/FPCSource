{ %opt=-Sen -vn }

Var B:Longint;

Procedure Stuff;
Var I:Longint;
Begin
   Case B Of
      0:For I:=1 To 100 Do WriteLn;
   End;
End;

Begin
   B:=0;
   Stuff;
End.
