Uses Math;

Var
   I : Integer;

Begin
   For I := 0 To 359 Do
   Begin
      WriteLn( RadToDeg(ArcSin( DegToRad(Real(I)))):3:18);
   End
End.
