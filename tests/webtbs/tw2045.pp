{ Source provided for Free Pascal Bug Report 2045 }
{ Submitted by "Eero Tanskanen" on  2002-07-16 }
{ e-mail: yendor@nic.fi }
Unit tw2045;
Interface

  Type

    TCoords = Object

      X, Y, Z: Real;

    End;

    PEmpty = ^TEmpty;
    TEmpty = Object

    End;

    PBugger = ^TBugger;
    TBugger = Object

      A : TCoords;
      B : TCoords;
      C : TCoords;

      Empty : PEmpty;

      Function DoInternalError : PBugger;

    End;

Implementation

  Function CallForBug(A, B, C, D, E, F, G, H, I : Real) : PEmpty;
  Begin

  End;

  Function TBugger.DoInternalError : PBugger;
  Var
    TempEmpty : PEmpty;
  Begin

    DoInternalError := Nil;

    TEmpEmpty := CallForBug(DoInternalError^.A.X, DoInternalError^.A.Y, DoInternalError^.A.Z,
      DoInternalError^.B.X, DoInternalError^.B.Y, DoInternalError^.B.Z,
        DoInternalError^.C.X, DoInternalError^.C.Y,  DoInternalError^.C.Z);

  End;


Begin
End.
