{ Source provided for Free Pascal Bug Report 2481 }
{ Submitted by "Eero Tanskanen" on  2003-05-04 }
{ e-mail: yendor@nic.fi }

{$mode objfpc}

Type
  TMoo = Class

    A : Real;
    Constructor Init;

  End;

Var
  Moo : TMoo;

  Operator := (V : Real) B : TMoo;
  Begin
    B:=TMoo.Create;
    B.A := V;
  End;

Constructor TMoo.Init;
Begin

  A := 0;

End;

Begin

  Moo := TMoo.Init;
  Moo := 0.2;

End.
