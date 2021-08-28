{ %NORUN }

{$MODESWITCH ADVANCEDRECORDS}
Program tw39310;

Type
  Rec0 = record
  end;

  Rec1 = record
    r:Rec0;
    Procedure Proc;
  end;

Procedure Rec1.Proc;
  begin
  end;

Begin
End.

