{ Old file: tbs0244.pp }
{ nested procedures can't have same name as global ones (same as tbs0237) OK 0.99.13 (PM) }

Unit tb0208;

{test also with -So !!!}

Interface

Procedure t(a,b: longint);

Implementation

Procedure t(a,b: longint);
begin
end;

Procedure t2;

  Procedure t(l: Longint);
  Begin
  End;

Begin
End;

End.
