{ %FAIL }
{ Old file: tbf0110.pp }
{ SigSegv when using undeclared var in Case             OK 0.99.6 (PFV) }

Begin
  Case Pai(hp1)^.typ Of
    ait_instruction:
  End
End.
