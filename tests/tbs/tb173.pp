{ Old file: tbs0199.pp }
{ bugs in mul code                                       OK 0.99.11  (FK) }

PROGRAM PRTest;

TYPE
  ptRec = ^tRec;
  tRec = Record
           D : DWORD;
         END;

VAR
  pR1, pR2 : ptRec;
BEGIN
  GetMem(pR1, SizeOf(tRec));
  GetMem(pR2, SizeOf(tRec));

  pR1^.D := 10;
  Move(pR1^,pR2^,SizeOf(tRec));
  WriteLn(pR1^.D:16,pR2^.D:16);

  pR1^.D := 1;
  pR2^.D := pR1^.D*2;                   { THE BUG IS HERE }
  WriteLn(pR1^.D:16,pR2^.D:16);
  if (pR1^.D<>1) or (pR2^.D<>2) then
    Halt(1);
END.
