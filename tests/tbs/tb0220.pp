{ %CPU=i386 }
{ %OPT= -O1 }

{ Old file: tbs0259.pp }
{ problem with optimizer for real math (use -O1)       OK 0.99.12 (PM) }
{ -O1 is not allowed for m68k }

VAR time1,time2 : Real;
BEGIN
  time1 := 0;
  time2 := time1*time1;
END.
