TYPE  PObj = ^TObj;
      TObj = OBJECT
        ii          : INTEGER;
        CONSTRUCTOR Init(i :INTEGER);
        DESTRUCTOR  Done;
      END;

CONSTRUCTOR TObj.Init(i :INTEGER);
BEGIN
  ii := i;
END;

DESTRUCTOR TObj.Done;
BEGIN
END;

VAR   Obj  : ARRAY[1..2] OF TObj;

BEGIN
  Obj[1].Init(10);
  WITH Obj[2] DO Init(Obj[1].ii + 1); (* equal Init(0+1) = wrong *)

  Writeln;
  Writeln(Obj[1].ii:10);
  Writeln(Obj[2].ii:10);
  if Obj[2].ii<>11 then
   halt(1);

(* this should report 10 and 11, when ok *)
END.
