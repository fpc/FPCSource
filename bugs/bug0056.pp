PROGRAM ShowBug;

(* This will compile
VAR N, E: Integer;*)

(* This will NOT compile*)
VAR N, E: LongInt;

BEGIN
   E := 2;
   WriteLn(E);
   N := 44 - E;
   WriteLn(N);
END.
