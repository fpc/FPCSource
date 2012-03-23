{ from gpc testsuite (martin3.pas) }

Program PackedAssignTest;
Uses uprec6;

Var I,J:Integer;
    APackedBoolean:TPackedBoolean;
Begin
// writeln(sizeof(APackedBoolean[0])); Index is 1..5 range
 writeln(sizeof(APackedBoolean[1]));
// writeln(ptruint(@APackedBoolean[1])-ptruint(@APackedBoolean[0]));
// writeln(ptruint(@APackedBoolean[2])-ptruint(@APackedBoolean[1]));
// writeln(ptruint(@APackedBoolean[3])-ptruint(@APackedBoolean[2]));
 for I := 1 to MaxA do
   for J := 1 to MaxB do
     APackedBoolean[I, J] := J = I + 1;
 ARecord.C:=99;
 ARecord.D:=100;
 ARecord.PackedBoolean:=APackedBoolean;
 for I := 1 to MaxA do
   for J := 1 to MaxB do
     if ARecord.PackedBoolean[I, J] <> (J = I + 1) then
       Begin
         WriteLn ('failed ', I, ' ', J);
         Halt(1);
       end;
 if (ARecord.D = 100) and (ARecord.C = 99) then WriteLn ('OK') else begin WriteLn ('failed 2'); halt(2) end;
end.
