{

  Program to test set functions
}

{$define FPC_HAS_SET_INEQUALITIES}

program tset1;

{$ifdef cpujvm}
uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{$macro on}
{$define writeln:=jlsystem.fout.println}
{$define write:=jlsystem.fout.println}
{$endif}


Procedure InitMSTimer;
begin
end;


{Get MS Timer}
Function MSTimer:longint;
begin
  MSTimer:=0;
end;


const
  Lval=2000;
VAR Box1, Box2:         ARRAY [0..255] OF BYTE;
    OneWOTwo, TwoWOOne,
    UnionSet, InterSet,
    Set1, Set2, Set3:   SET OF BYTE;
    K, MaxNr, L,
    N, Low, Hi:         INTEGER;
    Start:              LONGINT;

begin
   WriteLn ('Set operators functional and speed test');
   WriteLn;

   RandSeed := 17;

   for L := 0 TO 255 DO begin
      Box1 [L] := L;
   end;
   MaxNr := 255;
   for L := 0 TO 255 DO begin
      K := Random (MaxNr+1);
      Box2 [L] := Box1 [K];
      Box1 [K] := Box1 [MaxNr];
      Dec (MaxNr);
   end;

   Start :=MSTimer;

   Set1 := [];
   Set2 := [];
   for L := 0 TO 255 DO begin
      Set1 := Set1 + [Box2 [L]];
      if NOT (Box2 [L] IN Set1) then begin
         WriteLn ('error in AddElem or InSet functions');
         Halt;
         end;
      Set2 := Set2 + [Box2 [L]] + [];
   end;

{$ifdef FPC_HAS_SET_INEQUALITIES }
   if (Set1 <> Set2) OR (NOT (Set1 <= Set2)) OR (NOT (Set1 >= Set2)) then begin
{$else FPC_HAS_SET_INEQUALITIES }
   if (Set1 <> Set2) then begin
{$endif FPC_HAS_SET_INEQUALITIES }
      WriteLn ('error in relational operators 1');
      Halt;
      end;

   for L := 0 TO 255 DO begin
      Set1 := Set1 - [Box2 [L]];
      if Box2 [L] IN Set1 then begin
         WriteLn ('error in set difference 1');
         Halt;
         end;
   end;

   if Set1 <> [] then begin
      WriteLn ('error in set difference 2');
      Halt;
      end;

   for L := 1 TO LVal DO begin
      REPEAT
         Low := Random (256);
         Hi  := Random (256);
      UNTIL Low <= Hi;

      Set1 := [];
      Set1 := Set1 + [Low..Hi];
      for K := 0 TO 255 DO begin
         if (K IN Set1) AND ((K < Low) OR (K > Hi)) then begin
            WriteLn ('wrong set inclusion in add range');
            Halt;
            end;
         if (NOT (K IN Set1)) AND ((K >= Low) AND (K <= Hi)) then begin
            WriteLn ('wrong set exclusion in add range');
            Halt;
            end;
      end;
   end;

   for L := 1 TO LVal DO begin
      Set1 := [];
      Set2 := [];

      for K := 1 TO 10 DO begin
         Low := Random (256);
         Hi  := Low + Random (256-Low);
         Set2:= Set1 + [Low..Hi];
{$ifdef FPC_HAS_SET_INEQUALITIES }
         if (Set1 >= Set2) AND (Set1 <> Set2) then begin
{$else FPC_HAS_SET_INEQUALITIES }
         if (Set1 <> Set2) then begin
{$endif FPC_HAS_SET_INEQUALITIES }
            WriteLn ('error in relational operators 2');
            Halt;
            end;
{$ifdef FPC_HAS_SET_INEQUALITIES }
         if NOT (Set1 <= Set2) then begin
            WriteLn ('error in relational operators 3');
            Halt;
            end;
{$endif FPC_HAS_SET_INEQUALITIES }
         Set1 := Set2;

      end;
   end;

   for L := 1 TO LVal DO begin
      Set1 := [];
      for K := 1 TO 10 DO begin
         Low := Random (256);
         Hi  := Low + Random (256-Low);
         Set1:= Set1 + [Low..Hi];
      end;
      Set2 := [];
      for K := 1 TO 10 DO begin
         Low := Random (256);
         Hi  := Low + Random (256-Low);
         Set2:= Set2 + [Low..Hi];
      end;

      OneWOTwo := Set1 - Set2;
      TwoWOOne := Set2 - Set1;
      InterSet := Set1 * Set2;
      UnionSet := Set1 + Set2;

      if InterSet <> (Set2 * Set1) then begin
         WriteLn ('error in set difference');
         Halt;
         end;

      if (InterSet + OneWOTwo) <> Set1 then begin
         WriteLn ('error in set difference or intersection');
         Halt;
         end;

      if (InterSet + TwoWOOne) <> Set2 then begin
         WriteLn ('error in set difference or intersection');
         Halt;
         end;

      if (OneWOTwo + TwoWOOne + InterSet) <> UnionSet then begin
         WriteLn ('error in set union, intersection or difference');
         Halt;
         end;

   end;
  Start:=MSTimer-Start;
//  WriteLn('Set test completes in ',Start,' ms');
end.
