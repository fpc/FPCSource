{
  $Id$

  Program to test heap functions, timing doesn't work
}
PROGRAM TestHeap;

Procedure InitMSTimer;
begin
end;



{Get MS Timer}
Function MSTimer:longint;
begin
  MSTimer:=0;
end;


VAR Dummy,Start, LoopTime,LoopTime2: LONGINT;
    Delta, TotalTime: LONGINT;
    L,Choice,K,T: WORD;
    BlkPtr:  ARRAY [1..10000] OF POINTER;
    BlkSize: ARRAY [1..10000] OF WORD;
    Permutation: ARRAY [1..10000] OF WORD;

BEGIN
  INitMSTimer;
   WriteLn ('Test of TP heap functions');
   WriteLn;
   TotalTime := 0;
   RandSeed := 997;
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   Start :=MSTimer;
   FOR L := 1 TO 10000 DO BEGIN
   END;
   LoopTime := MSTimer-Start;
   FOR L := 1 TO 10000 DO BEGIN
      BlkSize [L] := Random (512) + 1;
   END;
   Write ('Allocating 10000 blocks at the end of the heap: ');
   Start := MSTImer;
   FOR L := 1 TO 10000 DO BEGIN
      GetMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   Write ('Deallocating same 10000 blocks in reverse order:');
   Start := MSTimer;
   FOR L := 1 TO 10000 DO BEGIN
      FreeMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   Write ('Allocating 10000 blocks at the end of the heap: ');
   Start := MSTimer;
   FOR L := 1 TO 10000 DO BEGIN
      GetMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   FOR L := 1 TO 10000 DO BEGIN
      Permutation [L] := L;
   END;
   Start := MSTimer;
   FOR L := 10000 DOWNTO 1 DO BEGIN
      Choice := Random (L)+1;
      K := Permutation [Choice];
      Permutation [Choice] := Permutation [L];
   END;
   LoopTime2 := MSTimer - Start;
   FOR L := 1 TO 10000 DO BEGIN
      Permutation [L] := L;
   END;
   Write ('Deallocating same 10000 blocks at random:       ');
   Start := MSTimer;
   FOR L := 10000 DOWNTO 1 DO BEGIN
      Choice := Random (L)+1;
      K := Permutation [Choice];
      Permutation [Choice] := Permutation [L];
      FreeMem (BlkPtr [K], BlkSize [K]);
   END;
   Delta := MSTimer - Start - LoopTime2;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   Write ('Allocating 10000 blocks at the end of the heap: ');
   Start := MSTimer;
   FOR L := 1 TO 10000 DO BEGIN
      GetMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   FOR L := 1 TO 10000 DO BEGIN
      Permutation [L] := L;
   END;
   Start := MSTimer;
   FOR L := 10000 DOWNTO 1 DO BEGIN
      Choice := Random (L)+1;
      K := Permutation [Choice];
      T:= Permutation [L];
      Permutation [L] := Permutation [Choice];
      Permutation [Choice] := T;
   END;
   LoopTime2 := MSTimer - Start;
   FOR L := 1 TO 10000 DO BEGIN
      Permutation [L] := L;
   END;
   Write ('Deallocating 5000 blocks at random:             ');
   Start := MSTimer;
   FOR L := 10000 DOWNTO 5001 DO BEGIN
      Choice := Random (L)+1;
      K := Permutation [Choice];
      T:= Permutation [L];
      Permutation [L] := Permutation [Choice];
      Permutation [Choice] := T;
      SYSTEM.FreeMem (BlkPtr [K], BlkSize [K]);
   END;
   Delta := MSTimer-Start-LoopTime2;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   Start := MSTimer;
   FOR L := 1 TO 10000 DO BEGIN
      Dummy := MaxAvail;
   END;
   Delta := MSTimer-Start;
   Inc (TotalTime, (Delta + 5) DIV 10);
   WriteLn ('10000 calls to MaxAvail:                        ', Delta:5, ' ms');
   Start := MSTimer;
   FOR L := 1 TO 10000 DO BEGIN
      Dummy := MemAvail;
   END;
   Delta := MSTimer - Start;
   Inc (TotalTime, (Delta + 5) DIV 10);
   WriteLn ('10000 calls to MemAvail:                        ', Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   Write ('Reallocating deallocated 500 blocks at random: ');
   Start := MSTimer;
   FOR L := 5001 TO 10000 DO BEGIN
      GetMem (BlkPtr [Permutation [L]], BlkSize [Permutation [L]]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   Write ('Deallocating all 10000 blocks at random:        ');
   Start := MSTimer;
   FOR L := 10000 DOWNTO 1 DO BEGIN
      FreeMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   WriteLn ('MaxAvail: ', MaxAvail, '   MemAvail: ', MemAvail);
   WriteLn;
   WriteLn ('Total time for benchmark: ', TotalTime, ' ms');
END.



