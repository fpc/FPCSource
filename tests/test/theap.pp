{

  Program to test heap functions, timing doesn't work
}
PROGRAM TestHeap;

uses
  erroru;

const
{$ifdef cpusparc}
  Blocks = 1000;
{$else}
  Blocks = 10000;
{$endif}

Procedure InitMSTimer;
begin
end;



{Get MS Timer}
Function MSTimer:longint;
begin
  MSTimer:=0;
end;


procedure ShowHeap;
var
  hstatus : TFPCHeapstatus;
begin
   hstatus:=GetFPCHeapStatus;
   WriteLn ('Used: ', hstatus.CurrHeapUsed, '   Free: ', hstatus.CurrHeapFree,'   Size: ',hstatus.CurrHeapSize);
end;


VAR Start, LoopTime,LoopTime2: LONGINT;
    Delta, TotalTime: LONGINT;
    L,Choice,K,T: WORD;
    BlkPtr:  ARRAY [1..Blocks] OF POINTER;
    BlkSize: ARRAY [1..Blocks] OF WORD;
    Permutation: ARRAY [1..Blocks] OF WORD;

BEGIN
  INitMSTimer;
   WriteLn ('Test of TP heap functions');
   WriteLn;
   TotalTime := 0;
   RandSeed := 997;
   ShowHeap;
   Start :=MSTimer;
   FOR L := 1 TO Blocks DO BEGIN
   END;
   LoopTime := MSTimer-Start;
   FOR L := 1 TO Blocks DO BEGIN
      BlkSize [L] := Random (512) + 1;
   END;
   Write ('Allocating ',Blocks,' blocks at the end of the heap: ');
   Start := MSTImer;
   FOR L := 1 TO Blocks DO BEGIN
      GetMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   ShowHeap;
   Write ('Deallocating same ',Blocks,' blocks in reverse order:');
   Start := MSTimer;
   FOR L := 1 TO Blocks DO BEGIN
      FreeMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   ShowHeap;
   Write ('Allocating ',Blocks,' blocks at the end of the heap: ');
   Start := MSTimer;
   FOR L := 1 TO Blocks DO BEGIN
      GetMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   ShowHeap;
   FOR L := 1 TO Blocks DO BEGIN
      Permutation [L] := L;
   END;
   Start := MSTimer;
   FOR L := Blocks DOWNTO 1 DO BEGIN
      Choice := Random (L)+1;
      K := Permutation [Choice];
      Permutation [Choice] := Permutation [L];
   END;
   LoopTime2 := MSTimer - Start;
   FOR L := 1 TO Blocks DO BEGIN
      Permutation [L] := L;
   END;
   Write ('Deallocating same ',Blocks,' blocks at random:       ');
   Start := MSTimer;
   FOR L := Blocks DOWNTO 1 DO BEGIN
      Choice := Random (L)+1;
      K := Permutation [Choice];
      Permutation [Choice] := Permutation [L];
      FreeMem (BlkPtr [K], BlkSize [K]);
   END;
   Delta := MSTimer - Start - LoopTime2;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   ShowHeap;
   Write ('Allocating ',Blocks,' blocks at the end of the heap: ');
   Start := MSTimer;
   FOR L := 1 TO Blocks DO BEGIN
      GetMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   ShowHeap;
   FOR L := 1 TO Blocks DO BEGIN
      Permutation [L] := L;
   END;
   Start := MSTimer;
   FOR L := Blocks DOWNTO 1 DO BEGIN
      Choice := Random (L)+1;
      K := Permutation [Choice];
      T:= Permutation [L];
      Permutation [L] := Permutation [Choice];
      Permutation [Choice] := T;
   END;
   LoopTime2 := MSTimer - Start;
   FOR L := 1 TO Blocks DO BEGIN
      Permutation [L] := L;
   END;
   Write ('Deallocating ',(Blocks div 2 + 1),' blocks at random:             ');
   Start := MSTimer;
   FOR L := Blocks DOWNTO (Blocks div 2 + 1) DO BEGIN
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
   ShowHeap;
   Write ('Reallocating deallocated ',(Blocks div 2 + 1),' blocks at random: ');
   Start := MSTimer;
   FOR L := (Blocks div 2+1) TO Blocks DO BEGIN
      GetMem (BlkPtr [Permutation [L]], BlkSize [Permutation [L]]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   ShowHeap;
   Write ('Deallocating all ',Blocks,' blocks at random:        ');
   Start := MSTimer;
   FOR L := Blocks DOWNTO 1 DO BEGIN
      FreeMem (BlkPtr [L], BlkSize [L]);
   END;
   Delta := MSTimer-Start-LoopTime;
   Inc (TotalTime, Delta);
   WriteLn (Delta:5, ' ms');
   ShowHeap;
   WriteLn;
   WriteLn ('Total time for benchmark: ', TotalTime, ' ms');
END.
