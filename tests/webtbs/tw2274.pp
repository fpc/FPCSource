{ Source provided for Free Pascal Bug Report 2274 }
{ Submitted by "Sergey Kosarevsky" on  2002-12-21 }
{ e-mail: netsurfer@au.ru }
Unit tw2274;

{$STATIC ON}

Interface

Type Lfloat=Single;

Type pTimer=^tTimer;
     tTimer=Object
       Private
        RecepCyclesPerSecond:Lfloat;Static;        //     1/CyclesPerSecond
        OldCycles:Int64;
        NewCycles:Int64;
        WorldUpTime:Lfloat;
       Public
        Constructor Init;
        // tTimer
        Function GetDeltaSeconds:Lfloat;
        Function GetWorldTime:Lfloat;          // in seconds
        Function GetCycles:Int64;Static;
        Function GetSeconds:Lfloat;
     End;

Implementation

Constructor tTimer.Init;
Begin
   RecepCyclesPerSecond:=0;
   OldCycles:=GetCycles;
End;

Function tTimer.GetWorldTime:Lfloat;
Begin
   Exit(GetSeconds-WorldUpTime);
End;

Function tTimer.GetCycles:Int64;
begin
  GetCycles:=0;
End;

Function tTimer.GetDeltaSeconds:Lfloat;
Begin
   NewCycles:=GetCycles;
   GetDeltaSeconds:=(NewCycles-OldCycles)*RecepCyclesPerSecond;
   OldCycles:=NewCycles;
End;

Function tTimer.GetSeconds:Lfloat;
Begin
   Exit(GetCycles*RecepCyclesPerSecond);
End;

Begin
End.
