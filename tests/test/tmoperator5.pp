program tmoperator5;

{$MODE DELPHI}

type
  TR1 = record
  private
    class operator Initialize(var aR1: TR1);
    class operator Finalize(var aR1: TR1);
  public
    I: Integer;
  end;

  TR2 = record
  private
    class operator Initialize(var aR2: TR2);
    class operator Finalize(var aR2: TR2);
  public
    S: string;
  end;

{ TR1 }

class operator TR1.Initialize(var aR1: TR1);
begin
  WriteLn('TR1.Initialize');
  aR1.I := 1;
end;

class operator TR1.Finalize(var aR1: TR1);
begin
  if aR1.I <> 2 then
    Halt(1);
  aR1.I := 3;
  WriteLn('TR1.Finalize');
end;

{ TR2 }

class operator TR2.Initialize(var aR2: TR2);
begin
  WriteLn('TR2.Initialize');
  aR2.S := 'A';
end;

class operator TR2.Finalize(var aR2: TR2);
begin
  if aR2.S <> 'B' then
    Halt(2);
  WriteLn('TR2.Finalize');
end;

{ TA }

type 
  TA = object
  public 
    F1: TR1;
  end;

  TB = object(TA)
  public
    F2: TR2;
  end;
  
procedure Foo();
var
  LO: TB;
begin
  if LO.F1.I <> 1 then
    Halt(4);
  if LO.F2.S <> 'A' then
    Halt(5);
  LO.F1.I := 2;
  LO.F2.S := 'B';
end;

var
  O: TB;
  P: ^TB;
begin
  WriteLn('=== Global object variable [begin] ===');
  
  if O.F1.I <> 1 then
    Halt(3);
  if O.F2.S <> 'A' then
    Halt(4);
    
  WriteLn;
  WriteLn('=== Local variable ===');
  Foo();      
    
  WriteLn;
  WriteLn('=== New and Dispose ===');
  New(P);
  if P^.F1.I <> 1 then
    Halt(10);
  if P^.F2.S <> 'A' then
    Halt(11);
  P^.F1.I := 2;
  P^.F2.S := 'B';
  Dispose(P); 
  
  WriteLn;
  WriteLn('=== InitializeArray and FinalizeArray ===');
  GetMem(P, SizeOf(TB));
  InitializeArray(P, TypeInfo(TB), 1);
  if P^.F1.I <> 1 then
    Halt(12);
  if P^.F2.S <> 'A' then
    Halt(13);
  P^.F1.I := 2;  
  P^.F2.S := 'B';  
  FinalizeArray(P, TypeInfo(TB), 1);
  if P^.F1.I <> 3 then
    Halt(14);
  FreeMem(P);

  WriteLn;
  WriteLn('=== Initialize and Finalize ===');
  GetMem(P, SizeOf(TB));
  Initialize(P^);
  if P^.F1.I <> 1 then
    Halt(15);
  if P^.F2.S <> 'A' then
    Halt(16);
  P^.F1.I := 2;  
  P^.F2.S := 'B';  
  Finalize(P^);
  if P^.F1.I <> 3 then
    Halt(17);
  FreeMem(P);

  WriteLn;
  WriteLn('=== Global variable [end] ===');
  O.F1.I := 2;
  O.F2.S := 'B'; 
end. 