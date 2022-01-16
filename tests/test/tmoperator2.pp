program tmoperator2;

{$MODE DELPHI}

type

  { TFoo }

  PFoo = ^TFoo;
  TFoo = record
  private
    class operator Initialize(var aFoo: TFoo);
    class operator Finalize(var aFoo: TFoo);
  public
    F: Integer;
    S: string;
  end;

{ TFoo }

class operator TFoo.Initialize(var aFoo: TFoo);
begin
  WriteLn;
  WriteLn('TFoo.Initialize');
  if aFoo.S <> '' then
    Halt(1);
  aFoo.F := 1;
  aFoo.S := 'A';
end;

class operator TFoo.Finalize(var aFoo: TFoo);
begin
  if aFoo.F <> 2 then
    Halt(2);
  if aFoo.S <> 'B' then
    Halt(3);
  aFoo.F := 3;
  WriteLn('TFoo.Finalize');
  WriteLn;
end;

{ TBar }
type 
  TBar = class
  private 
    F: TFoo;
  end;

procedure Foo();
var
  F: TFoo;
begin
  if F.F <> 1 then
    Halt(4);
  if F.S <> 'A' then
    Halt(5);
  F.F := 2;
  F.S := 'B';
end;

var
  F: TFoo;
  B: TBar;
  PF: PFoo;
begin
  WriteLn('=== Global variable [begin] ===');
  WriteLn;
  
  if F.F <> 1 then
    Halt(6);

  if F.S <> 'A' then
    Halt(7);
    
  WriteLn('=== Local variable ===');
  Foo();  
    
  WriteLn('=== Field in class ===');
  B := TBar.Create();
  if B.F.F <> 1 then
    Halt(8);
  if B.F.S <> 'A' then
    Halt(9);
  B.F.F := 2;
  B.F.S := 'B';
  B.Free; 
    
  WriteLn('=== New and Dispose ===');
  New(PF);
  if PF^.F <> 1 then
    Halt(10);
  if PF^.S <> 'A' then
    Halt(11);
  PF^.F := 2;
  PF^.S := 'B';
  Dispose(PF); 
  
  WriteLn('=== InitializeArray and FinalizeArray ===');
  GetMem(PF, SizeOf(TFoo));
  InitializeArray(PF, TypeInfo(TFoo), 1);
  if PF^.F <> 1 then
    Halt(12);
  if PF^.S <> 'A' then
    Halt(13);
  PF^.F := 2;  
  PF^.S := 'B';  
  FinalizeArray(PF, TypeInfo(TFoo), 1);
  if PF^.F <> 3 then
    Halt(14);
  FreeMem(PF);

  WriteLn('=== Initialize and Finalize ===');
  GetMem(PF, SizeOf(TFoo));
  Initialize(PF^);
  if PF^.F <> 1 then
    Halt(15);
  if PF^.S <> 'A' then
    Halt(16);
  PF^.F := 2;  
  PF^.S := 'B';  
  Finalize(PF^);
  if PF^.F <> 3 then
    Halt(17);
  FreeMem(PF);
    
  WriteLn('=== Global variable [end] ===');
  F.F := 2;
  F.S := 'B';
end. 