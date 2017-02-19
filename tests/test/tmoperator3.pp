program tmoperator3;

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
  end;

{ TFoo }

class operator TFoo.Initialize(var aFoo: TFoo);
begin
  WriteLn;
  WriteLn('TFoo.Initialize');
  aFoo.F := 1;
end;

class operator TFoo.Finalize(var aFoo: TFoo);
begin
  if aFoo.F <> 2 then
    Halt(2);
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
    Halt(3);
  F.F := 2;
end;

var
  F: TFoo;
  B: TBar;
  PF: PFoo;
begin
  WriteLn('=== Global variable [begin] ===');
  WriteLn;
  
  if F.F <> 1 then
    Halt(4);
    
  WriteLn('=== Local variable ===');
  Foo();  
    
  WriteLn('=== Field in class ===');
  B := TBar.Create();
  if B.F.F <> 1 then
    Halt(5);
  B.F.F := 2;
  B.Free; 
    
  WriteLn('=== New and Dispose ===');
  New(PF);
  if PF.F <> 1 then
    Halt(6);
  PF^.F := 2;
  Dispose(PF); 
  
  WriteLn('=== InitializeArray and FinalizeArray ===');
  GetMem(PF, SizeOf(TFoo));
  InitializeArray(PF, TypeInfo(TFoo), 1);
  if PF.F <> 1 then
    Halt(7);
  PF^.F := 2;  
  FinalizeArray(PF, TypeInfo(TFoo), 1);
  if PF^.F <> 3 then
    Halt(8);
  FreeMem(PF);

  WriteLn('=== Initialize and Finalize ===');
  GetMem(PF, SizeOf(TFoo));
  Initialize(PF^);
  if PF.F <> 1 then
    Halt(9);
  PF^.F := 2;  
  Finalize(PF^);
  if PF^.F <> 3 then
    Halt(10);
  FreeMem(PF);
    
  F.F := 2;
  WriteLn('=== Global variable [end] ===');
end. 