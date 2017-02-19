program tmoperator7;

{$MODE DELPHI}

type
  TFoo = record
  private
    class operator Initialize(var aFoo: TFoo);
    class operator Finalize(var aFoo: TFoo);
  public
    I: Integer;
  public class var
    InitializeCount: Integer;
    FinalizeCount: Integer;
  end;

  TFooObj = object
  public
    F: TFoo;
  end;  

  TFooArray = array of TFoo; 
  TFooObjArray = array of TFooObj; 

{ TFoo }

class operator TFoo.Initialize(var aFoo: TFoo);
begin
  Inc(InitializeCount);
  if aFoo.I <> 0 then // for dyn array and old obj
    Halt(1);
    
  WriteLn('TFoo.Initialize');
  aFoo.I := 1;
end;

class operator TFoo.Finalize(var aFoo: TFoo);
begin
  Inc(FinalizeCount);
  if aFoo.I <> 2 then
    Halt(2);
  WriteLn('TFoo.Finalize');
end;

procedure CheckFooInit(var AValue: Integer; const AExpectedInitializeCount: Integer);
begin
  if AValue <> 1 then
    Halt(3);
  AValue := 2;
  
  if TFoo.InitializeCount <> AExpectedInitializeCount then
    Halt(4); 
end;

procedure CheckFooFini(const AExpectedFinalizeCount: Integer);
begin
  if TFoo.FinalizeCount <> AExpectedFinalizeCount then
    Halt(5);   
end;

procedure FooTest;
var
  Foos: TFooArray;
  FoosObj: TFooObjArray;
begin
  WriteLn('=== DynArray of Records ===');
  
  SetLength(Foos, 1);
  CheckFooInit(Foos[0].I, 1);

  SetLength(Foos, 2);
  CheckFooInit(Foos[1].I, 2);
    
  SetLength(Foos, 1);
  CheckFooFini(1);

  SetLength(Foos, 2);
  CheckFooInit(Foos[1].I, 3);

  Foos := nil;
  CheckFooFini(3);
    
  WriteLn('=== DynArray of Objects ===');
  TFoo.InitializeCount := 0;
  TFoo.FinalizeCount := 0;
  
  SetLength(FoosObj, 1);
  CheckFooInit(FoosObj[0].F.I, 1);

  SetLength(FoosObj, 2);
  CheckFooInit(FoosObj[1].F.I, 2);
    
  SetLength(FoosObj, 1);
  CheckFooFini(1);

  SetLength(FoosObj, 2);
  CheckFooInit(FoosObj[1].F.I, 3);

  FoosObj := nil;
  CheckFooFini(3);
end;

begin
  FooTest;
end. 