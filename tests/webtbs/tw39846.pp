program tw39846;

{$mode objfpc}

uses
  fgl;

type
  IFoo = interface
    ['{5B3D3E0C-7F4A-4C0E-9A55-2C1D6B1E3F10}']
  end;

  TFoo = class(TInterfacedObject, IFoo)
  public
    destructor Destroy; override;
  end;

  TIntfMap = specialize TFPGMapInterfacedObjectData<Integer, IFoo>;

var
  gDestroyed: Integer = 0;

destructor TFoo.Destroy;

begin
  Inc(gDestroyed);
  inherited Destroy;
end;


// Halts with aCode when the reference counts of aA and aB differ from the expected ones
procedure CheckRefs(aCode: Integer; aA, aB: TFoo; aExpectedA, aExpectedB: Integer);

begin
  if (aA.RefCount<>aExpectedA) or (aB.RefCount<>aExpectedB) then
    begin
    Writeln(aCode,': refcounts ',aA.RefCount,'/',aB.RefCount,', expected ',aExpectedA,'/',aExpectedB);
    Halt(aCode);
    end;
end;


// Tests a map with interface data
procedure TestIntfMap;

var
  lObjA, lObjB: TFoo;
  lA, lB: IFoo;
  lMap: TIntfMap;
  i: Integer;

begin
  lObjA:=TFoo.Create;
  lA:=lObjA;
  lObjB:=TFoo.Create;
  lB:=lObjB;
  lMap:=TIntfMap.Create;
  lMap.Add(1,lA);
  CheckRefs(1,lObjA,lObjB,2,1);
  for i:=1 to 5 do
    begin
    lMap[1]:=lB;
    lMap[1]:=lA;
    end;
  CheckRefs(2,lObjA,lObjB,2,1);
  lMap.Data[0]:=lB;
  CheckRefs(3,lObjA,lObjB,1,2);
  lMap.AddOrSetData(1,lA);
  CheckRefs(4,lObjA,lObjB,2,1);
  lMap.Add(2,lB);
  CheckRefs(5,lObjA,lObjB,2,2);
  lMap.Remove(2);
  CheckRefs(6,lObjA,lObjB,2,1);
  lMap.Add(2,lB);
  lMap.Clear;
  CheckRefs(7,lObjA,lObjB,1,1);
  lMap.Add(1,lA);
  lMap.Add(2,lB);
  lMap.Free;
  CheckRefs(8,lObjA,lObjB,1,1);
  lA:=nil;
  lB:=nil;
  if gDestroyed<>2 then
    Halt(9);
end;


begin
  TestIntfMap;
  Writeln('ok');
end.
