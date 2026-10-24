{ for-in over the Classes containers returns every element in order }
program tenumerators;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes;

const
  C_COUNT = 50;

var
  lErrors: Integer;

{ Reports a failed check }
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      writeln('FAIL: ', aMsg);
      Inc(lErrors);
    end;
end;


{ Enumerates a TFPList, a TList and empty instances of both }
procedure TestLists;

var
  lFPList: TFPList;
  lList: TList;
  lP: Pointer;
  i, lIndex: Integer;

begin
  lFPList:=TFPList.Create;
  lList:=TList.Create;
  try
    lIndex:=0;
    for lP in lFPList do
      Inc(lIndex);
    Check(lIndex=0, 'Empty TFPList yields no element');
    for lP in lList do
      Inc(lIndex);
    Check(lIndex=0, 'Empty TList yields no element');
    for i:=1 to C_COUNT do
      begin
        lFPList.Add(Pointer(PtrUInt(i)));
        lList.Add(Pointer(PtrUInt(i*2)));
      end;
    lIndex:=0;
    for lP in lFPList do
      begin
        Check(lP=lFPList[lIndex], 'TFPList element '+IntToStr(lIndex));
        Inc(lIndex);
      end;
    Check(lIndex=C_COUNT, 'TFPList yields Count elements');
    lIndex:=0;
    for lP in lList do
      begin
        Check(lP=lList[lIndex], 'TList element '+IntToStr(lIndex));
        Inc(lIndex);
      end;
    Check(lIndex=C_COUNT, 'TList yields Count elements');
  finally
    lList.Free;
    lFPList.Free;
  end;
end;


{ Enumerates a TStringList }
procedure TestStrings;

var
  lStrings: TStringList;
  lS: string;
  i, lIndex: Integer;

begin
  lStrings:=TStringList.Create;
  try
    for i:=1 to C_COUNT do
      lStrings.Add('Item'+IntToStr(i));
    lIndex:=0;
    for lS in lStrings do
      begin
        Check(lS='Item'+IntToStr(lIndex+1), 'TStringList element '+IntToStr(lIndex));
        Inc(lIndex);
      end;
    Check(lIndex=C_COUNT, 'TStringList yields Count elements');
  finally
    lStrings.Free;
  end;
end;


{ Enumerates a TCollection }
procedure TestCollection;

var
  lCollection: TCollection;
  lItem: TCollectionItem;
  i, lIndex: Integer;

begin
  lCollection:=TCollection.Create(TCollectionItem);
  try
    for i:=1 to C_COUNT do
      lCollection.Add;
    lIndex:=0;
    for lItem in lCollection do
      begin
        Check(lItem=lCollection.Items[lIndex], 'TCollection element '+IntToStr(lIndex));
        Inc(lIndex);
      end;
    Check(lIndex=C_COUNT, 'TCollection yields Count elements');
  finally
    lCollection.Free;
  end;
end;


{ Enumerates the owned components of a TComponent }
procedure TestComponent;

var
  lOwner, lComp: TComponent;
  i, lIndex: Integer;

begin
  lOwner:=TComponent.Create(nil);
  try
    for i:=1 to C_COUNT do
      TComponent.Create(lOwner);
    lIndex:=0;
    for lComp in lOwner do
      begin
        Check(lComp=lOwner.Components[lIndex], 'TComponent element '+IntToStr(lIndex));
        Inc(lIndex);
      end;
    Check(lIndex=C_COUNT, 'TComponent yields ComponentCount elements');
  finally
    lOwner.Free;
  end;
end;


{ Enumerates a TInterfaceList }
procedure TestInterfaceList;

var
  lIntfs: TInterfaceList;
  lIntf: IUnknown;
  i, lIndex: Integer;

begin
  lIntfs:=TInterfaceList.Create;
  try
    for i:=1 to C_COUNT do
      lIntfs.Add(TInterfacedObject.Create);
    lIndex:=0;
    for lIntf in lIntfs do
      begin
        Check(lIntf=lIntfs[lIndex], 'TInterfaceList element '+IntToStr(lIndex));
        Inc(lIndex);
      end;
    lIntf:=nil;
    Check(lIndex=C_COUNT, 'TInterfaceList yields Count elements');
  finally
    lIntfs.Free;
  end;
end;


begin
  lErrors:=0;
  TestLists;
  TestStrings;
  TestCollection;
  TestComponent;
  TestInterfaceList;
  if lErrors>0 then
    Halt(1);
  writeln('ok');
end.
