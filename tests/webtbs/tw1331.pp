program tw1331;

{$mode objfpc}

uses Classes, SysUtils, TypInfo, uw1331;

type
  DummyEnum1 = (tr1,tr2);
  DummyEnum2 = tr1..tr2;

  DummyClass = class(TPersistent)
  private
    FMyDummyEnum1:DummyEnum1;
    FMyDummyEnum2:DummyEnum2;
    FMyDummyEnum3:DummyEnum3;
    FMyDummyEnum4:DummyEnum4;
  published
    property MyDummyEnum1:DummyEnum1 read FMyDummyEnum1 write FMyDummyEnum1;
    property MyDummyEnum2:DummyEnum2 read FMyDummyEnum2 write FMyDummyEnum2;
    property MyDummyEnum3:DummyEnum3 read FMyDummyEnum3 write FMyDummyEnum3;
    property MyDummyEnum4:DummyEnum4 read FMyDummyEnum4 write FMyDummyEnum4;
  end;

var Dummy1:DummyClass;
    List:PPropList;
    Count,Index,I:integer;
    EnumType:PTypeInfo;

begin
  // create a dummyclass instance
  Dummy1:=DummyClass.Create;
  // get property list
  Count:=GetTypeData(DummyClass.ClassInfo)^.Propcount;
  GetMem(List,Count * SizeOf(Pointer));
  GetPropInfos(DummyClass.ClassInfo,List);
  Index:=Count-1;
  while (Index>=0) do begin
    EnumType:=List^[Index]^.PropType;
    if EnumType^.Kind=tkEnumeration then begin
      // print all enumeration types
      writeln('PropertyName=',EnumType^.Name);
      with GetTypeData(EnumType)^ do
        // write all possible values for this type
        for I := MinValue to MaxValue do
          writeln(I,': ''',GetEnumName(EnumType, I),'''');
    end;
    dec(Index);
  end;
  FreeMem(List);
  Dummy1.Free;
end.
