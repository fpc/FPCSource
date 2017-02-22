Unit tccompstreaming;

interface

Uses
  SysUtils,Classes,tcstreaming,fpcunit, testregistry;

Type 

{ TTestComponentStream }

TTestComponentStream = Class(TTestStreaming)
  Published
    Procedure TestTEmptyComponent;
    Procedure TestTIntegerComponent;
    Procedure TestTIntegerComponent2;
    Procedure TestTIntegerComponent3;
    Procedure TestTIntegerComponent4;
    Procedure TestTIntegerComponent5;
    Procedure TestTInt64Component;
    Procedure TestTInt64Component2;
    Procedure TestTInt64Component3;
    Procedure TestTInt64Component4;
    Procedure TestTInt64Component5;
    Procedure TestTInt64Component6;
    Procedure TestTStringComponent;
    Procedure TestTStringComponent2;
    Procedure TestTWideStringComponent;
    Procedure TestTWideStringComponent2;
    Procedure TestTSingleComponent;
    Procedure TestTDoubleComponent;
    Procedure TestTExtendedComponent;
    Procedure TestTCompComponent;
    Procedure TestTCurrencyComponent;
    Procedure TestTDateTimeComponent;
    Procedure TestTDateTimeComponent2;
    Procedure TestTDateTimeComponent3;
    Procedure TestTEnumComponent;
    Procedure TestTEnumComponent2;
    Procedure TestTEnumComponent3;
    Procedure TestTEnumComponent4;
    Procedure TestTEnumComponent5;
    Procedure TestTSetComponent;
    Procedure TestTSetComponent2;
    Procedure TestTSetComponent3;
    Procedure TestTSetComponent4;
    Procedure TestTMultipleComponent;
    Procedure TestTPersistentComponent;
    Procedure TestTCollectionComponent;
    Procedure TestTCollectionComponent2;
    Procedure TestTCollectionComponent3;
    Procedure TestTCollectionComponent4;
    Procedure TestTCollectionComponent5;
    Procedure TestTOwnedComponent;
    Procedure TestTStreamedOwnedComponent;
    Procedure TestTStreamedOwnedComponents;
    Procedure TestTMethodComponent;
    Procedure TestTMethodComponent2;
    Procedure TestTOwnedInterface;
  end;
  { TMyItem }

  TMyItem = Class(TCollectionItem)
  private
    FNR: Integer;
    FStr: String;
  Public
    Procedure Assign(Source : TPersistent); override;
  Published
    Property Nr : Integer Read FNR Write FNR;
    Property Str: String Read FStr Write FStr;
  end;

  { TMyColl }

  TMyColl = Class(TCollection)
  private
    function GetIt(index : Integer): TMyItem;
    procedure SetIt(index : Integer; const AValue: TMyItem);
  Public
    Property It[index : Integer] : TMyItem Read GetIt Write SetIt; default;
  end;

  { TCollComp }

  TCollComp = Class(TComponent)
  private
    FMyColl: TMyColl;
    procedure SetMyColl(const AValue: TMyColl);
  Public
    Constructor Create(AOwner : TComponent); override;
    Destructor Destroy; override;
    Function ToStream : TStream;
    Procedure FromStream(AStream : TStream);
  Published
    Property MyColl : TMyColl Read FMyColl Write SetMyColl;
  end;


  { TTestCollectionStream }

  TTestCollectionStream = Class(TTestCase)

  private
    procedure CompareColl(CA, CB: TMyColl);
    function CreateColl(Anr: Integer): TCollComp;
    function EmptyComp: TCollComp;
    procedure TestNr(ACount: Integer);
  Published
    procedure Test1;
    procedure Test2;
    procedure Test3;
    procedure TestClear;
    procedure TestEmpty;
  end;

Implementation

uses testcomps;


Procedure TTestComponentStream.TestTEmptyComponent;

Var
  C : TComponent;

begin
  C:=TEmptyComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEmptyComponent');
    ExpectBareString('TestTEmptyComponent');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTIntegerComponent;

Var
  C : TComponent;

begin
  C:=TIntegerComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('TestTIntegerComponent');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTIntegerComponent2;

Var
  C : TComponent;

begin
  C:=TIntegerComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent2');
    ExpectBareString('TestTIntegerComponent2');
    ExpectBareString('IntProp');
    ExpectInteger(1024);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTIntegerComponent3;

Var
  C : TComponent;

begin
  C:=TIntegerComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent3');
    ExpectBareString('TestTIntegerComponent3');
    ExpectBareString('IntProp');
    ExpectInteger(262144);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTIntegerComponent4;

Var
  C : TComponent;

begin
  C:=TIntegerComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent4');
    ExpectBareString('TestTIntegerComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTIntegerComponent5;

Var
  C : TComponent;

begin
  C:=TIntegerComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent5');
    ExpectBareString('TestTIntegerComponent5');
    ExpectBareString('IntProp');
    ExpectInteger(5);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTInt64Component;

Var
  C : TComponent;

begin
  C:=TInt64Component.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component');
    ExpectBareString('TestTInt64Component');
    ExpectBareString('Int64Prop');
    ExpectInteger(4);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTInt64Component2;

Var
  C : TComponent;

begin
  C:=TInt64Component2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component2');
    ExpectBareString('TestTInt64Component2');
    ExpectBareString('Int64Prop');
    ExpectInteger(1024);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTInt64Component3;

Var
  C : TComponent;

begin
  C:=TInt64Component3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component3');
    ExpectBareString('TestTInt64Component3');
    ExpectBareString('Int64Prop');
    ExpectInteger(262144);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTInt64Component4;

Var
  C : TComponent;

begin
  C:=TInt64Component4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component4');
    ExpectBareString('TestTInt64Component4');
    ExpectBareString('Int64Prop');
    ExpectInt64(2147745791);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTInt64Component5;

Var
  C : TComponent;

begin
  C:=TInt64Component5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component5');
    ExpectBareString('TestTInt64Component5');
    ExpectBareString('Int64Prop');
    ExpectInteger(7);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTInt64Component6;

Var
  C : TComponent;

begin
  C:=TInt64Component6.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TInt64Component6');
    ExpectBareString('TestTInt64Component6');
    ExpectBareString('Int64Prop');
    ExpectInteger(8);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTStringComponent;

Var
  C : TComponent;

begin
  C:=TStringComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent');
    ExpectBareString('TestTStringComponent');
    ExpectBareString('StringProp');
    ExpectString('A string');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTStringComponent2;

Var
  C : TComponent;

begin
  C:=TStringComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent2');
    ExpectBareString('TestTStringComponent2');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTWideStringComponent;

Var
  C : TComponent;

begin
  C:=TWideStringComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TWideStringComponent');
    ExpectBareString('TestTWideStringComponent');
    ExpectBareString('WideStringProp');
    ExpectString('Some WideString');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTWideStringComponent2;

Var
  C : TComponent;

begin
  C:=TWideStringComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TWideStringComponent2');
    ExpectBareString('TestTWideStringComponent2');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTSingleComponent;

Var
  C : TComponent;

begin
  C:=TSingleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSingleComponent');
    ExpectBareString('TestTSingleComponent');
    ExpectBareString('SingleProp');
    ExpectExtended(1.23);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTDoubleComponent;

Var
  C : TComponent;

begin
  C:=TDoubleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDoubleComponent');
    ExpectBareString('TestTDoubleComponent');
    ExpectBareString('DoubleProp');
    ExpectExtended(2.34);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTExtendedComponent;

Var
  C : TComponent;

begin
  C:=TExtendedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TExtendedComponent');
    ExpectBareString('TestTExtendedComponent');
    ExpectBareString('ExtendedProp');
    ExpectExtended(3.45);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTCompComponent;

Var
  C : TComponent;

begin
  C:=TCompComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCompComponent');
    ExpectBareString('TestTCompComponent');
    ExpectBareString('ExtendedProp');
    ExpectExtended(5.00);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTCurrencyComponent;

Var
  C : TComponent;

begin
  C:=TCurrencyComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCurrencyComponent');
    ExpectBareString('TestTCurrencyComponent');
    ExpectBareString('CurrencyProp');
    ExpectExtended(5.67);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTDateTimeComponent;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent');
    ExpectBareString('TestTDateTimeComponent');
    ExpectBareString('DateTimeProp');
    ExpectExtended(35278.00);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTDateTimeComponent2;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent2');
    ExpectBareString('TestTDateTimeComponent2');
    ExpectBareString('DateTimeProp');
    ExpectExtended(0.97);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTDateTimeComponent3;

Var
  C : TComponent;

begin
  C:=TDateTimeComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TDateTimeComponent3');
    ExpectBareString('TestTDateTimeComponent3');
    ExpectBareString('DateTimeProp');
    ExpectExtended(35278.97);
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTEnumComponent;

Var
  C : TComponent;

begin
  C:=TEnumComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent');
    ExpectBareString('TestTEnumComponent');
    ExpectBareString('Dice');
    ExpectIdent('four');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTEnumComponent2;

Var
  C : TComponent;

begin
  C:=TEnumComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent2');
    ExpectBareString('TestTEnumComponent2');
{$ifndef FPC}
    // FPC does not stream an undeclared default value, it assumes the
    // 0-the value is the default.
    ExpectBareString('Dice');
    ExpectIdent('one');
{$endif FPC}
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTEnumComponent3;

Var
  C : TComponent;

begin
  C:=TEnumComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent3');
    ExpectBareString('TestTEnumComponent3');
    ExpectBareString('Dice');
    ExpectIdent('three');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTEnumComponent4;

Var
  C : TComponent;

begin
  C:=TEnumComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent4');
    ExpectBareString('TestTEnumComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;

Procedure TTestComponentStream.TestTEnumComponent5;

Var
  C : TComponent;

begin
  C:=TEnumComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TEnumComponent5');
    ExpectBareString('TestTEnumComponent5');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTSetComponent;

Var
  C : TComponent;

begin
  C:=TSetComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent');
    ExpectBareString('TestTSetComponent');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('two');
    ExpectBareString('five');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTSetComponent2;

Var
  C : TComponent;

begin
  C:=TSetComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent2');
    ExpectBareString('TestTSetComponent2');
{$ifndef FPC}
    // Same as for sets: a set with undeclared default is regarded as
    // A set with default [], and is not streamed if it is empty.
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('');
{$endif FPC}
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTSetComponent3;

Var
  C : TComponent;

begin
  C:=TSetComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent3');
    ExpectBareString('TestTSetComponent3');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('one');
    ExpectBareString('four');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTSetComponent4;

Var
  C : TComponent;

begin
  C:=TSetComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TSetComponent4');
    ExpectBareString('TestTSetComponent4');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTMultipleComponent;

Var
  C : TComponent;

begin
  C:=TMultipleComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMultipleComponent');
    ExpectBareString('TestTMultipleComponent');
    ExpectBareString('IntProp');
    ExpectInteger(1);
    ExpectBareString('StringProp');
    ExpectString('A String');
    ExpectBareString('CurrencyProp');
    ExpectExtended(2.30);
    ExpectBareString('Dice');
    ExpectIdent('two');
    ExpectBareString('Throw');
    ExpectValue(vaSet);
    ExpectBareString('three');
    ExpectBareString('four');
    ExpectBareString('');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTPersistentComponent;

Var
  C : TComponent;

begin
  C:=TPersistentComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TPersistentComponent');
    ExpectBareString('TestTPersistentComponent');
    ExpectBareString('Persist.AInteger');
    ExpectInteger(3);
    ExpectBareString('Persist.AString');
    ExpectString('A persistent string');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTCollectionComponent;

Var
  C : TComponent;

begin
  C:=TCollectionComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent');
    ExpectBareString('TestTCollectionComponent');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTCollectionComponent2;

Var
  C : TComponent;

begin
  C:=TCollectionComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent2');
    ExpectBareString('TestTCollectionComponent2');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('First');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Second');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Third');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTCollectionComponent3;

Var
  C : TComponent;

begin
  C:=TCollectionComponent3.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent3');
    ExpectBareString('TestTCollectionComponent3');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('First');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Third');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTCollectionComponent4;

Var
  C : TComponent;

begin
  C:=TCollectionComponent4.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent4');
    ExpectBareString('TestTCollectionComponent4');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp');
    ExpectString('Something');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;

Procedure TTestComponentStream.TestTCollectionComponent5;

Var
  C : TComponent;

begin
  C:=TCollectionComponent5.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TCollectionComponent5');
    ExpectBareString('TestTCollectionComponent5');
    ExpectBareString('Coll');
    ExpectValue(vaCollection);
    ExpectValue(vaList);
    ExpectBareString('StrProp1');
    ExpectString('Something');
    ExpectBareString('StrProp2');
    ExpectString('Otherthing');
    ExpectEndOfList;
    ExpectValue(vaList);
    ExpectBareString('StrProp1');
    ExpectString('Something 2');
    ExpectBareString('StrProp2');
    ExpectString('Otherthing 2');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTOwnedComponent;

Var
  C : TComponent;

begin
  C:=TOwnedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TOwnedComponent');
    ExpectBareString('TestTOwnedComponent');
    ExpectBareString('CompProp');
    ExpectIdent('SubComponent');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTStreamedOwnedComponent;

Var
  C : TComponent;

begin
  C:=TStreamedOwnedComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStreamedOwnedComponent');
    ExpectBareString('TestTStreamedOwnedComponent');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('Sub');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfStream;
  Finally
    C.Free;
    end;
end;

Procedure TTestComponentStream.TestTStreamedOwnedComponents;

Var
  C : TComponent;

begin
  C:=TStreamedOwnedComponents.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TStreamedOwnedComponents');
    ExpectBareString('TestTStreamedOwnedComponents');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TIntegerComponent');
    ExpectBareString('SubA');
    ExpectBareString('IntProp');
    ExpectInteger(3);
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TStringComponent');
    ExpectBareString('SubB');
    ExpectBareString('StringProp');
    ExpectString('A string');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfStream;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTMethodComponent;

Var
  C : TComponent;

begin
  C:=TMethodComponent.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent');
    ExpectBareString('TestTMethodComponent');
    ExpectBareString('MethodProp');
    ExpectIdent('MyMethod');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;


Procedure TTestComponentStream.TestTMethodComponent2;

Var
  C : TComponent;

begin
  C:=TMethodComponent2.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent2');
    ExpectBareString('TestTMethodComponent2');
    ExpectEndOfList;
    ExpectFlags([],0);
    ExpectBareString('TMethodComponent');
    ExpectBareString('AComponent');
    ExpectBareString('MethodProp');
    ExpectIdent('MyMethod2');
    ExpectEndOfList;
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;

Procedure TTestComponentStream.TestTOwnedInterface;

Var
  C : TComponent;

begin
  C:=TOwnedInterface.Create(Nil);
  Try
    SaveToStream(C);
    ExpectSignature;
    ExpectFlags([],0);
    ExpectBareString('TOwnedInterface');
    ExpectBareString('TestTOwnedInterface');
    ExpectBareString('IntfProp');
    ExpectIdent('InterfacedComponent');
    ExpectEndOfList;
    ExpectEndOfList;
  Finally
    C.Free;
    end;
end;

{ TMyColl }

function TMyColl.GetIt(index : Integer): TMyItem;
begin
  Result:=Items[Index] as TMyItem;
end;

procedure TMyColl.SetIt(index : Integer; const AValue: TMyItem);
begin
  Items[Index]:=AValue;
end;

{ TCollComp }

procedure TCollComp.SetMyColl(const AValue: TMyColl);
begin
  if (FMyColl=AValue) then
    exit;
  FMyColl.Assign(AValue);
end;

constructor TCollComp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMyColl:=TMyCOll.Create(TMyItem);
end;

destructor TCollComp.Destroy;
begin
  FreeAndNil(FMyColl);
  inherited Destroy;
end;

function TCollComp.ToStream: TStream;
begin
  Result:=TMemoryStream.Create;
  Result.WriteComponent(Self);
  Result.Position:=0;
end;

procedure TCollComp.FromStream(AStream: TStream);
begin
  AStream.ReadComponent(Self);
  Astream.Free;
end;

procedure TMyItem.Assign(Source: TPersistent);

Var
  I : TMyItem;

begin
  If (Source is TMyItem) then
    begin
    I:=Source as TMyItem;
    FNR:=I.NR;
    FStr:=I.Str;
    end
  else
    inherited Assign(Source);
end;

Procedure TTestCollectionStream.CompareColl(CA,CB : TMyColl);

Var
  I : Integer;

begin
  AssertEquals('Counts differ: %d %d',CA.Count,CB.Count);
  For I:=0 to CA.Count-1 do
    begin
    AssertEquals(Format('Nr property of element %d equals',[I]),CA[i].Nr,CB[i].Nr);
    AssertEquals(Format('Str property of element %d equals',[I]),CA[i].Str,CB[i].Str);
    end;
end;

Function TTestCollectionStream.EmptyComp : TCollComp;

begin
  Result:=TCollComp.Create(Nil);
end;

Function TTestCollectionStream.CreateColl(Anr : Integer) : TCollComp;

Var
  I : Integer;
  T : TMyItem;

begin
  Result:=EmptyComp;
  Result.Name:='C'+IntToStr(Anr);
  For I:=0 to ANr-1 do
    begin
    T:=Result.MyColl.Add as TMyItem;
    T.Nr:=I; // not I+1, so the default value gets tested too
    T.Str:=IntToStr(I+1);
    end;
end;

Procedure TTestCollectionStream.TestEmpty;

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(0);
  try
    CB:=EmptyComp;
    Try
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.TestNr(ACount : Integer);

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(ACount);
  try
    CB:=EmptyComp;
    Try
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.TestClear;

Var
 CA,CB : TCollComp;

begin
  CA:=CreateColl(3);
  try
    CB:=CreateColl(1);
    CB.Name:='';
    Try
      // CB collection should be cleared before loading.
      CB.FromStream(CA.ToStream);
      CompareColl(CA.MyColl,CB.MyColl);
    Finally
      CB.Free;
    end;
  Finally
    CA.Free;
  end;
end;

Procedure TTestCollectionStream.Test1;

begin
  TestNr(1);
end;

Procedure TTestCollectionStream.Test2;

begin
  TestNr(2);
end;

Procedure TTestCollectionStream.Test3;

begin
  TestNr(3);
end;

begin
  RegisterTests([TTestComponentStream,TTestCollectionStream]);
end.
