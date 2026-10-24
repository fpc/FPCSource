{ TObjectVMT describes the VMT of old-style objects }
program tobjvmt;

{$mode objfpc}
{$objectchecks on}

type
  PBase = ^TBase;
  TBase = object
    FValue: LongInt;
    // Initializes the object
    constructor Init;
    // Returns 1
    function Foo: LongInt; virtual;
    // Returns 10
    function Bar: LongInt; virtual;
    // Finalizes the object
    destructor Done; virtual;
  end;

  PChild = ^TChild;
  TChild = object(TBase)
    FExtra: array[0..2] of Pointer;
    // Returns 2
    function Foo: LongInt; virtual;
    // Returns 30
    function Baz: LongInt; virtual;
  end;

  TNoVmt = object
    FData: Word;
  end;

  TOnNoVmt = object(TNoVmt)
    // Initializes the object
    constructor Init;
    // Returns 5
    function Qux: LongInt; virtual;
  end;

var
  lErrors: Integer;


constructor TBase.Init;

begin
  FValue:=0;
end;


function TBase.Foo: LongInt;

begin
  Result:=1;
end;


function TBase.Bar: LongInt;

begin
  Result:=10;
end;


destructor TBase.Done;

begin
end;


function TChild.Foo: LongInt;

begin
  Result:=2;
end;


function TChild.Baz: LongInt;

begin
  Result:=30;
end;


constructor TOnNoVmt.Init;

begin
end;


function TOnNoVmt.Qux: LongInt;

begin
  Result:=5;
end;


{ Reports a failed check }
procedure Check(aCondition: Boolean; const aMsg: string);

begin
  if not aCondition then
    begin
      writeln('FAIL: ', aMsg);
      Inc(lErrors);
    end;
end;


{ Returns virtual method slot aIndex of aVmt }
function Slot(aVmt: PObjectVMT; aIndex: Integer): CodePointer;

begin
  Result:=PCodePointer(PByte(aVmt)+SizeOf(TObjectVMT))[aIndex];
end;


{ Checks the header fields of the VMTs }
procedure TestHeader;

var
  lBase, lChild, lOnNoVmt: PObjectVMT;

begin
  Check(SizeOf(TObjectVMT)=2*SizeOf(SizeInt)+SizeOf(Pointer), 'TObjectVMT has two SizeInt fields and a pointer');
  lBase:=PObjectVMT(TypeOf(TBase));
  lChild:=PObjectVMT(TypeOf(TChild));
  lOnNoVmt:=PObjectVMT(TypeOf(TOnNoVmt));
  Check(lBase^.vInstanceSize=SizeOf(TBase), 'TBase vInstanceSize is SizeOf(TBase)');
  Check(lBase^.vInstanceSize2=-SizeOf(TBase), 'TBase vInstanceSize2 is -SizeOf(TBase)');
  Check(lChild^.vInstanceSize=SizeOf(TChild), 'TChild vInstanceSize is SizeOf(TChild)');
  Check(lChild^.vInstanceSize2=-SizeOf(TChild), 'TChild vInstanceSize2 is -SizeOf(TChild)');
  Check(lOnNoVmt^.vInstanceSize=SizeOf(TOnNoVmt), 'TOnNoVmt vInstanceSize is SizeOf(TOnNoVmt)');
  Check(lBase^.vParentRef=nil, 'TBase has no parent reference');
  Check(lBase^.vParent=nil, 'TBase has no parent');
  Check(lChild^.vParentRef<>nil, 'TChild has a parent reference');
  Check(lChild^.vParent=lBase, 'TChild parent is the TBase VMT');
  Check(lOnNoVmt^.vParent=nil, 'Parent without VMT gives nil vParent');
end;


{ Checks the virtual method pointers after the header }
procedure TestSlots;

var
  lBase, lChild, lOnNoVmt: PObjectVMT;

begin
  lBase:=PObjectVMT(TypeOf(TBase));
  lChild:=PObjectVMT(TypeOf(TChild));
  lOnNoVmt:=PObjectVMT(TypeOf(TOnNoVmt));
  Check(Slot(lBase, 0)=CodePointer(@TBase.Foo), 'TBase slot 0 is TBase.Foo');
  Check(Slot(lBase, 1)=CodePointer(@TBase.Bar), 'TBase slot 1 is TBase.Bar');
  Check(Slot(lBase, 2)=CodePointer(@TBase.Done), 'TBase slot 2 is TBase.Done');
  Check(Slot(lBase, 3)=nil, 'TBase method list ends with nil');
  Check(Slot(lChild, 0)=CodePointer(@TChild.Foo), 'TChild slot 0 is TChild.Foo');
  Check(Slot(lChild, 1)=CodePointer(@TBase.Bar), 'TChild slot 1 is TBase.Bar');
  Check(Slot(lChild, 2)=CodePointer(@TBase.Done), 'TChild slot 2 is TBase.Done');
  Check(Slot(lChild, 3)=CodePointer(@TChild.Baz), 'TChild slot 3 is TChild.Baz');
  Check(Slot(lChild, 4)=nil, 'TChild method list ends with nil');
  Check(Slot(lOnNoVmt, 0)=CodePointer(@TOnNoVmt.Qux), 'TOnNoVmt slot 0 is TOnNoVmt.Qux');
end;


{ Checks instances, allocation and checked virtual calls through the parent chain }
procedure TestInstances;

var
  lBase: PBase;
  lStatic: TChild;
  lOnNoVmt: TOnNoVmt;

begin
  lBase:=New(PChild, Init);
  Check(PObjectVMT(TypeOf(lBase^))=PObjectVMT(TypeOf(TChild)), 'Instance created as TChild has the TChild VMT');
  Check(lBase^.Foo=2, 'Virtual call through PBase reaches TChild.Foo');
  Check(lBase^.Bar=10, 'Virtual call through PBase reaches TBase.Bar');
  Dispose(lBase, Done);
  lStatic.Init;
  Check(PObjectVMT(TypeOf(lStatic))=PObjectVMT(TypeOf(TChild)), 'Initialized TChild variable has the TChild VMT');
  Check(lStatic.Baz=30, 'Virtual call on a TChild variable reaches TChild.Baz');
  lStatic.Done;
  lOnNoVmt.Init;
  Check(lOnNoVmt.Qux=5, 'Virtual call on an object whose parent has no VMT');
end;


begin
  lErrors:=0;
  TestHeader;
  TestSlots;
  TestInstances;
  if lErrors>0 then
    begin
      writeln(lErrors, ' errors');
      Halt(1);
    end;
  writeln('ok');
end.
