{$mode objfpc}
{$R-}

program TestProgram;
uses
{$ifdef go32v2}
  dpmiexcp,
{$endif}
    test1, Test2;


const A =  1234;
      C =  #1#2#3#4;
      ConstBool1 = true;
      ConstBool2 = boolean(5);
      ConstChar = 'A';
      ConstSet = ['A'..'Z'];
      ConstSet2 = [15..254];
      ConstFloat = 3.1415;

{$i empty.inc}

type
      PObj = ^TObj;
      TObj = object
        constructor Init;
        function    Func: boolean;
        procedure   Proc; virtual;
        destructor  Done; virtual;
      private
        Z: integer;
      end;

      TObj2 = object(TObj)
        procedure Proc; virtual;
      end;

      TObj3  = object(TObj)
      end;

      TObj32 = object(TObj3)
      end;

      TObj4 = object(TObj)
      end;

      TClass = class
        name : string;
        constructor Create;
      end;

      TClass2 = class(TClass)
        X : longint;
        constructor Create;
      end;

      EnumTyp = (enum1,enum2,enum3);
      ArrayTyp = array[1..10] of EnumTyp;
      ProcTyp = function(A: word; var B: longint; const C: EnumTyp): real;
      SetTyp = set of EnumTyp;

const
      ConstOrd = enum1;

var Hello : word;
    X: PRecord;
    Bool: boolean;
    T : TRecord;
    Str20 : string[20];
    Str255: string;
    ArrayW: array[2..45] of word;
    ArrayVar: ArrayTyp;
    EnumVar: (enumElem1,enumElem2,enumElem3);
    EnumVar2: EnumTyp;
    FileVar: file;
    FileVarR: file of TRecord;
    FileVarW: file of word;
    ProcVar: procedure;
    ProcVarD: function(X: real): boolean;
    ProcVarI: ProcTyp;
    SetVarD: set of char;
    SetVarI: SetTyp;
    Float1: real;
    Float2: double;
    Float3: comp;
    Float4: extended;
    Pointer1: pointer;
    Pointer2: PObj;
    ClassVar1: TClass;
    ClassVar2: TClass2;
    Obj1: TObj;
    Obj2: TObj2;
    CharArray : Array[1..2000] of char;
    ExtendedArray : Array[1..2000] of extended;
    ExtendedPackedArray : packed Array[1..2000] of extended;
    SingleArrayArray : Array[1..10,1..10] of single;

constructor TObj.Init;
begin
  Z:=1;
end;

function TObj.Func: boolean;
begin
  Func:=true;
end;

procedure TObj.Proc;
begin
  if Func=false then Halt;
end;

destructor TObj.Done;
begin
end;

procedure TObj2.Proc;
begin
  Z:=4;
end;

constructor TClass.Create;
begin
  Name:='TClass instance';
end;

constructor TClass2.Create;
begin
  Name:='TClass2 instance';
  X:=7;
end;

function Func1(x,z : word; var y : boolean; const r: TRecord): shortint;

var loc : string;

  procedure test_local(c,f : longint);
   var
      int_loc : longint;
   begin
      Writeln('dummy for browser');
   end;

  procedure indirect_call;
   var
     loc : longint;
   begin
     loc:=1;
     test_local(5,7);
   end;
begin
  loc:='This is a string';
  if Hello=0 then X:=0 else X:=1;
  test_local(0,2);
  indirect_call;
  Func1:=X;
end;

var i,j : longint;
    Length : longint;

BEGIN
{$ifdef m68k}
  asm
    beq @L13
    bhi @L13
    blo @L13
    dbeq d0,@L13
    dbcs d0,@L13
 //   dblo d0,@L13
@L13:
  end;
{$endif}
  for i:=low(ExtendedArray) to high(ExtendedArray) do
    ExtendedArray[i]:=i;
  for i:=low(ExtendedPackedArray) to high(ExtendedPackedArray) do
    ExtendedPackedArray[i]:=i;

  for i:=1 to 10 do
    for j:=1 to 10 do
      SingleArrayArray[i,j]:=i*j;

  ClassVar1:=TClass2.create;
  Obj1.Init;
  pointer2:=@Obj1;
  Writeln('Obj1.Z=',Obj1.Z);
  Obj1.done;
  X:=nil;
  // fg
  for i:=1 to 2000 do
    CharArray[i]:=chr(32+(i mod (255-32)));
  writeln('Hello world!');
  Writeln('ParamCount = ',ParamCount);
  For i:=0 to paramcount do
   writeln('Paramstr(',i,') = '+Paramstr(i));
  writeln(IsOdd(3));
  writeln(Func1(5,5,Bool,T));
  new(X);
  new(X^.next);
  X^.next^.next:=X;
  dispose(X);
 { for i:=1 to 99 do
    Writeln('Line ',i); }
  Halt(4);
END.
