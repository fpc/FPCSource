{ Tests the compile time operator "type of <operand>", modeswitch typeinquiry }
program ttypeinquiry1;

{$mode objfpc}{$H+}

uses
  typinfo;

type
  generic TBird<T> = class
  public
    Value: T;
  end;

  TRec = record
    b: byte;
    w: word;
  end;

  TIntArray = array of integer;

  TEnum = (eA, eB, eC);

const
  CInt = 5;
  CEnum = eB;
  CTyped: word = 7;

var
  GlobalInt: integer = 0;
  GlobalByte: byte = 0;
  GlobalWord: word = 0;
  GlobalDWord: dword = 0;
  GlobalRec: TRec;
  GlobalArr: TIntArray;
  GlobalStr: string = '';

type
  { a plain alias }
  TInt = type of GlobalInt;
  { a distinct type }
  TUniqueInt = type type of GlobalInt;

  TObj = class
  private
    FVal: word;
  public
    { property type }
    property Val: type of GlobalWord read FVal write FVal;
  end;

  TFieldRec = record
    { field type }
    f: type of GlobalByte;
  end;

var
  { variable type }
  VarOfVar: type of GlobalByte;
  { derived types }
  DynArr: array of type of GlobalInt;
  PtrTo: ^type of GlobalInt;
  SetOf: set of type of GlobalByte;
  Gen: specialize TBird<type of GlobalInt>;
  { expressions }
  Sum: type of (GlobalWord+GlobalDWord);
  ConstSmall: type of (1+2);
  ConstBig: type of (int64(1) shl 40);
  { postfix operators bind to the operand }
  Member: type of GlobalRec.w;
  Element: type of GlobalArr[0];
  Deref: type of PtrTo^;
  StrVar: type of GlobalStr;
  AliasVar: TInt;
  UniqueVar: TUniqueInt;
  FieldRec: TFieldRec;
  Obj: TObj;
  { constants }
  ConstVar: type of CInt;
  EnumVar: type of CEnum;
  TypedConstVar: type of CTyped;
  HighVar: type of High(longint);

procedure Check(Ok: boolean; Id: longint);
begin
  if not Ok then
    begin
      writeln('failed: ',Id);
      halt(Id);
    end;
end;

{ parameter type }
procedure TakesByte(a: type of GlobalByte);
begin
  Check(SizeOf(a)=1,10);
  Check(Low(a)=0,11);
  Check(High(a)=255,12);
end;

{ function result type }
function FuncByte: type of GlobalByte;
begin
  Result:=42;
end;

{ the result type of a function }
function FuncOfFuncResult: type of FuncByte;
begin
  Result:=43;
end;

{ open array parameter element type }
function SumOpenArray(const a: array of type of GlobalByte): longint;
var
  i: longint;
begin
  Result:=0;
  for i:=Low(a) to High(a) do
    Inc(Result,a[i]);
end;

begin
  { the type of a variable }
  Check(SizeOf(VarOfVar)=1,20);
  Check(TypeInfo(type of GlobalByte)=TypeInfo(byte),21);

  { array of, ^, set of }
  SetLength(DynArr,2);
  DynArr[0]:=7;
  Check(TypeInfo(DynArr[0])=TypeInfo(integer),30);
  PtrTo:=@GlobalInt;
  GlobalInt:=99;
  Check(PtrTo^=99,31);
  Check(TypeInfo(PtrTo^)=TypeInfo(integer),32);
  SetOf:=[1,2];
  Check((1 in SetOf) and not (3 in SetOf),33);

  { generic specialization }
  Gen:=specialize TBird<type of GlobalInt>.Create;
  Gen.Value:=5;
  Check(Gen.Value=5,40);
  Check(TypeInfo(Gen.Value)=TypeInfo(integer),41);
  Gen.Free;

  { expressions in brackets get the type the compiler gives them }
  Check(SizeOf(Sum)=SizeOf(GlobalWord+GlobalDWord),50);
  Check(SizeOf(ConstSmall)=SizeOf(1+2),51);
  Check(SizeOf(ConstBig)=8,52);

  { postfix operators }
  Member:=0;
  Element:=0;
  Deref:=0;
  Check(TypeInfo(Member)=TypeInfo(word),60);
  Check(TypeInfo(Element)=TypeInfo(integer),61);
  Check(TypeInfo(Deref)=TypeInfo(integer),62);
  StrVar:='abc';
  Check(StrVar='abc',63);
  Check(TypeInfo(StrVar)=TypeInfo(string),64);

  { alias and distinct type }
  AliasVar:=1;
  UniqueVar:=2;
  Check(AliasVar+UniqueVar=3,70);
  Check(TypeInfo(AliasVar)=TypeInfo(integer),71);
  Check(TypeInfo(UniqueVar)<>TypeInfo(integer),72);

  { record field and property }
  FieldRec.f:=3;
  Check(TypeInfo(FieldRec.f)=TypeInfo(byte),80);
  Obj:=TObj.Create;
  Obj.Val:=$1234;
  Check(Obj.Val=$1234,81);
  Obj.Free;

  { parameters and function results }
  GlobalByte:=1;
  TakesByte(GlobalByte);
  Check(FuncByte=42,90);
  Check(FuncOfFuncResult=43,91);
  Check(SizeOf(FuncOfFuncResult)=1,92);
  Check(SumOpenArray([1,2,3])=6,93);

  { constants }
  ConstVar:=1;
  Check(ConstVar=1,94);
  Check(SizeOf(ConstVar)=SizeOf(CInt),95);
  EnumVar:=eC;
  Check(TypeInfo(EnumVar)=TypeInfo(TEnum),96);
  TypedConstVar:=CTyped;
  Check(TypedConstVar=7,97);
  Check(TypeInfo(TypedConstVar)=TypeInfo(word),98);
  HighVar:=1;
  Check(TypeInfo(HighVar)=TypeInfo(longint),99);

  { intrinsics expecting a type }
  Check(SizeOf(type of GlobalInt)=SizeOf(integer),100);
  Check(High(type of GlobalByte)=255,101);
  Check(Low(type of GlobalByte)=0,102);
  Check(Default(type of GlobalByte)=0,103);
  Check(TypeInfo(type of GlobalInt)=TypeInfo(integer),104);
  Check(GetTypeKind(type of GlobalByte)=tkInteger,105);

  { type cast: GlobalByte is a variable, so (GlobalWord) is a cast }
  GlobalWord:=$1234;
  Check(type of GlobalByte(GlobalWord)=$34,110);
  Check(SizeOf(type of GlobalWord(GlobalByte))=2,111);

  writeln('ok');
end.
