unit tests.value;

{$mode ObjFPC}{$H+}

interface

uses
  fpcunit,testregistry, testutils,
  Classes, SysUtils, Rtti;

Type

  { TTestTValue }

  TTestTValue = class(TTestCase)
  private
    FSrc: Variant;
    FValue: TValue;
    FVarRec: TVarRec;
  Public
    Procedure Setup; override;
    Procedure TearDown; override;
    Procedure DoFromVariant;
    Procedure DoFromVarRec;
    Property Value : TValue Read FValue;
    Property Src : Variant Read FSrc;
    Property VarRec : TVarRec Read FVarRec;
  Published
    Procedure TestFromVariantInteger;
    Procedure TestFromVariantBoolean;
    Procedure TestFromVariantSmallInt;
    Procedure TestFromVariantOleStr;
    Procedure TestFromVariantInt64;
    Procedure TestFromVariantQWord;
    Procedure TestFromVariantShortInt;
    Procedure TestFromVariantByte;
    Procedure TestFromVariantWord;
    Procedure TestFromVariantLongWord;
    Procedure TestFromVariantSingle;
    Procedure TestFromVariantDouble;
    Procedure TestFromVariantDate;
    Procedure TestFromVariantDispatch;
    Procedure TestFromVariantError;
    Procedure TestFromVariantUnknown;
    Procedure TestFromVariantCurrency;
    Procedure TestFromVariantString;
    Procedure TestFromVariantUnicodeString;
    Procedure TestFromVarrecInteger;
    Procedure TestFromVarrecBoolean;
    Procedure TestFromVarRecChar;
    Procedure TestFromVarRecExtended;
    Procedure TestFromVarRecString;
    Procedure TestFromVarRecPointer;
    Procedure TestFromVarRecPChar;
    Procedure TestFromVarRecObject;
    Procedure TestFromVarRecClass;
    Procedure TestFromVarRecWideChar;
    Procedure TestFromVarRecPWideChar;
    Procedure TestFromVarRecAnsiString;
    Procedure TestFromVarRecCurrency;
    Procedure TestFromVarRecVariant;
    Procedure TestFromVarRecInterface;
    Procedure TestFromVarRecWideString;
    Procedure TestFromVarRecInt64;
    Procedure TestFromVarRecQWord;
    Procedure TestFromVarRecUnicodeString;
    Procedure TestArrayOfConstToTValue;
  end;

  { TMyUNknown }

  TMyUNknown = Class(TInterfacedObject,IDispatch)
    function GetTypeInfoCount(out count : longint) : HResult;stdcall;
    function GetTypeInfo(Index,LocaleID : longint; out TypeInfo): HResult;stdcall;
    function GetIDsOfNames(const iid: TGUID; names: Pointer; NameCount, LocaleID: LongInt; DispIDs: Pointer) : HResult;stdcall;
    function Invoke(DispID: LongInt;const iid : TGUID; LocaleID : longint; Flags: Word;var params; VarResult,ExcepInfo,ArgErr : pointer) : HResult;stdcall;
  end;


implementation

uses variants;

{ TTestTValue }

procedure TTestTValue.Setup;
begin
  inherited Setup;
  FValue:=Default(TValue);
  FSrc:=unassigned;
end;

procedure TTestTValue.TearDown;
begin
  FValue:=Default(TValue);
  FSrc:=unassigned;
  inherited TearDown;
end;

procedure TTestTValue.DoFromVariant;
begin
  FValue:=TValue.FromVariant(Src);
end;

procedure TTestTValue.DoFromVarRec;
begin
  FValue:=TValue.FromVarRec(FVarRec);
end;

procedure TTestTValue.TestFromVarrecInteger;
begin
  FVarrec.VType:=vtInteger;
  FVarrec.VInteger:=1;
  DoFromVarRec;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Integer)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVarrecBoolean;
begin
  FVarrec.VType:=vtBoolean;
  FVarrec.VBoolean:=True;
  DoFromVarRec;
  CheckEquals(True,Value.AsBoolean,'Value');
  CheckTrue(TypeInfo(Boolean)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecChar;
begin
  FVarrec.VType:=vtChar;
  FVarrec.VChar:='c';
  DoFromVarRec;
  CheckEquals('c',Value.AsAnsiChar,'Value');
  CheckTrue(TypeInfo(AnsiChar)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecExtended;

var
  E : Extended;

begin
  E:=1.23;
  FVarRec.VExtended:=@E;
  FVarRec.vType:=vtExtended;
  DoFromVarRec;
  CheckEquals(1.23,Value.AsExtended,0.01,'Value');
  CheckTrue(TypeInfo(Extended)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecString;

Var
  s : ShortString;

begin
  S:='123';
  FVarrec.VType:=vtString;
  FVarrec.VString:=@S;
  DoFromVarRec;
  CheckEquals('123',Value.AsString,'Value');
  CheckTrue(TypeInfo(ShortString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecPointer;
Var
  s : ShortString;

begin
  S:='123';
  FVarrec.VType:=vtPointer;
  FVarrec.VString:=@S;
  DoFromVarRec;
  CheckTrue(@S=Value.AsPointer,'Value');
  CheckTrue(TypeInfo(Pointer)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecPChar;
Var
  s : AnsiString;

begin
  S:='123';
  FVarrec.VType:=vtPChar;
  FVarrec.VPChar:=PAnsiChar(S);
  DoFromVarRec;
  CheckTrue(S=Value.AsAnsiString,'Value');
  // In delphi it is String, but not widestring !
  CheckTrue(TypeInfo(AnsiString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecObject;
Var
  C : TObject;

begin
  C:=TComponent.Create(Nil);
  FVarrec.VType:=vtObject;
  FVarrec.VObject:=C;
  DoFromVarRec;
  CheckSame(C,Value.AsObject,'Value');
  // In delphi it is String, but not widestring !
  CheckTrue(TypeInfo(TComponent)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, True,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecClass;
Var
  C : TClass;

begin
  C:=TComponent;
  FVarrec.VType:=vtClass;
  FVarrec.VClass:=C;
  DoFromVarRec;
  CheckEquals(C,Value.AsClass,'Value');
  // In delphi it is String, but not widestring !
  CheckTrue(TypeInfo(TClass)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, True,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');

end;

procedure TTestTValue.TestFromVarRecWideChar;
begin
  FVarrec.VType:=vtWideChar;
  FVarrec.VWideChar:='c';
  DoFromVarRec;
  CheckEquals('c',Value.AsWideChar,'Value');
  CheckTrue(TypeInfo(WideChar)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecPWideChar;
Var
  s : WideString;

begin
  S:='123';
  FVarrec.VType:=vtPWideChar;
  FVarrec.VPWideChar:=PWideChar(S);
  DoFromVarRec;
  CheckEquals('123',Value.AsUnicodeString,'Value');
  CheckTrue(TypeInfo(WideString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecAnsiString;
Var
  s : AnsiString;

begin
  S:='123';
  FVarrec.VType:=vtAnsiString;
  FVarrec.VAnsiString:=Pointer(S);
  DoFromVarRec;
  CheckEquals('123',Value.AsAnsiString,'Value');
  CheckTrue(TypeInfo(AnsiString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecCurrency;

var
  C : Currency;

begin
  C:=1.23;
  FVarRec.VCurrency:=@C;
  FVarRec.vType:=vtCurrency;
  DoFromVarRec;
  CheckEquals(1.23,Value.AsCurrency,0.01,'Value');
  CheckTrue(TypeInfo(Currency)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecVariant;

var
  V : Variant;

begin
  V:='1.23';
  FVarRec.VVariant:=@V;
  FVarRec.vType:=vtVariant;
  DoFromVarRec;
  CheckEquals(V,String(Value.AsVariant),'Value');
  CheckTrue(TypeInfo(Variant)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecInterface;
Var
  U : IInterface;

begin
  U:=TMyUNknown.Create;
  FVarRec.VInterface:=U;
  FVarRec.VType:=vtInterface;
  DoFromVarRec;
  CheckTrue(U=Value.AsInterface,'Value');
  CheckTrue(TypeInfo(IInterface)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecWideString;
Var
  s : WideString;

begin
  S:='123';
  FVarrec.VType:=vtWideString;
  FVarrec.VWideString:=Pointer(S);
  DoFromVarRec;
  CheckEquals('123',Value.AsUnicodeString,'Value');
  CheckTrue(TypeInfo(WideString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecInt64;

Var
  I : Int64;

begin
  I:=Int64(1);
  FVarRec.VInt64:=@I;
  FVarRec.vType:=vtInt64;
  DoFromVarRec;
  CheckEquals(1,Value.AsInt64,'Value');
  CheckTrue(TypeInfo(Int64)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecQWord;

Var
  Q : QWord;


begin
  Q:=1;
  FVarRec.VQWord:=@Q;
  FVarRec.vType:=vtQWord;
  DoFromVarRec;
  CheckEquals(1,Value.AsUInt64,'Value');
  CheckTrue(TypeInfo(QWord)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVarRecUnicodeString;

Var
  s : UnicodeString;

begin
  S:='123';
  FVarrec.VType:=vtUnicodeString;
  FVarrec.VUnicodeString:=Pointer(S);
  DoFromVarRec;
  CheckEquals('123',Value.AsUnicodeString,'Value');
  CheckTrue(TypeInfo(UnicodeString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;


procedure TTestTValue.TestFromVariantInteger;


begin
  FSrc:=Integer(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Longint)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;


procedure TTestTValue.TestFromVariantBoolean;
begin
  FSrc:=True;
  DoFromVariant;
  CheckEquals(True,Value.AsBoolean,'Value');
  CheckTrue(TypeInfo(Boolean)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantSmallInt;
begin
  FSrc:=SmallInt(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(SmallInt)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantOleStr;

begin
  FSrc:=WideString('1.23');
  DoFromVariant;
  CheckEquals('1.23',Value.AsUnicodeString,'Value');
  CheckTrue(TypeInfo(WideString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVariantInt64;
begin
  FSrc:=Int64(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInt64,'Value');
  CheckTrue(TypeInfo(Int64)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantQWord;
begin
  FSrc:=QWord(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInt64,'Value');
  CheckTrue(TypeInfo(QWord)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantShortInt;
begin
  FSrc:=ShortInt(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Shortint)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantByte;
begin
  FSrc:=Byte(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Byte)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantWord;
begin
  FSrc:=Word(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Word)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantLongWord;
begin
  FSrc:=Cardinal(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Cardinal)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantSingle;
begin
  FSrc:=Single(1.23); // Results in double...
  VarCast(FSrc,FSrc,varSingle);
  DoFromVariant;
  CheckEquals(1.23,Value.AsSingle,0.01,'Value');
  CheckTrue(TypeInfo(Single)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVariantDouble;
begin
  FSrc:=Double(1.23);
  DoFromVariant;
  CheckEquals(1.23,Value.AsDouble,0.01,'Value');
  CheckTrue(TypeInfo(Double)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVariantDate;

Var
  D : TDateTime;

begin
  D:=Time;
  FSrc:=D;
  DoFromVariant;
  CheckEquals(D,Value.AsDateTime,0.01,'Value');
  CheckTrue(TypeInfo(TDateTime)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVariantDispatch;
Var
  U : IDispatch;

begin
  U:=TMyUNknown.Create;
  FSrc:=U;
  DoFromVariant;
  CheckTrue(U=Value.AsInterface,'Value');
  CheckTrue(TypeInfo(IDispatch)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVariantError;

begin
  TVarData(FSrc).verror:=S_FALSE;
  TVarData(FSrc).vtype:=varError;
  DoFromVariant;
  CheckTrue(S_FALSE=Value.AsError,'Value');
  CheckTrue(TypeInfo(HRESULT)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestTValue.TestFromVariantUnknown;

Var
  U : IInterface;

begin
  U:=TMyUNknown.Create;
  FSrc:=U;
  DoFromVariant;
  CheckTrue(U=Value.AsInterface,'Value');
  CheckTrue(TypeInfo(IInterface)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVariantCurrency;
begin
  FSrc:=Currency(1.23);
  DoFromVariant;
  CheckEquals(1.23,Value.AsCurrency,0.01,'Value');
  CheckTrue(TypeInfo(Currency)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVariantString;
begin
  FSrc:='1.23';
  DoFromVariant;
  CheckEquals('1.23',Value.AsString,'Value');
  CheckTrue(TypeInfo(AnsiString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestTValue.TestFromVariantUnicodeString;
begin
  TVarData(FSrc).vustring:=Pointer(UnicodeString('1.23'));
  TVarData(FSrc).vtype:=varUString;
  DoFromVariant;
  CheckEquals('1.23',Value.AsString,'Value');
  CheckTrue(TypeInfo(UnicodeString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;



procedure TTestTValue.TestArrayOfConstToTValue;

Var
  S:TValueArray;

begin
  S:=ArrayOfConstToTValueArray([1,'something',1.23]);
  CheckEquals(3,Length(S),'Length');
  CheckEquals(1,S[0].AsInteger,'Value 1');
  CheckEquals('something',S[1].AsString,'Value 3');
  CheckEquals(1.23,S[2].AsDouble,0.01,'Value 3');
end;

{ TMyUNknown }

function TMyUNknown.GetTypeInfoCount(out count: longint): HResult; stdcall;
begin
  count:=0;
  Result:=S_OK;
end;

function TMyUNknown.GetTypeInfo(Index, LocaleID: longint; out TypeInfo
  ): HResult; stdcall;
begin

  Result:=S_OK;
end;

function TMyUNknown.GetIDsOfNames(const iid: TGUID; names: Pointer; NameCount,
  LocaleID: LongInt; DispIDs: Pointer): HResult; stdcall;
begin
  Result:=S_OK;
end;

function TMyUNknown.Invoke(DispID: LongInt; const iid: TGUID;
  LocaleID: longint; Flags: Word; var params; VarResult, ExcepInfo,
  ArgErr: pointer): HResult; stdcall;
begin
  Result:=S_OK;
end;


initialization
  RegisterTest(TTestTValue);
end.

