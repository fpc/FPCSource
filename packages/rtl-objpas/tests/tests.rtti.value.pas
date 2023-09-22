unit tests.rtti.value;

{$mode ObjFPC}{$H+}

interface

uses
  fpcunit,testregistry, testutils, typinfo,
  Classes, SysUtils, Rtti;

Type
  TTestValueGeneral = Class(TTestCase)
  Published
    procedure TestDataSize;
    procedure TestDataSizeEmpty;
    procedure TestReferenceRawData;
    procedure TestReferenceRawDataEmpty;

    procedure TestIsManaged;
  end;

  TTestValueSimple = Class(TTestCase)
  private
    procedure MakeFromOrdinalTObject;
    procedure MakeFromOrdinalSet;
    procedure MakeFromOrdinalString;
    procedure MakeFromOrdinalNil;
  Published
    // Moved here from Tests.rtti
    procedure TestIsType;
    procedure TestMakeNil;
    procedure TestMakeObject;
    procedure TestMakeSingle;
    procedure TestMakeDouble;
    procedure TestMakeExtended;
    procedure TestMakeCurrency;
    procedure TestMakeComp;
    procedure TestMakeEnum;
    procedure TestMakeAnsiChar;
    procedure TestMakeWideChar;

    procedure TestMakeNativeInt;


    procedure TestMakeGenericNil;
    procedure TestMakeGenericLongInt;
    procedure TestMakeGenericString;
    procedure TestMakeGenericObject;
    procedure TestMakeGenericDouble;
    procedure TestMakeGenericAnsiChar;
    procedure TestMakeGenericWideChar;

    procedure TestFromOrdinal;
  end;

  { TTestValueArray }

  TTestValueArray = class(TTestCase)
  Published
    procedure TestMakeArrayDynamic;
    procedure TestMakeArrayStatic;
    procedure TestMakeFromArray;
    {$ifdef fpc}
    procedure TestMakeArrayOpen;
    Procedure TestOpenArrayToDyn;
    {$ENDIF}
  end;

  { TTestValueVariant }

  TTestValueVariant = class(TTestCase)
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

uses tests.rtti.types, variants;

{ TTestValueVariant }

procedure TTestValueVariant.Setup;
begin
  inherited Setup;
  FValue:=Default(TValue);
  FSrc:=unassigned;
end;

procedure TTestValueVariant.TearDown;
begin
  FValue:=Default(TValue);
  FSrc:=unassigned;
  inherited TearDown;
end;

procedure TTestValueVariant.DoFromVariant;
begin
  FValue:=TValue.FromVariant(Src);
end;

procedure TTestValueVariant.DoFromVarRec;
begin
  FValue:=TValue.FromVarRec(FVarRec);
end;

procedure TTestValueVariant.TestFromVarrecInteger;
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

procedure TTestValueVariant.TestFromVarrecBoolean;
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

procedure TTestValueVariant.TestFromVarRecChar;
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

procedure TTestValueVariant.TestFromVarRecExtended;

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

procedure TTestValueVariant.TestFromVarRecString;

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

procedure TTestValueVariant.TestFromVarRecPointer;
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

procedure TTestValueVariant.TestFromVarRecPChar;
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

procedure TTestValueVariant.TestFromVarRecObject;
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

procedure TTestValueVariant.TestFromVarRecClass;
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

procedure TTestValueVariant.TestFromVarRecWideChar;
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

procedure TTestValueVariant.TestFromVarRecPWideChar;
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

procedure TTestValueVariant.TestFromVarRecAnsiString;
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

procedure TTestValueVariant.TestFromVarRecCurrency;

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

procedure TTestValueVariant.TestFromVarRecVariant;

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

procedure TTestValueVariant.TestFromVarRecInterface;
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

procedure TTestValueVariant.TestFromVarRecWideString;
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

procedure TTestValueVariant.TestFromVarRecInt64;

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

procedure TTestValueVariant.TestFromVarRecQWord;

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

procedure TTestValueVariant.TestFromVarRecUnicodeString;

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


procedure TTestValueVariant.TestFromVariantInteger;


begin
  FSrc:=Integer(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Longint)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;


procedure TTestValueVariant.TestFromVariantBoolean;
begin
  FSrc:=True;
  DoFromVariant;
  CheckEquals(True,Value.AsBoolean,'Value');
  CheckTrue(TypeInfo(Boolean)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantSmallInt;
begin
  FSrc:=SmallInt(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(SmallInt)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantOleStr;

begin
  FSrc:=WideString('1.23');
  DoFromVariant;
  CheckEquals('1.23',Value.AsUnicodeString,'Value');
  CheckTrue(TypeInfo(WideString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantInt64;
begin
  FSrc:=Int64(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInt64,'Value');
  CheckTrue(TypeInfo(Int64)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantQWord;
begin
  FSrc:=QWord(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInt64,'Value');
  CheckTrue(TypeInfo(QWord)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantShortInt;
begin
  FSrc:=ShortInt(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Shortint)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantByte;
begin
  FSrc:=Byte(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Byte)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantWord;
begin
  FSrc:=Word(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Word)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantLongWord;
begin
  FSrc:=Cardinal(1);
  DoFromVariant;
  CheckEquals(1,Value.AsInteger,'Value');
  CheckTrue(TypeInfo(Cardinal)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, True,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantSingle;
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

procedure TTestValueVariant.TestFromVariantDouble;
begin
  FSrc:=Double(1.23);
  DoFromVariant;
  CheckEquals(1.23,Value.AsDouble,0.01,'Value');
  CheckTrue(TypeInfo(Double)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantDate;

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

procedure TTestValueVariant.TestFromVariantDispatch;
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

procedure TTestValueVariant.TestFromVariantError;

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

procedure TTestValueVariant.TestFromVariantUnknown;

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

procedure TTestValueVariant.TestFromVariantCurrency;
begin
  FSrc:=Currency(1.23);
  DoFromVariant;
  CheckEquals(1.23,Value.AsCurrency,0.01,'Value');
  CheckTrue(TypeInfo(Currency)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantString;
begin
  FSrc:='1.23';
  DoFromVariant;
  CheckEquals('1.23',Value.AsString,'Value');
  CheckTrue(TypeInfo(AnsiString)=Value.TypeInfo,'Correct typeinfo');
  CheckEquals(Value.IsClass, False,'Class');
  CheckEquals(Value.IsObject, False,'Object');
  CheckEquals(Value.IsOrdinal, False,'Ordinal');
end;

procedure TTestValueVariant.TestFromVariantUnicodeString;
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



procedure TTestValueVariant.TestArrayOfConstToTValue;

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

type
  TMyLongInt = type LongInt;

procedure TTestValueSimple.TestIsType;
{ Delphi does not provide type information for local types :/ }
{type
  TMyLongInt = type LongInt;}
var
  v: TValue;
  l: LongInt;
  ml: TMyLongInt;
begin
  l := 42;
  ml := 42;
  TValue.Make(@l, TypeInfo(LongInt), v);
  Check(v.IsType(TypeInfo(LongInt)));
  Check(not v.IsType(TypeInfo(TMyLongInt)));
  Check(not v.IsType(TypeInfo(String)));
  Check(v.{$ifdef fpc}specialize{$endif} IsType<LongInt>);
  Check(not v.{$ifdef fpc}specialize{$endif} IsType<TMyLongInt>);
  Check(not v.{$ifdef fpc}specialize{$endif} IsType<String>);

  TValue.Make(@ml, TypeInfo(TMyLongInt), v);
  Check(v.IsType(TypeInfo(TMyLongInt)));
  Check(not v.IsType(TypeInfo(LongInt)));
  Check(not v.IsType(TypeInfo(String)));
  Check(v.{$ifdef fpc}specialize{$endif} IsType<TMyLongInt>);
  Check(not v.{$ifdef fpc}specialize{$endif} IsType<LongInt>);
  Check(not v.{$ifdef fpc}specialize{$endif} IsType<String>);
end;

procedure TTestValueSimple.TestMakeNil;
var
  value: TValue;
begin
  TValue.Make(Nil, Nil, value);
  CheckTrue(value.Kind = tkUnknown);
  CheckTrue(value.IsEmpty);
  CheckTrue(value.IsObject);
  CheckTrue(value.IsClass);
  CheckTrue(value.IsOrdinal);
  CheckFalse(value.IsArray);
  CheckTrue(value.AsObject = Nil);
  CheckTrue(value.AsClass = Nil);
  CheckTrue(value.AsInterface = Nil);
  CheckEquals(0, value.AsOrdinal);

  TValue.Make(Nil, TypeInfo(TObject), value);
  CheckTrue(value.IsEmpty);
  CheckTrue(value.IsObject);
  CheckTrue(value.IsClass);
  CheckTrue(value.IsOrdinal);
  CheckFalse(value.IsArray);
  CheckTrue(value.AsObject=Nil);
  CheckTrue(value.AsClass=Nil);
  CheckTrue(value.AsInterface=Nil);
  CheckEquals(0, value.AsOrdinal);

  TValue.Make(Nil, TypeInfo(TClass), value);
  CheckTrue(value.IsEmpty);
  CheckTrue(value.IsClass);
  CheckTrue(value.IsOrdinal);
  CheckFalse(value.IsArray);
  CheckTrue(value.AsObject=Nil);
  CheckTrue(value.AsClass=Nil);
  CheckTrue(value.AsInterface=Nil);
  CheckEquals(0, value.AsOrdinal);

  TValue.Make(Nil, TypeInfo(LongInt), value);
  CheckTrue(value.IsOrdinal);
  CheckFalse(value.IsEmpty);
  CheckFalse(value.IsClass);
  CheckFalse(value.IsObject);
  CheckFalse(value.IsArray);
  CheckEquals(0, value.AsOrdinal);
  CheckEquals(0, value.AsInteger);
  CheckEquals(0, value.AsInt64);
  CheckEquals(0, value.AsUInt64);

  TValue.Make(Nil, TypeInfo(String), value);
  CheckFalse(value.IsEmpty);
  CheckFalse(value.IsObject);
  CheckFalse(value.IsClass);
  CheckFalse(value.IsArray);
  CheckEquals('', value.AsString);
end;

procedure TTestValueSimple.TestMakeObject;
var
  AValue: TValue;
  ATestClass: TTestValueClass;
begin
  ATestClass := TTestValueClass.Create;
  ATestClass.AInteger := 54329;
  TValue.Make(@ATestClass, TypeInfo(TTestValueClass),AValue);
  CheckEquals(AValue.IsClass, False);
  CheckEquals(AValue.IsObject, True);
  Check(AValue.AsObject=ATestClass);
  Check(PPointer(AValue.GetReferenceToRawData)^ = Pointer(ATestClass));
  CheckEquals(TTestValueClass(AValue.AsObject).AInteger, 54329);
  ATestClass.Free;
end;

procedure TTestValueArray.TestMakeArrayDynamic;
var
  arr: TArrayOfLongintDyn;
  value: TValue;
begin
  SetLength(arr, 2);
  arr[0] := 42;
  arr[1] := 21;
  TValue.Make(@arr, TypeInfo(TArrayOfLongintDyn), value);
  CheckEquals(value.IsArray, True);
  CheckEquals(value.IsObject, False);
  CheckEquals(value.IsOrdinal, False);
  CheckEquals(value.IsClass, False);
  CheckEquals(value.GetArrayLength, 2);
  CheckEquals(value.GetArrayElement(0).AsInteger, 42);
  CheckEquals(value.GetArrayElement(1).AsInteger, 21);
  Check(PPointer(value.GetReferenceToRawData)^ = Pointer(arr));
  value.SetArrayElement(0, Integer(84));
  CheckEquals(arr[0], 84);
end;

procedure TTestValueArray.TestMakeArrayStatic;
type
  TArrStat = array[0..1] of LongInt;
  TArrStat2D = array[0..1, 0..1] of LongInt;
var
  arr: TArrStat;
  arr2D: TArrStat2D;
  value: TValue;
begin
  arr[0] := 42;
  arr[1] := 21;
  TValue.Make(@arr, TypeInfo(TArrStat), value);
  CheckEquals(value.IsArray, True);
  CheckEquals(value.IsObject, False);
  CheckEquals(value.IsOrdinal, False);
  CheckEquals(value.IsClass, False);
  CheckEquals(value.GetArrayLength, 2);
  CheckEquals(value.GetArrayElement(0).AsInteger, 42);
  CheckEquals(value.GetArrayElement(1).AsInteger, 21);
  value.SetArrayElement(0, integer(84));
  { since this is a static array the original array isn't touched! }
  CheckEquals(arr[0], 42);

  arr2D[0, 0] := 42;
  arr2D[0, 1] := 21;
  arr2D[1, 0] := 84;
  arr2D[1, 1] := 63;

  TValue.Make(@arr2D, TypeInfo(TArrStat2D), value);
  CheckEquals(value.IsArray, True);
  CheckEquals(value.GetArrayLength, 4);
  CheckEquals(value.GetArrayElement(0).AsInteger, 42);
  CheckEquals(value.GetArrayElement(1).AsInteger, 21);
  CheckEquals(value.GetArrayElement(2).AsInteger, 84);
  CheckEquals(value.GetArrayElement(3).AsInteger, 63);
end;

{$ifdef fpc}
procedure TTestValueArray.TestMakeArrayOpen;

  procedure TestOpenArrayValueCopy(aArr: array of LongInt);
  var
    value: TValue;
  begin
    TValue.MakeOpenArray(@aArr[0], Length(aArr), PTypeInfo(TypeInfo(aArr)), value);
    CheckEquals(value.IsArray, True);
    CheckEquals(value.IsOpenArray, True);
    CheckEquals(value.IsObject, False);
    CheckEquals(value.IsOrdinal, False);
    CheckEquals(value.IsClass, False);
    CheckEquals(value.GetArrayLength, 2);
    CheckEquals(value.GetArrayElement(0).AsInteger, 42);
    CheckEquals(value.GetArrayElement(1).AsInteger, 21);
    value.SetArrayElement(0, Integer(84));
    { since this is an open array the original array is modified! }
    CheckEquals(aArr[0], 84);
  end;

  procedure TestOpenArrayValueVar(var aArr: array of LongInt);
  var
    value: TValue;
  begin
    TValue.MakeOpenArray(@aArr[0], Length(aArr), PTypeInfo(TypeInfo(aArr)), value);
    CheckEquals(value.IsArray, True);
    CheckEquals(value.IsOpenArray, True);
    CheckEquals(value.IsObject, False);
    CheckEquals(value.IsOrdinal, False);
    CheckEquals(value.IsClass, False);
    CheckEquals(value.GetArrayLength, 2);
    CheckEquals(value.GetArrayElement(0).AsInteger, 42);
    CheckEquals(value.GetArrayElement(1).AsInteger, 21);
    value.SetArrayElement(0, 84);
    { since this is an open array the original array is modified! }
    CheckEquals(aArr[0], 84);
  end;

  procedure TestOpenArrayValueOut(var aArr: array of LongInt);
  var
    value: TValue;
  begin
    TValue.MakeOpenArray(@aArr[0], Length(aArr), PTypeInfo(TypeInfo(aArr)), value);
    CheckEquals(value.IsArray, True);
    CheckEquals(value.IsOpenArray, True);
    CheckEquals(value.IsObject, False);
    CheckEquals(value.IsOrdinal, False);
    CheckEquals(value.IsClass, False);
    CheckEquals(value.GetArrayLength, 2);
    CheckEquals(value.GetArrayElement(0).AsInteger, 42);
    CheckEquals(value.GetArrayElement(1).AsInteger, 21);
    value.SetArrayElement(0, 84);
    value.SetArrayElement(1, 128);
    { since this is an open array the original array is modified! }
    CheckEquals(aArr[0], 84);
    CheckEquals(aArr[1], 128);
    CheckEquals(value.GetArrayElement(0).AsInteger, 84);
    CheckEquals(value.GetArrayElement(1).AsInteger, 128);
  end;

var
  arr: array of LongInt;
begin
  TestOpenArrayValueCopy([42, 21]);

  arr := [42, 21];
  TestOpenArrayValueVar(arr);
  CheckEquals(arr[0], 84);
  CheckEquals(arr[1], 21);

  arr := [42, 21];
  TestOpenArrayValueOut(arr);
  CheckEquals(arr[0], 84);
  CheckEquals(arr[1], 128);
end;

{$endif}

procedure TTestValueSimple.TestMakeSingle;
var
  fs: Single;
  v: TValue;
  hadexcept: Boolean;
begin
  fs := 3.14;

  TValue.Make(@fs, TypeInfo(Single), v);
  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, False);
  CheckEquals(v.IsOrdinal, False);
  Check(v.AsExtended=fs);
  Check(v.GetReferenceToRawData <> @fs);

  try
    hadexcept := False;
    v.AsInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No signed type conversion exception');

  try
    hadexcept := False;
    v.AsUInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No unsigned type conversion exception');
end;

procedure TTestValueSimple.TestMakeDouble;
var
  fd: Double;
  v: TValue;
  hadexcept: Boolean;
begin
  fd := 3.14;

  TValue.Make(@fd, TypeInfo(Double), v);
  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, False);
  CheckEquals(v.IsOrdinal, False);
  Check(v.AsExtended=fd);
  Check(v.GetReferenceToRawData <> @fd);

  try
    hadexcept := False;
    v.AsInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No signed type conversion exception');

  try
    hadexcept := False;
    v.AsUInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No unsigned type conversion exception');
end;

procedure TTestValueSimple.TestMakeExtended;
var
  fe: Extended;
  v: TValue;
  hadexcept: Boolean;
begin
  fe := 3.14;

  TValue.Make(@fe, TypeInfo(Extended), v);
  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, False);
  CheckEquals(v.IsOrdinal, False);
  Check(v.AsExtended=fe);
  Check(v.GetReferenceToRawData <> @fe);

  try
    hadexcept := False;
    v.AsInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No signed type conversion exception');

  try
    hadexcept := False;
    v.AsUInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No unsigned type conversion exception');
end;

procedure TTestValueSimple.TestMakeCurrency;
var
  fcu: Currency;
  v: TValue;
  hadexcept: Boolean;
begin
  fcu := 3.14;

  TValue.Make(@fcu, TypeInfo(Currency), v);
  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, False);
  CheckEquals(v.IsOrdinal, False);
  Check(v.AsExtended=Extended(fcu));
  Check(v.AsCurrency=fcu);
  Check(v.GetReferenceToRawData <> @fcu);

  try
    hadexcept := False;
    v.AsInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No signed type conversion exception');

  try
    hadexcept := False;
    v.AsUInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No unsigned type conversion exception');
end;

procedure TTestValueSimple.TestMakeComp;
var
  fco: Comp;
  v: TValue;
  hadexcept: Boolean;
begin
  fco := 314;

  TValue.Make(@fco, TypeInfo(Comp), v);

  if v.Kind <> tkFloat then
    Exit;

  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, False);
  CheckEquals(v.IsOrdinal, False);
  Check(v.AsExtended=Extended(fco));
  Check(v.GetReferenceToRawData <> @fco);

  try
    hadexcept := False;
    CheckEquals(v.AsInt64, 314);
  except
    hadexcept := True;
  end;

  CheckFalse(hadexcept, 'Had signed type conversion exception');

  try
    hadexcept := False;
    CheckEquals(v.AsUInt64, 314);
  except
    hadexcept := True;
  end;

  CheckFalse(hadexcept, 'Had unsigned type conversion exception');
end;

procedure TTestValueSimple.TestMakeEnum;
var
  e: TTestEnum;
  v: TValue;
begin
  e := te1;

  TValue.Make(@e, TypeInfo(TTestEnum), v);
  Check(not v.IsClass);
  Check(not v.IsArray);
  Check(not v.IsEmpty);
{$ifdef fpc}
  Check(not v.IsOpenArray);
{$endif}
  Check(not v.IsObject);
  Check(v.IsOrdinal);

  Check(v.GetReferenceToRawData <> @e);
  Check(TTestEnum(v.AsOrdinal) = te1);
end;

procedure TTestValueSimple.TestMakeAnsiChar;
var
  c: AnsiChar;
  v: TValue;
begin
  c := #20;

  TValue.Make(@c, TypeInfo(AnsiChar), v);
  Check(not v.IsClass);
  Check(not v.IsArray);
  Check(not v.IsEmpty);
{$ifdef fpc}
  Check(not v.IsOpenArray);
{$endif}
  Check(not v.IsObject);
  Check(v.IsOrdinal);

  Check(v.GetReferenceToRawData <> @c);
  Check(AnsiChar(v.AsOrdinal) = #20);
  Check(v.AsAnsiChar = #20);
end;

procedure TTestValueSimple.TestMakeWideChar;
var
  c: WideChar;
  v: TValue;
begin
  c := #$1234;

  TValue.Make(@c, TypeInfo(WideChar), v);
  Check(not v.IsClass);
  Check(not v.IsArray);
  Check(not v.IsEmpty);
{$ifdef fpc}
  Check(not v.IsOpenArray);
{$endif}
  Check(not v.IsObject);
  Check(v.IsOrdinal);

  Check(v.GetReferenceToRawData <> @c);
  Check(WideChar(v.AsOrdinal) = #$1234);
  Check(v.AsWideChar = #$1234);
end;

procedure TTestValueSimple.TestMakeNativeInt;
var
  fni: NativeInt;
  s: AnsiString;
  v: TValue;
  o: TObject;
begin
  fni := 2021;

  TValue.Make(fni, TypeInfo(LongInt), v);
  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, False);
  CheckEquals(v.IsOrdinal, True);
  Check(NativeInt(v.GetReferenceToRawData) <> fni);
  CheckEquals(v.AsOrdinal, 2021);

  s := 'Hello World';
  TValue.Make(NativeInt(s), TypeInfo(AnsiString), v);
  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, False);
  CheckEquals(v.IsOrdinal, False);
  CheckEquals(v.AsString, s);

  o := TObject.Create;
  TValue.Make(NativeInt(o), TypeInfo(TObject), v);
  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, True);
  CheckEquals(v.IsOrdinal, False);
  Check(PPointer(v.GetReferenceToRawData)^ = Pointer(o));
  Check(v.AsObject = o);
  o.Free;
end;

procedure TTestValueArray.TestMakeFromArray;
var
  arr, subarr: array of TValue;
  v, varr: TValue;
  ti: PTypeInfo;
  i: LongInt;
begin
  SetLength(arr, 3 * 4);
  for i := 0 to High(arr) do
    TValue.{$ifdef fpc}specialize{$endif} Make<LongInt>(i + 1, arr[i]);

  ti := PTypeInfo(TypeInfo(LongInt));

  v := TValue.FromArray(TypeInfo(TArrayOfLongintDyn), arr);
  Check(not v.IsEmpty, 'Array is empty');
  Check(v.IsArray, 'Value is not an array');
  CheckEquals(Length(arr), v.GetArrayLength, 'Array length does not match');
  for i := 0 to High(arr) do begin
    varr := v.GetArrayElement(i);
    Check(varr.TypeInfo = ti, 'Type info of array element does not match');
    Check(varr.IsOrdinal, 'Array element is not an ordinal');
    Check(varr.AsInteger = arr[i].AsInteger, 'Value of array element does not match');
  end;

  subarr := Copy(arr, 0, 4);
  v := TValue.FromArray(TypeInfo(TArrayOfLongintStatic), subarr);
  Check(not v.IsEmpty, 'Array is empty');
  Check(v.IsArray, 'Value is not an array');
  CheckEquals(Length(subarr), v.GetArrayLength, 'Array length does not match');
  for i := 0 to High(subarr) do begin
    varr := v.GetArrayElement(i);
    Check(varr.TypeInfo = ti, 'Type info of array element does not match');
    Check(varr.IsOrdinal, 'Array element is not an ordinal');
    Check(varr.AsInteger = subarr[i].AsInteger, 'Value of array element does not match');
  end;

  v := TValue.FromArray(TypeInfo(TArrayOfLongint2DStatic), arr);
  Check(not v.IsEmpty, 'Array is empty');
  Check(v.IsArray, 'Value is not an array');
  CheckEquals(Length(arr), v.GetArrayLength, 'Array length does not match');
  for i := 0 to High(arr) do begin
    varr := v.GetArrayElement(i);
    Check(varr.TypeInfo = ti, 'Type info of array element does not match');
    Check(varr.IsOrdinal, 'Array element is not an ordinal');
    Check(varr.AsInteger = arr[i].AsInteger, 'Value of array element does not match');
  end;
end;

procedure TTestValueSimple.TestMakeGenericNil;
var
  value: TValue;
begin
  TValue.{$ifdef fpc}specialize{$endif} Make<TObject>(Nil, value);
  CheckTrue(value.IsEmpty);
  CheckTrue(value.IsObject);
  CheckTrue(value.IsClass);
  CheckTrue(value.IsOrdinal);
  CheckFalse(value.IsArray);
  CheckTrue(value.AsObject=Nil);
  CheckTrue(value.AsClass=Nil);
  CheckTrue(value.AsInterface=Nil);
  CheckEquals(0, value.AsOrdinal);

  TValue.{$ifdef fpc}specialize{$endif} Make<TClass>(Nil, value);
  CheckTrue(value.IsEmpty);
  CheckTrue(value.IsClass);
  CheckTrue(value.IsOrdinal);
  CheckFalse(value.IsArray);
  CheckTrue(value.AsObject=Nil);
  CheckTrue(value.AsClass=Nil);
  CheckTrue(value.AsInterface=Nil);
  CheckEquals(0, value.AsOrdinal);
end;

procedure TTestValueSimple.TestMakeGenericLongInt;
var
  value: TValue;
begin
  TValue.{$ifdef fpc}specialize{$endif} Make<LongInt>(0, value);
  CheckTrue(value.IsOrdinal);
  CheckFalse(value.IsEmpty);
  CheckFalse(value.IsClass);
  CheckFalse(value.IsObject);
  CheckFalse(value.IsArray);
  CheckEquals(0, value.AsOrdinal);
  CheckEquals(0, value.AsInteger);
  CheckEquals(0, value.AsInt64);
  CheckEquals(0, value.AsUInt64);
end;

procedure TTestValueSimple.TestMakeGenericString;
var
  value: TValue;
begin
  TValue.{$ifdef fpc}specialize{$endif} Make<String>('test', value);
  CheckFalse(value.IsEmpty);
  CheckFalse(value.IsObject);
  CheckFalse(value.IsClass);
  CheckFalse(value.IsArray);
  CheckEquals('test', value.AsString);
end;

procedure TTestValueSimple.TestMakeGenericObject;
var
  value: TValue;
  TestClass: TTestValueClass;
begin
  TestClass := TTestValueClass.Create;
  TestClass.AInteger := 54329;
  TValue.{$ifdef fpc}specialize{$endif} Make<TTestValueClass>(TestClass, value);
  CheckEquals(value.IsClass, False);
  CheckEquals(value.IsObject, True);
  Check(value.AsObject=TestClass);
  Check(PPointer(value.GetReferenceToRawData)^ = Pointer(TestClass));
  CheckEquals(TTestValueClass(value.AsObject).AInteger, 54329);
  TestClass.Free;
end;

procedure TTestValueSimple.TestMakeGenericDouble;
var
  fd: Double;
  v: TValue;
  hadexcept: Boolean;
begin
  fd := 3.14;

  TValue.{$ifdef fpc}specialize{$endif} Make<Double>(fd, v);
  CheckEquals(v.IsClass, False);
  CheckEquals(v.IsObject, False);
  CheckEquals(v.IsOrdinal, False);
  Check(v.AsExtended=fd);
  Check(v.GetReferenceToRawData <> @fd);

  try
    hadexcept := False;
    v.AsInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No signed type conversion exception');

  try
    hadexcept := False;
    v.AsUInt64;
  except
    hadexcept := True;
  end;

  CheckTrue(hadexcept, 'No unsigned type conversion exception');
end;


procedure TTestValueSimple.TestMakeGenericAnsiChar;
var
  c: AnsiChar;
  v: TValue;
begin
  c := #20;

  TValue.{$ifdef fpc}specialize{$endif} Make<AnsiChar>(c, v);
  Check(not v.IsClass);
  Check(not v.IsArray);
  Check(not v.IsEmpty);
{$ifdef fpc}
  Check(not v.IsOpenArray);
{$endif}
  Check(not v.IsObject);
  Check(v.IsOrdinal);

  Check(v.GetReferenceToRawData <> @c);
  Check(AnsiChar(v.AsOrdinal) = #20);
  Check(v.AsAnsiChar = #20);
end;

procedure TTestValueSimple.TestMakeGenericWideChar;
var
  c: WideChar;
  v: TValue;
begin
  c := #$1234;

  TValue.{$ifdef fpc}specialize{$endif} Make<WideChar>(c, v);
  Check(not v.IsClass);
  Check(not v.IsArray);
  Check(not v.IsEmpty);
{$ifdef fpc}
  Check(not v.IsOpenArray);
{$endif}
  Check(not v.IsObject);
  Check(v.IsOrdinal);

  Check(v.GetReferenceToRawData <> @c);
  Check(WideChar(v.AsOrdinal) = #$1234);
  Check(v.AsWideChar = #$1234);
end;

procedure TTestValueSimple.MakeFromOrdinalTObject;
begin
  TValue.FromOrdinal(TypeInfo(TObject), 42);
end;

procedure TTestValueSimple.MakeFromOrdinalSet;
begin
  TValue.FromOrdinal(TypeInfo(TTestSet), 42);
end;

procedure TTestValueSimple.MakeFromOrdinalString;
begin
  TValue.FromOrdinal(TypeInfo(AnsiString), 42);
end;

procedure TTestValueSimple.MakeFromOrdinalNil;
begin
  TValue.FromOrdinal(Nil, 42);
end;

procedure TTestValueSimple.TestFromOrdinal;
var
  v: TValue;
begin
  v := TValue.FromOrdinal(TypeInfo(LongInt), 42);
  Check(v.IsOrdinal);
  CheckEquals(v.AsOrdinal, 42);

  v := TValue.FromOrdinal(TypeInfo(Boolean), Ord(True));
  Check(v.IsOrdinal);
  CheckEquals(v.AsOrdinal, Ord(True));

  v := TValue.FromOrdinal(TypeInfo(Int64), $1234123412341234);
  Check(v.IsOrdinal);
  CheckEquals(v.AsOrdinal, $1234123412341234);

  v := TValue.FromOrdinal(TypeInfo(QWord), $1234123412341234);
  Check(v.IsOrdinal);
  CheckEquals(v.AsOrdinal, $1234123412341234);

  v := TValue.FromOrdinal(TypeInfo(LongBool), Ord(True));
  Check(v.IsOrdinal);
  CheckEquals(v.AsOrdinal, Ord(True));

  v := TValue.FromOrdinal(TypeInfo(TTestEnum), Ord(te1));
  Check(v.IsOrdinal);
  CheckEquals(v.AsOrdinal, Ord(te1));

  v := TValue.FromOrdinal(TypeInfo(AnsiChar), Ord(#20));
  Check(v.IsOrdinal);
  CheckEquals(v.AsOrdinal, Ord(#20));

  v := TValue.FromOrdinal(TypeInfo(WideChar), Ord(#$1234));
  Check(v.IsOrdinal);
  CheckEquals(v.AsOrdinal, Ord(#$1234));

  CheckException({$ifdef fpc}@{$endif}MakeFromOrdinalNil, EInvalidCast);
  CheckException({$ifdef fpc}@{$endif}MakeFromOrdinalTObject, EInvalidCast);
  CheckException({$ifdef fpc}@{$endif}MakeFromOrdinalSet, EInvalidCast);
  CheckException({$ifdef fpc}@{$endif}MakeFromOrdinalString, EInvalidCast);
end;

{ TTestValueArray }


{$ifdef fpc}
procedure TTestValueArray.TestOpenArrayToDyn;

  procedure OpenArrayProc(aArr: array of LongInt);
  var
    value: TValue;
  begin
{$ifndef InLazIDE}
    value := specialize OpenArrayToDynArrayValue<LongInt>(aArr);
{$endif}
    CheckEquals(value.IsArray, True);
    CheckEquals(value.IsOpenArray, False);
    CheckEquals(value.IsObject, False);
    CheckEquals(value.IsOrdinal, False);
    CheckEquals(value.IsClass, False);
    CheckEquals(value.GetArrayLength, 2);
    CheckEquals(value.GetArrayElement(0).AsInteger, 42);
    CheckEquals(value.GetArrayElement(1).AsInteger, 84);
    value.SetArrayElement(0, Integer(21));
    { since this is a copy the original array is not modified! }
    CheckEquals(aArr[0], 42);
  end;

begin
  OpenArrayProc([42, 84]);
end;
{$endif}

procedure TTestValueGeneral.TestDataSize;
var
  u8: UInt8;
  u16: UInt16;
  u32: UInt32;
  u64: UInt64;
  s8: Int8;
  s16: Int16;
  s32: Int32;
  s64: Int64;
  f32: Single;
  f64: Double;
{$ifdef FPC_HAS_TYPE_EXTENDED}
  f80: Extended;
{$endif}
  fco: Comp;
  fcu: Currency;
  ss: ShortString;
  sa: AnsiString;
  su: UnicodeString;
  sw: WideString;
  o: TObject;
  c: TClass;
  i: IInterface;
  ad: TArrayOfLongintDyn;
  _as: TArrayOfLongintStatic;
  b8: Boolean;
{$ifdef fpc}
  b16: Boolean16;
  b32: Boolean32;
  b64: Boolean64;
{$endif}
  bl8: ByteBool;
  bl16: WordBool;
  bl32: LongBool;
{$ifdef fpc}
  bl64: QWordBool;
{$endif}
  e: TTestEnum;
  s: TTestSet;
  t: TTestRecord;
  p: Pointer;
  proc: TTestProc;
  method: TTestMethod;

  value: TValue;
begin
  u8:=245;
  TValue.Make(@u8, TypeInfo(UInt8), value);
  CheckEquals(1, value.DataSize, 'Size of UInt8 differs');
  u16:=789;
  TValue.Make(@u16, TypeInfo(UInt16), value);
  CheckEquals(2, value.DataSize, 'Size of UInt16 differs');
  u32:=568789;
  TValue.Make(@u32, TypeInfo(UInt32), value);
  CheckEquals(4, value.DataSize, 'Size of UInt32 differs');
  u64:=$abdcefadbcef;
  TValue.Make(@u64, TypeInfo(UInt64), value);
  CheckEquals(8, value.DataSize, 'Size of UInt64 differs');
  s8:=-32;
  TValue.Make(@s8, TypeInfo(Int8), value);
  CheckEquals(1, value.DataSize, 'Size of Int8 differs');
  s16:=-5345;
  TValue.Make(@s16, TypeInfo(Int16), value);
  CheckEquals(2, value.DataSize, 'Size of Int16 differs');
  s32:=-234567;
  TValue.Make(@s32, TypeInfo(Int32), value);
  CheckEquals(4, value.DataSize, 'Size of Int32 differs');
  s64:=23456789012;
  TValue.Make(@s64, TypeInfo(Int64), value);
  CheckEquals(8, value.DataSize, 'Size of Int64 differs');
  b8:=false;
  TValue.Make(@b8, TypeInfo(Boolean), value);
  CheckEquals(1, value.DataSize, 'Size of Boolean differs');
{$ifdef fpc}
  b16:=true;
  TValue.Make(@b16, TypeInfo(Boolean16), value);
  CheckEquals(2, value.DataSize, 'Size of Boolean16 differs');
  b32:=false;
  TValue.Make(@b32, TypeInfo(Boolean32), value);
  CheckEquals(4, value.DataSize, 'Size of Boolean32 differs');
  b64:=true;
  TValue.Make(@b64, TypeInfo(Boolean64), value);
  CheckEquals(8, value.DataSize, 'Size of Boolean64 differs');
{$endif}
  bl8:=true;
  TValue.Make(@bl8, TypeInfo(ByteBool), value);
  CheckEquals(1, value.DataSize, 'Size of ByteBool differs');
  bl16:=false;
  TValue.Make(@bl16, TypeInfo(WordBool), value);
  CheckEquals(2, value.DataSize, 'Size of WordBool differs');
  bl32:=false;
  TValue.Make(@bl32, TypeInfo(LongBool), value);
  CheckEquals(4, value.DataSize, 'Size of LongBool differs');
{$ifdef fpc}
  bl64:=true;
  TValue.Make(@bl64, TypeInfo(QWordBool), value);
  CheckEquals(8, value.DataSize, 'Size of QWordBool differs');
{$endif}
  f32:=4.567;
  TValue.Make(@f32, TypeInfo(Single), value);
  CheckEquals(4, value.DataSize, 'Size of Single differs');
  f64:=-3456.678;
  TValue.Make(@f64, TypeInfo(Double), value);
  CheckEquals(8, value.DataSize, 'Size of Double differs');
{$ifdef FPC_HAS_TYPE_EXTENDED}
  f80:=-2345.678;
  TValue.Make(@f80, TypeInfo(Extended), value);
  CheckEquals(10, value.DataSize, 'Size of Extended differs');
{$endif}
  fcu:=56.78;
  TValue.Make(@fcu, TypeInfo(Currency), value);
  CheckEquals(SizeOf(Currency), value.DataSize, 'Size of Currency differs');
  fco:=456;
  TValue.Make(@fco, TypeInfo(Comp), value);
  CheckEquals(SizeOf(Comp), value.DataSize, 'Size of Comp differs');
  ss := '';
  TValue.Make(@ss, TypeInfo(ShortString), value);
  CheckEquals(254, value.DataSize, 'Size ofShortString differs');
  sa:= '';
  TValue.Make(@sa, TypeInfo(AnsiString), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of AnsiString differs');
  sw := '';
  TValue.Make(@sw, TypeInfo(WideString), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of WideString differs');
  su:='';
  TValue.Make(@su, TypeInfo(UnicodeString), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of UnicodeString differs');
  o := TTestValueClass.Create;
  TValue.Make(@o, TypeInfo(TObject), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of TObject differs');
  o.Free;
  c := TObject;
  TValue.Make(@c, TypeInfo(TClass), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of TClass differs');
  i := Nil;
  TValue.Make(@i, TypeInfo(IInterface), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of IInterface differs');
  TValue.Make(@t, TypeInfo(TTestRecord), value);
  CheckEquals(SizeOf(TTestRecord), value.DataSize, 'Size of TTestRecord differs');
  proc := Nil;
  TValue.Make(@proc, TypeInfo(TTestProc), value);
  CheckEquals(SizeOf(TTestProc), value.DataSize, 'Size of TTestProc differs');
  method := Nil;
  TValue.Make(@method, TypeInfo(TTestMethod), value);
  CheckEquals(SizeOf(TTestMethod), value.DataSize, 'Size of TTestMethod differs');
  TValue.Make(@_as, TypeInfo(TArrayOfLongintStatic), value);
  CheckEquals(SizeOf(TArrayOfLongintStatic), value.DataSize, 'Size of TArrayOfLongintStatic differs');
  TValue.Make(@ad, TypeInfo(TArrayOfLongintDyn), value);
  CheckEquals(SizeOf(TArrayOfLongintDyn), value.DataSize, 'Size of TArrayOfLongintDyn differs');
  e:=low(TTestEnum);
  TValue.Make(@e, TypeInfo(TTestEnum), value);
  CheckEquals(SizeOf(TTestEnum), value.DataSize, 'Size of TTestEnum differs');
  s:=[low(TTestEnum),high(TTestEnum)];
  TValue.Make(@s, TypeInfo(TTestSet), value);
  CheckEquals(SizeOf(TTestSet), value.DataSize, 'Size of TTestSet differs');
  p := Nil;
  TValue.Make(@p, TypeInfo(Pointer), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of Pointer differs');
end;

procedure TTestValueGeneral.TestDataSizeEmpty;
var
  value: TValue;
begin
  TValue.Make(Nil, TypeInfo(UInt8), value);
  CheckEquals(1, value.DataSize, 'Size of UInt8 differs');
  TValue.Make(Nil, TypeInfo(UInt16), value);
  CheckEquals(2, value.DataSize, 'Size of UInt16 differs');
  TValue.Make(Nil, TypeInfo(UInt32), value);
  CheckEquals(4, value.DataSize, 'Size of UInt32 differs');
  TValue.Make(Nil, TypeInfo(UInt64), value);
  CheckEquals(8, value.DataSize, 'Size of UInt64 differs');
  TValue.Make(Nil, TypeInfo(Int8), value);
  CheckEquals(1, value.DataSize, 'Size of Int8 differs');
  TValue.Make(Nil, TypeInfo(Int16), value);
  CheckEquals(2, value.DataSize, 'Size of Int16 differs');
  TValue.Make(Nil, TypeInfo(Int32), value);
  CheckEquals(4, value.DataSize, 'Size of Int32 differs');
  TValue.Make(Nil, TypeInfo(Int64), value);
  CheckEquals(8, value.DataSize, 'Size of Int64 differs');
  TValue.Make(Nil, TypeInfo(Boolean), value);
  CheckEquals(1, value.DataSize, 'Size of Boolean differs');
{$ifdef fpc}
  TValue.Make(Nil, TypeInfo(Boolean16), value);
  CheckEquals(2, value.DataSize, 'Size of Boolean16 differs');
  TValue.Make(Nil, TypeInfo(Boolean32), value);
  CheckEquals(4, value.DataSize, 'Size of Boolean32 differs');
  TValue.Make(Nil, TypeInfo(Boolean64), value);
  CheckEquals(8, value.DataSize, 'Size of Boolean64 differs');
{$endif}
  TValue.Make(Nil, TypeInfo(ByteBool), value);
  CheckEquals(1, value.DataSize, 'Size of ByteBool differs');
  TValue.Make(Nil, TypeInfo(WordBool), value);
  CheckEquals(2, value.DataSize, 'Size of WordBool differs');
  TValue.Make(Nil, TypeInfo(LongBool), value);
  CheckEquals(4, value.DataSize, 'Size of LongBool differs');
{$ifdef fpc}
  TValue.Make(Nil, TypeInfo(QWordBool), value);
  CheckEquals(8, value.DataSize, 'Size of QWordBool differs');
{$endif}
  TValue.Make(Nil, TypeInfo(Single), value);
  CheckEquals(4, value.DataSize, 'Size of Single differs');
  TValue.Make(Nil, TypeInfo(Double), value);
  CheckEquals(8, value.DataSize, 'Size of Double differs');
{$ifdef FPC_HAS_TYPE_EXTENDED}
  TValue.Make(Nil, TypeInfo(Extended), value);
  CheckEquals(10, value.DataSize, 'Size of Extended differs');
{$endif}
  TValue.Make(Nil, TypeInfo(Currency), value);
  CheckEquals(SizeOf(Currency), value.DataSize, 'Size of Currency differs');
  TValue.Make(Nil, TypeInfo(Comp), value);
  CheckEquals(SizeOf(Comp), value.DataSize, 'Size of Comp differs');
  TValue.Make(Nil, TypeInfo(ShortString), value);
  CheckEquals(254, value.DataSize, 'Size of ShortString differs');
  TValue.Make(Nil, TypeInfo(AnsiString), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of Pointer differs');
  TValue.Make(Nil, TypeInfo(WideString), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of WideString differs');
  TValue.Make(Nil, TypeInfo(UnicodeString), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of UnicodeString differs');
  TValue.Make(Nil, TypeInfo(TObject), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of TObject differs');
  TValue.Make(Nil, TypeInfo(TClass), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of TClass differs');
  TValue.Make(Nil, TypeInfo(IInterface), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of IInterface differs');
  TValue.Make(Nil, TypeInfo(TTestRecord), value);
  CheckEquals(SizeOf(TTestRecord), value.DataSize, 'Size of TTestRecord differs');
  TValue.Make(Nil, TypeInfo(TTestProc), value);
  CheckEquals(SizeOf(TTestProc), value.DataSize, 'Size of TTestProc differs');
  TValue.Make(Nil, TypeInfo(TTestMethod), value);
  CheckEquals(SizeOf(TTestMethod), value.DataSize, 'Size of TTestMethod differs');
  TValue.Make(Nil, TypeInfo(TArrayOfLongintStatic), value);
  CheckEquals(SizeOf(TArrayOfLongintStatic), value.DataSize, 'Size of TArrayOfLongintStatic differs');
  TValue.Make(Nil, TypeInfo(TArrayOfLongintDyn), value);
  CheckEquals(SizeOf(TArrayOfLongintDyn), value.DataSize, 'Size of TArrayOfLongintDyn differs');
  TValue.Make(Nil, TypeInfo(TTestEnum), value);
  CheckEquals(SizeOf(TTestEnum), value.DataSize, 'Size of TTestEnum differs');
  TValue.Make(Nil, TypeInfo(TTestSet), value);
  CheckEquals(SizeOf(TTestSet), value.DataSize, 'Size of TTestSet differs');
  TValue.Make(Nil, TypeInfo(Pointer), value);
  CheckEquals(SizeOf(Pointer), value.DataSize, 'Size of Pointer differs');
end;

procedure TTestValueGeneral.TestIsManaged;
begin
  CheckEquals(true, IsManaged(TypeInfo(ansistring)), 'IsManaged for tkAString');
  CheckEquals(true, IsManaged(TypeInfo(widestring)), 'IsManaged for tkWString');
  CheckEquals(true, IsManaged(TypeInfo(Variant)), 'IsManaged for tkVariant');
  CheckEquals(true, IsManaged(TypeInfo(TArrayOfManagedRec)),
    'IsManaged for tkArray (with managed ElType)');
  CheckEquals(true, IsManaged(TypeInfo(TArrayOfString)),
    'IsManaged for tkArray (with managed ElType)');
  CheckEquals(true, IsManaged(TypeInfo(TManagedRec)), 'IsManaged for tkRecord');
  {$ifdef fpc}
  CheckEquals(true, IsManaged(TypeInfo(TManagedRecOp)), 'IsManaged for tkRecord');
  {$endif}
  CheckEquals(true, IsManaged(TypeInfo(IInterface)), 'IsManaged for tkInterface');
  CheckEquals(true, IsManaged(TypeInfo(TManagedObj)), 'IsManaged for tkObject');
  {$ifdef fpc}
  CheckEquals(true, IsManaged(TypeInfo(specialize TArray<byte>)), 'IsManaged for tkDynArray');
  {$else}
  CheckEquals(true, IsManaged(TypeInfo(TArray<byte>)), 'IsManaged for tkDynArray');
  {$endif}
  CheckEquals(true, IsManaged(TypeInfo(unicodestring)), 'IsManaged for tkUString');
  CheckEquals(false, IsManaged(TypeInfo(shortstring)), 'IsManaged for tkSString');
  CheckEquals(false, IsManaged(TypeInfo(Byte)), 'IsManaged for tkInteger');
  CheckEquals(false, IsManaged(TypeInfo(Char)), 'IsManaged for tkChar');
  CheckEquals(false, IsManaged(TypeInfo(TTestEnum)), 'IsManaged for tkEnumeration');
  CheckEquals(false, IsManaged(TypeInfo(Single)), 'IsManaged for tkFloat');
  CheckEquals(false, IsManaged(TypeInfo(TTestSet)), 'IsManaged for tkSet');
  {$ifdef fpc}
  CheckEquals(false, IsManaged(TypeInfo(TTestMethod)), 'IsManaged for tkMethod');
  {$else}
  { Delphi bug (or sabotage). For some reason Delphi considers method pointers to be managed (only in newer versions, probably since XE7) :/ }
  CheckEquals({$if RTLVersion>=28}true{$else}false{$endif}, IsManaged(TypeInfo(TTestMethod)), 'IsManaged for tkMethod');
  {$endif}
  CheckEquals(false, IsManaged(TypeInfo(TArrayOfByte)),
    'IsManaged for tkArray (with non managed ElType)');
  CheckEquals(false, IsManaged(TypeInfo(TArrayOfNonManagedRec)),
    'IsManaged for tkArray (with non managed ElType)');
  CheckEquals(false, IsManaged(TypeInfo(TNonManagedRec)), 'IsManaged for tkRecord');
  CheckEquals(false, IsManaged(TypeInfo(TObject)), 'IsManaged for tkClass');
  CheckEquals(false, IsManaged(TypeInfo(TNonManagedObj)), 'IsManaged for tkObject');
  CheckEquals(false, IsManaged(TypeInfo(WideChar)), 'IsManaged for tkWChar');
  CheckEquals(false, IsManaged(TypeInfo(Boolean)), 'IsManaged for tkBool');
  CheckEquals(false, IsManaged(TypeInfo(Int64)), 'IsManaged for tkInt64');
  CheckEquals(false, IsManaged(TypeInfo(UInt64)), 'IsManaged for tkQWord');
  {$ifdef fpc}
  CheckEquals(false, IsManaged(TypeInfo(ICORBATest)), 'IsManaged for tkInterfaceRaw');
  {$endif}
  CheckEquals(false, IsManaged(TypeInfo(TTestProc)), 'IsManaged for tkProcVar');
  CheckEquals(false, IsManaged(TypeInfo(TTestHelper)), 'IsManaged for tkHelper');
  {$ifdef fpc}
  CheckEquals(false, IsManaged(TypeInfo(file)), 'IsManaged for tkFile');
  {$endif}
  CheckEquals(false, IsManaged(TypeInfo(TClass)), 'IsManaged for tkClassRef');
  CheckEquals(false, IsManaged(TypeInfo(Pointer)), 'IsManaged for tkPointer');
  CheckEquals(false, IsManaged(nil), 'IsManaged for nil');
end;

procedure TTestValueGeneral.TestReferenceRawData;
var
  value: TValue;
  str: String;
  intf: IInterface;
  i: LongInt;
  test: TTestRecord;
  arrdyn: TArrayOfLongintDyn;
  arrstat: TArrayOfLongintStatic;
begin
  str := 'Hello World';
  UniqueString(str);
  TValue.Make(@str, TypeInfo(String), value);
  Check(PPointer(value.GetReferenceToRawData)^ = Pointer(str), 'Reference to string data differs');

  intf := TInterfacedObject.Create;
  TValue.Make(@intf, TypeInfo(IInterface), value);
  Check(PPointer(value.GetReferenceToRawData)^ = Pointer(intf), 'Reference to interface data differs');

  i := 42;
  TValue.Make(@i, TypeInfo(LongInt), value);
  Check(value.GetReferenceToRawData <> @i, 'Reference to longint is equal');
  Check(PLongInt(value.GetReferenceToRawData)^ = PLongInt(@i)^, 'Reference to longint data differs');

  test.value1 := 42;
  test.value2 := 'Hello World';
  TValue.Make(@test, TypeInfo(TTestRecord), value);
  Check(value.GetReferenceToRawData <> @test, 'Reference to record is equal');
  Check(PTestRecord(value.GetReferenceToRawData)^.value1 = PTestRecord(@test)^.value1, 'Reference to record data value1 differs');
  Check(PTestRecord(value.GetReferenceToRawData)^.value2 = PTestRecord(@test)^.value2, 'Reference to record data value2 differs');

  SetLength(arrdyn, 3);
  arrdyn[0] := 42;
  arrdyn[1] := 23;
  arrdyn[2] := 49;
  TValue.Make(@arrdyn, TypeInfo(TArrayOfLongintDyn), value);
  Check(PPointer(value.GetReferenceToRawData)^ = Pointer(arrdyn), 'Reference to dynamic array data differs');

  arrstat[0] := 42;
  arrstat[1] := 23;
  arrstat[2] := 49;
  arrstat[3] := 59;
  TValue.Make(@arrstat, TypeInfo(TArrayOfLongintStatic), value);
  Check(value.GetReferenceToRawData <> @arrstat, 'Reference to static array is equal');
  Check(PLongInt(value.GetReferenceToRawData)^ = PLongInt(@arrstat)^, 'Reference to static array data differs');
end;

procedure TTestValueGeneral.TestReferenceRawDataEmpty;
var
  value: TValue;
begin
  TValue.Make(Nil, TypeInfo(String), value);
  Check(Assigned(value.GetReferenceToRawData()), 'Reference to empty String is not assigned');
  Check(not Assigned(PPointer(value.GetReferenceToRawData)^), 'Empty String data is assigned');

  TValue.Make(Nil, TypeInfo(IInterface), value);
  Check(Assigned(value.GetReferenceToRawData()), 'Reference to empty interface is not assigned');
  Check(not Assigned(PPointer(value.GetReferenceToRawData)^), 'Empty interface data is assigned');

  TValue.Make(Nil, TypeInfo(LongInt), value);
  Check(Assigned(value.GetReferenceToRawData()), 'Reference to empty LongInt is not assigned');
  Check(PLongInt(value.GetReferenceToRawData)^ = 0, 'Empty longint data is not 0');

  TValue.Make(Nil, TypeInfo(TTestRecord), value);
  Check(Assigned(value.GetReferenceToRawData()), 'Reference to empty record is not assigned');
  Check(PTestRecord(value.GetReferenceToRawData)^.value1 = 0, 'Empty record data value1 is not 0');
  Check(PTestRecord(value.GetReferenceToRawData)^.value2 = '', 'Empty record data value2 is not empty');

  TValue.Make(Nil, TypeInfo(TArrayOfLongintDyn), value);
  Check(Assigned(value.GetReferenceToRawData()), 'Reference to empty dynamic array is not assigned');
  Check(not Assigned(PPointer(value.GetReferenceToRawData)^), 'Empty dynamic array data is assigned');

  TValue.Make(Nil, TypeInfo(TArrayOfLongintStatic), value);
  Check(Assigned(value.GetReferenceToRawData()), 'Reference to empty static array is not assigned');
  Check(PLongInt(value.GetReferenceToRawData)^ = 0, 'Empty static array data is not 0');
end;

initialization
  RegisterTest(TTestValueGeneral);
  RegisterTest(TTestValueSimple);
  RegisterTest(TTestValueSimple);
  RegisterTest(TTestValueVariant);
end.

