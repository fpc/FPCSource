{$mode objfpc}
{$h+}
unit tcstreaming;

interface

Uses
  SysUtils,Classes, fpcunit, testutils, testregistry;

Type

  { TTestStreaming }

  TTestStreaming = Class(TTestCase)
  Private
    FStream : TMemoryStream;
    Function ReadByte : byte;
    Function ReadWord : Word;
    Function ReadInteger : LongInt;
    Function ReadInt64 : Int64;
    function ReadBareStr: string;
    function ReadString(V : TValueType): string;
    function ReadWideString(V : TValueType): WideString;
    Procedure Fail(FMt : String; Args : Array of const); overload;
  Public
    Procedure Setup; override;
    Procedure TearDown; override;
    Procedure SaveToStream(C : TComponent);
    Function ReadValue : TValueType;
    Procedure ExpectValue(AValue : TValueType);
    Procedure ExpectFlags(Flags : TFilerFlags; APosition : Integer);
    Procedure ExpectInteger(AValue : Integer);
    Procedure ExpectByte(AValue : Byte);
    Procedure ExpectInt64(AValue : Int64);
    Procedure ExpectBareString(AValue : String);
    Procedure ExpectString(AValue : String);
    Procedure ExpectSingle(AValue : Single);
    Procedure ExpectExtended(AValue : Extended);
    Procedure ExpectCurrency(AValue : Currency);
    Procedure ExpectIdent(AValue : String);
    Procedure ExpectDate(AValue : TDateTime);
    Procedure ExpectWideString(AValue : WideString);
    Procedure ExpectEndofList;
    Procedure ExpectSignature;
    Procedure ExpectEndOfStream;
  end;

implementation

uses typinfo;

Function ValName(V : TValueType) : String;

begin
  Result:=GetEnumName(TypeInfo(TValueType),Ord(v));
end;

{ TTestStreaming }


procedure TTestStreaming.ExpectByte(AValue: Byte);

Var
  B : Byte;

begin
  B:=ReadByte;
  If (B<>AValue) then
    Fail('Expected byte %d, got %d',[AValue,B]);
end;

procedure TTestStreaming.ExpectCurrency(AValue: Currency);

Var
  C : Currency;

begin
  ExpectValue(vaCurrency);
  FStream.Read(C,Sizeof(C));
  If (C<>AValue) then
    Fail('Expected currency %f, got %f',[AValue,C]);
end;

procedure TTestStreaming.ExpectDate(AValue: TDateTime);

Var
  C : TDateTime;

begin
  ExpectValue(vaDate);
  FStream.Read(C,Sizeof(C));
  If (C<>AValue) then
    Fail('Expected datetime %f, got %f',[AValue,C]);
end;

procedure TTestStreaming.ExpectEndofList;
begin
  ExpectValue(vaNull);
end;

procedure TTestStreaming.ExpectExtended(AValue: Extended);

Var
  E : Extended;

begin
  ExpectValue(vaExtended);
  FStream.Read(E,Sizeof(E));
  If Abs(E-AValue)>0.01 then
    Fail('Expected extended %f, got %f',[AValue,E]);
end;

procedure TTestStreaming.ExpectFlags(Flags: TFilerFlags;
  APosition: Integer);

var
  Prefix: Byte;
  F : TFilerFlags;
  B : Byte;
  I : Integer;

begin
  F := [];
  I:=0;
  B:=ReadByte;
  if B and $F0 = $F0 then
    begin
    Integer(F) := B and $0F;
    if ffChildPos in Flags then
      I:=ReadInteger;
    end
  else
    FStream.Position:=FStream.Position-1;
  If (FLags<>F) then
    Fail('Wrong Flags, expected %d, got %d',[Integer(Flags),B]);
  If I<>APosition then
    Fail('Wrong position, expected %d, got %d',[APosition,I]);
end;

procedure TTestStreaming.ExpectIdent(AValue: String);

var
  L : Byte;
  V : TValueType;
  S : String;
begin
  V:=ReadValue;
  case V of
    vaIdent:
      begin
      L:=ReadByte;
      SetLength(S,L);
      FStream.Read(S[1], L);
      end;
    vaFalse:
      S := 'False';
    vaTrue:
      S := 'True';
    vaNil:
      S := 'nil';
    vaNull:
      S := 'Null';
  else
    Fail('Expected identifier property type, got %s',[valName(V)]);
  end;
  If (S<>AValue) then
    Fail('Wrong identifier %s, expected %s',[S,AValue]);
end;

procedure TTestStreaming.ExpectInt64(AValue: Int64);

Var
  V : TValueType;
  I : Int64;

begin
  V:=ReadValue;
  Case V of
    vaInt8  : I:=ReadByte;
    vaInt16 : I:=ReadWord;
    vaInt32 : I:=ReadInteger;
    vaInt64 : I:=ReadInt64;
  else
    Fail('Expected integer property type, got %s',[valName(V)]);
  end;
  If (AValue<>I) then
    Fail('Expected integer %d, but got %d',[AValue,I]);
end;

procedure TTestStreaming.ExpectInteger(AValue: Integer);

Var
  V : TValueType;
  I : Integer;

begin
  V:=ReadValue;
  Case V of
    vaInt8  : I:=ReadByte;
    vaInt16 : I:=ReadWord;
    vaInt32 : I:=ReadInteger;
  else
    Fail('Expected integer  property type, got %s',[valName(V)]);
  end;
  If (AValue<>I) then
    Fail('Expected integer %d, but got %d',[AValue,I]);
end;



procedure TTestStreaming.ExpectSignature;

const
  Sig : array[1..4] of Char = 'TPF0';

var
  E,L : Longint;

begin
  L:=ReadInteger;
  E:=Longint(Sig);
  if L<>E then
    Fail('Invalid signature %d, expected %d',[L,E]);
end;

procedure TTestStreaming.ExpectSingle(AValue: Single);

Var
  S : Single;

begin
  ExpectValue(vaSingle);
  FStream.Read(S,SizeOf(Single));
  If Abs(AValue-S)>0.0001 then
    Fail('Expected single %f, but got %s',[AValue,S]);
end;

function TTestStreaming.ReadString(V : TValueType): string;

var
  L: Integer;
  B : Byte;

begin
  If V in [vaWString, vaUTF8String] then
    Result := ReadWideString(V)
  else
    begin
    L := 0;
    case V of
      vaString:
        begin
        FStream.Read(B, SizeOf(B));
        L:=B;
        end;
      vaLString:
        FStream.Read(L, SizeOf(Integer));
    else
      Fail('Wrong type %s, expected string type.',[ValName(V)]);
    end;
    SetLength(Result, L);
    If (L>0) then
      FStream.Read(PByte(Result)^, L);
    end;
end;

function TTestStreaming.ReadWideString(V : TValueType): WideString;

var
  L: Integer;
  Temp: String;

begin
  if V in [vaString, vaLString] then
    Result := ReadString(V)
  else
    begin
    L := 0;
    case V of
      vaWString:
        begin
        FStream.Read(L, SizeOf(Integer));
        SetLength(Result, L);
        FStream.Read(Pointer(Result)^, L * 2);
        end;
      vaUTF8String:
        begin
        FStream.Read(L, SizeOf(Integer));
        SetLength(Temp, L);
        FStream.Read(Pointer(Temp)^, L);
        Result:=Temp
        end;
    else
      Fail('Wrong type %s, expected widestring type.',[ValName(V)]);
    end;
  end;
end;

procedure TTestStreaming.ExpectString(AValue: String);

Var
  V : TValueType;
  S : String;
begin
  V:=ReadValue;
  If v in [vaString,vaLstring,vaWString,vaUTF8String] then
    S:=ReadString(V)
  else
    Fail('Expected string type, but got : %s',[ValName(V)]);
  If (S<>AValue) then
    Fail('Expected string "%s", but got "%s"',[AVAlue,S]);
end;

procedure TTestStreaming.ExpectValue(AValue: TValueType);

Var
  V : TValueType;

begin
  V:=ReadValue;
  If (V<>AValue) then
    Fail('Expecting value %s, but read %s',[ValName(AValue),ValName(V)]);
end;

procedure TTestStreaming.ExpectWideString(AValue: WideString);

Var
  W : WideString;
  V : TValueType;

begin
  V:=ReadValue;
  If v in [vaString,vaLstring,vaWString,vaUTF8String] then
    W:=ReadWideString(V)
  else
    Fail('Expected string type, but got : %s',[ValName(V)]);
  If (W<>AValue) then
    Fail('Expected string "%s", but got "%s"',[AVAlue,W]);
end;


procedure TTestStreaming.Fail(Fmt: String; Args: array of const);
begin
  Fail(Format(Fmt,Args));
end;

function TTestStreaming.ReadValue: TValueType;
{$IFDEF FPC}
var b : byte;
{$ENDIF}
begin
{$IFDEF FPC}
  FStream.Read(b,1);
  result := TValueType(b);
{$ELSE}
  FStream.Read(Result,SizeOf(Result));
{$ENDIF}
end;

procedure TTestStreaming.Setup;
begin
  FStream:=TMemoryStream.Create;
end;

procedure TTestStreaming.SaveToStream(C: TComponent);
var
  s: TStream;
begin
  C.Name:='Test'+C.ClassName;
  FStream.Clear;
  FStream.WriteComponent(C);
  FStream.Position:=0;
  // for debugging purposes, you can write a component to file too
  // set the class name of the component you want to write to disk in the next line
  if (C.ClassName='TStreamedOwnedComponentsX') then begin
    s := TFileStream.Create(C.ClassName+'.txt', fmCreate, fmShareDenyNone );
    s.WriteComponent(C);
    s.Free;
  end;
end;

procedure TTestStreaming.TearDown;
begin
  FreeAndNil(FStream);
end;

function TTestStreaming.ReadByte: byte;
begin
  FStream.Read(Result,SizeOf(Result));
end;

function TTestStreaming.ReadInt64: Int64;
begin
  FStream.Read(Result,SizeOf(Result));
end;

function TTestStreaming.ReadInteger: LongInt;
begin
  FStream.Read(Result,SizeOf(Result));
end;

function TTestStreaming.ReadWord: Word;
begin
  FStream.Read(Result,SizeOf(Result));
end;

function TTestStreaming.ReadBareStr: string;

var
  L: Byte;
begin
  L:=ReadByte;
  SetLength(Result,L);
  Fstream.Read(Result[1], L);
end;

procedure TTestStreaming.ExpectBareString(AValue: String);

Var
  S : String;

begin
  S:=ReadBareStr;
  If (S<>AValue) then
    Fail('Expected bare string %s, got :%s',[AValue,S]);
end;

procedure TTestStreaming.ExpectEndOfStream;
begin
  If (FStream.Position<>FStream.Size) then
    Fail('Expected at end of stream, current position=%d, size=%d',
          [FStream.Position,FStream.Size]);
end;

end.
