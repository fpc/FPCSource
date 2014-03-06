{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements streaming to binary file.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$INCLUDE sdo_global.inc}
unit sdo_serialization_binary;

interface

uses
  Classes, SysUtils, Contnrs,
  sdo_binary_streamer,
  sdo_types, sdo, sdo_consts, sdo_serialization_utils;

{$DEFINE sdo_binary_header}
  
const
  sBINARY_FORMAT_NAME = 'sdo-binary';
  sROOT   = 'ROOT';
  sSCOPE_INNER_NAME = 'INNER_VAL';
  sFORMAT = 'format';

type

  TDataName = String;
  TDataType = (
    dtInt8U  = 1,    dtInt8S = 2,
    dtInt16U = 3,   dtInt16S = 4,
    dtInt32U = 5,   dtInt32S = 6,
    dtInt64U = 7,   dtInt64S = 8,
    dtBool   = 9,
    dtAnsiChar   = 10, dtWideChar   = 11, dtEnum     = 12,
    dtSingle     = 13, dtDouble     = 14, dtExtended = 15, dtCurrency = 16,
    dtAnsiString = 17, dtWideString = 18,
{$IFDEF USE_UNICODE}
    dtUnicodeString = 19,
{$ENDIF USE_UNICODE}
    dtObject = 30, dtArray = 31,
    dtByteDynArray = 32
  );
const
  dtDefaultString =
    {$IFDEF USE_UNICODE}
      {$IFDEF DELPHI}
        dtUnicodeString
      {$ENDIF DELPHI}
      {$IFDEF FPC}
        dtAnsiString
      {$ENDIF FPC}
    {$ELSE USE_UNICODE}
      dtAnsiString
    {$ENDIF USE_UNICODE}
    ;

type

  PAnsiStringBuffer = ^TAnsiStringBuffer;
  PWideStringBuffer = ^TWideStringBuffer;
{$IFDEF USE_UNICODE}
  PUnicodeStringBuffer = ^TUnicodeStringBuffer;
{$ENDIF USE_UNICODE}
  PObjectBuffer = ^TObjectBuffer;
  PArrayBuffer = ^TArrayBuffer;
  PByteDynArrayBuffer = ^TByteDynArrayBuffer;
  PDataBuffer = ^TDataBuffer;

  TDataBuffer = Record
    Name : TDataName;
    Case DataType : TDataType of
      dtInt8S    : ( Int8S : TInt8S );
      dtInt8U    : ( Int8U : TInt8U );
      dtInt16U   : ( Int16U : TInt16U );
      dtInt16S   : ( Int16S : TInt16S );
      dtInt32U   : ( Int32U : TInt32U );
      dtInt32S   : ( Int32S : TInt32S );
      dtInt64U   : ( Int64U : TInt64U );
      dtInt64S   : ( Int64S : TInt64S );
      dtBool     : ( BoolData : TBoolData );
      dtAnsiChar : ( AnsiCharData : TAnsiCharacter; );
      dtWideChar : ( WideCharData : TWideCharacter; );
      dtEnum     : ( EnumData : TEnumData );
      dtSingle   : ( SingleData : TFloat_Single_4 );
      dtDouble   : ( DoubleData : TFloat_Double_8 );
      dtExtended   : ( ExtendedData : TFloat_Extended_10 );
      dtCurrency   : ( CurrencyData : TFloat_Currency_8 );
      
      dtAnsiString : ( AnsiStrData : PAnsiStringBuffer );
      dtWideString : ( WideStrData : PWideStringBuffer );
{$IFDEF USE_UNICODE}
      dtUnicodeString : ( UnicodeStrData : PUnicodeStringBuffer );
{$ENDIF USE_UNICODE}
      dtObject   : ( ObjectData : PObjectBuffer );
      dtArray    : ( ArrayData : PArrayBuffer );
      dtByteDynArray : ( ByteDynArrayData : PByteDynArrayBuffer );
  End;

  TAnsiStringBuffer = record
    Data : TAnsiStringData;
  end;

  TWideStringBuffer = record
    Data : TWideStringData;
  end;

{$IFDEF USE_UNICODE}
  TUnicodeStringBuffer = record
    Data : TUnicodeStringData;
  end;
{$ENDIF USE_UNICODE}



  PObjectBufferItem = ^TObjectBufferItem;
  TObjectBufferItem = Record
    Data : PDataBuffer;
    Next : PObjectBufferItem;
  End;

  TObjectBuffer = Record
    NilObject   : TBoolData;
    Count       : Integer;
    Head        : PObjectBufferItem;
    Last        : PObjectBufferItem;
    Attributes  : PObjectBuffer;
    InnerData   : PDataBuffer;
  End;

  PDataBufferList = ^TDataBufferList;
  TDataBufferList = array[0..MAX_ARRAY_LENGTH] of PDataBuffer;
  TArrayBuffer = Record
    Count : Integer;
    Items : PDataBufferList;
    Attributes  : PObjectBuffer;
  End;

  TByteDynArrayBuffer = record
    Data : TByteDynArray;
  end;

  { TStackItem }

  TStackItem = class
  private
    FScopeObject: PDataBuffer;
  protected
    procedure CopyTo(const AClone : TStackItem);virtual;
  Public
    constructor Create(const AScopeObject : PDataBuffer);virtual;
    function Clone() : TStackItem;virtual;
    function GetItemCount():Integer;virtual;abstract;
    function Find(var AName : TDataName):PDataBuffer;virtual;abstract;
    function GetByIndex(const AIndex : Integer):PDataBuffer;virtual;abstract;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;virtual;abstract;
    function CreateInnerBuffer(const ADataType : TDataType):PDataBuffer;virtual;abstract;
    function GetInnerBuffer():PDataBuffer;virtual;abstract;
    procedure NilCurrentScope();virtual;abstract;
    function IsCurrentScopeNil():Boolean;virtual;abstract;
    property ScopeObject : PDataBuffer Read FScopeObject;

    function GetScopeItemNames(const AReturnList : TStrings) : Integer;virtual;abstract;
  End;
  TStackItemClass = class of TStackItem;

  { TObjectStackItem }

  TObjectStackItem = class(TStackItem)
  Public
    function GetItemCount():Integer;override;
    function Find(var AName : TDataName):PDataBuffer;override;
    function GetByIndex(const AIndex : Integer):PDataBuffer;override;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;override;
    function CreateInnerBuffer(const ADataType : TDataType):PDataBuffer;override;
    function GetInnerBuffer():PDataBuffer;override;
    procedure NilCurrentScope();override;
    function IsCurrentScopeNil():Boolean;override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  End;
  TObjectStackItemClass = class of TObjectStackItem;


  { TObjectBaseArrayStackItem }

  TObjectBaseArrayStackItem = class(TObjectStackItem)
  private
    FIndex : Integer;
    FItemName : TDataName;
  protected
    procedure CopyTo(const AClone : TStackItem);override;
  public
    function GetItemCount():Integer;override;
    function Find(var AName : TDataName):PDataBuffer;override;
    procedure SetItemName(const AValue : TDataName);
  end;

  { TArrayStackItem }

  TArrayStackItem = class(TStackItem)
  Private
    FIndex : Integer;
  protected
    procedure CopyTo(const AClone : TStackItem);override;
  Public
    function GetItemCount():Integer;override;
    function Find(var AName : TDataName):PDataBuffer;override;
    function GetByIndex(const AIndex : Integer):PDataBuffer;override;
    function CreateBuffer(
      Const AName     : String;
      const ADataType : TDataType
    ):PDataBuffer;override;
    function CreateInnerBuffer(const ADataType : TDataType):PDataBuffer;override;
    function GetInnerBuffer():PDataBuffer;overload;override;
    procedure NilCurrentScope();override;
    function IsCurrentScopeNil():Boolean;override;
    function GetScopeItemNames(const AReturnList : TStrings) : Integer;override;
  End;
  TArrayStackItemClass = class of TArrayStackItem;

  TStreamBinaryBookmark = class(TStreamBookmark)
  private
    FNameStyle: TNameStyle;
    FStack: TObjectStackEx;
    FRootData : PDataBuffer;
    FSerializationStyle: TSerializationStyle;
  public
    destructor Destroy();override;
    property SerializationStyle : TSerializationStyle read FSerializationStyle;
    property NameStyle : TNameStyle read FNameStyle;
    property RootData : PDataBuffer read FRootData;
    property Stack : TObjectStackEx read FStack;
  end;

  { TSDOSerializationStreamBinary }

  TSDOSerializationStreamBinary = class(TInterfacedObject,IInterface,ISDOSerializerStream)
  private
    FRootData : PDataBuffer;
    FStack : TObjectStackEx;
    FSerializationStyle : TSerializationStyle;
    FNameStyle : TNameStyle;
  protected
    function GetCurrentScopeObject():PDataBuffer;
  protected
    function HasScope():Boolean;
    procedure CheckScope();
    procedure ClearStack();{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PushStack(AScopeObject : PDataBuffer;Const AScopeType : TScopeType = stObject);{$IFDEF USE_INLINE}inline;{$ENDIF}
    function StackTop():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function PopStack():TStackItem;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetRootData() : PDataBuffer;{$IFDEF USE_INLINE}inline;{$ENDIF}
  protected
    procedure PutBoolean(const AName : string; const AData : TSDOBoolean);{$IFDEF USE_INLINE}inline;{$ENDIF}
    procedure PutByte(const AName : string; const AData : TSDOByte);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_BYTES}
    procedure PutBytes(const AName : string; const AData : TSDOBytes);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    procedure PutChar(const AName : string; const AData : TSDOChar);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    procedure PutCurrency(const AName : string; const AData : TSDOCurrency);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CURRENCY}
    procedure PutDate(const AName : string; const AData : TSDODateTime);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_DOUBLE}
    procedure PutDouble(const AName : string; const AData : TSDODouble);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    procedure PutFloat(const AName : string; const AData : TSDOFloat);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_FLOAT}
    procedure PutInteger(const AName : string; const AData : TSDOInteger);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_LONG}
    procedure PutLong(const AName : string; const AData : TSDOLong);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    procedure PutShort(const AName : string; const AData : TSDOShort);{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}
    procedure PutString(const AName : string; const AData : TSDOString);{$IFDEF USE_INLINE}inline;{$ENDIF}

    function GetDataBuffer(
      var AName : string;
      out AResultBuffer : PDataBuffer
    ) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetBoolean(var AName : string;var AData : TSDOBoolean) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
    function GetByte(var AName : string;var AData : TSDOByte) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_BYTES}
    function GetBytes(var AName : string;var AData : TSDOBytes) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    function GetChar(var AName : string;var AData : TSDOChar) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    function GetCurrency(var AName : string;var AData : TSDOCurrency) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_CURRENCY}
    function GetDate(var AName : string;var AData : TSDODateTime) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_DOUBLE}
    function GetDouble(var AName : string;var AData : TSDODouble) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    function GetFloat(var AName : string;var AData : TSDOFloat) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_FLOAT}
    function GetInteger(var AName : string;var AData : TSDOInteger) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$IFDEF HAS_SDO_LONG}
    function GetLong(var AName : string;var AData : TSDOLong) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    function GetShort(var AName : string;var AData : TSDOShort) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}
{$ENDIF HAS_SDO_SHORT}
    function GetString(var AName : string;var AData : TSDOString) : Boolean;{$IFDEF USE_INLINE}inline;{$ENDIF}

  protected // ISDOSerializerStream
    function GetFormatName() : string;
    procedure SetSerializationStyle(const ASerializationStyle : TSerializationStyle);
    function GetSerializationStyle():TSerializationStyle;
    procedure SetNameStyle(const AValue : TNameStyle);
    function GetNameStyle() : TNameStyle;
    function GetCurrentScope():string;
    procedure Clear();
    procedure Initialize();

    procedure BeginObject(
      Const AName      : string;
      Const ATypeInfo  : ISDOType
    );
    procedure BeginArray(
      const AName         : string;
      const AItemTypeInfo : ISDOType;
      const ABounds       : array of Integer
    );
    procedure NilCurrentScope();
    function IsCurrentScopeNil():Boolean;
    procedure EndScope();
    function BeginObjectRead(
      var   AScopeName : string;
      const ATypeInfo  : ISDOType
    ) : Integer;
    function BeginArrayRead(
      var   AScopeName : string;
      const ATypeInfo  : ISDOType;
      const AItemName  : string
    ):Integer;
    function GetScopeItemNames(
      const AItemStyle : TSerializationStyle;
      const AReturnList : TStrings
    ) : Integer;
    procedure EndScopeRead();

    procedure Put(
      const AName     : string;
      const ATypeInfo : ISDOType;
      const AData
    );overload;
    procedure Put(
      const ANameSpace,
            AName     : string;
      const ATypeInfo : ISDOType;
      const AData
    );overload;
    procedure PutScopeInnerValue(
      const ATypeInfo : ISDOType;
      const AData
    );
    function Get(
      const ATypeInfo : ISDOType;
      var   AName     : string;
      var   AData
    ) : Boolean;overload;
    function Get(
      const ANameSpace : string;
      const ATypeInfo  : ISDOType;
      var   AName      : string;
      var   AData
    ) : Boolean;overload;
    function GetScopeInnerValue(
      const ATypeInfo : ISDOType;
      var   AData
    ) : Boolean;
    function ReadBuffer(const AName : string) : string;
    //Please use this method if and _only_ if you do not have another way to achieve your aim!
    procedure WriteBuffer(const AValue : string);

    procedure SaveToStream(AStream : TStream);overload;
    procedure SaveToFile(const AFileName : string);overload;
    procedure LoadFromStream(AStream : TStream);overload;
    procedure LoadFromFile(const AFileName : string);overload;

    function GetBookMark() : TStreamBookmark;
    function GotoBookmark(const AValue : TStreamBookmark) : Boolean;
    // This procedures will raise exceptions!!!
    procedure Error(Const AMsg:string);overload;
    procedure Error(Const AMsg:string; Const AArgs : array of const);overload;
  public
    constructor Create();
    destructor Destroy();override;
  end;

  TDBGPinterProc = procedure(const AMsg:string);

  procedure ClearObj(const AOwner: PDataBuffer);
  procedure FreeObjectBuffer(var ABuffer : PDataBuffer); overload;
  function LoadObjectFromStream(const AStoreRdr : IDataStoreReader):PDataBuffer;
  procedure SaveObjectToStream(const ARoot: PDataBuffer; const ADest : IDataStore);
  function CreateArrayBuffer(
    const ALength   : Integer;
    const AName     : TDataName;
    const AOwner    : PDataBuffer = nil
  ):PDataBuffer;
  function CreateObjBuffer(
    const ADataType : TDataType;
    const AName     : TDataName;
    const AOwner    : PDataBuffer = nil
  ):PDataBuffer;

  function ToStr(const ABuffer : PDataBuffer) : TSDOString;
  procedure PrintObj(const ARoot: PDataBuffer; const ALevel : Integer; const APrinterProc : TDBGPinterProc);


implementation

function sdo_GetMem(Size : PtrInt) : Pointer;{$IFDEF USE_INLINE}inline;{$ENDIF}
begin
  {$IFDEF FPC}
  Result := GetMem(Size);
  {$ELSE}
  GetMem(Result,Size);
  {$ENDIF}
end;

function ToStr(const ABuffer : PDataBuffer) : TSDOString;
begin
  Result := '';
  if ( ABuffer <> nil ) then begin
    case ABuffer^.DataType of
      dtInt8U     : Result := TSDOConvertHelper.ByteToString(ABuffer^.Int8U);
      dtInt8S     : Result := TSDOConvertHelper.ShortToString(ABuffer^.Int8S);
      dtInt16U    : Result := TSDOConvertHelper.IntegerToString(ABuffer^.Int16U);
      dtInt16S    : Result := TSDOConvertHelper.ShortToString(ABuffer^.Int16S);
      dtInt32U    : Result := TSDOConvertHelper.LongToString(ABuffer^.Int32U);
      dtInt32S    : Result := TSDOConvertHelper.IntegerToString(ABuffer^.Int32S);
      dtInt64U    : Result := IntToStr(ABuffer^.Int64U);
      dtInt64S    : Result := IntToStr(ABuffer^.Int64S);

      dtBool      : Result := TSDOConvertHelper.BoolToString(ABuffer^.BoolData);
      dtAnsiChar  : Result := String(ABuffer^.AnsiCharData);
      dtWideChar  : Result := ABuffer^.WideCharData;
      dtEnum      : Result := IntToStr(ABuffer^.EnumData);
      dtSingle    : Result := TSDOConvertHelper.FloatToString(ABuffer^.SingleData);
      dtDouble    : Result := TSDOConvertHelper.FloatToString(ABuffer^.DoubleData);
      dtExtended  : Result := TSDOConvertHelper.FloatToString(ABuffer^.ExtendedData);
      dtCurrency  : Result := TSDOConvertHelper.CurrencyToString(ABuffer^.CurrencyData);
      dtAnsiString: Result := String(ABuffer^.AnsiStrData^.Data);
      dtWideString: Result := ABuffer^.WideStrData^.Data;
{$IFDEF USE_UNICODE}
      dtUnicodeString : Result := ABuffer^.UnicodeStrData^.Data;
{$ENDIF USE_UNICODE}
    end;
  end;
end;

{var FFile : TMemoryStream = nil;
procedure DBG_Write(const AMsg:string);
var
  s : string;
begin
  if ( FFile = nil ) then
    FFile := TMemoryStream.Create();
  s := AMsg + sLineBreak;
  FFile.Write(s[1],Length(s));
end;}

procedure PrintObj(const ARoot: PDataBuffer; const ALevel : Integer; const APrinterProc : TDBGPinterProc);
Var
  p : PObjectBufferItem;
  s : string;
  i ,j: Integer;
  da : TByteDynArray;
Begin
  If Not Assigned(ARoot) Then
    Exit;
  s := StringOfChar(' ',ALevel);
  Case ARoot^.DataType Of
    dtInt8S   : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int8S) );
    dtInt8U   : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int8U) );
    dtInt32U  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int32U) );
    dtInt32S  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int32S) );
    dtInt64U  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int64U) );
    dtInt64S  : APrinterProc( s + ARoot^.Name + ' = ' + IntToStr(ARoot^.Int64S) );

    dtSingle  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.SingleData) );
    dtDouble  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.DoubleData) );
    dtExtended  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.ExtendedData) );
    dtCurrency  : APrinterProc( s + ARoot^.Name + ' = ' + FloatToStr(ARoot^.CurrencyData) );

    dtAnsiString  : APrinterProc( s + ARoot^.Name + ' = ' + String(ARoot^.AnsiStrData^.Data) );
    dtWideString  : APrinterProc( s + ARoot^.Name + ' = ' + ARoot^.WideStrData^.Data );
{$IFDEF USE_UNICODE}
    dtUnicodeString  : APrinterProc( s + ARoot^.Name + ' = ' + ARoot^.UnicodeStrData^.Data );
{$ENDIF USE_UNICODE}
    dtObject  :
      Begin
        APrinterProc( s + ARoot^.Name + ' = ');
        If Not Assigned(ARoot^.ObjectData) Then Begin
          APrinterProc(s + '  <Vide>');
        End Else Begin
          APrinterProc('( ' + IntToStr(ARoot^.ObjectData^.Count) + ' Objects )');
          p := ARoot^.ObjectData^.Head;
          i := ALevel + 1;
          While Assigned(p) Do Begin
            PrintObj(p^.Data,i,APrinterProc);
            p := p^.Next;
          End;
        End;
      End;
    dtArray :
      Begin
        APrinterProc( s + ARoot^.Name + ' = ');
        If Not Assigned(ARoot^.ArrayData) Then Begin
          APrinterProc(s + '  <Vide>');
        End Else Begin
          j := ARoot^.ArrayData^.Count;
          APrinterProc('( Objects[ '+ IntToStr(j)+ '] )');
          i := ALevel + 1;
          For j := 0 To Pred(j) Do Begin
            PrintObj(ARoot^.ArrayData^.Items^[j],i,APrinterProc);
          End;
        End;
      End;
    dtByteDynArray :
      begin
        APrinterProc( s + ARoot^.Name + ' = ');
        if ( ARoot^.ByteDynArrayData = nil ) then begin
          APrinterProc(s + '  <Vide>');
        end else begin
          i := Length(ARoot^.ByteDynArrayData^.Data);
          j := i;
          da := ARoot^.ByteDynArrayData^.Data;
          for j := 1 to j do
            s := s + Format(' %d',[da[Pred(j)]]);
          APrinterProc('( Bytes[ '+ IntToStr(i)+ '] )' + s);
        end;
      end;
  End;
End;

function FindObj(const AOwner: PDataBuffer; const AName : TDataName) : PDataBuffer;
Var
  p : PObjectBufferItem;
Begin
  Assert(AOwner^.DataType >= dtObject);
  Result := Nil;
   p:= AOwner^.ObjectData^.Head;
  While Assigned(p) Do Begin
    If AnsiSameText(AName,p^.Data^.Name) Then Begin
      Result := p^.Data;
      Exit;
    End;
    p := p^.Next;
  End;
End;

procedure AddObj(
  const AOwner, AChildData: PDataBuffer;
  const AIndex : Integer = -1
);
Var
  p : PObjectBufferItem;
Begin
  If ( AOwner^.DataType = dtObject ) Then Begin
    p := sdo_GetMem(SizeOf(TObjectBufferItem));
    p^.Data := AChildData;
    p^.Next := Nil;
    If Assigned(AOwner^.ObjectData^.Head) Then Begin
      AOwner^.ObjectData^.Last^.Next := p;
    End Else Begin
      AOwner^.ObjectData^.Head := p;
    End;
    AOwner^.ObjectData^.Last := p;
    Inc(AOwner^.ObjectData^.Count);
  End Else If ( AOwner^.DataType = dtArray ) Then Begin
    If ( AIndex >= 0 ) And ( AIndex < AOwner^.ArrayData^.Count ) Then
      AOwner^.ArrayData^.Items^[AIndex] := AChildData
    Else
      raise ESDOSerializationException.CreateFmt(SERR_IndexOutOfBound,[AIndex])
  End Else Begin
    Raise ESDOSerializationException.CreateFmt(SERR_InvalidDataTypeInContext,[IntToStr(Ord(AOwner^.DataType))])
  End;
End;

function CreateObjBuffer(
  const ADataType : TDataType;
  const AName     : TDataName;
  const AOwner    : PDataBuffer = nil
):PDataBuffer;
var
  resLen, i : Integer;
begin
  resLen := SizeOf(TDataBuffer);
  Result := sdo_GetMem(resLen);
  Try
    FillChar(Result^,resLen,#0);
    Result^.Name := AName;
    Result^.DataType := ADataType;
    Case Result^.DataType Of
      dtAnsiString :
        Begin
          i := SizeOf(TAnsiStringBuffer);
          Result^.AnsiStrData := sdo_GetMem(i);
          FillChar(Result^.AnsiStrData^,i,#0);
          Result^.AnsiStrData^.Data := '';
        End;
      dtWideString :
        begin
          i := SizeOf(TWideStringBuffer);
          Result^.WideStrData := sdo_GetMem(i);
          FillChar(Result^.WideStrData^,i,#0);
          Result^.WideStrData^.Data := '';
        end;
{$IFDEF USE_UNICODE}
      dtUnicodeString :
        begin
          i := SizeOf(TUnicodeStringBuffer);
          Result^.UnicodeStrData := sdo_GetMem(i);
          FillChar(Result^.UnicodeStrData^,i,#0);
          Result^.UnicodeStrData^.Data := '';
        end;
{$ENDIF USE_UNICODE}
      dtObject :
        Begin
          Result^.ObjectData := sdo_GetMem(SizeOf(TObjectBuffer));
          FillChar(Result^.ObjectData^,SizeOf(TObjectBuffer),#0);
        End;
      dtByteDynArray :
        begin
          i := SizeOf(TByteDynArrayBuffer);
          Result^.ByteDynArrayData := sdo_GetMem(i);
          FillChar(Result^.ByteDynArrayData^,i,#0);
          Result^.ByteDynArrayData^.Data := nil;
        end;
    End;
    If Assigned(AOwner) Then
      AddObj(AOwner,Result);
  Except
    Freemem(Result,resLen);
    Result := nil;
    Raise;
  End;
end;

function CreateArrayBuffer(
  const ALength   : Integer;
  const AName     : TDataName;
  const AOwner    : PDataBuffer = nil
):PDataBuffer;
Var
  i, resLen : Integer;
begin
  Assert(ALength>=0);
  resLen := SizeOf(TDataBuffer);
  Result := sdo_GetMem(resLen);
  Try
    FillChar(Result^,resLen,#0);
    Result^.Name := AName;
    Result^.DataType := dtArray;
    Result^.ArrayData := sdo_GetMem(SizeOf(TArrayBuffer));
    FillChar(Result^.ArrayData^,SizeOf(TArrayBuffer),#0);
    Result^.ArrayData^.Count := ALength;
    If ( ALength > 0 ) Then Begin
      i := ALength*SizeOf(PDataBuffer);
      Result^.ArrayData^.Items := sdo_GetMem(i);
      FillChar(Result^.ArrayData^.Items^[0],i,#0);
    End Else Begin
      Result^.ArrayData^.Items := Nil;
    End;
    If Assigned(AOwner) Then
      AddObj(AOwner,Result);
  Except
    Freemem(Result,resLen);
    Result := nil;
    Raise;
  End;
end;

procedure SaveObjectToStream(const ARoot: PDataBuffer; const ADest : IDataStore);
Var
  p : PObjectBufferItem;
  i : TInt32S;
Begin
  If Not Assigned(ARoot) Then
    Exit;
  i := Ord(ARoot^.DataType);
  ADest.WriteInt32S(i);
  ADest.WriteAnsiStr(ARoot^.Name);
  Case ARoot^.DataType Of
    dtInt8S  : ADest.WriteInt8S(ARoot^.Int8S);
      dtInt8U  : ADest.WriteInt8U(ARoot^.Int8U);
    dtInt16U  : ADest.WriteInt16U(ARoot^.Int16U);
      dtInt16S  : ADest.WriteInt16S(ARoot^.Int16S);
    dtInt32U  : ADest.WriteInt32U(ARoot^.Int32U);
      dtInt32S  : ADest.WriteInt32S(ARoot^.Int32S);
    dtInt64U  : ADest.WriteInt64U(ARoot^.Int64U);
      dtInt64S  : ADest.WriteInt64S(ARoot^.Int64S);
      
    dtSingle  : ADest.WriteSingle(ARoot^.SingleData);
    dtDouble  : ADest.WriteDouble(ARoot^.DoubleData);
    dtExtended  : ADest.WriteExtended(ARoot^.ExtendedData);
    dtCurrency  : ADest.WriteCurrency(ARoot^.CurrencyData);
    
    dtAnsiString  : ADest.WriteAnsiStr(ARoot^.AnsiStrData^.Data);
    dtWideString  : ADest.WriteWideStr(ARoot^.WideStrData^.Data);
{$IFDEF USE_UNICODE}
    dtUnicodeString  : ADest.WriteUnicodeStr(ARoot^.UnicodeStrData^.Data);
{$ENDIF USE_UNICODE}
    dtBool    : ADest.WriteBool(ARoot^.BoolData);
    dtAnsiChar    : ADest.WriteAnsiChar(ARoot^.AnsiCharData);
    dtWideChar    : ADest.WriteWideChar(ARoot^.WideCharData);
    dtEnum    : ADest.WriteEnum(ARoot^.EnumData);
    dtObject :
      Begin
        ADest.WriteBool(ARoot^.ObjectData^.NilObject) ;
        if not ARoot^.ObjectData^.NilObject then begin
          i := ARoot^.ObjectData^.Count;
          ADest.WriteInt32S(i);

          If ( i > 0 ) Then Begin
            p := ARoot^.ObjectData^.Head;
            For i := 1 To i Do Begin
              SaveObjectToStream(p^.Data,ADest);
              p := p^.Next;
            End;
          End;
          ADest.WriteBool(Assigned(ARoot^.ObjectData^.InnerData));
          if Assigned(ARoot^.ObjectData^.InnerData) then
            SaveObjectToStream(ARoot^.ObjectData^.InnerData,ADest);
        end;
      End;
    dtArray :
      Begin
        i := ARoot^.ArrayData^.Count;
        ADest.WriteInt32S(i);

        If ( i > 0 ) Then Begin
          For i := 0 To Pred(i) Do Begin
            SaveObjectToStream(ARoot^.ArrayData^.Items^[i],ADest);
          End;
        End;
      End;
    dtByteDynArray : ADest.WriteBinary(ARoot^.ByteDynArrayData^.Data);
  End;
End;

function LoadObjectFromStream(const AStoreRdr : IDataStoreReader):PDataBuffer;
Var
  i : TInt32S;
  s : string;
Begin
  Result := Nil;
  If AStoreRdr.IsAtEof() Then
    Exit;
  i := AStoreRdr.ReadInt32S();
  s := AStoreRdr.ReadAnsiStr();
  if (TDataType(i) < dtArray) or (TDataType(i) = dtByteDynArray) then
    Result := CreateObjBuffer(TDataType(i),s);
  Case TDataType(i) Of
    dtInt8S   : Result^.Int8S := AStoreRdr.ReadInt8S();
    dtInt8U   : Result^.Int8U := AStoreRdr.ReadInt8U();
    dtInt16U  : Result^.Int16U := AStoreRdr.ReadInt16U();
    dtInt16S  : Result^.Int16S := AStoreRdr.ReadInt16S();
    dtInt32U  : Result^.Int32U := AStoreRdr.ReadInt32U();
    dtInt32S  : Result^.Int32S := AStoreRdr.ReadInt32S();
    dtInt64U  : Result^.Int64U := AStoreRdr.ReadInt64U();
    dtInt64S  : Result^.Int64S := AStoreRdr.ReadInt64S();

    dtSingle  : Result^.SingleData := AStoreRdr.ReadSingle();
    dtDouble  : Result^.DoubleData := AStoreRdr.ReadDouble();
    dtExtended  : Result^.ExtendedData := AStoreRdr.ReadExtended();
    dtCurrency  : Result^.CurrencyData := AStoreRdr.ReadCurrency();

    dtAnsiString  : Result^.AnsiStrData^.Data := AStoreRdr.ReadAnsiStr();
    dtWideString  : Result^.WideStrData^.Data := AStoreRdr.ReadWideStr();
{$IFDEF USE_UNICODE}
    dtUnicodeString  : Result^.UnicodeStrData^.Data := AStoreRdr.ReadUnicodeStr();
{$ENDIF USE_UNICODE}
    dtBool    : Result^.BoolData := AStoreRdr.ReadBool();
    dtAnsiChar    : Result^.AnsiCharData := AStoreRdr.ReadAnsiChar();
    dtWideChar    : Result^.WideCharData := AStoreRdr.ReadWideChar();
    dtEnum    : Result^.EnumData := AStoreRdr.ReadEnum();
    dtObject  :
      Begin
        Result^.ObjectData^.NilObject := AStoreRdr.ReadBool();
        if not Result^.ObjectData^.NilObject then begin
          i := AStoreRdr.ReadInt32S();
          For i := 1 To i Do Begin
            AddObj(Result,LoadObjectFromStream(AStoreRdr));
          End;
          if AStoreRdr.ReadBool() then
            Result^.ObjectData^.InnerData := LoadObjectFromStream(AStoreRdr);
        end;
      end;
    dtArray  :
      Begin
        i := AStoreRdr.ReadInt32S();
        Result := CreateArrayBuffer(i,s);
        For i := 0 To Pred(i) Do Begin
          AddObj(Result,LoadObjectFromStream(AStoreRdr),i);
        End;
      End;
    dtByteDynArray  : Result^.ByteDynArrayData^.Data := AStoreRdr.ReadBinary();
  End;
End;

procedure FreeObjectBuffer(var ABuffer : PObjectBuffer);overload;
var
  p,q : PObjectBufferItem;
begin
  if Assigned(ABuffer) then begin
    if Assigned(ABuffer^.Attributes) then
      FreeObjectBuffer(ABuffer^.Attributes);
    p := ABuffer^.Head;
    while Assigned(p) do begin
      q := p;
      p := p^.Next;
      ClearObj(q^.Data);
      Freemem(q^.Data);
      q^.Data := Nil;
      Freemem(q);
    end;
    if Assigned(ABuffer^.InnerData) then begin
      ClearObj(ABuffer^.InnerData);
      Freemem(ABuffer^.InnerData);
      ABuffer^.InnerData := nil;
    end;
    //ABuffer^.Head := nil;
    //ABuffer^.Last := nil;
    Freemem(ABuffer);
    ABuffer := nil;
  end;
end;

procedure FreeObjectBuffer(var ABuffer : PDataBuffer);
var
  tmpBuffer : PDataBuffer;
begin
  if ( ABuffer <> nil ) then begin
    tmpBuffer := ABuffer;
    ABuffer := nil;
    ClearObj(tmpBuffer);
    FreeMem(tmpBuffer)
  end;
end;

procedure ClearObj(const AOwner: PDataBuffer);
Var
  i , j: Integer;
  eltLen : Integer;
Begin
  AOwner^.Name := '';
  Case AOwner^.DataType Of
    dtAnsiString :
      Begin
        AOwner^.AnsiStrData^.Data := '';
        Freemem(AOwner^.AnsiStrData);
        AOwner^.AnsiStrData := Nil;
      End;
    dtWideString :
      begin
        AOwner^.WideStrData^.Data := '';
        Freemem(AOwner^.WideStrData);
        AOwner^.WideStrData := Nil;
      end;
{$IFDEF USE_UNICODE}
    dtUnicodeString :
      begin
        AOwner^.UnicodeStrData^.Data := '';
        Freemem(AOwner^.UnicodeStrData);
        AOwner^.UnicodeStrData := Nil;
      end;
{$ENDIF USE_UNICODE}
    dtObject :
      Begin
        FreeObjectBuffer(AOwner^.ObjectData);
      End;
    dtArray :
      Begin
        eltLen := SizeOf(TDataBuffer);
        For j := 0 to Pred(AOwner^.ArrayData^.Count) Do Begin
          if (AOwner^.ArrayData^.Items^[j] <> nil) then begin
            ClearObj(AOwner^.ArrayData^.Items^[j]);
            Freemem(AOwner^.ArrayData^.Items^[j],eltLen);
          end;
          AOwner^.ArrayData^.Items^[j] := Nil;
        End;
        i := AOwner^.ArrayData^.Count * SizeOf(PDataBuffer);
        Freemem(AOwner^.ArrayData^.Items,i);
        AOwner^.ArrayData^.Items := Nil;
        FreeObjectBuffer(AOwner^.ArrayData^.Attributes);
        i := SizeOf(TArrayBuffer);
        Freemem(AOwner^.ArrayData,i);
        AOwner^.ArrayData := Nil;
      End;
    dtByteDynArray :
      begin
        SetLength(AOwner^.ByteDynArrayData^.Data,0);
        AOwner^.ByteDynArrayData^.Data := nil;
        Freemem(AOwner^.ByteDynArrayData);
        AOwner^.ByteDynArrayData := Nil;
      end;
  End;
End;

{ TObjectBaseArrayStackItem }

procedure TObjectBaseArrayStackItem.CopyTo(const AClone: TStackItem);
begin
  inherited CopyTo(AClone);
  TObjectBaseArrayStackItem(AClone).FIndex := FIndex;
  TObjectBaseArrayStackItem(AClone).FItemName := FItemName;
end;

function TObjectBaseArrayStackItem.GetItemCount: Integer;
var
  p : PObjectBufferItem;
begin
  Result := 0;
  p := FScopeObject^.ObjectData^.Head;
  while (p <> nil) do begin
    if AnsiSameText(FItemName,p^.Data^.Name) then
      Inc(Result);
    p := p^.Next;
  end;
end;

function TObjectBaseArrayStackItem.Find(var AName: TDataName): PDataBuffer;
var
  p : PObjectBufferItem;
  i : Integer;
begin
  Result := nil;
  if (FIndex >= ScopeObject^.ObjectData^.Count) then
    exit;
  i := -1;
  p := FScopeObject^.ObjectData^.Head;
  while (i < FIndex) and (p <> nil) do begin
    if AnsiSameText(FItemName,p^.Data^.Name) then begin
      Inc(i);
      if (i = FIndex) then begin
        Result := p^.Data;
        Inc(FIndex);
      end;
    end;
    p := p^.Next;
  end;
end;

procedure TObjectBaseArrayStackItem.SetItemName(const AValue: TDataName);
begin
  FItemName := AValue;
end;

{ TStackItem }

procedure TStackItem.CopyTo(const AClone: TStackItem);
begin
  AClone.FScopeObject := Self.FScopeObject;
end;

constructor TStackItem.Create(const AScopeObject: PDataBuffer);
begin
  Assert(Assigned(AScopeObject));
  FScopeObject := AScopeObject;
end;

function TStackItem.Clone() : TStackItem;
begin
  Result := TStackItemClass(Self.ClassType).Create(FScopeObject);
  try
    CopyTo(Result);
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TObjectStackItem }

function TObjectStackItem.GetItemCount(): Integer;
begin
  Result := ScopeObject^.ObjectData^.Count;
end;

function TObjectStackItem.Find(var AName: TDataName): PDataBuffer;
begin
  Result := FindObj(ScopeObject,AName);
end;

function TObjectStackItem.GetByIndex(const AIndex: Integer): PDataBuffer;
Var
  p : PObjectBufferItem;
  i : Integer;
begin
  If ( AIndex >=0 ) And ( AIndex < ScopeObject^.ObjectData^.Count) Then Begin
    p := ScopeObject^.ObjectData^.Head;
    For i := 1 To AIndex Do
      p := p^.Next;
    Result := p^.Data;
  End Else
    Raise ESDOSerializationException.CreateFmt(SERR_IndexOutOfBound,[AIndex]);
end;

function TObjectStackItem.CreateBuffer(
  Const AName     : String;
  const ADataType : TDataType
):PDataBuffer;
begin
  Result := CreateObjBuffer(ADataType,AName,ScopeObject);
end;

function TObjectStackItem.CreateInnerBuffer(const ADataType: TDataType): PDataBuffer;
begin
  Result := CreateObjBuffer(ADataType,sSCOPE_INNER_NAME,nil);
  ScopeObject^.ObjectData^.InnerData := Result;
end;

function TObjectStackItem.GetInnerBuffer(): PDataBuffer;
begin
  Result := ScopeObject^.ObjectData^.InnerData;
end;

procedure TObjectStackItem.NilCurrentScope();
begin
  Assert(ScopeObject^.ObjectData^.Count = 0);
  ScopeObject^.ObjectData^.NilObject := True;
end;

function TObjectStackItem.IsCurrentScopeNil(): Boolean;
begin
  Result := ScopeObject^.ObjectData^.NilObject;
end;

//----------------------------------------------------------------
function TObjectStackItem.GetScopeItemNames(const AReturnList: TStrings): Integer;
var
  locBuffer : PObjectBufferItem;
begin
  AReturnList.Clear();
  if Assigned(ScopeObject) and ( ScopeObject^.ObjectData^.Count > 0 ) then begin
    locBuffer := ScopeObject^.ObjectData^.Head;
    while Assigned(locBuffer) do begin
      AReturnList.Add(locBuffer^.Data^.Name);
      locBuffer := locBuffer^.Next;
    end;
  end;
  Result := AReturnList.Count;
end;

{ TSDOSerializationStreamBinary }

procedure TSDOSerializationStreamBinary.ClearStack();
Var
  i, c : Integer;
begin
  c := FStack.Count;
  For I := 1 To c Do
    FStack.Pop().Free();
end;

procedure TSDOSerializationStreamBinary.PushStack(AScopeObject: PDataBuffer;const AScopeType: TScopeType);
begin
  if ( AScopeType = stObject ) then begin
    FStack.Push(TObjectStackItem.Create(AScopeObject))
  end else if (AScopeType = stArray) then begin
    if (AScopeObject^.DataType = dtObject) then
      FStack.Push(TObjectBaseArrayStackItem.Create(AScopeObject))
    else
      FStack.Push(TArrayStackItem.Create(AScopeObject));
  end else begin
    Assert(False);
  end;
end;

function TSDOSerializationStreamBinary.StackTop(): TStackItem;
begin
  Result := FStack.Peek() as TStackItem;
end;

function TSDOSerializationStreamBinary.PopStack(): TStackItem;
begin
  Result := FStack.Pop() as TStackItem;
end;

function TSDOSerializationStreamBinary.GetRootData(): PDataBuffer;
begin
  Result := FRootData;
end;

function TSDOSerializationStreamBinary.GetCurrentScopeObject(): PDataBuffer;
begin
  Result := StackTop().ScopeObject;
end;

procedure TSDOSerializationStreamBinary.SetSerializationStyle(
  const ASerializationStyle: TSerializationStyle
);
begin
  FSerializationStyle := ASerializationStyle;
end;

function TSDOSerializationStreamBinary.GetSerializationStyle(): TSerializationStyle;
begin
  Result := FSerializationStyle;
end;

function TSDOSerializationStreamBinary.HasScope(): Boolean;
begin
  Result := ( FStack.Count > 0 );
end;

procedure TSDOSerializationStreamBinary.CheckScope();
begin
  If Not HasScope() Then
    Error(SERR_NoScope);
end;

function TSDOSerializationStreamBinary.GetCurrentScope: String;
begin
  Result := GetCurrentScopeObject()^.Name;
end;

function TSDOSerializationStreamBinary.GetDataBuffer(
  var AName: string;
  out AResultBuffer : PDataBuffer
) : Boolean;
begin
  AResultBuffer := StackTop().Find(AName);
  Result := ( AResultBuffer <> nil );
end;

procedure TSDOSerializationStreamBinary.Clear();
begin
  ClearStack();
  if ( FRootData <> nil ) then begin
    ClearObj(FRootData);
    Freemem(FRootData);
  end;
  //FRootData := CreateObjBuffer(dtObject,sROOT);
  //PushStack(FRootData,stObject);
  FRootData := nil;
end;

procedure TSDOSerializationStreamBinary.BeginArray(
  const AName         : string;
  const AItemTypeInfo : ISDOType;
  const ABounds       : array of Integer
);
var
  i, j, k : Integer;
begin
  if ( Length(ABounds) < 2 ) then
    Error(SERR_InvalidArrayBounds);
  i := ABounds[0];
  j := ABounds[1];
  k := ( j - i + 1 );
  if ( k < 0 ) then
    Error(SERR_InvalidArrayBounds);
  PushStack(CreateArrayBuffer(k,AName,StackTop().ScopeObject),stArray);
end;

procedure TSDOSerializationStreamBinary.NilCurrentScope();
begin
  CheckScope();
  StackTop().NilCurrentScope();
end;

function TSDOSerializationStreamBinary.IsCurrentScopeNil(): Boolean;
begin
  Result := StackTop().IsCurrentScopeNil();
end;

procedure TSDOSerializationStreamBinary.BeginObject(
  const AName: string;
  const ATypeInfo: ISDOType
);
begin
  if HasScope() then begin
    PushStack(StackTop().CreateBuffer(AName,dtObject));
  end else begin
    FRootData := CreateObjBuffer(dtObject,AName);
    PushStack(FRootData,stObject);
  end;
end;

procedure TSDOSerializationStreamBinary.EndScope();
begin
  FStack.Pop().Free();
end;

function TSDOSerializationStreamBinary.BeginObjectRead(
  var   AScopeName : string;
  const ATypeInfo  : ISDOType
): Integer;
var
  locNode : PDataBuffer;
  stk : TStackItem;
begin
  stk := StackTop();
  locNode := stk.Find(AScopeName);
  if (locNode = nil) then
    exit(-1);
  PushStack(locNode,stObject);
  Result := StackTop().GetItemCount();
end;

function TSDOSerializationStreamBinary.BeginArrayRead(
  var   AScopeName : string;
  const ATypeInfo  : ISDOType;
  const AItemName  : string
): Integer;
var
  locNode : PDataBuffer;
  stk : TStackItem;
begin
  Result := -1;
  stk := StackTop();
  locNode := stk.Find(AScopeName);
  if (locNode <> nil) then begin
    if (locNode^.DataType <> dtArray) then begin
      PushStack(stk.ScopeObject,stArray);
      (StackTop() as TObjectBaseArrayStackItem).SetItemName(AItemName);
    end else begin
      PushStack(locNode,stArray);
    end;
    Result := StackTop().GetItemCount();
  end;
end;

function TSDOSerializationStreamBinary.GetScopeItemNames(
  const AItemStyle : TSerializationStyle;
  const AReturnList : TStrings
) : Integer;
begin
  CheckScope();
  Result := StackTop.GetScopeItemNames(AReturnList);
end;

procedure TSDOSerializationStreamBinary.EndScopeRead();
begin
  PopStack().Free();
end;

procedure TSDOSerializationStreamBinary.Put(
  const AName     : string;
  const ATypeInfo : ISDOType;
  const AData
);
var
  valBuffer : TValueBuffer;
  strData : TSDOString;
  bytesData : TSDOBytes;
begin
  case ATypeInfo.getTypeEnum() Of
    BooleanType :
      begin
        valBuffer.BooleanValue := TSDOBoolean(AData);
        PutBoolean(AName,valBuffer.BooleanValue);
      end;
    ByteType :
      begin
        valBuffer.ByteValue := TSDOByte(AData);
        PutByte(AName,valBuffer.ByteValue);
      end;
{$IFDEF HAS_SDO_BYTES}
     BytesType :
      begin
        bytesData := TSDOBytes(AData);
        PutBytes(AName,bytesData);
      end;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    CharacterType :
      begin
        valBuffer.CharValue := TSDOChar(AData);
        PutChar(AName,valBuffer.CharValue);
      end;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType :
      begin
        valBuffer.CurrencyValue := TSDOCurrency(AData);
        PutCurrency(AName,valBuffer.CurrencyValue);
      end;
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType :
      begin
        valBuffer.DateValue := TSDODateTime(AData);
        PutDate(AName,valBuffer.DateValue);
      end;
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType :
      begin
        valBuffer.DoubleValue := TSDODouble(AData);
        PutDouble(AName,valBuffer.DoubleValue);
      end;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType :
      begin
        valBuffer.FloatValue := TSDOFloat(AData);
        PutFloat(AName,valBuffer.FloatValue);
      end;
{$ENDIF HAS_SDO_FLOAT}
    IntegerType :
      begin
        valBuffer.IntegerValue := TSDOInteger(AData);
        PutInteger(AName,valBuffer.IntegerValue);
      end;
{$IFDEF HAS_SDO_LONG}
    LongType :
      begin
        valBuffer.LongValue := TSDOLong(AData);
        PutLong(AName,valBuffer.LongValue);
      end;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType :
      begin
        valBuffer.ShortValue := TSDOShort(AData);
        PutShort(AName,valBuffer.ShortValue);
      end;
{$ENDIF HAS_SDO_SHORT}
    StringType  :
      begin
        strData := TSDOString(AData);
        PutString(AName,strData);
      end;
    else
      Assert(False);
  end;
end;

procedure TSDOSerializationStreamBinary.Put(
  const ANameSpace,
        AName     : string;
  const ATypeInfo : ISDOType;
  const AData
);
begin
  Put(AName,ATypeInfo,AData);
end;

procedure TSDOSerializationStreamBinary.PutScopeInnerValue(
  const ATypeInfo : ISDOType;
  const AData
);


  procedure doPutDate();
  var
    locBuffer : TByteDynArray;
    locDate : TSDODateTime;
  begin
    locDate := TSDODateTime(AData);
    SetLength(locBuffer,SizeOf(TSDODateTime));
    Move(locDate,locBuffer[0],SizeOf(TSDODateTime));
    ReverseBytes(locBuffer[0],SizeOf(TSDODateTime));
    StackTop().CreateInnerBuffer(dtByteDynArray)^.ByteDynArrayData^.Data := locBuffer;
  end;


begin
  CheckScope();
  case ATypeInfo.getTypeEnum() Of
    BooleanType : StackTop().CreateInnerBuffer(dtBool)^.BoolData := TSDOBoolean(AData);
{$IFDEF HAS_SDO_BYTES}
    BytesType   : StackTop().CreateInnerBuffer(dtByteDynArray)^.ByteDynArrayData^.Data := Copy(TSDOBytes(AData));
{$ENDIF HAS_SDO_BYTES}
    ByteType    : StackTop().CreateInnerBuffer(dtInt8U)^.Int8U := TSDOByte(AData);
{$IFDEF HAS_SDO_CHAR}
  {$IFDEF USE_UNICODE}
    CharacterType : StackTop().CreateInnerBuffer(dtWideChar)^.WideCharData := TSDOChar(AData);
  {$ELSE USE_UNICODE}
    CharacterType : StackTop().CreateInnerBuffer(dtAnsiChar)^.AnsiCharData := TSDOChar(AData);
  {$ENDIF USE_UNICODE}
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType : StackTop().CreateInnerBuffer(dtCurrency)^.CurrencyData := TSDOCurrency(AData);
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType: doPutDate();
    IntegerType : StackTop().CreateInnerBuffer(dtInt32S)^.Int32S := TSDOInteger(AData);
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType  : StackTop().CreateInnerBuffer(dtDouble)^.DoubleData := TSDODouble(AData);
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType   : StackTop().CreateInnerBuffer(dtSingle)^.SingleData := TSDOFloat(AData);
{$ENDIF HAS_SDO_FLOAT}
{$IFDEF HAS_SDO_LONG}
    LongType    : StackTop().CreateInnerBuffer(dtInt64S)^.Int64S := TSDOLong(AData);
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType   : StackTop().CreateInnerBuffer(dtInt16S)^.Int16S := TSDOShort(AData);
{$ENDIF HAS_SDO_SHORT}
    StringType  :
{$IFDEF USE_UNICODE}
      StackTop().CreateInnerBuffer(dtUnicodeString)^.UnicodeStrData^.Data := TSDOString(AData);
{$ELSE USE_UNICODE}
      StackTop().CreateInnerBuffer(dtAnsiString)^.AnsiStrData^.Data := TSDOString(AData);
{$ENDIF USE_UNICODE}
    else
      Assert(False);
  end;
end;

function TSDOSerializationStreamBinary.Get(
  const ATypeInfo: ISDOType;
  var AName: String;
  var AData
) : Boolean;
var
  valBuffer : TValueBuffer;
  strData : TSDOString;
  bytesData : TSDOBytes;
begin
  FillChar(valBuffer,SizeOf(valBuffer),#0);
  case ATypeInfo.getTypeEnum() of
    BooleanType :
      begin
        Result := GetBoolean(AName,valBuffer.BooleanValue);
        if Result then
          TSDOBoolean(AData) := valBuffer.BooleanValue;
      end;
    ByteType :
      begin
        Result := GetByte(AName,valBuffer.ByteValue);
        if Result then
          TSDOByte(AData) := valBuffer.ByteValue;
      end;
{$IFDEF HAS_SDO_BYTES}
    BytesType :
      begin
        Result := GetBytes(AName,bytesData);
        if Result then
          TSDOBytes(AData) := bytesData;
      end;
{$ENDIF HAS_SDO_BYTES}
{$IFDEF HAS_SDO_CHAR}
    CharacterType :
      begin
        Result := GetChar(AName,valBuffer.CharValue);
        if Result then
          TSDOChar(AData) := valBuffer.CharValue;
      end;
{$ENDIF HAS_SDO_CHAR}
{$IFDEF HAS_SDO_CURRENCY}
    CurrencyType :
      begin
        Result := GetCurrency(AName,valBuffer.CurrencyValue);
        if Result then
          TSDOCurrency(AData) := valBuffer.CurrencyValue;
      end;
{$ENDIF HAS_SDO_CURRENCY}
    DateTimeType :
      begin
        Result := GetDate(AName,valBuffer.DateValue);
        if Result then
          TSDODateTime(AData) := valBuffer.DateValue;
      end;
{$IFDEF HAS_SDO_DOUBLE}
    DoubleType :
      begin
        Result := GetDouble(AName,valBuffer.DoubleValue);
        if Result then
          TSDODouble(AData) := valBuffer.DoubleValue;
      end;
{$ENDIF HAS_SDO_DOUBLE}
{$IFDEF HAS_SDO_FLOAT}
    FloatType :
      begin
        Result := GetFloat(AName,valBuffer.FloatValue);
        if Result then
          TSDOFloat(AData) := valBuffer.FloatValue;
      end;
{$ENDIF HAS_SDO_FLOAT}
    IntegerType :
      begin
        Result := GetInteger(AName,valBuffer.IntegerValue);
        if Result then
          TSDOInteger(AData) := valBuffer.IntegerValue;
      end;
{$IFDEF HAS_SDO_LONG}
    LongType :
      begin
        Result := GetLong(AName,valBuffer.LongValue);
        if Result then
          TSDOLong(AData) := valBuffer.LongValue;
      end;
{$ENDIF HAS_SDO_LONG}
{$IFDEF HAS_SDO_SHORT}
    ShortType :
      begin
        Result := GetShort(AName,valBuffer.ShortValue);
        if Result then
          TSDOShort(AData) := valBuffer.ShortValue;
      end;
{$ENDIF HAS_SDO_SHORT}
    StringType  :
      begin
        strData := TSDOString(AData);
        Result := GetString(AName,strData);
        if Result then
          TSDOString(AData) := strData;
      end;
    else
      Result := False;
  end;
end;

function TSDOSerializationStreamBinary.Get(
  const ANameSpace : string;
  const ATypeInfo  : ISDOType;
  var   AName      : string;
  var   AData
) : Boolean;
begin
  Result := Get(ATypeInfo,AName,AData);
end;

function TSDOSerializationStreamBinary.GetScopeInnerValue(
  const ATypeInfo : ISDOType;
  var   AData
) : Boolean;
var
  dataBuffer : PDataBuffer;

  function HandleDate() : Boolean;
  var
    locDate : TSDODateTime;
    locBuffer : TByteDynArray;
  begin
    Result := False;
    locBuffer := Copy(dataBuffer^.ByteDynArrayData^.Data);
    if ( Length(locBuffer) = SizeOf(TSDODateTime) ) then begin
      ReverseBytes(locBuffer[0],SizeOf(TSDODateTime));
      Move(locBuffer[0],locDate,SizeOf(TSDODateTime));
      TSDODateTime(AData) := locDate;
      Result := True;
    end;
  end;

begin
  CheckScope();
  Result := True;
  dataBuffer := StackTop().GetInnerBuffer();
  case ATypeInfo.getTypeEnum() of
    BooleanType    : TSDOBoolean(AData) := dataBuffer^.BoolData;
    ByteType       : TSDOByte(AData) := dataBuffer^.Int8U;
    CharacterType  : TSDOChar(AData) := {$IFDEF USE_UNICODE}dataBuffer^.WideCharData{$ELSE}dataBuffer^.AnsiCharData{$ENDIF};
    CurrencyType   : TSDOCurrency(AData) := dataBuffer^.CurrencyData;
    DateTimeType   : HandleDate();
    DoubleType     : TSDODouble(AData) := dataBuffer^.DoubleData;
    FloatType      : TSDOFloat(AData) := dataBuffer^.SingleData;
    IntegerType    : TSDOInteger(AData) := dataBuffer^.Int32S;
    LongType       : TSDOLong(AData) := dataBuffer^.Int64S;
    ShortType      : TSDOShort(AData) := dataBuffer^.Int16S;
    StringType     : TSDOString(AData) := {$IFDEF USE_UNICODE}dataBuffer^.UnicodeStrData^.Data{$ELSE}dataBuffer^.AnsiStrData^.Data{$ENDIF};
    else
      Assert(False);
  end;
end;

function TSDOSerializationStreamBinary.ReadBuffer (const AName : string) : string;
Var
  locStore : IDataStore;
  bffr : PDataBuffer;
  locName : string;
  locStream : TStringStream;
begin
  Result := '';
  if GetDataBuffer(locName,bffr) then begin
    locStream := TStringStream.Create('');
    try
      locStore := CreateBinaryWriter(locStream);
      SaveObjectToStream(bffr,locStore);
      Result := locStream.DataString;
    finally
      locStream.Free();
    end;
  end;
end;

procedure TSDOSerializationStreamBinary.SaveToStream(AStream: TStream);
Var
  locStore : IDataStore;
begin
  locStore := CreateBinaryWriter(AStream);
  SaveObjectToStream(FRootData,locStore);
end;

procedure TSDOSerializationStreamBinary.LoadFromStream(AStream: TStream);
Var
  locRdr : IDataStoreReader;
  tmpRoot : PDataBuffer;
begin
  locRdr := CreateBinaryReader(AStream);
  tmpRoot := LoadObjectFromStream(locRdr);

  Clear();
  FRootData := tmpRoot;
  PushStack(FRootData,stObject);
end;

procedure TSDOSerializationStreamBinary.Error(const AMsg: string);
begin
  Raise ESDOSerializationException.Create(AMsg);
end;

procedure TSDOSerializationStreamBinary.Error(const AMsg: string;const AArgs: array of const);
begin
  Raise ESDOSerializationException.CreateFmt(AMsg,AArgs);
end;

constructor TSDOSerializationStreamBinary.Create();
begin
  //FRootData := CreateObjBuffer(dtObject,sROOT);
  FStack := TObjectStackEx.Create();
  //PushStack(FRootData,stObject);
end;

destructor TSDOSerializationStreamBinary.Destroy();
begin
  ClearStack();
  FreeAndNil(FStack);
  if ( FRootData <> nil ) then begin
    ClearObj(FRootData);
    Freemem(FRootData);
  end;
  inherited Destroy();
end;

function TSDOSerializationStreamBinary.GetFormatName() : string;
begin
  Result := sBINARY_FORMAT_NAME;
end;

procedure TSDOSerializationStreamBinary.WriteBuffer(const AValue: string);
var
  locStore : IDataStoreReader;
  bffr : PDataBuffer;
  locStream : TStringStream;
begin
  CheckScope();
  locStream := TStringStream.Create(AValue);
  try
    locStream.Position := 0;
    locStore := CreateBinaryReader(locStream);
    bffr := LoadObjectFromStream(locStore);
    AddObj(StackTop.ScopeObject,bffr);
  finally
    locStream.Free();
  end;
end;

function CopyStackItem(const AItem : TObject) : TObject;
begin
  if ( AItem <> nil ) then
    Result := TStackItem(AItem).Clone()
  else
    Result := nil;
end;

function TSDOSerializationStreamBinary.GetBookMark: TStreamBookmark;
var
  locRes : TStreamBinaryBookmark;
begin
  locRes := TStreamBinaryBookmark.Create();
  try
    locRes.FNameStyle := Self.FNameStyle;
    locRes.FStack := Self.FStack.Clone({$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyStackItem);
    locRes.FRootData := Self.FRootData;
    locRes.FSerializationStyle := Self.FSerializationStyle;
    Result := locRes;
  except
    FreeAndNil(locRes);
    raise;
  end;
end;

function TSDOSerializationStreamBinary.GetBoolean(var AName: string; var AData: TSDOBoolean): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.BoolData;
end;

function TSDOSerializationStreamBinary.GetByte(var AName: string; var AData: TSDOByte): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.Int8U;
end;

function TSDOSerializationStreamBinary.GetBytes(var AName: string; var AData: TSDOBytes): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := Copy(locBuffer^.ByteDynArrayData^.Data);
end;

function TSDOSerializationStreamBinary.GetChar(var AName: string; var AData: TSDOChar): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := {$IFDEF USE_UNICODE}locBuffer^.WideCharData{$ELSE}locBuffer^.AnsiCharData{$ENDIF};

end;

function TSDOSerializationStreamBinary.GetCurrency(var AName: string; var AData: TSDOCurrency): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.CurrencyData;
end;

function TSDOSerializationStreamBinary.GetDate(var AName: string; var AData: TSDODateTime): Boolean;
var
  locBytesBuffer : TByteDynArray;
  locBuffer : PDataBuffer;
begin
  Result := False;
  if GetDataBuffer(AName,locBuffer) then begin
    locBytesBuffer := Copy(locBuffer^.ByteDynArrayData^.Data);
    if ( Length(locBytesBuffer) = SizeOf(TSDODateTime) ) then begin
      ReverseBytes(locBytesBuffer[0],SizeOf(TSDODateTime));
      Move(locBytesBuffer[0],AData,SizeOf(TSDODateTime));
      Result := True;
    end;
  end;
end;

function TSDOSerializationStreamBinary.GetDouble(var AName: string; var AData: TSDODouble): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.DoubleData;
end;

function TSDOSerializationStreamBinary.GetFloat(var AName: string; var AData: TSDOFloat): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.SingleData;
end;

function TSDOSerializationStreamBinary.GetInteger(var AName: string; var AData: TSDOInteger): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.Int32S;
end;

function TSDOSerializationStreamBinary.GetLong(var AName: string; var AData: TSDOLong): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.Int64S;
end;

function TSDOSerializationStreamBinary.GetNameStyle: TNameStyle;
begin
  Result := FNameStyle;
end;

function TSDOSerializationStreamBinary.GetShort(var AName: string; var AData: TSDOShort): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then
    AData := locBuffer^.Int16S;
end;

function TSDOSerializationStreamBinary.GetString(var AName: string; var AData: TSDOString): Boolean;
var
  locBuffer : PDataBuffer;
begin
  Result := GetDataBuffer(AName,locBuffer);
  if Result then begin
    if ( locBuffer^.DataType = dtAnsiString ) then
      AData := locBuffer^.AnsiStrData^.Data
    else if ( locBuffer^.DataType = dtWideString ) then
      AData := locBuffer^.WideStrData^.Data
{$IFDEF USE_UNICODE}
    else if ( locBuffer^.DataType = dtUnicodeString ) then
      AData := locBuffer^.UnicodeStrData^.Data
{$ENDIF USE_UNICODE}
    else
      AData := ToStr(locBuffer);
  end;
end;

function TSDOSerializationStreamBinary.GotoBookmark(const AValue: TStreamBookmark): Boolean;
var
  locBM : TStreamBinaryBookmark;
begin
  Result := False;
  if ( AValue <> nil ) then begin
    locBM := AValue as TStreamBinaryBookmark;
    if (locBM.FRootData = Self.FRootData) then begin
      ClearStack();
      FreeAndNil(FStack);
      FStack := locBM.FStack.Clone({$IFDEF ATT_PROC_ADDRESS}@{$ENDIF}CopyStackItem);
      FSerializationStyle := locBM.SerializationStyle;
      FNameStyle := locBM.NameStyle;
      FRootData := locBM.RootData;
      Result := True;
    end;
  end;
end;

procedure TSDOSerializationStreamBinary.Initialize;
begin
  ClearStack();
  if ( FRootData <> nil ) then
    PushStack(FRootData);
end;

procedure TSDOSerializationStreamBinary.LoadFromFile(const AFileName: string);
var
  locStream : TStream;
begin
  if not FileExists(AFileName) then
    Error(SMSG_FileNotFound,[AFileName]);
  locStream := TFileStream.Create(AFileName,fmOpenRead or fmShareDenyWrite);
  try
    locStream.Position := 0;
    LoadFromStream(locStream);
  finally
    locStream.Free();
  end;
end;

procedure TSDOSerializationStreamBinary.PutBoolean(const AName: string; const AData: TSDOBoolean);
begin
  StackTop().CreateBuffer(AName,dtBool)^.BoolData := AData;
end;

procedure TSDOSerializationStreamBinary.PutByte(const AName: string; const AData: TSDOByte);
begin
  StackTop().CreateBuffer(AName,dtInt8U)^.Int8U := AData;
end;

procedure TSDOSerializationStreamBinary.PutBytes(const AName: string; const AData: TSDOBytes);
begin
  StackTop().CreateBuffer(AName,dtByteDynArray)^.ByteDynArrayData^.Data := Copy(AData);
end;

procedure TSDOSerializationStreamBinary.PutChar(const AName: string; const AData: TSDOChar);
begin
{$IFDEF USE_UNICODE}
  StackTop().CreateBuffer(AName,dtWideChar)^.WideCharData := AData;
{$ELSE USE_UNICODE}
  StackTop().CreateBuffer(AName,dtAnsiChar)^.AnsiCharData := AData;
{$ENDIF USE_UNICODE}
end;

procedure TSDOSerializationStreamBinary.PutCurrency(const AName: string; const AData: TSDOCurrency);
begin
  StackTop().CreateBuffer(AName,dtCurrency)^.CurrencyData := AData;
end;

procedure TSDOSerializationStreamBinary.PutDate(const AName: string; const AData: TSDODateTime);
var
  locBuffer : TByteDynArray;
begin
  SetLength(locBuffer,SizeOf(TSDODateTime));
  Move(AData,locBuffer[0],SizeOf(TSDODateTime));
  ReverseBytes(locBuffer[0],SizeOf(TSDODateTime));
  StackTop().CreateBuffer(AName,dtByteDynArray)^.ByteDynArrayData^.Data := locBuffer;
end;

procedure TSDOSerializationStreamBinary.PutDouble(const AName: string; const AData: TSDODouble);
begin
  StackTop().CreateBuffer(AName,dtDouble)^.DoubleData := AData;
end;

procedure TSDOSerializationStreamBinary.PutFloat(const AName: string; const AData: TSDOFloat);
begin
  StackTop().CreateBuffer(AName,dtSingle)^.SingleData := AData;
end;

procedure TSDOSerializationStreamBinary.PutInteger(const AName: string; const AData: TSDOInteger);
begin
  StackTop().CreateBuffer(AName,dtInt32S)^.Int32S := AData;
end;

procedure TSDOSerializationStreamBinary.PutLong(const AName: string; const AData: TSDOLong);
begin
  StackTop().CreateBuffer(AName,dtInt64S)^.Int64S := AData;
end;

procedure TSDOSerializationStreamBinary.PutShort(const AName: string; const AData: TSDOShort);
begin
  StackTop().CreateBuffer(AName,dtInt16S)^.Int16S := AData;
end;

procedure TSDOSerializationStreamBinary.PutString(const AName: string; const AData: TSDOString);
begin
{$IFDEF USE_UNICODE}
  StackTop().CreateBuffer(AName,dtUnicodeString)^.UnicodeStrData^.Data := AData;
{$ELSE}
  StackTop().CreateBuffer(AName,dtAnsiString)^.AnsiStrData^.Data := AData;
{$ENDIF}
end;

procedure TSDOSerializationStreamBinary.SaveToFile(const AFileName: string);
var
  locStream : TStream;
begin
  locStream := TFileStream.Create(AFileName,fmCreate);
  try
    SaveToStream(locStream);
  finally
    locStream.Free();
  end;
end;

procedure TSDOSerializationStreamBinary.SetNameStyle(const AValue: TNameStyle);
begin
  if ( AValue <> FNameStyle ) then
    FNameStyle := AValue;
end;

{ TArrayStackItem }

function TArrayStackItem.GetItemCount(): Integer;
begin
  Result := ScopeObject^.ArrayData^.Count;
end;

function TArrayStackItem.Find(var AName: TDataName): PDataBuffer;
begin
  If ( FIndex >= 0 ) And ( FIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := ScopeObject^.ArrayData^.Items^[FIndex]
  Else
    Raise ESDOSerializationException.CreateFmt(SERR_IndexOutOfBound,[FIndex]);
  Inc(FIndex);
end;

function TArrayStackItem.GetByIndex(const AIndex: Integer): PDataBuffer;
begin
  If ( AIndex >= 0 ) And ( AIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := ScopeObject^.ArrayData^.Items^[AIndex]
  Else
    Raise ESDOSerializationException.CreateFmt(SERR_IndexOutOfBound,[AIndex]);
end;

function TArrayStackItem.CreateBuffer(
  const AName     : String;
  const ADataType : TDataType
): PDataBuffer;
begin
  If ( FIndex >= 0 ) And ( FIndex < ScopeObject^.ArrayData^.Count ) Then
    Result := CreateObjBuffer(ADataType,AName,Nil)
  Else
    Raise ESDOSerializationException.CreateFmt(SERR_IndexOutOfBound,[FIndex]);
  ScopeObject^.ArrayData^.Items^[FIndex] := Result;
  Inc(FIndex);
end;

{$WARNINGS OFF}
function TArrayStackItem.CreateInnerBuffer(const ADataType: TDataType): PDataBuffer;
begin
  raise ESDOSerializationException.CreateFmt(SERR_UnsupportedOperation,['TArrayStackItem.CreateInnerBuffer']);
end;

function TArrayStackItem.GetInnerBuffer(): PDataBuffer;
begin
  raise ESDOSerializationException.CreateFmt(SERR_UnsupportedOperation,['TArrayStackItem.GetInnerBuffer']);
end;
{$WARNINGS ON}

procedure TArrayStackItem.NilCurrentScope();
begin
end;

function TArrayStackItem.IsCurrentScopeNil(): Boolean;
begin
  Result := False;
end;

function TArrayStackItem.GetScopeItemNames(const AReturnList: TStrings): Integer;
var
  locBuffer : PDataBufferList;
  i : PtrInt;
begin
  AReturnList.Clear();
  if Assigned(ScopeObject) and ( ScopeObject^.ArrayData^.Count > 0 ) then begin
    locBuffer := ScopeObject^.ArrayData^.Items;
    for i := 0 to Pred(ScopeObject^.ArrayData^.Count) do begin
      AReturnList.Add(locBuffer^[i]^.Name);
    end;
  end;
  Result := AReturnList.Count;
end;

procedure TArrayStackItem.CopyTo(const AClone: TStackItem);
begin
  inherited CopyTo(AClone);
  TArrayStackItem(AClone).FIndex := Self.FIndex;
end;

{ TStreamBinaryBookmark }

destructor TStreamBinaryBookmark.Destroy();
var
  i : PtrInt;
begin
  if ( FStack <> nil ) and ( FStack.Count > 0 ) then begin
    for i := 0 to Pred(FStack.Count) do
      FStack.Pop().Free();
  end;
  FreeAndNil(FStack);
  inherited;
end;


end.
