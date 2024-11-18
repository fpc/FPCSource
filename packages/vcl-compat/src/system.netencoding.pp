  {
    This file is part of the Free Pascal run time library.
    Copyright (c) 2019 by Michael Van Canneyt, member of the
    Free Pascal development team

    VCL compatible TNetEncoding unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$H+}

unit System.NetEncoding;

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, System.Types;
{$ELSE FPC_DOTTEDUNITS}
uses Sysutils, Classes, Types;
{$ENDIF FPC_DOTTEDUNITS}

type
  // Not used here
  EHTTPException = class(Exception);

  UnsafeChar = Byte;
  TUnsafeChars = set of UnsafeChar;
  TURLEncoding = Class;

  { TNetEncoding }

  TNetEncoding = class
  private
    type
      TStandardEncoding = (
        seBase64,
        seBase64String,
        seHTML,
        seURL);
    Class var
      FStdEncodings : Array[TStandardEncoding] of TNetEncoding;
    Class Function GetStdEncoding(aIndex : TStandardEncoding) : TNetEncoding; Static;
    Class Destructor Destroy;
    class function GetURLEncoding: TURLEncoding; static;
  protected
    // These must be implemented by descendents
    Function DoDecode(const aInput: RawByteString): RawByteString; overload; virtual; abstract;
    Function DoEncode(const aInput: RawByteString): RawByteString; overload; virtual; abstract;

    // These can be overridden by descendents for effiency
    Function DoDecode(const aInput: UnicodeString): UnicodeString; overload; virtual;
    Function DoEncode(const aInput: UnicodeString): UnicodeString; overload; virtual;

    Function DoDecode(const aInput, aOutput: TStream): Integer; overload; virtual;
    Function DoEncode(const aInput, aOutput: TStream): Integer; overload; virtual;

    Function DoDecode(const aInput: array of Byte): TBytes; overload; virtual;
    Function DoEncode(const aInput: array of Byte): TBytes; overload; virtual;

    Function DoDecodeStringToBytes(const aInput: RawByteString): TBytes; virtual; overload;
    Function DoDecodeStringToBytes(const aInput: UnicodeString): TBytes; virtual; overload;
    Function DoEncodeBytesToString(const aInput: array of Byte): UnicodeString; overload; virtual;
    Function DoEncodeBytesToString(const aInput: Pointer; Size: Integer): UnicodeString; overload; virtual;
  public
    Class Procedure FreeStdEncodings;
    // Public stubs, they call the Do* versions
    // Stream
    Function Decode(const aInput, aOutput: TStream): Integer; overload;
    Function Encode(const aInput, aOutput: TStream): Integer; overload;
    // TBytes
    Function Decode(const aInput: array of Byte): TBytes; overload;
    Function Encode(const aInput: array of Byte): TBytes; overload;
    // Strings
    Function Decode(const aInput: UnicodeString): UnicodeString; overload;
    Function Encode(const aInput: UnicodeString): UnicodeString; overload;
    Function Decode(const aInput: RawByteString): RawByteString; overload;
    Function Encode(const aInput: RawByteString): RawByteString; overload;
    // UnicodeString to Bytes
    Function DecodeStringToBytes(const aInput: UnicodeString): TBytes;
    Function DecodeStringToBytes(const aInput: RawByteString): TBytes;
    Function EncodeBytesToString(const aInput: array of Byte): UnicodeString; overload;
    Function EncodeBytesToString(const aInput: Pointer; Size: Integer): UnicodeString; overload;
    // Default instances
    class property Base64: TNetEncoding Index seBase64 read GetStdEncoding;
    class property Base64String: TNetEncoding Index seBase64String read GetStdEncoding;
    class property HTML: TNetEncoding Index seHTML read GetStdEncoding;
    class property URL: TURLEncoding read GetURLEncoding;
  end;

  { TCustomBase64Encoding }

  TCustomBase64Encoding = class(TNetEncoding)
  protected const
    kCharsPerLine = 76;
    kLineSeparator = #13#10;
  protected
    FCharsPerline: Integer;
    FLineSeparator: string;
    FPadEnd: Boolean;
  protected
    Function DoDecode(const aInput, aOutput: TStream): Integer; overload; override;
    Function DoEncode(const aInput, aOutput: TStream): Integer; overload; override;

    Function DoDecode(const aInput: RawByteString): RawByteString; overload; override;
    Function DoEncode(const aInput: RawByteString): RawByteString; overload; override;

    Function DoDecode(const aInput: array of Byte): TBytes; overload; override;
    Function DoEncode(const aInput: array of Byte): TBytes; overload; override;
  end;

  { TBase64Encoding }

  TBase64Encoding = class(TCustomBase64Encoding)
  public
    constructor Create; overload; virtual;
    constructor Create(CharsPerLine: Integer); overload; virtual;
    constructor Create(CharsPerLine: Integer; LineSeparator: string); overload; virtual;
  end;

  { TBase64StringEncoding }

  TBase64StringEncoding = class(TCustomBase64Encoding)
  public
    constructor Create; overload; virtual;
  end;

  { TURLEncoding }

  TURLEncoding = class(TNetEncoding)
  protected
    Function DoEncode(const aInput: RawBytestring): RawBytestring; overload; override;
    Function DoDecode(const aInput: RawBytestring): RawBytestring; overload; override;
  Public
    Type
      UnsafeChar = Byte;
      TUnsafeChars = set of UnsafeChar;
      TEncodeOption = (SpacesAsPlus, EncodePercent);
      TEncodeOptions = set of TEncodeOption;
      TDecodeOption = (PlusAsSpaces);
      TDecodeOptions = set of TDecodeOption;
  Public
    function Encode(const aInput: string; const aSet: TUnsafeChars; const aOptions: TEncodeOptions; aEncoding: TEncoding = nil): string; overload;
    function EncodeQuery(const aInput: string; const aExtraUnsafeChars: TUnsafeChars): string;
    function EncodePath(const aPath: string; const aExtraUnsafeChars: TUnsafeChars): string;
    class function URIDecode(const aValue: string; aPlusAsSpaces: Boolean): string;
  end;

  THTMLEncoding = class(TNetEncoding)
  protected
    Function DoDecode(const aInput: UnicodeString): UnicodeString; override;
    Function DoDecode(const aInput: RawBytestring): RawBytestring; overload; override;
    Function DoEncode(const aInput: UnicodeString): UnicodeString; override;
    Function DoEncode(const aInput: RawBytestring): RawBytestring; overload; override;
  end;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.Hash.Base64, FpWeb.Http.Protocol, Html.Defs, Xml.Read;
{$ELSE FPC_DOTTEDUNITS}
uses base64, httpprotocol, HTMLDefs, xmlread;
{$ENDIF FPC_DOTTEDUNITS}

Resourcestring
  sInvalidHTMLEntity = 'Invalid HTML encoded character: %s';

{ TCustomBase64Encoding }

function TCustomBase64Encoding.DoDecode(const aInput, aOutput: TStream): Integer;

Var
  S : TBase64DecodingStream;

begin
  S:=TBase64DecodingStream.Create(aInput,bdmMIME);
  try
    Result:=S.Size;
    aOutput.CopyFrom(S,Result);
  finally
    S.Free;
  end;
end;

function TCustomBase64Encoding.DoDecode(const aInput: array of Byte): TBytes;
var
  Instream  : TBytesStream;
  Outstream : TBytesStream;
  Decoder   : TBase64DecodingStream;
const
  cPad: AnsiChar = '=';
begin
  if Length(aInput)=0 then
    Exit(nil);
  Instream:=TBytesStream.Create;
  try
    Instream.WriteBuffer(aInput[0], Length(aInput));
    while Instream.Size mod 4 > 0 do
      Instream.WriteBuffer(cPad, 1);
    Instream.Position:=0;
    Outstream:=TBytesStream.Create;
    try
      Decoder:=TBase64DecodingStream.Create(Instream,bdmMIME);
      try
         Outstream.CopyFrom(Decoder,Decoder.Size);
         Result:=Outstream.Bytes;
         SetLength(Result,Outstream.Size);
      finally
        Decoder.Free;
      end;
    finally
      Outstream.Free;
    end;
  finally
    Instream.Free;
  end;
end;

function TCustomBase64Encoding.DoEncode(const aInput, aOutput: TStream): Integer;
Var
  S : TBase64EncodingStream;

begin
  S:=TBase64EncodingStream.Create(aOutput,FCharsPerline,FLineSeparator,FPadEnd);
  try
    Result:=S.CopyFrom(aInput,0);
  finally
    S.Free;
  end;
end;

function TCustomBase64Encoding.DoEncode(const aInput: array of Byte): TBytes;
var
  Outstream : TBytesStream;
  Encoder   : TBase64EncodingStream;
begin
  if Length(aInput)=0 then
    Exit(nil);
  Outstream:=TBytesStream.Create;
  try
    Encoder:=TBase64EncodingStream.create(outstream,FCharsPerline,FLineSeparator,FPadEnd);
    try
      Encoder.Write(aInput[0],Length(aInput));
    finally
      Encoder.Free;
    end;
    Result:=Outstream.Bytes;
    SetLength(Result,Outstream.Size);
  finally
    Outstream.free;
  end;
end;

function TCustomBase64Encoding.DoDecode(const aInput: RawByteString): RawByteString;
begin
  Result:=DecodeStringBase64(aInput,False);
end;

function TCustomBase64Encoding.DoEncode(const aInput: RawByteString): RawByteString;
var
  Outstream : TStringStream;
  Encoder   : TBase64EncodingStream;
begin
  if Length(aInput)=0 then
    Exit('');
  Outstream:=TStringStream.Create('');
  try
    Encoder:=TBase64EncodingStream.create(outstream,FCharsPerline,FLineSeparator,FPadEnd);
    try
      Encoder.Write(aInput[1],Length(aInput));
    finally
      Encoder.Free;
    end;
    Result:=Outstream.DataString;
  finally
    Outstream.free;
  end;
end;

{ TBase64Encoding }

constructor TBase64Encoding.Create(CharsPerLine: Integer);
begin
  Create(CharsPerLine, kLineSeparator);
end;

constructor TBase64Encoding.Create(CharsPerLine: Integer; LineSeparator: string);
begin
  inherited Create;
  FCharsPerline:=CharsPerLine;
  FLineSeparator:=LineSeparator;
  FPadEnd:=True;
end;

constructor TBase64Encoding.Create;
begin
  Create(kCharsPerLine, kLineSeparator);
end;

{ TBase64StringEncoding }

constructor TBase64StringEncoding.Create;
begin
  inherited Create;
  FCharsPerline:=0;
  FLineSeparator:='';
  FPadEnd:=True;
end;

{ ---------------------------------------------------------------------
  TNetEncoding
  ---------------------------------------------------------------------}

class procedure TNetEncoding.FreeStdEncodings;

Var
  I : TStandardEncoding;

begin
  For I in TStandardEncoding do
    FreeAndNil(FStdEncodings[i]);
end;

class destructor TNetEncoding.Destroy;
begin
  FreeStdEncodings;
end;

class function TNetEncoding.GetURLEncoding: TURLEncoding;
begin
  Result:=TURLEncoding(GetStdEncoding(seURL));
end;

class function TNetEncoding.GetStdEncoding(aIndex: TStandardEncoding): TNetEncoding;
begin
  Result:=FStdEncodings[aIndex];
  if Assigned(Result) then
  begin
{$ifdef FPC_HAS_FEATURE_THREADING}
    ReadDependencyBarrier; // Read Result contents (by caller) after Result pointer.
{$endif}
    Exit;
  end;

  case aIndex of
    seBase64: Result:=TBase64Encoding.Create;
    seBase64String: Result:=TBase64StringEncoding.Create;
    seHTML: Result:=THTMLEncoding.Create;
    seURL: Result:=TURLEncoding.Create;
  end;

{$ifdef FPC_HAS_FEATURE_THREADING}
  WriteBarrier; // Write FStdEncodings[aIndex] after Result contents.
  if InterlockedCompareExchange(Pointer(FStdEncodings[aIndex]), Pointer(Result), nil) <> nil then
  begin
    Result.Free;
    Result := FStdEncodings[aIndex];
  end;
{$else}
  FStdEncodings[aIndex] := Result;
{$endif}
end;

// Public API

function TNetEncoding.Encode(const aInput: array of Byte): TBytes;
begin
  Result:=DoEncode(aInput);
end;

function TNetEncoding.Encode(const aInput, aOutput: TStream): Integer;
begin
  Result:=DoEncode(aInput, aOutput);
end;

function TNetEncoding.Decode(const aInput: RawByteString): RawByteString;
begin
  Result:=DoDecode(aInput);
end;

function TNetEncoding.Encode(const aInput: RawByteString): RawByteString;

begin
  Result:=DoEncode(aInput);
end;

function TNetEncoding.Encode(const aInput: UnicodeString): UnicodeString;
begin
  Result:=DoEncode(aInput);
end;

function TNetEncoding.EncodeBytesToString(const aInput: array of Byte): UnicodeString;
begin
  Result:=DoEncodeBytesToString(aInput);
end;

function TNetEncoding.EncodeBytesToString(const aInput: Pointer; Size: Integer): UnicodeString;
begin
  Result:=DoEncodeBytesToString(aInput, Size);
end;

function TNetEncoding.Decode(const aInput, aOutput: TStream): Integer;
begin
  Result:=DoDecode(aInput,aOutput);
end;

function TNetEncoding.Decode(const aInput: UnicodeString): UnicodeString;
begin
  Result:=DoDecode(aInput);
end;

function TNetEncoding.DecodeStringToBytes(const aInput: UnicodeString): TBytes;
begin
  Result:=DoDecodeStringToBytes(aInput);
end;

function TNetEncoding.DecodeStringToBytes(const aInput: RawByteString): TBytes;
begin
  Result:=DoDecodeStringToBytes(aInput);
end;

function TNetEncoding.Decode(const aInput: array of Byte): TBytes;
begin
  Result:=DoDecode(aInput);
end;

// Protected

function TNetEncoding.DoDecode(const aInput: UnicodeString): UnicodeString;

Var
  U : UTF8String;

begin
  U:=UTF8Encode(aInput);
  Result:=UTF8Decode(DoDecode(U));
end;

function TNetEncoding.DoEncode(const aInput: UnicodeString): UnicodeString;

Var
  U : UTF8String;

begin
  U:=UTF8Encode(aInput);
  Result:=UTF8Decode(DoEncode(U));
end;

function TNetEncoding.DoDecode(const aInput: array of Byte): TBytes;

begin
  if Length(aInput)=0 then
    Result:=Default(TBytes)
  else
    Result:=TEncoding.UTF8.GetBytes(DoDecode(UTF8ToString(aInput)));
end;

function TNetEncoding.DoDecode(const aInput, aOutput: TStream): Integer;

var
  Src,Dest: TBytes;
  Len : Integer;

begin
  Result:=0;
  Len:=aInput.Size;
  if Len<>0 then
    begin
    Src:=Default(TBytes);
    SetLength(Src,Len);
    aInput.ReadBuffer(Src,Len);
    Dest:=DoDecode(Src);
    Result:=Length(Dest);
    aOutput.WriteBuffer(Dest,Result);
    end
end;

function TNetEncoding.DoDecodeStringToBytes(const aInput: UnicodeString): TBytes;

begin
  Result:=TEncoding.UTF8.GetBytes(DoDecode(aInput));
end;

function TNetEncoding.DoEncode(const aInput: array of Byte): TBytes;
begin
  if Length(aInput)=0 then
    Result:=Default(TBytes)
  else
    Result:=TEncoding.UTF8.GetBytes(DoEncode(UTF8ToString(aInput)))
end;

function TNetEncoding.DoDecodeStringToBytes(const aInput: RawByteString): TBytes;

Var
  U : RawByteString;

begin
  U:=AInput;
  UniqueString(U);
  SetCodePage(U,CP_UTF8,True);
  Result:=DoDecodeStringToBytes(UTF8Decode(U));
end;

function TNetEncoding.DoEncodeBytesToString(const aInput: array of Byte): UnicodeString;
begin
  Result:=TEncoding.UTF8.GetString(DoEncode(aInput));
end;


function TNetEncoding.DoEncodeBytesToString(const aInput: Pointer; Size: Integer): UnicodeString;

Var
  Src : TBytes;

begin
  Src:=Default(TBytes);
  SetLength(Src,Size);
  Move(aInput^,Src[0],Size);
  Result:=DoEncodeBytesToString(Src);
end;

function TNetEncoding.DoEncode(const aInput, aOutput: TStream): Integer;
var
  InBuf: array of Byte;
  OutBuf: TBytes;
begin
  if aInput.Size > 0 then
  begin
    SetLength(InBuf, aInput.Size);
    aInput.Read(InBuf[0], aInput.Size);
    OutBuf:=DoEncode(InBuf);
    Result:=Length(OutBuf);
    aOutput.Write(OutBuf, Result);
    SetLength(InBuf, 0);
  end
  else
    Result:=0;
end;

{ TBase64Encoding }


{ TURLEncoding }

function TURLEncoding.DoDecode(const aInput: RawBytestring): RawBytestring;

begin
  Result:=HTTPDecode(aInput);
end;

function TURLEncoding.Encode(const aInput: string; const aSet: TUnsafeChars; const aOptions: TEncodeOptions; aEncoding: TEncoding): string;


var
  S : TUnsafeChars;

begin
  S:=aSet;
  if (TEncodeOption.EncodePercent in aOptions) then
    S:=aSet+[Ord('%')];
  Result:=HttpEncode(aInput,S,TEncodeOption.SpacesAsPlus in aOptions);
end;

function TURLEncoding.DoEncode(const aInput: RawBytestring): RawBytestring;

begin
  Result:=HTTPEncode(aInput)
end;

function TURLEncoding.EncodeQuery(const aInput: string; const aExtraUnsafeChars: TUnsafeChars): string;

const
  QueryUnsafeChars: TUnsafeChars = [Ord('''')+Ord('%')];

var
  Unsafe: TUnsafeChars;

begin
  Unsafe:=QueryUnsafeChars+aExtraUnsafeChars;
  Result:=HTTPEncode(aInput,Unsafe,True);
end;

function TURLEncoding.EncodePath(const aPath: string; const aExtraUnsafeChars: TUnsafeChars): string;


var
  lPaths: TStringDynArray;
  I,Last: Integer;
  LUnsafeChars: TUnsafeChars;

begin
  if APath = '' then
    Exit('/');
  Result:='';
  lPaths:=APath.Split(['/'], TStringSplitOptions.ExcludeEmpty);
  Last:=Length(lPaths)-1;
  for I:=0 to Last do
    Result:=Result+'/'+HTTPEncode(LPaths[I],aExtraUnsafeChars,True);
end;

class function TURLEncoding.URIDecode(const aValue: string; aPlusAsSpaces: Boolean): string;
begin
  Result:=HTTPDecode(aValue,aPlusAsSpaces);
end;


{ THTMLEncoding }

Function THTMLEncoding.DoEncode(const aInput: UnicodeString): UnicodeString;

Var
  S : UTF8String;

begin
  S:=UTF8Encode(aInput);
  Result:=UTF8Decode(DoEncode(S));
end;

Function THTMLEncoding.DoEncode(const aInput: RawByteString): RawByteString;

var
  Src, Curr, OrigDest,Dest : PAnsiChar;

  Procedure CopyData(S : String);

  Var
    len : integer;

  begin
    Len:=(Curr-Src);
    if Len>0 then
      Move(Src^,Dest^,Len);
    Src:=Curr;
    Inc(Src);
    inc(Dest,Len);
    Len:=Length(S);
    if Len>0 then
      Move(S[1],Dest^,Len);
    inc(Dest,Len);
  end;

begin
  SetLength(Result,Length(aInput)*6);
  if Length(aInput)=0 then exit;
  Src:=PAnsiChar(aInput);
  Curr:=Src;
  OrigDest:=PAnsiChar(Result);
  Dest:=OrigDest;
  // Convert: &, <, >, "
  while Curr^<>#0 do
    begin
    case Curr^ of
      '&': CopyData('&amp;');
      '<': CopyData('&lt;');
      '>': CopyData('&gt;');
      '"': CopyData('&quot;');
    end;
    Inc(Curr);
    end;
  CopyData('');
  SetLength(Result,Dest-OrigDest);
end;

Function THTMLEncoding.DoDecode(const aInput: RawByteString): RawByteString;

Var
  S : RawByteString;


begin
  S:=aInput;
  UniqueString(S);
  SetCodePage(S,CP_UTF8,true);
  Result:=UTF8Encode(DoDecode(UTF8Decode(S)));
end;

Function THTMLEncoding.DoDecode(const aInput: UnicodeString): UnicodeString;

var
  Src, Curr, Dest : PWideChar;

  Procedure CopyData(S : UnicodeString);

  Var
    len : integer;

  begin
    Len:=(Curr-Src);
    if Len>0 then
      begin
      Move(Src^,Dest^,Len*Sizeof(UnicodeChar));
      inc(Dest,Len);
      end;
    Len:=Length(S);
    if Len>0 then
      begin
      Move(S[1],Dest^,Len*Sizeof(UnicodeChar));
      inc(Dest,Len);
      end;
  end;

Var
  Len : Integer;
  U : UnicodeChar;
  US : Unicodestring;
  Ent,OrigDest : PWideChar;

begin
  SetLength(Result, Length(aInput));
  if Length(Result)=0 then exit;
  Src:=PWideChar(aInput);
  OrigDest:=PWideChar(Result);
  Dest:=OrigDest;
  Curr:=Src;
  while Curr^ <> #0 do
    begin
    If Curr^='&' then
      begin
      CopyData('');
      Src:=Curr;
      Ent:=Curr;
      While Not (Ent^ in [#0,';']) do
        Inc(Ent);
      Len:=Ent-Curr-1;
      SetLength(US,Len);
      if Len>0 then
        Move(Curr[1],US[1],Len*SizeOf(UnicodeChar));
      if not ResolveHTMLEntityReference(US,U) then
        raise EConvertError.CreateFmt(sInvalidHTMLEntity,[US]);
      CopyData(U);
      Curr:=Ent;
      Src:=Curr;
      Inc(Src);
      end;
    Inc(Curr);
    end;
  CopyData('');
  SetLength(Result,Dest-OrigDest);
end;

end.

