{
    This file is part of the Free Component Library.

    Copyright (c) 2015 by:

      . Michael Van Canneyt michael@freepascal.org
      . Silvio Clecio github.com/silvioprog

    Text reader classes.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$mode objfpc}
{$h+}
unit streamex;

Interface

uses
  Classes, SysUtils, RtlConsts;

const
  MIN_BUFFER_SIZE = 128;
  BUFFER_SIZE = 4096;
  FILE_RIGHTS = 438;

type

   { TBidirBinaryObjectReader }

   TBidirBinaryObjectReader = class(TBinaryObjectReader)
   protected
      function GetPosition: Longint;
      procedure SetPosition(const AValue: Longint);
   public
      property Position: Longint read GetPosition write SetPosition;
   end;
   
   { TBidirBinaryObjectWriter }

   TBidirBinaryObjectWriter = class(TBinaryObjectWriter)
   protected
      function GetPosition: Longint;
      procedure SetPosition(const AValue: Longint);
   public
      property Position: Longint read GetPosition write SetPosition;
   end;
  
   { TDelphiReader }

   TDelphiReader = class(TReader)
   protected
      function GetPosition: LongInt;
      procedure SetPosition(const AValue: LongInt);
      function CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectReader; override;
   public
      function GetDriver: TBidirBinaryObjectReader;
      function ReadStr: string;
      procedure Read(var Buf; Count: LongInt); override;
      property Position: LongInt read GetPosition write SetPosition;
   end;

   { TDelphiWriter }

   TDelphiWriter = class(TWriter)
   protected
      function GetPosition: Longint;
      procedure SetPosition(const AValue: LongInt);
      function CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectWriter; override;
   public
      function GetDriver: TBidirBinaryObjectWriter;
      procedure FlushBuffer;
      procedure Write(const Buf; Count: LongInt); override;
      procedure WriteStr(const Value: string);
      procedure WriteValue(Value: TValueType);
      property Position: LongInt read GetPosition write SetPosition;
   end;

   { TTextReader }

   TTextReader = class(TObject)
   Protected
     function IsEof: Boolean; virtual; abstract;
   public
     constructor Create; virtual;
     procedure Reset; virtual; abstract;
     procedure Close; virtual; abstract;
     procedure ReadLine(out AString: string); virtual; abstract; overload;
     function ReadLine: string; overload;
     property Eof: Boolean read IsEof;
   end;

   { TStreamReader }

   TStreamReader = class(TTextReader)
   private
     FBufferRead: Integer;
     FBufferPosition: Integer;
     FClosed,
     FOwnsStream: Boolean;
     FStream: TStream;
     FBuffer: array of Byte;
     procedure FillBuffer;
   Protected  
     function IsEof: Boolean; override;
   public
     constructor Create(AStream: TStream; ABufferSize: Integer;
       AOwnsStream: Boolean); virtual;
     constructor Create(AStream: TStream); virtual;
     destructor Destroy; override;
     procedure Reset; override;
     procedure Close; override;
     procedure ReadLine(out AString: string); override; overload;
     property BaseStream: TStream read FStream;
     property OwnsStream: Boolean read FOwnsStream write FOwnsStream;
   end;

   { TStringReader }

   TStringReader = class(TTextReader)
   private
     FReader: TTextReader;
   Protected  
     function IsEof: Boolean; override;
   public
     constructor Create(const AString: string; ABufferSize: Integer); virtual;
     constructor Create(const AString: string); virtual;
     destructor Destroy; override;
     procedure Reset; override;
     procedure Close; override;
     procedure ReadLine(out AString: string); override; overload;
   end;

   { TFileReader }

   TFileReader = class(TTextReader)
   private
     FReader: TTextReader;
   Protected
     function IsEof: Boolean; override;
   public
     constructor Create(const AFileName: TFileName; AMode: Word;
       ARights: Cardinal; ABufferSize: Integer); virtual;
     constructor Create(const AFileName: TFileName; AMode: Word;
       ABufferSize: Integer); virtual;
     constructor Create(const AFileName: TFileName; ABufferSize: Integer); virtual;
     constructor Create(const AFileName: TFileName); virtual;
     destructor Destroy; override;
     procedure Reset; override;
     procedure Close; override;
     procedure ReadLine(out AString: string); override; overload;
   end;

   { TTextWriter }

   TTextWriter = class
   public
     procedure Close; virtual; abstract;
     procedure Flush; virtual; abstract;
     procedure Write(aValue: Boolean); overload; virtual; abstract;
     procedure Write(aValue: Char); overload; virtual; abstract;
     procedure Write(aValue: Char; aCount: Integer); overload; virtual;
     procedure Write(const aValue: TCharArray); overload; virtual; abstract;
     procedure Write(aValue: Double); overload; virtual; abstract;
     procedure Write(aValue: Integer); overload; virtual; abstract;
     procedure Write(aValue: Int64); overload; virtual; abstract;
     procedure Write(aValue: TObject); overload; virtual; abstract;
     procedure Write(aValue: Single); overload; virtual; abstract;
     procedure Write(const aValue: string); overload; virtual; abstract;
     procedure Write(aValue: Cardinal); overload; virtual; abstract;
     procedure Write(aValue: UInt64); overload; virtual; abstract;
     procedure Write(const Fmt: string; aArgs: array of const); overload; virtual; abstract;
     procedure Write(const aValue: TCharArray; aIndex, aCount: Integer); overload; virtual; abstract;
     procedure WriteLine; overload; virtual; abstract;
     procedure WriteLine(aValue: Boolean); overload; virtual; abstract;
     procedure WriteLine(aValue: Char); overload; virtual; abstract;
     procedure WriteLine(const aValue: TCharArray); overload; virtual; abstract;
     procedure WriteLine(aValue: Double); overload; virtual; abstract;
     procedure WriteLine(aValue: Integer); overload; virtual; abstract;
     procedure WriteLine(aValue: Int64); overload; virtual; abstract;
     procedure WriteLine(aValue: TObject); overload; virtual; abstract;
     procedure WriteLine(aValue: Single); overload; virtual; abstract;
     procedure WriteLine(const aValue: string); overload; virtual; abstract;
     procedure WriteLine(aValue: Cardinal); overload; virtual; abstract;
     procedure WriteLine(aValue: UInt64); overload; virtual; abstract;
     procedure WriteLine(const Format: string; Args: array of const); overload; virtual; abstract;
     procedure WriteLine(const aValue: TCharArray; Index, Count: Integer); overload; virtual; abstract;
   end;

   { TStringWriter }

   TStringWriter = class(TTextWriter)
   private
     FBuilder: TStringBuilder;
     FFreeBuilder: Boolean;
   public
     constructor Create; overload;
     constructor Create(aBuilder: TStringBuilder); overload;
     destructor Destroy; override;
     procedure Close; override;
     procedure Flush; override;
     procedure Write(aValue: Boolean); override;
     procedure Write(aValue: Char); override;
     procedure Write(aValue: Char; aCount: Integer); override;
     procedure Write(const aValue: TCharArray); override;
     procedure Write(aValue: Double); override;
     procedure Write(aValue: Integer); override;
     procedure Write(aValue: Int64); override;
     procedure Write(aValue: TObject); override;
     procedure Write(aValue: Single); override;
     procedure Write(const aValue: string); override;
     procedure Write(aValue: Cardinal); override;
     procedure Write(aValue: QWord); override;
     procedure Write(const aFmt: string; aArgs: array of const); override;
     procedure Write(const aValue: TCharArray; aIndex, aCount: Integer); override;
     procedure WriteLine; override;
     procedure WriteLine(aValue: Boolean); override;
     procedure WriteLine(aValue: Char); override;
     procedure WriteLine(const aValue: TCharArray); override;
     procedure WriteLine(aValue: Double); override;
     procedure WriteLine(aValue: Integer); override;
     procedure WriteLine(aValue: Int64); override;
     procedure WriteLine(aValue: TObject); override;
     procedure WriteLine(aValue: Single); override;
     procedure WriteLine(const aValue: string); override;
     procedure WriteLine(aValue: Cardinal); override;
     procedure WriteLine(aValue: UInt64); override;
     procedure WriteLine(const aFmt: string; aArgs: array of const); override;
     procedure WriteLine(const aValue: TCharArray; aIndex, aCount: Integer); override;
     function ToString: string; override;
   end;

   { TStreamWriter }

   TStreamWriter = class(TTextWriter)
   private
     FStream: TStream;
     FFreeStream: Boolean;
     FEncoding: TEncoding;
     FNewLine: string;
     FAutoFlush: Boolean;
   protected
     FBufferIndex: Integer;
     FBuffer: TBytes;
     procedure WriteBytes(Bytes: TBytes);
   public
     constructor Create(aStream: TStream); overload;
     constructor Create(aStream: TStream; aEncoding: TEncoding; aBufferSize: Integer = 4096); overload;
     constructor Create(const aFilename: string; aAppend: Boolean = False); overload;
     constructor Create(const aFilename: string; aAppend: Boolean; aEncoding: TEncoding; aBufferSize: Integer = 4096); overload;
     destructor Destroy; override;
     procedure Close; override;
     procedure Flush; override;
     procedure OwnStream; inline;
     procedure Write(aValue: Boolean); override;
     procedure Write(aValue: Char); override;
     procedure Write(const aValue: TCharArray); override;
     procedure Write(aValue: Double); override;
     procedure Write(aValue: Integer); override;
     procedure Write(aValue: Int64); override;
     procedure Write(aValue: TObject); override;
     procedure Write(aValue: Single); override;
     procedure Write(const aValue: string); override;
     procedure Write(aValue: Cardinal); override;
     procedure Write(aValue: UInt64); override;
     procedure Write(const Fmt: string; aArgs: array of const); override;
     procedure Write(const aValue: TCharArray; aIndex, aCount: Integer); override;
     procedure WriteLine; override;
     procedure WriteLine(aValue: Boolean); override;
     procedure WriteLine(aValue: Char); override;
     procedure WriteLine(const aValue: TCharArray); override;
     procedure WriteLine(aValue: Double); override;
     procedure WriteLine(aValue: Integer); override;
     procedure WriteLine(aValue: Int64); override;
     procedure WriteLine(aValue: TObject); override;
     procedure WriteLine(aValue: Single); override;
     procedure WriteLine(const aValue: string); override;
     procedure WriteLine(aValue: Cardinal); override;
     procedure WriteLine(aValue: UInt64); override;
     procedure WriteLine(const Fmt: string; Args: array of const); override;
     procedure WriteLine(const aValue: TCharArray; aIndex, aCount: Integer); override;
     property AutoFlush: Boolean read FAutoFlush write FAutoFlush;
     property NewLine: string read FNewLine write FNewLine;
     property Encoding: TEncoding read FEncoding;
     property BaseStream: TStream read FStream;
   end;



  { allows you to represent just a small window of a bigger stream as a substream. 
    also makes sure one is actually at the correct position before clobbering stuff. }

  TWindowedStream = class(TOwnerStream)
  private
    fStart : Int64; // in the source.
    fFrontier : Int64; // in the source.
    fStartingPositionHere : Int64; // position in this Stream corresponding to Position = fStart in the source.
    fPositionHere : Int64; // position in this Stream.
  protected
     //function  GetPosition() : Int64; override; = Seek(0, soCurrent) already.
     function  GetSize() : Int64; override;
     procedure SetSize(const NewSize: Int64); override; overload;
  public
    constructor Create(aStream : TStream; const aSize : Int64; const aPositionHere : Int64 = 0);
    destructor Destroy(); override;
    function Read(var aBuffer; aCount : longint) : longint; override;
    function Write(const aBuffer; aCount : Longint): Longint; override;
    function Seek(const aOffset: Int64; aOrigin: TSeekorigin): Int64; override;
  end;


  TStreamHelper = class helper for TStream
    function  ReadWordLE :word;
    function  ReadDWordLE:dword;
    function  ReadQWordLE:qword;
    procedure WriteWordLE (w:word);
    procedure WriteDWordLE(dw:dword);
    procedure WriteQWordLE(dq:qword);
    function  ReadWordBE :word;
    function  ReadDWordBE:dword;
    function  ReadQWordBE:qword;
    procedure WriteWordBE (w:word);
    procedure WriteDWordBE(dw:dword);
    procedure WriteQWordBE(dq:qword);
    function  ReadSingle:Single;
    function  ReadDouble:Double;
    procedure WriteSingle(s:Single);
    procedure WriteDouble(d:double);
    {$ifndef FPC}
    function ReadByte  : Byte;
    function ReadWord  : Word;
    function ReadDWord : DWord;
    function ReadQWord : QWord;
    procedure WriteByte  (b : Byte);
    procedure WriteWord  (b : word);
    procedure WriteDWord (b : DWord);
    procedure WriteQWord (b : QWord);
    {$endif}
  end;

Implementation

ResourceString
  SErrCannotWriteOutsideWindow = 'Cannot write outside allocated window.';
  SErrInvalidSeekWindow = 'Cannot seek outside allocated window.';
  SErrInvalidSeekOrigin = 'Invalid seek origin.';
  SErrCannotChangeWindowSize  = 'Cannot change the size of a windowed stream';

{ TTextWriter }

procedure TTextWriter.Write(aValue: Char; aCount: Integer);
begin
  Write(StringOfChar(aValue,aCount));
end;

{ TStreamWriter }

procedure TStreamWriter.WriteBytes(Bytes: TBytes);
var
  ByteLen,Count,WritePos,ToWrite: Integer;
  P : PByte;
begin
  ByteLen:=Length(Bytes);
  ToWrite:=ByteLen;
  WritePos:=0;
  P:=PByte(Bytes);
  while ToWrite>0 do
    begin
    Count:=ToWrite;
    if Count>ByteLen-WritePos then
      Count:=ByteLen-WritePos;
    Move(P^, FBuffer[FBufferIndex], Count);
    Inc(WritePos,Count);
    Inc(P,Count);
    Dec(ToWrite,Count);
    if FBufferIndex >= Length(FBuffer) then
      Flush;
    end;
  if FAutoFlush then
    Flush;
end;

constructor TStreamWriter.Create(aStream: TStream);
begin
  Create(aStream,TEncoding.UTF8,1024)
end;

constructor TStreamWriter.Create(aStream: TStream; aEncoding: TEncoding;
  aBufferSize: Integer);
begin
  FStream:=aStream;
  FFreeStream:=False;
  FEncoding:=aEncoding;
  SetLength(FBuffer,aBufferSize);
  FNewLine:=sLineBreak;
end;

constructor TStreamWriter.Create(const aFilename: string; aAppend: Boolean);

begin
  Create(aFileName,aAppend,TEncoding.UTF8,1024);
end;

constructor TStreamWriter.Create(const aFilename: string; aAppend: Boolean;
  aEncoding: TEncoding; aBufferSize: Integer);

var
  F : TStream;
begin
  if (aAppend and FileExists(aFilename)) then
    begin
    F := TFileStream.Create(aFilename, fmOpenWrite);
    F.Seek(0, soEnd);
    end
  else
    F := TFileStream.Create(aFilename, fmCreate);
  Create(F,aEncoding,aBufferSize);
  OwnStream;
end;

destructor TStreamWriter.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TStreamWriter.Close;
begin
  Flush;
  if FFreeStream then
    FreeAndNil(FStream);
end;

procedure TStreamWriter.Flush;

var
  aCount: Integer;
begin
  if (FStream=Nil) or (FBufferIndex=0) then
    exit;
  aCount:=FBufferIndex;
  FBufferIndex:=0;
  FStream.WriteBuffer(FBuffer,aCount);
end;

procedure TStreamWriter.OwnStream;
begin
  FFreeStream:=True;
end;

procedure TStreamWriter.Write(aValue: Boolean);
begin
  Write(BoolToStr(aValue,True));
end;

procedure TStreamWriter.Write(aValue: Char);
begin
  Write(String(aValue));
end;

procedure TStreamWriter.Write(const aValue: TCharArray);
begin
  Write(aValue,0,Length(aValue));
end;

procedure TStreamWriter.Write(aValue: Double);
begin
  Write(FloatToStr(aValue));
end;

procedure TStreamWriter.Write(aValue: Integer);
begin
  Write(IntToStr(aValue));
end;

procedure TStreamWriter.Write(aValue: Int64);
begin
  Write(IntToStr(aValue));
end;

procedure TStreamWriter.Write(aValue: TObject);
begin
  Write(aValue.ToString);
end;

procedure TStreamWriter.Write(aValue: Single);
begin
  Write(FloatToStr(aValue));
end;

procedure TStreamWriter.Write(const aValue: string);
begin
  WriteBytes(FEncoding.GetAnsiBytes(aValue));
end;

procedure TStreamWriter.Write(aValue: Cardinal);
begin
  Write(IntToStr(aValue));
end;

procedure TStreamWriter.Write(aValue: UInt64);
begin
  Write(IntToStr(aValue));
end;

procedure TStreamWriter.Write(const Fmt: string; aArgs: array of const);
begin
  Write(Format(Fmt,aArgs));
end;

procedure TStreamWriter.Write(const aValue: TCharArray; aIndex, aCount: Integer);
var
  S : String;
begin
  if aCount=0 then exit;
  SetLength(S,aCount);
  Move(aValue[aIndex],PChar(S)^,aCount*SizeOf(Char));
  WriteBytes(FEncoding.GetAnsiBytes(S));
end;

procedure TStreamWriter.WriteLine;
begin
  Write(NewLine);
end;

procedure TStreamWriter.WriteLine(aValue: Boolean);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(aValue: Char);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(const aValue: TCharArray);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(aValue: Double);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(aValue: Integer);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(aValue: Int64);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(aValue: TObject);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(aValue: Single);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(const aValue: string);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(aValue: Cardinal);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(aValue: UInt64);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(const Fmt: string; Args: array of const);
begin
  Write(Fmt,Args);
  WriteLine;
end;

procedure TStreamWriter.WriteLine(const aValue: TCharArray; aIndex, aCount: Integer
  );
begin
  Write(aValue,aIndex,aCount);
  WriteLine;
end;

{ TStringWriter }

constructor TStringWriter.Create;
begin
  FBuilder := TStringBuilder.Create;
  FFreeBuilder := True;
end;

constructor TStringWriter.Create(aBuilder: TStringBuilder);
begin
  FBuilder := TStringBuilder.Create;
  FFreeBuilder := False;
end;

destructor TStringWriter.Destroy;
begin
  if FFreeBuilder then
    FreeAndNil(FBuilder);
  inherited Destroy;
end;

procedure TStringWriter.Close;
begin
  // nothing to do
end;

procedure TStringWriter.Flush;
begin
  // Nothing to do
end;

procedure TStringWriter.Write(aValue: Boolean);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(aValue: Char);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(aValue: Char; aCount: Integer);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(const aValue: TCharArray);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(aValue: Double);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(aValue: Integer);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(aValue: Int64);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(aValue: TObject);
begin
  FBuilder.Append(aValue.ToString);
end;

procedure TStringWriter.Write(aValue: Single);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(const aValue: string);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(aValue: Cardinal);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(aValue: QWord);
begin
  FBuilder.Append(aValue);
end;

procedure TStringWriter.Write(const aFmt: string; aArgs: array of const);
begin
  FBuilder.Append(aFmt,aArgs);
end;

procedure TStringWriter.Write(const aValue: TCharArray; aIndex, aCount: Integer
  );
begin
  FBuilder.Append(aValue,aIndex,aCount);
end;

procedure TStringWriter.WriteLine;
begin
   FBuilder.AppendLine;
end;

procedure TStringWriter.WriteLine(aValue: Boolean);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(aValue: Char);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(const aValue: TCharArray);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(aValue: Double);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(aValue: Integer);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(aValue: Int64);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(aValue: TObject);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(aValue: Single);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(const aValue: string);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(aValue: Cardinal);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(aValue: UInt64);
begin
  Write(aValue);
  WriteLine;
end;

procedure TStringWriter.WriteLine(const aFmt: string; aArgs: array of const);
begin
  Write(aFmt,aArgs);
  WriteLine;
end;

procedure TStringWriter.WriteLine(const aValue: TCharArray; aIndex,
  aCount: Integer);
begin
  Write(aValue,aIndex,aCount);
  WriteLine;
end;

function TStringWriter.ToString: string;
begin
  Result:=FBuilder.ToString;
end;


{ TBidirBinaryObjectReader }

function TBidirBinaryObjectReader.GetPosition: Longint;
begin
   Result := FStream.Position - (FBufEnd - FBufPos);
end;

procedure TBidirBinaryObjectReader.SetPosition(const AValue: Longint);
begin
   FStream.Position := AValue;
   FBufPos := 0;
   FBufEnd := 0;
end;

{ TBidirBinaryObjectWriter }

function TBidirBinaryObjectWriter.GetPosition: Longint;
begin
   Result := FStream.Position - (FBufEnd - FBufPos);
end;

procedure TBidirBinaryObjectWriter.SetPosition(const AValue: Longint);
begin
   FStream.Position := AValue;
   FBufPos := 0;
   FBufEnd := 0;
end;



{ TDelphiReader }

function TDelphiReader.GetDriver: TBidirBinaryObjectReader;
begin
   Result := (Driver as TBidirBinaryObjectReader);
end;

function TDelphiReader.GetPosition: LongInt;
begin
   Result := GetDriver.Position;
end;

procedure TDelphiReader.SetPosition(const AValue: LongInt);
begin
   GetDriver.Position := AValue;
end;

function TDelphiReader.CreateDriver(Stream: TStream; BufSize:
Integer): TAbstractObjectReader;
begin
   Result := TBidirBinaryObjectReader.Create(Stream, BufSize);
end;


function TDelphiReader.ReadStr: string;
begin
   Result := GetDriver.ReadStr;
end;

procedure TDelphiReader.Read(var Buf; Count: LongInt);
begin
   GetDriver.Read(Buf, Count);
end;

{ TDelphiWriter }

function TDelphiWriter.GetDriver: TBidirBinaryObjectWriter;
begin
   Result := (Driver as TBidirBinaryObjectWriter);
end;

function TDelphiWriter.GetPosition: LongInt;
begin
   Result := GetDriver.Position;
end;

procedure TDelphiWriter.SetPosition(const AValue: LongInt);
begin
   GetDriver.Position := AValue;
end;

function TDelphiWriter.CreateDriver(Stream: TStream; BufSize: Integer): TAbstractObjectWriter;
begin
   Result := TBidirBinaryObjectWriter.Create(Stream, BufSize);
end;

procedure TDelphiWriter.FlushBuffer;
begin
   GetDriver.FlushBuffer();
end;

procedure TDelphiWriter.Write(const Buf; Count: Longint);
begin
   GetDriver.Write(Buf, Count);
end;

procedure TDelphiWriter.WriteStr(const Value: string);
begin
   GetDriver.WriteStr(Value);
end;

procedure TDelphiWriter.WriteValue(Value: TValueType);
begin
   GetDriver.WriteValue(Value);
end;

{ TTextReader }

constructor TTextReader.Create;
begin
  inherited Create;
end;

function TTextReader.ReadLine: string;

begin
  ReadLine(Result);
end;

{ TStreamReader }

constructor TStreamReader.Create(AStream: TStream; ABufferSize: Integer;
  AOwnsStream: Boolean);
begin
  inherited Create;
  if not Assigned(AStream) then
    raise EArgumentException.CreateFmt(SParamIsNil, ['AStream']);
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  FClosed:=False;
  if ABufferSize >= MIN_BUFFER_SIZE then
    SetLength(FBuffer, ABufferSize)
  else
    SetLength(FBuffer, MIN_BUFFER_SIZE);
end;

constructor TStreamReader.Create(AStream: TStream);
begin
  Create(AStream, BUFFER_SIZE, False);
end;

destructor TStreamReader.Destroy;
begin
  Close;
  inherited Destroy;
end;

procedure TStreamReader.FillBuffer;
begin
  if FClosed then 
    begin
    FBufferRead:=0;
    FBufferPosition:=0;
    end
  else  
    begin
    FBufferRead := FStream.Read(FBuffer[0], Pred(Length(FBuffer)));
    FBuffer[FBufferRead] := 0;
    FBufferPosition := 0;
    end;
end;

procedure TStreamReader.Reset;
begin
  FBufferRead := 0;
  FBufferPosition := 0;
  if Assigned(FStream) then
    FStream.Seek(0, 0);
end;

procedure TStreamReader.Close;
begin
  if FOwnsStream then
    FreeAndNil(FStream);
  FClosed:=True;
end;

function TStreamReader.IsEof: Boolean;
begin
  if FClosed or not Assigned(FStream) then
    Exit(True);
  Result := FBufferPosition >= FBufferRead;
  if Result then
  begin
    FillBuffer;
    Result := FBufferRead = 0;
  end;
end;

procedure TStreamReader.ReadLine(out AString: string);
var
  VPByte: PByte;
  VPosition, VStrLength, VLength: Integer;
begin
  VPosition := FBufferPosition;
  SetLength(AString, 0);
  if FClosed then exit;
  repeat
    VPByte := @FBuffer[FBufferPosition];
    while (FBufferPosition < FBufferRead) and not (VPByte^ in [10, 13]) do
    begin
      Inc(VPByte);
      Inc(FBufferPosition);
    end;
    if FBufferPosition = FBufferRead then
    begin
      VLength := FBufferPosition - VPosition;
      if VLength > 0 then
      begin
        VStrLength := Length(AString);
        SetLength(AString, VStrLength + VLength);
        Move(FBuffer[VPosition], AString[Succ(VStrLength)], VLength);
      end;
      FillBuffer;
      VPByte := @FBuffer[FBufferPosition];
      VPosition := FBufferPosition;
    end;
  until (FBufferPosition = FBufferRead) or (VPByte^ in [10, 13]);
  VLength := FBufferPosition - VPosition;
  if VLength > 0 then
  begin
    VStrLength := Length(AString);
    SetLength(AString, VStrLength + VLength);
    Move(FBuffer[VPosition], AString[Succ(VStrLength)], VLength);
  end;
  if (VPByte^ in [10, 13]) and (FBufferPosition < FBufferRead) then
  begin
    Inc(FBufferPosition);
    if VPByte^ = 13 then
    begin
      if FBufferPosition = FBufferRead then
        FillBuffer;
      if (FBufferPosition < FBufferRead) and (FBuffer[FBufferPosition] = 10) then
        Inc(FBufferPosition);
    end;
  end;
end;


{ TStringReader }

constructor TStringReader.Create(const AString: string; ABufferSize: Integer);
begin
  inherited Create;
  FReader := TStreamReader.Create(TStringStream.Create(AString), ABufferSize, True);
end;

constructor TStringReader.Create(const AString: string);
begin
  Create(AString, BUFFER_SIZE);
end;

destructor TStringReader.Destroy;
begin
  FReader.Free;
  inherited Destroy;
end;

procedure TStringReader.Reset;
begin
  FReader.Reset;
end;

procedure TStringReader.Close;
begin
  FReader.Close;
end;

function TStringReader.IsEof: Boolean;
begin
  Result := FReader.IsEof;
end;

procedure TStringReader.ReadLine(out AString: string);
begin
  FReader.ReadLine(AString);
end;

{ TFileReader }

constructor TFileReader.Create(const AFileName: TFileName; AMode: Word;
  ARights: Cardinal; ABufferSize: Integer);
begin
  inherited Create;
  FReader := TStreamReader.Create(TFileStream.Create(AFileName, AMode, ARights),
    ABufferSize, True);
end;

constructor TFileReader.Create(const AFileName: TFileName; AMode: Word;
  ABufferSize: Integer);
begin
  Create(AFileName, AMode, FILE_RIGHTS, ABufferSize);
end;

constructor TFileReader.Create(const AFileName: TFileName; ABufferSize: Integer);
begin
  Create(AFileName, fmOpenRead or fmShareDenyWrite, ABufferSize);
end;

constructor TFileReader.Create(const AFileName: TFileName);
begin
  Create(AFileName, BUFFER_SIZE);
end;

destructor TFileReader.Destroy;
begin
  FReader.Free;
  inherited Destroy;
end;

procedure TFileReader.Reset;
begin
  FReader.Reset;
end;

procedure TFileReader.Close;
begin
  FReader.Close;
end;

function TFileReader.IsEof: Boolean;
begin
  Result := FReader.IsEof;
end;

procedure TFileReader.ReadLine(out AString: string);
begin
  FReader.ReadLine(AString);
end;

{ TStreamHelper }

function TStreamHelper.readwordLE:word;
begin
  result:=LEtoN(readword);
end;

function TStreamHelper.readdwordLE:dword;
begin
  result:=LEtoN(readdword);
end;

function TStreamHelper.readqwordLE:qword;
begin
  result:=LEtoN(readqword);
end;

function TStreamHelper.readwordBE:word;
begin
  result:=BEtoN(readword);
end;

function TStreamHelper.readdwordBE:dword;
begin
  result:=BEtoN(readdword);
end;

function TStreamHelper.readqwordBE:qword;
begin
  result:=BEtoN(readqword);
end;

procedure TStreamHelper.WriteWordBE(w:word);
begin
  WriteWord(NtoBE(w));
end;

procedure TStreamHelper.WriteDWordBE(dw:dword);
begin
  WriteDWord(NtoBE(dw));
end;

procedure TStreamHelper.WriteQWordBE(dq:qword);
begin
  WriteQWord(NtoBE(dq));
end;

procedure TStreamHelper.WriteWordLE(w:word);
begin
  WriteWord(NtoLE(w));
end;

procedure TStreamHelper.WriteDWordLE(dw:dword);
begin
  WriteDWord(NtoLE(dw));
end;

procedure TStreamHelper.WriteQWordLE(dq:qword);
begin
  WriteQWord(NtoLE(dq));
end;

function  TStreamHelper.ReadSingle:Single;
begin
  self.Read(result,sizeof(result));
end;
function  TStreamHelper.ReadDouble:Double;
begin
  self.Read(result,sizeof(result));
end;
procedure TStreamHelper.WriteSingle(s:Single);
begin
  self.Write(s,sizeof(s));
end;
procedure TStreamHelper.WriteDouble(d:double);
begin
  self.Write(d,sizeof(d));
end;


{$ifndef FPC}
// there can only be one helper per class, and I use these in Delphi for FPC compatibility.
function TStreamHelper.ReadByte: Byte;
begin
 self.Read(result,sizeof(result));
end;

function TStreamHelper.ReadDWord: DWord;
begin
 self.Read(result,sizeof(result));
end;

function TStreamHelper.ReadWord: Word;
begin
 self.Read(result,sizeof(result));
end;

procedure TStreamHelper.WriteByte(b: Byte);
begin
 self.Write(b,sizeof(b));
end;

procedure TStreamHelper.WriteDWord(b: DWord);
begin
 self.Write(b,sizeof(b));
end;

procedure TStreamHelper.WriteWord(b: Word);
begin
 self.Write(b,sizeof(b));
end;
{$endif}

{ TWindowedStream }

constructor TWindowedStream.Create(aStream : TStream; const aSize : Int64; const aPositionHere : Int64 = 0);
begin
  inherited Create(aStream);
  fStart := aStream.Position;
  fFrontier := fStart + aSize;
  fStartingPositionHere := aPositionHere;
  fPositionHere := aPositionHere;
end;

destructor TWindowedStream.Destroy();
begin
  inherited Destroy();
end;

function TWindowedStream.Read(var aBuffer; aCount : longint) : longint;
var
  vSourcePosition : Int64;
  vNewSourcePosition : Int64;
begin
  vSourcePosition := Source.Position;
  vNewSourcePosition := fStart + fPositionHere - fStartingPositionHere;
  if vNewSourcePosition <> vSourcePosition then // someone modified the file position. Bad bad.
    Source.Seek(vNewSourcePosition, 0);

  if vNewSourcePosition + aCount > fFrontier then // trying to access outside.
    aCount := fFrontier - vNewSourcePosition;
    
  Result := Source.Read(aBuffer, aCount);
  Inc(fPositionHere, Result);
end;


function TWindowedStream.Write(const aBuffer; aCount : Longint): Longint;
var
  vSourcePosition : Int64;
  vNewSourcePosition : Int64;
begin
  vSourcePosition := Source.Position;
  vNewSourcePosition := fStart + fPositionHere - fStartingPositionHere;
  if vNewSourcePosition <> vSourcePosition then // someone modified the file position. Bad bad.
    Source.Seek(vNewSourcePosition, 0);

  if vNewSourcePosition + aCount > fFrontier then // trying to access outside.
    Raise EWriteError.Create(SErrCannotWriteOutsideWindow);
    //aCount := fFrontier - vNewSourcePosition;
    
  Result := Source.Write(aBuffer, aCount);
  Inc(fPositionHere, Result);
end;

function TWindowedStream.Seek(const aOffset: Int64; aOrigin: TSeekOrigin): Int64;
var
  vNewPositionHere : Int64;
  vSourcePosition : Int64;
begin
  {
  here                       there
  fStartingPositionHere .... fStart
  fPositionHere............. x
  }
  
  if (aOrigin = soCurrent) and (aOffset = 0) then begin // get position.
    Result := fPositionHere;
    Exit;
  end;
  
  if aOrigin = soBeginning then
    vNewPositionHere := aOffset
  else if aOrigin = soCurrent then
    vNewPositionHere := fPositionHere + aOffset
  else if aOrigin = soEnd then
    vNewPositionHere := fStartingPositionHere + fFrontier - fStart + aOffset
  else
    raise EReadError.Create(SErrInvalidSeekOrigin);

  vSourcePosition := fStart + vNewPositionHere - fStartingPositionHere;
  if (vSourcePosition < 0) or (vSourcePosition >= fFrontier) then
    raise EReadError.Create(SErrInvalidSeekWindow);
    
  Result := Source.Seek(vSourcePosition, 0);
  //if Result = -1 ??? can that happen?
  Result := vNewPositionHere;
end;

function TWindowedStream.GetSize() : Int64;
begin
  Result := fFrontier - fStart;
end;

procedure TWindowedStream.SetSize(const NewSize: Int64); overload;
begin
  if NewSize = Self.GetSize() then
    Exit;
  raise EWriteError.Create(SErrCannotChangeWindowSize);
end;


end.
