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
   public
     constructor Create; virtual;
     procedure Reset; virtual; abstract;
     procedure Close; virtual; abstract;
     function IsEof: Boolean; virtual; abstract;
     procedure ReadLine(out AString: string); virtual; abstract; overload;
     function ReadLine: string; virtual; abstract; overload;
     property Eof: Boolean read IsEof;
   end;

   { TStreamReader }

   TStreamReader = class(TTextReader)
   private
     FBufferRead: Integer;
     FBufferPosition: Integer;
     FOwnsStream: Boolean;
     FStream: TStream;
     FBuffer: array of Byte;
     procedure FillBuffer;
   public
     constructor Create(AStream: TStream; ABufferSize: Integer;
       AOwnsStream: Boolean); virtual;
     constructor Create(AStream: TStream); virtual;
     destructor Destroy; override;
     procedure Reset; override;
     procedure Close; override;
     function IsEof: Boolean; override;
     procedure ReadLine(out AString: string); override; overload;
     function ReadLine: string; override; overload;
     property BaseStream: TStream read FStream;
     property OwnsStream: Boolean read FOwnsStream write FOwnsStream;
   end;

   { TStringReader }

   TStringReader = class(TTextReader)
   private
     FReader: TTextReader;
   public
     constructor Create(const AString: string; ABufferSize: Integer); virtual;
     constructor Create(const AString: string); virtual;
     destructor Destroy; override;
     procedure Reset; override;
     procedure Close; override;
     function IsEof: Boolean; override;
     procedure ReadLine(out AString: string); override; overload;
     function ReadLine: string; override; overload;
   end;

   { TFileReader }

   TFileReader = class(TTextReader)
   private
     FReader: TTextReader;
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
     function IsEof: Boolean; override;
     procedure ReadLine(out AString: string); override; overload;
     function ReadLine: string; override; overload;
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

{ TStreamReader }

constructor TStreamReader.Create(AStream: TStream; ABufferSize: Integer;
  AOwnsStream: Boolean);
begin
  inherited Create;
  if not Assigned(AStream) then
    raise EArgumentException.CreateFmt(SParamIsNil, ['AStream']);
  FStream := AStream;
  FOwnsStream := AOwnsStream;
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
  FBufferRead := FStream.Read(FBuffer[0], Pred(Length(FBuffer)));
  FBuffer[FBufferRead] := 0;
  FBufferPosition := 0;
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
  begin
    FStream.Free;
    FStream := nil;
  end;
end;

function TStreamReader.IsEof: Boolean;
begin
  if not Assigned(FStream) then
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

function TStreamReader.ReadLine: string;
begin
  ReadLine(Result);
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

function TStringReader.ReadLine: string;
begin
  ReadLine(Result);
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

function TFileReader.ReadLine: string;
begin
  ReadLine(Result);
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

end.
