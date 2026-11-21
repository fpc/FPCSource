{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    QUIC byte buffer reader/writer with variable-length integers (RFC 9000 section 16) and range sets.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit FpQuic.Buffers;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, FpQuic.Consts;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, FpQuic.Consts;
{$ENDIF FPC_DOTTEDUNITS}

type
  // Sequential reader over a byte buffer; reading past the end raises EQuicError(FRAME_ENCODING_ERROR).
  TQuicReader = record
  private
    FData: PByte;
    FLength: Integer;
    FPosition: Integer;
    procedure Need(aCount: Integer);
  public
    // Starts reading aLength bytes at aData.
    procedure Init(aData: PByte; aLength: Integer);
    // Starts reading the whole of aBytes.
    procedure InitBytes(const aBytes: TBytes);
    // Reads one byte.
    function ReadByte: Byte;
    // Reads a big-endian 16-bit value.
    function ReadUInt16: Word;
    // Reads a big-endian 32-bit value.
    function ReadUInt32: Cardinal;
    // Reads a variable-length integer.
    function ReadVarInt: QWord;
    // Reads aCount bytes.
    function ReadBytes(aCount: QWord): TBytes;
    // Reads a variable-length integer length prefix followed by that many bytes.
    function ReadVarBytes: TBytes;
    // Skips aCount bytes.
    procedure Skip(aCount: Integer);
    // Returns the next byte without consuming it.
    function PeekByte: Byte;
    // Number of unread bytes.
    function Remaining: Integer;
    // True when all bytes are read.
    function EOF: Boolean;
    // Offset of the next byte to read.
    property Position: Integer read FPosition;
  end;

  // Growing byte buffer writer.
  TQuicWriter = record
  private
    FData: TBytes;
    FLength: Integer;
    procedure Grow(aCount: Integer);
  public
    // Empties the writer.
    procedure Init;
    // Appends one byte.
    procedure WriteByte(aValue: Byte);
    // Appends a big-endian 16-bit value.
    procedure WriteUInt16(aValue: Word);
    // Appends a big-endian 32-bit value.
    procedure WriteUInt32(aValue: Cardinal);
    // Appends a variable-length integer in its shortest encoding.
    procedure WriteVarInt(aValue: QWord);
    // Appends a variable-length integer encoded in exactly aSize (1, 2, 4 or 8) bytes.
    procedure WriteVarIntSized(aValue: QWord; aSize: Integer);
    // Appends aCount bytes from aData.
    procedure WriteBuffer(aData: PByte; aCount: Integer);
    // Appends a byte array.
    procedure WriteBytes(const aBytes: TBytes);
    // Appends a variable-length integer length prefix followed by aBytes.
    procedure WriteVarBytes(const aBytes: TBytes);
    // Appends aCount zero bytes.
    procedure WriteZeros(aCount: Integer);
    // Returns a copy of the written bytes.
    function ToBytes: TBytes;
    // Number of bytes written.
    property Length: Integer read FLength;
  end;

  // Inclusive range of 64-bit values.
  TQuicRange = record
    Low: QWord;
    High: QWord;
  end;
  TQuicRangeArray = array of TQuicRange;

  // Sorted set of disjoint inclusive ranges.
  TQuicRangeSet = class
  private
    FRanges: TQuicRangeArray;
    function GetCount: Integer;
    function GetRange(aIndex: Integer): TQuicRange;
    procedure Delete(aIndex: Integer);
    procedure Insert(aIndex: Integer; aLow, aHigh: QWord);
  public
    // Adds the values aLow..aHigh, merging adjacent and overlapping ranges.
    procedure Add(aLow, aHigh: QWord);
    // Removes the values aLow..aHigh.
    procedure Remove(aLow, aHigh: QWord);
    // True when aValue is in the set.
    function Contains(aValue: QWord): Boolean;
    // True when every value of aLow..aHigh is in the set.
    function ContainsRange(aLow, aHigh: QWord): Boolean;
    // Smallest value; the set must not be empty.
    function Min: QWord;
    // Largest value; the set must not be empty.
    function Max: QWord;
    // Removes all ranges.
    procedure Clear;
    // Number of disjoint ranges.
    property Count: Integer read GetCount;
    // Ranges in ascending order.
    property Ranges[aIndex: Integer]: TQuicRange read GetRange; default;
  end;

// Number of bytes needed to encode aValue as a variable-length integer.
function QuicVarIntSize(aValue: QWord): Integer;

// Returns a copy of aCount bytes at aData.
function QuicBytesOf(aData: PByte; aCount: Integer): TBytes;

// True when both byte arrays have the same content.
function QuicBytesEqual(const aLeft, aRight: TBytes): Boolean;

// True when both byte arrays have the same content; the time taken depends only on the lengths.
function QuicBytesEqualConstantTime(const aLeft, aRight: TBytes): Boolean;

// Overwrites the content of aBytes with zeros.
procedure QuicWipe(var aBytes: TBytes);

// Converts a hexadecimal string (whitespace allowed) to bytes.
function QuicHexToBytes(const aHex: String): TBytes;

// Converts bytes to a lowercase hexadecimal string.
function QuicBytesToHex(const aBytes: TBytes): String;

implementation

function QuicVarIntSize(aValue: QWord): Integer;

begin
  if aValue<64 then
    Result:=1
  else if aValue<16384 then
    Result:=2
  else if aValue<1073741824 then
    Result:=4
  else if aValue<=QUIC_MAX_VARINT then
    Result:=8
  else
    raise EQuicError.CreateCode(QUIC_INTERNAL_ERROR,'Value too large for a variable-length integer');
end;


function QuicBytesOf(aData: PByte; aCount: Integer): TBytes;

begin
  Result:=nil;
  SetLength(Result,aCount);
  if aCount>0 then
    Move(aData^,Result[0],aCount);
end;


function QuicBytesEqual(const aLeft, aRight: TBytes): Boolean;

begin
  Result:=(System.Length(aLeft)=System.Length(aRight))
          and ((System.Length(aLeft)=0) or CompareMem(@aLeft[0],@aRight[0],System.Length(aLeft)));
end;


function QuicBytesEqualConstantTime(const aLeft, aRight: TBytes): Boolean;

var
  I: Integer;
  lDiff: Byte;

begin
  if System.Length(aLeft)<>System.Length(aRight) then
    Exit(False);
  lDiff:=0;
  for I:=0 to System.Length(aLeft)-1 do
    lDiff:=lDiff or (aLeft[I] xor aRight[I]);
  Result:=lDiff=0;
end;


procedure QuicWipe(var aBytes: TBytes);

begin
  FillChar(PByte(aBytes)^,System.Length(aBytes),0);
  aBytes:=nil;
end;


function QuicHexToBytes(const aHex: String): TBytes;

var
  lClean: String;
  lChar: Char;
  I: Integer;

begin
  lClean:='';
  for lChar in aHex do
    if not (lChar in [' ',#9,#10,#13]) then
      lClean:=lClean+lChar;
  Result:=nil;
  SetLength(Result,System.Length(lClean) div 2);
  for I:=0 to System.Length(Result)-1 do
    Result[I]:=StrToInt('$'+Copy(lClean,I*2+1,2));
end;


function QuicBytesToHex(const aBytes: TBytes): String;

var
  I: Integer;

begin
  Result:='';
  for I:=0 to System.Length(aBytes)-1 do
    Result:=Result+LowerCase(IntToHex(aBytes[I],2));
end;


{ TQuicReader }

procedure TQuicReader.Need(aCount: Integer);

begin
  if (aCount<0) or (aCount>FLength-FPosition) then
    raise EQuicError.CreateCode(QUIC_FRAME_ENCODING_ERROR,'Unexpected end of data');
end;


procedure TQuicReader.Init(aData: PByte; aLength: Integer);

begin
  FData:=aData;
  FLength:=aLength;
  FPosition:=0;
end;


procedure TQuicReader.InitBytes(const aBytes: TBytes);

begin
  Init(PByte(aBytes),System.Length(aBytes));
end;


function TQuicReader.ReadByte: Byte;

begin
  Need(1);
  Result:=FData[FPosition];
  Inc(FPosition);
end;


function TQuicReader.ReadUInt16: Word;

begin
  Need(2);
  Result:=(Word(FData[FPosition]) shl 8) or FData[FPosition+1];
  Inc(FPosition,2);
end;


function TQuicReader.ReadUInt32: Cardinal;

begin
  Need(4);
  Result:=(Cardinal(FData[FPosition]) shl 24) or (Cardinal(FData[FPosition+1]) shl 16)
          or (Cardinal(FData[FPosition+2]) shl 8) or FData[FPosition+3];
  Inc(FPosition,4);
end;


function TQuicReader.ReadVarInt: QWord;

var
  lSize, I: Integer;

begin
  Need(1);
  lSize:=1 shl (FData[FPosition] shr 6);
  Need(lSize);
  Result:=FData[FPosition] and $3F;
  for I:=1 to lSize-1 do
    Result:=(Result shl 8) or FData[FPosition+I];
  Inc(FPosition,lSize);
end;


function TQuicReader.ReadBytes(aCount: QWord): TBytes;

begin
  if aCount>QWord(FLength-FPosition) then
    Need(FLength-FPosition+1);
  Result:=QuicBytesOf(FData+FPosition,aCount);
  Inc(FPosition,aCount);
end;


function TQuicReader.ReadVarBytes: TBytes;

begin
  Result:=ReadBytes(ReadVarInt);
end;


procedure TQuicReader.Skip(aCount: Integer);

begin
  Need(aCount);
  Inc(FPosition,aCount);
end;


function TQuicReader.PeekByte: Byte;

begin
  Need(1);
  Result:=FData[FPosition];
end;


function TQuicReader.Remaining: Integer;

begin
  Result:=FLength-FPosition;
end;


function TQuicReader.EOF: Boolean;

begin
  Result:=FPosition>=FLength;
end;


{ TQuicWriter }

procedure TQuicWriter.Grow(aCount: Integer);

var
  lNew: Integer;

begin
  if FLength+aCount<=System.Length(FData) then
    Exit;
  lNew:=System.Length(FData)*2;
  if lNew<FLength+aCount then
    lNew:=FLength+aCount+64;
  SetLength(FData,lNew);
end;


procedure TQuicWriter.Init;

begin
  FData:=nil;
  FLength:=0;
end;


procedure TQuicWriter.WriteByte(aValue: Byte);

begin
  Grow(1);
  FData[FLength]:=aValue;
  Inc(FLength);
end;


procedure TQuicWriter.WriteUInt16(aValue: Word);

begin
  WriteByte(aValue shr 8);
  WriteByte(aValue and $FF);
end;


procedure TQuicWriter.WriteUInt32(aValue: Cardinal);

begin
  WriteUInt16(aValue shr 16);
  WriteUInt16(aValue and $FFFF);
end;


procedure TQuicWriter.WriteVarInt(aValue: QWord);

begin
  WriteVarIntSized(aValue,QuicVarIntSize(aValue));
end;


procedure TQuicWriter.WriteVarIntSized(aValue: QWord; aSize: Integer);

const
  Prefix: array[1..8] of Byte = (0,$40,0,$80,0,0,0,$C0);

var
  I: Integer;

begin
  if QuicVarIntSize(aValue)>aSize then
    raise EQuicError.CreateCode(QUIC_INTERNAL_ERROR,'Value does not fit the requested size');
  Grow(aSize);
  for I:=aSize-1 downto 0 do
    begin
    FData[FLength+I]:=aValue and $FF;
    aValue:=aValue shr 8;
    end;
  FData[FLength]:=FData[FLength] or Prefix[aSize];
  Inc(FLength,aSize);
end;


procedure TQuicWriter.WriteBuffer(aData: PByte; aCount: Integer);

begin
  if aCount<=0 then
    Exit;
  Grow(aCount);
  Move(aData^,FData[FLength],aCount);
  Inc(FLength,aCount);
end;


procedure TQuicWriter.WriteBytes(const aBytes: TBytes);

begin
  WriteBuffer(PByte(aBytes),System.Length(aBytes));
end;


procedure TQuicWriter.WriteVarBytes(const aBytes: TBytes);

begin
  WriteVarInt(System.Length(aBytes));
  WriteBytes(aBytes);
end;


procedure TQuicWriter.WriteZeros(aCount: Integer);

begin
  if aCount<=0 then
    Exit;
  Grow(aCount);
  FillChar(FData[FLength],aCount,0);
  Inc(FLength,aCount);
end;


function TQuicWriter.ToBytes: TBytes;

begin
  Result:=QuicBytesOf(PByte(FData),FLength);
end;


{ TQuicRangeSet }

function TQuicRangeSet.GetCount: Integer;

begin
  Result:=System.Length(FRanges);
end;


function TQuicRangeSet.GetRange(aIndex: Integer): TQuicRange;

begin
  Result:=FRanges[aIndex];
end;


procedure TQuicRangeSet.Delete(aIndex: Integer);

begin
  System.Delete(FRanges,aIndex,1);
end;


procedure TQuicRangeSet.Insert(aIndex: Integer; aLow, aHigh: QWord);

var
  lRange: TQuicRange;

begin
  lRange.Low:=aLow;
  lRange.High:=aHigh;
  System.Insert(lRange,FRanges,aIndex);
end;


procedure TQuicRangeSet.Add(aLow, aHigh: QWord);

var
  I: Integer;

begin
  I:=0;
  while (I<Count) and (FRanges[I].High<aLow) and (aLow-FRanges[I].High>1) do
    Inc(I);
  while (I<Count) and ((FRanges[I].Low<=aHigh) or (FRanges[I].Low-aHigh=1)) do
    begin
    if FRanges[I].Low<aLow then
      aLow:=FRanges[I].Low;
    if FRanges[I].High>aHigh then
      aHigh:=FRanges[I].High;
    Delete(I);
    end;
  Insert(I,aLow,aHigh);
end;


procedure TQuicRangeSet.Remove(aLow, aHigh: QWord);

var
  I: Integer;
  lRange: TQuicRange;

begin
  I:=0;
  while I<Count do
    begin
    lRange:=FRanges[I];
    if (lRange.High<aLow) or (lRange.Low>aHigh) then
      Inc(I)
    else
      begin
      Delete(I);
      if lRange.Low<aLow then
        begin
        Insert(I,lRange.Low,aLow-1);
        Inc(I);
        end;
      if lRange.High>aHigh then
        begin
        Insert(I,aHigh+1,lRange.High);
        Inc(I);
        end;
      end;
    end;
end;


function TQuicRangeSet.Contains(aValue: QWord): Boolean;

begin
  Result:=ContainsRange(aValue,aValue);
end;


function TQuicRangeSet.ContainsRange(aLow, aHigh: QWord): Boolean;

var
  lRange: TQuicRange;

begin
  for lRange in FRanges do
    if (lRange.Low<=aLow) and (lRange.High>=aHigh) then
      Exit(True);
  Result:=False;
end;


function TQuicRangeSet.Min: QWord;

begin
  Result:=FRanges[0].Low;
end;


function TQuicRangeSet.Max: QWord;

begin
  Result:=FRanges[Count-1].High;
end;


procedure TQuicRangeSet.Clear;

begin
  FRanges:=nil;
end;


end.
