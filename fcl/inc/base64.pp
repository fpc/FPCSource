{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl
    base64 encoder & decoder (c) 1999 Sebastian Guenther

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

// Encoding and decoding streams for base64 data as described in RFC2045

{$MODE objfpc}
{$H+}

unit base64;

interface

uses classes;

type

  TBase64EncodingStream = class(TStream)
  protected
    OutputStream: TStream;
    TotalBytesProcessed, BytesWritten: LongWord;
    Buf: array[0..2] of Byte;
    BufSize: Integer;    // # of bytes used in Buf
  public
    constructor Create(AOutputStream: TStream);
    destructor Destroy; override;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
  end;


  TBase64DecodingStream = class(TStream)
  protected
    InputStream: TStream;
    CurPos, InputStreamSize: LongInt;
    Buf: array[0..2] of Byte;
    BufPos: Integer;    // Offset of byte which is to be read next
    fEOF: Boolean;
  public
    constructor Create(AInputStream: TStream);
    procedure Reset;

    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    property EOF: Boolean read fEOF;
  end;



implementation

const

  EncodingTable: PChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  DecTable: array[Byte] of Byte =
    (99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,  // 0-15
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,  // 16-31
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 62, 99, 99, 99, 63,  // 32-47
     52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 99, 99, 99, 00, 99, 99,  // 48-63
     99, 00, 01, 02, 03, 04, 05, 06, 07, 08, 09, 10, 11, 12, 13, 14,  // 64-79
     15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 99, 99, 99, 99, 99,  // 80-95
     99, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40,  // 96-111
     41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 99, 99, 99, 99, 99,  // 112-127
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99,
     99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99, 99);


constructor TBase64EncodingStream.Create(AOutputStream: TStream);
begin
  inherited Create;
  OutputStream := AOutputStream;
end;

destructor TBase64EncodingStream.Destroy;
var
  WriteBuf: array[0..3] of Char;
begin
  // Fill output to multiple of 4
  case (TotalBytesProcessed mod 3) of
    1: begin
        WriteBuf[0] := EncodingTable[Buf[0] shr 2];
        WriteBuf[1] := EncodingTable[(Buf[0] and 3) shl 4];
        WriteBuf[2] := '=';
        WriteBuf[3] := '=';
        OutputStream.Write(WriteBuf, 4);
      end;
    2: begin
        WriteBuf[0] := EncodingTable[Buf[0] shr 2];
        WriteBuf[1] := EncodingTable[(Buf[0] and 3) shl 4 or (Buf[1] shr 4)];
        WriteBuf[2] := EncodingTable[(Buf[1] and 15) shl 2];
        WriteBuf[3] := '=';
        OutputStream.Write(WriteBuf, 4);
      end;
  end;
  inherited Destroy;
end;

function TBase64EncodingStream.Read(var Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create('Invalid stream operation');
end;

function TBase64EncodingStream.Write(const Buffer; Count: Longint): Longint;
var
  ReadNow: LongInt;
  p: Pointer;
  WriteBuf: array[0..3] of Char;
begin
  Inc(TotalBytesProcessed, Count);
  Result := Count;

  p := @Buffer;
  while count > 0 do begin
    // Fetch data into the Buffer
    ReadNow := 3 - BufSize;
    if ReadNow > Count then break;    // Not enough data available
    Move(p^, Buf[BufSize], ReadNow);
    Inc(p, ReadNow);
    Dec(Count, ReadNow);

    // Encode the 3 bytes in Buf
    WriteBuf[0] := EncodingTable[Buf[0] shr 2];
    WriteBuf[1] := EncodingTable[(Buf[0] and 3) shl 4 or (Buf[1] shr 4)];
    WriteBuf[2] := EncodingTable[(Buf[1] and 15) shl 2 or (Buf[2] shr 6)];
    WriteBuf[3] := EncodingTable[Buf[2] and 63];
    OutputStream.Write(WriteBuf, 4);
    Inc(BytesWritten, 4);
    BufSize := 0;
  end;
  Move(p^, Buf[BufSize], count);
  Inc(BufSize, count);
end;

function TBase64EncodingStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  Result := BytesWritten;
  if BufSize > 0 then
    Inc(Result, 4);

  // This stream only supports the Seek modes needed for determining its size
  if not ((((Origin = soFromCurrent) or (Origin = soFromEnd)) and (Offset = 0))
     or ((Origin = soFromBeginning) and (Offset = Result))) then
    raise EStreamError.Create('Invalid stream operation');
end;




constructor TBase64DecodingStream.Create(AInputStream: TStream);
begin
  inherited Create;
  InputStream := AInputStream;
  Reset;
end;

procedure TBase64DecodingStream.Reset;
begin
  InputStreamSize := -1;
  BufPos := 3;
  fEOF := False;
end;

function TBase64DecodingStream.Read(var Buffer; Count: Longint): Longint;
var
  p: PChar;
  b: Char;
  ReadBuf: array[0..3] of Byte;
  ToRead, OrgToRead, HaveRead, ReadOK, i, j: Integer;
begin
  if Count <= 0 then exit(0);
  if InputStreamSize <> -1 then begin
    if CurPos + Count > InputStreamSize then
      Count := InputStreamSize - CurPos;
    if Count <= 0 then exit(0);
  end;

  Result := 0;
  p := PChar(@Buffer);
  while (Count > 0) and not fEOF do begin
    if BufPos > 2 then begin
      BufPos := 0;
      // Read the next 4 valid bytes
      ToRead := 4;
      ReadOK := 0;
      while ToRead > 0 do begin
        OrgToRead := ToRead;
        HaveRead := InputStream.Read(ReadBuf[ReadOK], ToRead);
        //WriteLn('ToRead = ', ToRead, ', HaveRead = ', HaveRead, ', ReadOK=', ReadOk);
        if HaveRead > 0 then begin
          i := ReadOk;
          while i<HaveRead do begin
            ReadBuf[i] := DecTable[ReadBuf[i]];
            if ReadBuf[i] = 99 then
              for j := i to 3 do
                ReadBuf[i] := ReadBuf[i + 1]
            else begin
              Inc(i);
              Inc(ReadOK);
              Dec(ToRead);
            end;
          end;
        end;
        if HaveRead <> OrgToRead then begin
          //WriteLn('Ende? ReadOK=', ReadOK, ', count=', Count);
          for i := ReadOK to 3 do
            ReadBuf[i] := Ord('=');
          fEOF := True;
          if ReadOK < 2 then exit;    // Not enough data available in input stream
          break;
        end;
      end;

      // Check for fill bytes
      if (Count >= 2) and (ReadBuf[3] = Ord('=')) then begin
        //WriteLn('Endemarkierung!');
        fEOF := True;
        if ReadBuf[2] = Ord('=') then
          Count := 1
        else
          Count := 2;
      end;

      // Decode the 4 bytes in the buffer to 3 undecoded bytes
      Buf[0] := ReadBuf[0] shl 2 or ReadBuf[1] shr 4;
      Buf[1] := (ReadBuf[1] and 15) shl 4 or ReadBuf[2] shr 2;
      Buf[2] := (ReadBuf[2] and 3) shl 6 or ReadBuf[3];
    end;

    p[0] := Chr(Buf[BufPos]);
    Inc(p);
    Inc(BufPos);
    Inc(CurPos);
    Dec(Count);
    Inc(Result);
  end;
end;

function TBase64DecodingStream.Write(const Buffer; Count: Longint): Longint;
begin
  raise EStreamError.Create('Invalid stream operation');
end;

function TBase64DecodingStream.Seek(Offset: Longint; Origin: Word): Longint;
var
  ipos: LongInt;
  endbytes: array[0..1] of Char;
begin
  {This will work only if the input stream supports seeking / Size. If not, the
   input stream will raise an exception; we don't handle them here but pass them
   to the caller.}
  if InputStreamSize = -1 then begin
    ipos := InputStream.Position;
    InputStreamSize := ((InputStream.Size - ipos + 3) div 4) * 3;
    InputStream.Seek(-2, soFromEnd);
    InputStream.Read(endbytes, 2);
    InputStream.Position := ipos;
    if endbytes[1] = '=' then begin
      Dec(InputStreamSize);
    if endbytes[0] = '=' then
      Dec(InputStreamSize);
    end;
  end;

  // This stream only supports the Seek modes needed for determining its size
  if (Origin = soFromCurrent) and (Offset = 0) then
    Result := CurPos
  else if (Origin = soFromEnd) and (Offset = 0) then
    Result := InputStreamSize
  else if (Origin = soFromBeginning) and (Offset = CurPos) then
    Result := CurPos
  else
    raise EStreamError.Create('Invalid stream operation');
end;


end.
