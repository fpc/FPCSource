{ **********************************************************************
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by the Free Pascal development team

    LZW stream (reader) based on implementation of fpReadTiff

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit LZWStream;



{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

Type
  ELZWStreamError = Class(EStreamError);

  TLZWStreamOption = (zoTIFFCodes);
  TLZWStreamOptions = Set of TLZWStreamOption;

type
  TLZWString = packed record
    Count: integer;
    Data: PByte;
    ShortData: array[0..3] of byte;
  end;
  TLZWStringArray = Array of TLZWString;

const
  // TIFF Codes
  TiffClearCode = 256; // clear table, start with 9bit codes
  TiffEoiCode = 257;   // end of input
  TiffNoCode = $7fff;
  LZWInitialBitLength = 9;
  LZWMaxBitLength = 12;

  LZWDefaultBufSize = 1024;

Type
  { TLZWStream }

  TLZWStream = class(TOwnerStream)
  Private
    FSkipCodes : Word;
    Foptions: TLZWStreamOptions;
    FCodeBuffer: DWord;
    FCodeBufferLength: byte;
    FCurBitLength: byte;
    FTable: TLZWStringArray;
    FTableCount: integer;
    FOldCode: Word;
    FBigEndian: boolean;
    FTableMargin: byte;
  Protected
    procedure ClearTable;
    procedure InitializeTable;
    function CodeToIndex(aCode: word): Integer; inline;
    function IsInTable(aCode: word): boolean;
    procedure Error(const Msg: string);
    procedure AddStringToTable(aCode, AddFirstCharFromCode: integer);
  Public
    Constructor Create(aSource : TStream; aOptions : TLZWStreamOptions); virtual;
    Destructor Destroy; override;
    Property Options: TLZWStreamOptions Read Foptions Write Foptions;
    Property TableCount : Integer Read FTableCount;
  end;

  { TLZWDecompressionStream }

  TLZWDecompressionStream = class(TLZWStream)
  Private
    FInBuffer : Array[Byte] of Byte;
    FInRead : Word;
    FInSize : Word;
    FOutBuffer : Array of byte;
    FOutSize : Cardinal;
    FOutRead : Cardinal;
    FFirstRead : Boolean;
    FPosition : Int64;
    procedure MoveFromBuffer(Dest: PByte; aCount: Integer);
  Protected
    function ReadByte(out aByte: Byte): Boolean;
    Function BufferCapacity : Integer;
    Function BufferSize : Integer;
    Function BytesInBuffer : Integer;
    function GetNextCode: Word;
    procedure WriteStringFromCode(aCode: integer; AddFirstChar: boolean = false);
    Procedure FillInBuffer;
    Procedure FillOutBuffer;
    function  GetPosition: Int64; override;
    procedure SetPosition(const Pos: Int64); override;
  Public
    Constructor Create(aSource : TStream; aOptions : TLZWStreamOptions; InitialBufsize : Word = LZWDefaultBufSize); overload;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
  end;

  TLZWCompressionStream = class(TStream)
    // Todo
  end;

implementation

procedure TLZWStream.Error(const Msg: string);
begin
  Raise ELZWStreamError.Create(Msg);
end;


procedure TLZWStream.ClearTable;
var
  i: Integer;
begin
  for i:=0 to TableCount-1 do
    if FTable[i].Data <> @FTable[i].ShortData then
      ReAllocMem(FTable[i].Data,0);
  FTableCount:=0;
end;

procedure TLZWStream.InitializeTable;
begin
  FCurBitLength:=LZWInitialBitLength;
  ClearTable;
end;

function TLZWStream.CodeToIndex(aCode: word): Integer;
begin
  Result:=aCode-FSkipCodes;
end;

function TLZWStream.IsInTable(aCode: word): boolean;
begin
  Result:=(CodeToIndex(aCode)<TableCount);
end;

constructor TLZWStream.Create(aSource: TStream; aOptions: TLZWStreamOptions);


begin
  Inherited Create(aSource);
  FOptions:=aOptions;
  FSkipCodes:=256;
  if zoTIFFCodes in Options then
    FSkipCodes:=FSkipCodes+2;
  SetLength(FTable,4096-1-FSkipCodes);
  FCurBitLength:=9;
  FCodeBufferLength := 0;
  FCodeBuffer := 0;
  FTableCount:=0;
  FOldCode := TiffNoCode;
end;

destructor TLZWStream.Destroy;
begin
  ClearTable;
  inherited Destroy;
end;

procedure TLZWStream.AddStringToTable(aCode, AddFirstCharFromCode: integer);
// add string from code plus first character of string from code as new string
var
  s1, s2: TLZWString;
  p: PByte;
  NewCount: integer;
begin
  // WriteLn('AddStringToTable Code=',aCode,' FCFCode=',AddFirstCharFromCode,' TableCount=',TableCount);
  //check whether can store more codes or not
  if TableCount=Length(FTable) then exit;
  // find string 1
  if aCode<256 then
    begin
    // string is byte
    s1.ShortData[0] := acode;
    s1.Data:=@s1.ShortData;
    s1.Count:=1;
    end
  else if (aCode>=FSkipCodes) then
    begin
    // normal string
    if CodeToIndex(aCode)>=TableCount then
      Error('LZW code out of bounds');
    s1:=FTable[CodeToindex(aCode)];
    end
  else
    Error('LZW code out of bounds');
  // find string 2
  if AddFirstCharFromCode<256 then begin
    // string is byte
    s2.ShortData[0] := AddFirstCharFromCode;
    s2.Data:=@s2.ShortData;
    s2.Count:=1;
  end else begin
    // normal string
    if CodeToIndex(AddFirstCharFromCode)>=TableCount then
      Error('LZW code out of bounds');
    s2:=FTable[CodeToIndex(AddFirstCharFromCode)];
  end;
  // set new FTable entry
  NewCount := s1.Count+1;
  FTable[TableCount].Count:= NewCount;
  if NewCount > 4 then
  begin
    p:=nil;
    GetMem(p,NewCount);
  end else
    p := @FTable[TableCount].ShortData;
  FTable[TableCount].Data:=p;
  System.Move(s1.Data^,p^,s1.Count);
  // add first character from string 2
  p[s1.Count]:=s2.Data^;
  // increase TableCount
  inc(FTableCount);
  case TableCount+FSkipCodes+FTableMargin of
  512,1024,2048: begin
      //check if there is room for a greater code
      if FCurBitLength<LZWMaxBitLength then
        inc(FCurBitLength);
    end;
  end;
end;

function TLZWDecompressionStream.BufferCapacity: Integer;
begin
  Result:=Length(FOutBuffer);
end;

function TLZWDecompressionStream.BufferSize: Integer;
begin
  Result:=FOutSize;
end;

function TLZWDecompressionStream.BytesInBuffer: Integer;
begin
  Result:=FOutSize-FOutRead;
end;


function TLZWDecompressionStream.GetNextCode : Word;

Var
  B : Byte;

begin
  while FCodeBufferLength<FCurBitLength do
    begin
    if not ReadByte(B) then
      Exit(TiffEoiCode);
    // Writeln('Byte: ',B);
    If FBigEndian then
      FCodeBuffer:=(FCodeBuffer shl 8) or B
    else
      FCodeBuffer:=FCodeBuffer or (DWord(B) shl FCodeBufferLength);
    Inc(FCodeBufferLength, 8);
    end;

  if FBigEndian then
    begin
    result := FCodeBuffer shr (FCodeBufferLength-FCurBitLength);
    Dec(FCodeBufferLength, FCurBitLength);
    FCodeBuffer := FCodeBuffer and ((1 shl FCodeBufferLength) - 1);
    end
  else
    begin
    result := FCodeBuffer and ((1 shl FCurBitLength)-1);
    Dec(FCodeBufferLength, FCurBitLength);
    FCodeBuffer := FCodeBuffer shr FCurBitLength;
    end;
end;

procedure TLZWDecompressionStream.WriteStringFromCode(aCode: integer; AddFirstChar: boolean = false);
var
  s: TLZWString;
//  i : Integer;

begin
  // WriteLn('WriteStringFromCode Code=',aCode,' AddFirstChar=',AddFirstChar);
  if aCode<256 then
    begin
    // write byte
    s.ShortData[0] := acode;
    s.Data:=@s.ShortData;
    s.Count:=1;
    end
  else if (aCode>=FSkipCodes) then
    begin
    // write string
    if CodeToIndex(aCode)>=TableCount then
      Error('LZW code out of bounds');
    s:=FTable[CodeToIndex(aCode)];
    end
  else
    Error('LZW code out of bounds');
  // Grow buffer
  if (FOutSize+S.Count+1>BufferCapacity) then
    SetLength(FOutBuffer,BufferCapacity*2+8);
  System.Move(s.Data^,FoutBuffer[FOutSize],s.Count);
  // System.Write(S.Count,':');for i:=0 to s.Count-1 do system.write(' ',HexStr(FoutBuffer[FOutSize+i],2)); // debug
  inc(FOutSize,s.Count);
  if AddFirstChar then
    begin
    FOutBuffer[FOutSize]:=S.Data^;
    // System.write('+ ',HexStr(FoutBuffer[FOutSize],2)); // debug
    inc(FOutSize);
    end;
  // writeln(',WriteStringFromCode'); // debug
end;


procedure TLZWDecompressionStream.FillOutBuffer;

Var
  lCode,FillCapacity : Word;

begin
  // Determine a number of bytes we want to read
  FOutSize:=0;
  FOutRead:=0;
  FillCapacity:=BufferCapacity-8;
  if FFirstRead and (zoTIFFCodes in FOptions) then
    begin
    FillInBuffer;
    if (FInSize>0) and (FInBuffer[0]=$80) then
    begin
      FBigEndian := true; //endian-ness of LZW is not necessarily consistent with the rest of the file
      FTableMargin := 1; //keep one free code to be able to write EOI code
    end else
    begin
      FBigEndian := false;
      FTableMargin := 0;
    end;
    FFirstRead:=False;
    end;
  repeat
    lCode:=GetNextCode;
    // WriteLn('DecompressLZW Code=',lCode);
    if lCode=TiffEoiCode then
      break;
    if lCode=TiffClearCode then
      begin
      InitializeTable;
      lCode:=GetNextCode;
      // WriteLn('DecompressLZW after clear Code=',lCode);
      if lCode=TiffEoiCode then
        break;
      if lCode=TiffClearCode then
        Error('LZW code out of bounds');
      WriteStringFromCode(lCode);
      FOldCode:=lCode;
      end
    else
      begin
      if IsInTable(lCode) then
        begin
        // Writeln(lCode,' in Table (',TableCount,')');
        WriteStringFromCode(lCode);
        if FOldCode <> TiffNoCode then
          AddStringToTable(FOldCode,lCode);
        FOldCode:=lCode;
        end
      else if {(Code=TableCount+258) and} (FOldCode <> TiffNoCode) then
        begin
        // Writeln(lCode,' not yet in Table (',TableCount,')');
        WriteStringFromCode(FOldCode,true);
        AddStringToTable(FOldCode,FOldCode);
        FOldCode:=lCode;
        end
      else
        Error('LZW code out of bounds');
      end;
  until FOutSize>=FillCapacity;
end;

function TLZWDecompressionStream.GetPosition: Int64;
begin
  Result:=FPosition;
end;

procedure TLZWDecompressionStream.SetPosition(const Pos: Int64);
begin
  if Pos=FPosition then
    exit;
  inherited SetPosition(Pos);
end;

constructor TLZWDecompressionStream.Create(aSource: TStream;
  aOptions: TLZWStreamOptions; InitialBufsize: Word);
begin
  inherited Create(aSource, aOptions);
  SetLength(FOutBuffer,InitialBufsize);
  FOutSize:=0;
  FOutRead:=0;
  FFirstRead:=True;
  FPosition:=0;
end;

function TLZWDecompressionStream.Seek(const Offset: Int64; Origin: TSeekOrigin
  ): Int64;
begin
  if (Offset=0) and (Origin=soCurrent) then
    Result:=FPosition
  else
    Result:=inherited Seek(Offset, Origin);
end;

procedure TLZWDecompressionStream.MoveFromBuffer(Dest : PByte; aCount : Integer);

begin
  Move(FOutBuffer[FOutRead],Dest^,aCount);
  Inc(FOutRead,aCount);
end;

procedure TLZWDecompressionStream.FillInBuffer;

begin
  FInSize:=Source.Read(FInBuffer,SizeOf(FInBuffer));
  FInRead:=0;
end;

function TLZWDecompressionStream.ReadByte(out aByte: Byte): Boolean;
begin
  if FinRead>=FInSize then
    FillInBuffer;
  Result:=FInRead<FInSize;
  if Result then
    begin
    aByte:=FInBuffer[FInRead];
    inc(FInRead);
    end;
end;

function TLZWDecompressionStream.Read(var Buffer; Count: Longint): Longint;

Var
  Dest : PByte;
  aMoveSize: Integer;

begin
  Result:=0;
  if Count=0 then
    exit;
  Dest:=@Buffer;
  if (BytesInBuffer=0) then
    FillOutBuffer;
  While (Count>0) and (BytesInBuffer>0) do
    begin
    aMoveSize:=BytesInBuffer;
    if aMoveSize>Count then
      aMoveSize:=Count;
    MoveFromBuffer(Dest,aMoveSize);
    Inc(Dest,aMoveSize);
    Inc(Result,aMoveSize);
    Dec(Count,aMoveSize);
    if Count>0 then // we need more data
      FillOutBuffer;
    end;
  Inc(FPosition,Result);
end;


end.

