{ **********************************************************************
  This file is part of the Free Component Library

  PDF File/Stream navigation
  Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}
{
  PDF needs to be able to seek to arbitrary positions in the file.
  Since we need to maintain a buffer for faster scanning,
  we implement this here.
}

unit fppdfsource;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

Const
  PDFDefaultBufferSize  = 4 * 1024; // 4K buffer

Type
  { TPDFSourceStream }

  TPDFSourceStream = Class
  Private
    FBuffer : Array of Byte;
    FDatasize : Cardinal;
    FCursor : PByte;
    FStream : TStream;
    FStreamSize,
    FPosition : Int64;
    function GetIsBOB: Boolean;
    function GetIsBof: Boolean;
    function GetIsEOB: Boolean;
    function GetIsEOF: Boolean;
    function GetPosInBuf: PTrInt;
  Protected
    // Is cursor before beginning of buffer ?
    Property IsBOB : Boolean Read GetIsBOB;
    // Position in buffer (0-Based)
    Property PosInBuf : PtrInt Read GetPosInBuf;
    // Size of data in buffer
    Property Datasize : Cardinal Read FDatasize;
  Public
    Constructor Create(aStream : TStream; aBufSize : Cardinal = PDFDefaultBufferSize);
    // First byte of buffer is aPosition. aPosition is 0 based.
    Function FillBufferForwardAt(aPosition : Int64): Boolean;
    // Last byte of buffer is at aPosition. aPosition is 0 based.
    Function FillBufferBackWardAt(aPosition : Int64): Boolean;
    // Return byte at current position.
    // If Reverse is False, move position forward (next)
    // If Reverse is true, move position backwards (Previous)
    function GetByte(Reverse : Boolean = False): Byte;
    // Return byte at current position, move position forward.
    Function CopyBytes(B : TBytes; aCount : Integer) : Integer;
    // Move position backwards
    Procedure Previous;
    // Move position forward
    Procedure Next;
    // Current position.
    Property Position : Int64 Read FPosition;
    // Points to Current byte in buffer.
    Property Cursor : PByte Read FCursor;
    // Is cursor after endof buffer ?
    Property IsEOB : Boolean Read GetIsEOB;
    // Current position is <0.
    Property IsBof : Boolean Read GetIsBof;
    // Current position is >=Size
    Property IsEOF : Boolean Read GetIsEOF;
    Property Stream : TStream Read FStream;
    Property StreamSize : Int64 Read FStreamSize;
  end;


implementation


function TPDFSourceStream.GetIsEOF: Boolean;
begin
  Result:=FPosition>=FStreamSize;
end;

function TPDFSourceStream.GetPosInBuf: PTrInt;
begin
  Result:=PTRUInt(FCursor)-PtrUInt(FBuffer)
end;

function TPDFSourceStream.GetIsBof: Boolean;
begin
  Result:=FPosition<0;
end;

function TPDFSourceStream.GetIsBOB: Boolean;
begin
  Result:=PTRUInt(FCursor)<PtrUInt(FBuffer)
end;

function TPDFSourceStream.GetIsEOB: Boolean;

var
  aBufPos : Integer;

begin
  aBufPos:=(PTRUInt(FCursor)-PtrUInt(FBuffer));
  Result:=aBufPos>=FDatasize;
end;

constructor TPDFSourceStream.Create(aStream: TStream; aBufSize: Cardinal);
begin
  FStream:=aStream;
  SetLength(FBuffer,aBufSize);
  FDatasize:=0;
  FCursor:=PByte(FBuffer);
  FStreamSize:=aStream.Size; // avoid calling this a zillion times
  aStream.Position:=0;
  FPosition:=0;
end;

function TPDFSourceStream.FillBufferForwardAt(aPosition: Int64): Boolean;
begin
  Result:=aPosition<FStreamSize;
  if Not Result then
    begin
    FDataSize:=0;
    Exit;
    end;
  FStream.Position:=aPosition;
  FDataSize:=FStream.Read(FBuffer[0],Length(FBuffer));
  FCursor:=PByte(FBuffer);
  FPosition:=aPosition;
end;

function TPDFSourceStream.FillBufferBackWardAt(aPosition: Int64): Boolean;

Var
  aReadLen: Integer;
  aStart : Int64;

begin
  Result:=(aPosition<FStreamSize) and (aPosition>=0);
  if not Result then
    exit;
  aReadLen:=Length(FBuffer);
  if aReadLen>aPosition+1 then
    aReadLen:=aPosition+1;
  aStart:=aPosition+1-aReadLen;
  FStream.Position:=aStart;
  FCursor:=PByte(FBuffer);
  FDataSize:=FStream.Read(FBuffer[0],aReadLen);
  Result:=(FDataSize>0);
  inc(FCursor,FDataSize-1);
  FPosition:=aPosition;
end;

function TPDFSourceStream.GetByte(Reverse: Boolean): Byte;

begin
  Result:=0;
  if Reverse then
    begin
    if (FCursor=Nil) then
      Raise EInOutError.Create('Read before end of stream');
    Result:=FCursor^;
    Dec(FCursor);
    Dec(FPosition);
    if IsBOB then
      FillBufferBackwardAt(FPosition);
    end
  else
    begin
    if (FCursor=Nil) then
      Raise EInOutError.Create('Read after end of stream');
    Result:=FCursor^;
    Inc(FCursor);
    Inc(FPosition);
    if isEOB then
      FillBufferForwardAt(FPosition);
    end;
end;


function TPDFSourceStream.CopyBytes(B: TBytes; aCount: Integer): Integer;

Var
  lMoveCount : Integer;

begin
  Result:=0;
  if FDataSize=0 then
    if not FillBufferForwardAt(FPosition) then
      Exit;
  if FCursor=Nil then
    Raise EInOutError.Create('Read after end of stream');
  While aCount>0 do
    begin
    lMoveCount:=FDataSize-PosInBuf;
    if lMoveCount>aCount then
      lMoveCount:=aCount;
    Move(FCursor^,B[Result],lMoveCount);
    Inc(Result,lMoveCount);
    inc(FPosition,lMoveCount);
    inc(FCursor,lMoveCount);
    Dec(aCount,lMoveCount);
    if (aCount>0) then
      if not FillBufferForwardAt(FPosition) then
        aCount:=0;
    end;
  if IsEOB then
    FillBufferForwardAt(FPosition);
end;

procedure TPDFSourceStream.Previous;
begin
  if isBOB then
    begin
    if not FillBufferBackWardAt(FPosition-1) then
      Raise EInOutError.Create('Read before end of stream');
    end
  else
    begin
    Dec(FCursor);
    Dec(FPosition);
    if isBOB and not IsBOF then
      if not FillBufferBackWardAt(FPosition) then
        Raise EInOutError.Create('Read before end of stream');
    end;
end;

procedure TPDFSourceStream.Next;
begin
  if isEOB then
    begin
    if not FillBufferForWardAt(FPosition+1) then
      Raise EInOutError.Create('Read after end of stream');
    end
  else
    begin
    Inc(FCursor);
    Inc(FPosition);
    if isEOB and not isEOF then
      begin
      if not FillBufferForWardAt(FPosition) then
        Raise EInOutError.Create('Read after end of stream 2');
      end;
    end;
end;

end.

