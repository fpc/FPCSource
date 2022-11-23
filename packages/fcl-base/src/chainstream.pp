{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2022 by Michael Van Canneyt
    member of the Free Pascal development team

    Chained Streams implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit chainstream;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TChainedStreamItem = record
    Stream : TStream;
    Size : Int64;
  end;
  TChainedStreamArray = Array of TChainedStreamItem;

  { TChainedStream }

  // Stream, backed by several other streams.
  // A read operation will read bytes from the next stream in the chain if there is one, till the requested number of bytes is read.
  // When writing, the current size of the streams is kept.
  // i.e. the write operation overflows to the next stream, if any.

  TChainedStream = class(TStream)
    FStreams : TChainedStreamArray;
    FPosition : Int64;
    FCurrentStreamIdx : Integer;
  private
    FOwnsStreams: Boolean;
    function GetStream(aIndex : Integer): TStream;
    function GetStreamCount: Integer;
  Protected
    Function CurrentStream : TStream;
    Function StreamSize : Int64;
    Function NextStream : Boolean;
    Function PrevStream : Boolean;
    Function GetTotalSize : Int64;
    function  GetSize: Int64; virtual;
  Public
    Constructor Create(aChain : Array of TStream; OwnsStreams : Boolean = False);
    Destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    property StreamCount : Integer Read GetStreamCount;
    property Streams[aIndex : Integer] : TStream Read GetStream;
    Property OwnsStreams : Boolean Read FOwnsStreams Write FOwnsStreams;
  end;

implementation

uses rtlconsts;

{ TChainedStream }

function TChainedStream.GetStreamCount: Integer;
begin
  Result:=Length(FStreams);
end;

function TChainedStream.GetStream(aIndex : Integer): TStream;
begin
  if (aIndex<0) or (aIndex>=Length(FStreams)) then
    Raise EListError.CreateFmt(SListIndexError,[aIndex]);
  Result:=FStreams[aIndex].Stream;
end;

function TChainedStream.CurrentStream: TStream;
begin
  if FCurrentStreamIdx<Length(FStreams) then
    Result:=FStreams[FCurrentStreamIdx].Stream
  else
    Result:=Nil;
end;

function TChainedStream.StreamSize: Int64;
begin
  if FCurrentStreamIdx<Length(FStreams) then
    begin
    if FStreams[FCurrentStreamIdx].Size=-1 then
      FStreams[FCurrentStreamIdx].Size:=FStreams[FCurrentStreamIdx].Stream.Size;
    Result:=FStreams[FCurrentStreamIdx].Size;
    end
  else
    Result:=0;
end;

function TChainedStream.NextStream: Boolean;
begin
  Inc(FCurrentStreamIdx);
  Result:=FCurrentStreamIdx<Length(FStreams);
end;

function TChainedStream.PrevStream: Boolean;
begin
  Dec(FCurrentStreamIdx);
  Result:=FCurrentStreamIdx>=0;
end;

function TChainedStream.GetTotalSize: Int64;

var
  aCurrent: Integer;

begin
  Result:=0;
  aCurrent:=FCurrentStreamIdx;
  try
    FCurrentStreamIdx:=0;
    While CurrentStream<>Nil do
      begin
      Result:=Result+StreamSize;
      NextStream;
      end;
  finally
    FCurrentStreamIdx:=aCurrent;
  end;
end;

function TChainedStream.GetSize: Int64;
begin
  Result:=GetTotalSize;
end;

constructor TChainedStream.Create(aChain: array of TStream; OwnsStreams: Boolean);

Var
  I : Integer;

begin
  SetLength(FStreams,Length(aChain));
  For I:=0 to Length(aChain)-1 do
    begin
    FStreams[i].Stream:=aChain[i];
    FStreams[i].Size:=-1;
    end;
  FCurrentStreamIdx:=0;
end;


destructor TChainedStream.Destroy;

Var
  I : Integer;

begin
  If OwnsStreams then
    For I:=0 to Length(FStreams) do
      FreeAndNil(FStreams[i].Stream);
  inherited Destroy;
end;

function TChainedStream.Read(var Buffer; Count: Longint): Longint;

Var
  aRead : Integer;
  P : PByte;

begin
  Result:=0;
  P:=@Buffer;
  While (Count>0) and Assigned(CurrentStream) do
    begin
    aRead:=CurrentStream.Read(P^, Count);
    Inc(P,aRead);
    Dec(Count,aRead);
    Inc(Result,aRead);
    Inc(FPosition,aRead);
    if Count>0 then
      if NextStream then
        CurrentStream.Position:=0
      else
        break;
    end;
end;

function TChainedStream.Write(const Buffer; Count: Longint): Longint;

Var
  aBufAvail,aToWrite,aWritten : Integer;
  P : PByte;
  
begin
  Result:=0;
  P:=@Buffer;
  While (Count>0) and Assigned(CurrentStream) do
    begin
    aBufAvail:=StreamSize-CurrentStream.Position;
    aToWrite:=Count;
    if aToWrite>aBufAvail then
      aToWrite:=aBufAvail;
    if aToWrite>0 then
      begin
      aWritten:=CurrentStream.Write(P^, aToWrite);
      Inc(P,aWritten);
      Dec(Count,aWritten);
      Inc(Result,aWritten);
      Inc(FPosition,aWritten);
      end;
    if (Count>0) then
      if NextStream then
        CurrentStream.Position:=0
      else
        break;
    end;
end;

function TChainedStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;

Var
  aOff : Int64;

  Procedure MoveForward(aStartPos : Int64; aOrigin : TSeekOrigin);

  begin
    while (aOff>StreamSize-aStartPos) do
      begin
      Dec(aOff,StreamSize-aStartPos);
      Inc(FPosition,StreamSize-aStartPos);
      if not NextStream then
        Break;
      aStartPos:=0;
      end;
    if CurrentStream=Nil then
      FCurrentStreamIdx:=Length(FStreams)-1;
    if aOff>StreamSize then
       aOff:=StreamSize;
    inc(FPosition,aOff);
    Result:=FPosition;
    CurrentStream.Seek(aOff,aOrigin);
  end;

  Procedure MoveBackward(aStartSize : Int64; aOrigin : TSeekOrigin);

  var
    aSize : Int64;

  begin
    aOff:=Abs(aOff);
    aSize:=aStartSize;
    while (aOff>aSize) do
      begin
      Dec(aOff,aSize);
      Dec(FPosition,aSize);
      if not PrevStream then
        Break
      else
        begin
        aSize:=StreamSize;
        CurrentStream.Seek(0,soEnd);
        end;
      end;
    if CurrentStream=Nil then
      FCurrentStreamIdx:=0;
    if aOff>aSize then
      aOff:=aSize;
    Dec(FPosition,aOff);
    Result:=FPosition;
    if (aOrigin=soCurrent) and (aStartSize<>StreamSize) then
      CurrentStream.Seek(-aOff,soCurrent)
    else
      CurrentStream.Seek(-aOff,soEnd);
  end;

begin
  if (Offset=0) and (Origin=soCurrent) then
    Exit(FPosition);
  aOff:=Offset;
  Case origin of
    soBeginning :
      begin
      FCurrentStreamIdx:=0;
      FPosition:=0;
      if aOff<0 then
        exit(FPosition);
      MoveForward(0,soBeginning);
      end;
    soCurrent :
      begin
      if aOff>0 then
        begin
        MoveForward(Currentstream.Position,soCurrent);
        end
      else
        begin
        MoveBackward(CurrentStream.Position,soCurrent)
        end;
      end;
    soEnd:
      begin
      FCurrentStreamIdx:=Length(FStreams)-1;
      FPosition:=GetTotalSize;
      MoveBackward(StreamSize,SoEnd)
      end;
  end;
end;

end.

