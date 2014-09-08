{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Michael Van Canneyt and Florian Klaempfl

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{ Fake stream that always returns empty data. Can be written to and will discard
all data.
Emulates a memorystream as far as needed: by writing past the end you can
increase its size; reading past the end gives an error}

unit nullstream;

interface

uses Classes;

type
  ENullStreamError = class(EStreamError);

  { TNullStream }

  TNullStream = class(THandleStream)
  private
    FPos : Int64;
    FSize: Int64;
  protected
    Function GetSize : int64; override;
    procedure SetSize(Const AValue: Int64); override;
    function  GetPosition: Int64; override;
    procedure InvalidSeek; override;
  public
    function Read(var Buffer; Count : LongInt) : Longint; override;
    function Write(const Buffer; Count : LongInt) : LongInt; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
    constructor Create;
  end;

implementation

Resourcestring
  SInvalidOperation = 'Cannot perform this operation on a NullStream.';

Function TNullStream.GetSize : int64;

begin
  Result:=FSize;
end;

procedure TNullStream.SetSize(const AValue: Int64);
begin
  FSize:=AValue;
  if FPos>FSize then
    FPos:=FSize;
end;

function TNullStream.GetPosition: Int64;
begin
  Result:=FPos;
end;

procedure TNullStream.InvalidSeek;
begin
  raise ENullStreamError.Create(SInvalidOperation);
end;

function TNullStream.Read(var Buffer; Count : LongInt) : Longint;
var
  RealCount: longint;
begin
  if (FPos+Count)>FSize then
    RealCount:=FSize-FPos
  else
    RealCount:=Count;
  FillChar(Buffer,RealCount,0);
  Result:=RealCount;
  Inc(FPos,RealCount);
end;

function TNullStream.Write(const Buffer; Count : LongInt) : LongInt;
begin
  Inc(FPos,Count);
  // Emulate a memorystream: increase size if needed
  If FPos>Fsize then
    FSize:=FPos;
end;


function TNullStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
var
  DesiredPos: int64;
begin
  if (Origin=soCurrent) and (Offset=0) then
    Result:=FPos
  else
    begin
    case Origin of
      soCurrent: DesiredPos:=FPos+Offset;
      soBeginning: DesiredPos:=Offset;
      soEnd: DesiredPos:=FSize-Offset;
    end;
    if (DesiredPos<0) or (DesiredPos>FSize) then
      InvalidSeek;
    FPos:=DesiredPos;
    end;
end;

constructor TNullStream.Create;
begin
  inherited create(0);
  FPos:=0;
  FSize:=0;
end;

end.
