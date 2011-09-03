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

unit iostream;

interface

uses Classes;

type
  TIOSType = (iosInput,iosOutPut,iosError);
  EIOStreamError = class(EStreamError);

  { TIOStream }

  TIOStream = class(THandleStream)
  private
    FType : longint;
    FPos : Int64;
    zIOSType : TIOSType;
  protected
    procedure SetSize(const NewSize: Int64); override;
    function  GetPosition: Int64; override;
    procedure InvalidSeek; override;
  public
    constructor Create(aIOSType : TiosType);
    function Read(var Buffer; Count : LongInt) : Longint; override;
    function Write(const Buffer; Count : LongInt) : LongInt; override;
    function Seek(const Offset: int64; Origin: TSeekOrigin): int64; override;
    end;

implementation

const
  SReadOnlyStream = 'Cannot write to an input stream.';
  SWriteOnlyStream = 'Cannot read from an output stream.';
  SInvalidOperation = 'Cannot perform this operation on a IOStream.';

procedure TIOStream.SetSize(const NewSize: Int64);
begin
  raise EIOStreamError.Create(SInvalidOperation);
end;

function TIOStream.GetPosition: Int64;
begin
  Result:=FPos;
end;

procedure TIOStream.InvalidSeek;
begin
  raise EIOStreamError.Create(SInvalidOperation);
end;

constructor TIOStream.Create(aIOSType : TIOSType);
begin
{$ifdef windows}
  case aIOSType of
    iosInput : FType := StdInputHandle;
    iosOutput : FType := StdOutputHandle;
    iosError : FType := StdErrorHandle;
  end;
{$else}
  FType := Ord(aIOSType);
{$endif}
  inherited Create(FType);
  zIOSType := aIOSType;
end;

function TIOStream.Read(var Buffer; Count : LongInt) : Longint;
begin
  if (zIOSType <> iosInput) then
    raise EIOStreamError.Create(SWriteOnlyStream)
  else begin
    result := inherited Read(Buffer,Count);
    inc(FPos,result);
  end;
end;

function TIOStream.Write(const Buffer; Count : LongInt) : LongInt;
begin
  if (zIOSType = iosInput) then
    raise EIOStreamError.Create(SReadOnlyStream)
  else begin
    result := inherited Write(Buffer,Count);
    inc(FPos,result);
  end;
end;


function TIOStream.Seek(const Offset: int64; Origin: TSeekOrigin): int64;
begin
  if (Origin=soCurrent) and (Offset=0) then
    Result:=FPos
  else
    begin
    if zIOSType in [iosOutput,iosError] then
      InvalidSeek;
    FakeSeekForward(Offset,Origin,FPos);
    Result:=FPos;
    end;
end;

end.
