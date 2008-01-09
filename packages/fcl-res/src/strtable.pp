{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    String table classes used internally by readers and writers

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit strtable;

{$MODE OBJFPC} {$H+}

interface

uses
  Classes;

type

  { TResStringTable }

  TResStringTable = class
  private
    fStartOfs : longword;
    fData : TMemoryStream;
    fUsed : boolean;
    function GetSize : longword;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(s : string) : longword;
    procedure Clear;
    procedure WriteToStream(aStream : TStream);
    property StartOfs : longword read fStartOfs write fStartOfs;
    property Size : longword read GetSize;
    property Used : boolean read fUsed;
  end;
  
  { TObjectStringTable }

  TObjectStringTable = class
  private
    fData : TMemoryStream;
    fStartOfs : longword;
    function GetSize : longword;
  protected
  public
    constructor Create(aStream : TStream; aSize : longword);
    destructor Destroy; override;
    function Get(aOffset : longword) : string;
    function Add(aString : string) : longword;
    procedure Clear;
    procedure WriteToStream(aStream : TStream);
    property Size : longword read GetSize;
    property StartOfs : longword read fStartOfs write fStartOfs;
  end;

implementation

{ TResStringTable }

function TResStringTable.GetSize: longword;
begin
  Result:=fData.Size;
end;

constructor TResStringTable.Create;
var b : byte;
begin
  fStartOfs:=0;
  fData:=TMemoryStream.Create;
  b:=0;
  fData.WriteBuffer(b,1);
  fUsed:=false;
end;

destructor TResStringTable.Destroy;
begin
  fData.Free;
end;

function TResStringTable.Add(s: string): longword;
var b : byte;
begin
  fUsed:=true;
  Result:=fData.Position;
  if s='' then Result:=0
  else
  begin
    fData.WriteBuffer(s[1],length(s));
    b:=0;
    fData.WriteBuffer(b,1);
  end;
end;

procedure TResStringTable.Clear;
begin
  fStartOfs:=0;
  fData.SetSize(1);
  fData.Position:=1;
  fUsed:=false;
end;

procedure TResStringTable.WriteToStream(aStream: TStream);
var oldpos : int64;
begin
  oldpos:=fData.Position;
  try
    fData.Position:=0;
    aStream.CopyFrom(fData,fData.Size);
  finally
    fData.Position:=oldpos;
  end;
end;

{ TObjectStringTable }

function TObjectStringTable.GetSize: longword;
begin
  Result:=fData.Size;
end;

constructor TObjectStringTable.Create(aStream: TStream; aSize: longword);
var b : byte;
begin
  fData:=TMemoryStream.Create;
  fData.Position:=0;
  if aStream=nil then
  begin
    b:=0;
    fData.WriteBuffer(b,1);
  end
  else
    fData.CopyFrom(aStream,aSize);
end;

destructor TObjectStringTable.Destroy;
begin
  fData.Free;
end;

function TObjectStringTable.Get(aOffset: longword): string;
var c : char;
begin
  Result:='';
  fData.Position:=aOffset;
  repeat
    fData.ReadBuffer(c,1);
    if c<>#0 then Result:=Result+c;
  until (c=#0) or (fData.Position>=fData.Size);
end;

function TObjectStringTable.Add(aString: string) : longword;
var b : byte;
begin
  Result:=fData.Position;
  if aString='' then Result:=0
  else
  begin
    fData.WriteBuffer(aString[1],length(aString));
    b:=0;
    fData.WriteBuffer(b,1);
  end;
end;

procedure TObjectStringTable.Clear;
begin
  fData.SetSize(1);
  fData.Position:=1;
end;

procedure TObjectStringTable.WriteToStream(aStream: TStream);
begin
  fData.Position:=0;
  aStream.CopyFrom(fData,fData.Size);
end;

end.
