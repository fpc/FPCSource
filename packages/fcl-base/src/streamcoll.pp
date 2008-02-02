{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit streamcoll;

interface

uses
  Classes,SysUtils;

type
  TStreamCollectionItem = Class(TCollectionItem)
  Protected
    Procedure WriteInteger(S : TStream; AValue : Integer);
    Procedure WriteBoolean(S : TStream; AValue : Boolean);
    Procedure WriteString(S : TStream; AValue : String);
    Procedure WriteCurrency(S : TStream; AValue : Currency);
    Procedure WriteDateTime(S : TStream; AValue : TDateTime);
    Procedure WriteFloat(S : TStream; AValue : Double);
    Function ReadInteger(S : TStream) : Integer;
    Function ReadBoolean(S : TStream) : Boolean;
    Function ReadString(S : TStream) : String;
    Function ReadCurrency(S : TStream) : Currency;
    Function ReadDateTime(S : TStream) : TDateTime;
    Function ReadFloat(S : TStream) : Double;
    Procedure LoadFromStream(S : TStream; Streamversion : Integer); virtual; abstract;
    Procedure SaveToStream(S : TStream); virtual; abstract;
  end;

  TStreamCollection = Class(TCollection)
  Private
    FStreaming : Boolean;
  Protected
    Procedure WriteInteger(S : TStream; AValue : Integer);
    Procedure WriteBoolean(S : TStream; AValue : Boolean);
    Procedure WriteString(S : TStream; AValue : String);
    Procedure WriteCurrency(S : TStream; AValue : Currency);
    Procedure WriteDateTime(S : TStream; AValue : TDateTime);
    Procedure WriteFloat(S : TStream; AValue : Double);
    Function ReadInteger(S : TStream) : Integer;
    Function ReadBoolean(S : TStream) : Boolean;
    Function ReadString(S : TStream) : String;
    Function ReadCurrency(S : TStream) : Currency;
    Function ReadDateTime(S : TStream) : TDateTime;
    Function ReadFloat(S : TStream) : Double;
    Procedure DoSaveToStream(S : TStream); virtual;
    Function CurrentStreamVersion : Integer; Virtual;
    Procedure DoLoadFromStream(S : TStream; Streamversion : Integer); virtual;
  Public
    Procedure LoadFromStream(S : TStream);
    Procedure SaveToStream(S : TStream);
    Property Streaming : Boolean Read FStreaming;
  end;


  EStreamColl = Class(Exception);

Procedure ColWriteInteger(S : TStream; AValue : Integer);
Procedure ColWriteBoolean(S : TStream; AValue : Boolean);
Procedure ColWriteString(S : TStream; AValue : String);
Procedure ColWriteCurrency(S : TStream; AValue : Currency);
Procedure ColWriteDateTime(S : TStream; AValue : TDateTime);
Procedure ColWriteFloat(S : TStream; AValue : Double);
Function ColReadInteger(S : TStream) : Integer;
Function ColReadBoolean(S : TStream) : Boolean;
Function ColReadString(S : TStream) : String;
Function ColReadCurrency(S : TStream) : Currency;
Function ColReadDateTime(S : TStream) : TDateTime;
Function ColReadFloat(S : TStream) : Double;

implementation

Resourcestring
  SErrIllegalStreamVersion = 'Illegal stream version: %d > %d.';

Procedure ColWriteInteger(S : TStream; AValue : Integer);

begin
  S.WriteBuffer(AValue,SizeOf(Integer));
end;

Procedure ColWriteBoolean(S : TStream; AValue : Boolean);

begin
  ColWriteInteger(S,Ord(AValue));
end;

Procedure ColWriteString(S : TStream; AValue : String);

Var
  L : Integer;

begin
  L:=Length(AValue);
  ColWriteInteger(S,L);
  If (L>0) then
    S.WriteBuffer(AValue[1],L);
end;

Procedure ColWriteCurrency(S : TStream; AValue : Currency);

begin
  S.WriteBuffer(AValue,SizeOf(Currency));
end;

Procedure ColWriteDateTime(S : TStream; AValue : TDateTime);

begin
  S.WriteBuffer(AValue,SizeOf(TDateTime));
end;

Procedure ColWriteFloat(S : TStream; AValue : Double);

begin
  S.WriteBuffer(AValue,SizeOf(Double));
end;

Function ColReadInteger(S : TStream) : Integer;

begin
  S.ReadBuffer(Result,SizeOf(Integer));
end;

Function ColReadBoolean(S : TStream) : Boolean;

Var
  I : Integer;

begin
  S.ReadBuffer(I,SizeOf(Integer));
  Result:=(I<>0);
end;

Function ColReadString(S : TStream) : String;

Var
  L : Integer;

begin
  L:=ColReadInteger(S);
  SetLength(Result,L);
  If (L>0) then
    S.ReadBuffer(Result[1],L);
end;

Function ColReadCurrency(S : TStream) : Currency;

begin
  S.ReadBuffer(Result,SizeOf(Currency));
end;

Function ColReadDateTime(S : TStream) : TDateTime;

begin
  S.ReadBuffer(Result,SizeOf(TDateTime));
end;

Function ColReadFloat(S : TStream) : Double;

begin
  S.ReadBuffer(Result,SizeOf(Double));
end;

{ TStreamCollectionItem }

function TStreamCollectionItem.ReadBoolean(S: TStream): Boolean;
begin
  Result:=ColReadBoolean(S);
end;

function TStreamCollectionItem.ReadCurrency(S: TStream): Currency;
begin
  Result:=ColReadCurrency(S);
end;

function TStreamCollectionItem.ReadDateTime(S: TStream): TDateTime;
begin
  Result:=ColReadDateTime(S);
end;

function TStreamCollectionItem.ReadFloat(S: TStream): Double;
begin
  Result:=ColReadFloat(S);
end;

function TStreamCollectionItem.ReadInteger(S: TStream): Integer;
begin
  Result:=ColReadinteger(S);
end;

function TStreamCollectionItem.ReadString(S: TStream): String;
begin
  Result:=ColReadString(S);
end;

procedure TStreamCollectionItem.WriteBoolean(S: TStream; AValue: Boolean);
begin
  ColWriteBoolean(S,AValue);
end;

procedure TStreamCollectionItem.WriteCurrency(S: TStream;
  AValue: Currency);
begin
  ColWriteCurrency(S,AValue);
end;

procedure TStreamCollectionItem.WriteDateTime(S: TStream;
  AValue: TDateTime);
begin
  ColWriteDateTime(S,AValue);
end;

procedure TStreamCollectionItem.WriteFloat(S: TStream; AValue: Double);
begin
  ColWriteFloat(S,AValue);
end;

procedure TStreamCollectionItem.WriteInteger(S: TStream; AValue: Integer);
begin
  ColWriteInteger(S,AValue);
end;

procedure TStreamCollectionItem.WriteString(S: TStream; AValue: String);
begin
  ColWriteString(S,AValue);
end;

{ TStreamCollection }

function TStreamCollection.CurrentStreamVersion: Integer;
begin
  Result:=0;
end;

procedure TStreamCollection.DoLoadFromStream(S: TStream;
  Streamversion: Integer);
begin
  If (Streamversion>CurrentStreamVersion) then
    Raise EStreamColl.CreateFmt(SErrIllegalStreamVersion,[Streamversion,CurrentStreamVersion]);
end;

procedure TStreamCollection.DoSaveToStream(S: TStream);
begin
  // Do nothing.
end;

procedure TStreamCollection.LoadFromStream(S: TStream);

Var
  I,V,C : Integer;

begin
  FStreaming:=True;
  Try
    V:=ReadInteger(S);
    DoLoadFromStream(S,V);
    Clear;
    C:=ReadInteger(S);
    For I:=1 to C do
      With Add as TStreamCollectionItem do
        LoadFromStream(S,V);
  Finally
    FStreaming:=False;
  end;
end;

function TStreamCollection.ReadBoolean(S: TStream): Boolean;
begin
  Result:=ColReadBoolean(S);
end;

function TStreamCollection.ReadCurrency(S: TStream): Currency;
begin
  Result:=ColReadCurrency(S);
end;

function TStreamCollection.ReadDateTime(S: TStream): TDateTime;
begin
  Result:=ColReadDateTime(S);
end;

function TStreamCollection.ReadFloat(S: TStream): Double;
begin
  Result:=ColReadFloat(S);
end;

function TStreamCollection.ReadInteger(S: TStream): Integer;
begin
  Result:=ColReadInteger(S);
end;

function TStreamCollection.ReadString(S: TStream): String;
begin
  Result:=ColReadString(S);
end;

procedure TStreamCollection.SaveToStream(S: TStream);

Var
  I : Integer;

begin
  FStreaming:=True;
  Try
    WriteInteger(S,CurrentStreamVersion);
    DoSaveToStream(S);
    WriteInteger(S,Count);
    For I:=0 to Count-1 do
      With TStreamCollectionItem(Items[i]) do
        SaveToStream(S);
  Finally
    FStreaming:=False;
  end;
end;

procedure TStreamCollection.WriteBoolean(S: TStream; AValue: Boolean);
begin
  ColWriteBoolean(S,AValue);
end;

procedure TStreamCollection.WriteCurrency(S: TStream; AValue: Currency);
begin
  ColWriteCurrency(S,AValue);
end;

procedure TStreamCollection.WriteDateTime(S: TStream; AValue: TDateTime);
begin
  ColWriteDateTime(S,AValue);
end;

procedure TStreamCollection.WriteFloat(S: TStream; AValue: Double);
begin
  ColWriteFloat(S,AValue);
end;

procedure TStreamCollection.WriteInteger(S: TStream; AValue: Integer);
begin
  ColWriteInteger(S,AValue);
end;

procedure TStreamCollection.WriteString(S: TStream; AValue: String);
begin
  ColWriteString(S,AValue);
end;


end.
