{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Basic JSON serialization

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Json;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, Data.Db,
{$ELSE}
  Classes, SysUtils, db,
{$ENDIF}
  Opf.Types;

Type

  { IOPSerializer }

  IOPSerializer = interface ['{5DC6652A-5085-4B6F-9929-25297A3F15A5}']
    { Returns the object's properties as JSON text }
    function Serialize(aObject: TObject): UTF8String;
    { Fills the object's properties from JSON text }
    procedure Deserialize(const aJSON: UTF8String; aObject: TObject);
  end;

  { TJsonRttiSerializer }

  TJsonRttiSerializer = class(TInterfacedObject, IOPSerializer)
  public
    { Raises a JSON error for the given operation with a fixed message }
    procedure RaiseError(const aOperation, aReason: string); virtual; overload;
    { Raises a JSON error for the given operation with a formatted message }
    procedure RaiseError(const aOperation, aFmt: string; const aArgs: array of const); virtual; overload;
    { Returns the object (or collection) properties as JSON text }
    function Serialize(aObject: TObject): UTF8String;
    { Fills the object (or collection) from JSON text }
    procedure Deserialize(const aJSON: UTF8String; aObject: TObject);
  end;

{ Returns the serializer used to convert objects to and from JSON }
function OPSerializer: IOPSerializer;
{ Replaces the serializer used to convert objects to and from JSON }
procedure SetOPSerializer(const aSerializer: IOPSerializer);

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  FpJson.Data, FpJson.Rtti,
{$ELSE}
  fpjson, fpjsonrtti,
{$ENDIF}
  Opf.Consts;

var
  _Serializer: IOPSerializer = nil;

function OPSerializer: IOPSerializer;
begin
  if _Serializer = nil then
  begin
    EnterCriticalSection(OPFSingletonLock);
    try
      if _Serializer = nil then
        _Serializer := TJsonRttiSerializer.Create;
    finally
      LeaveCriticalSection(OPFSingletonLock);
    end;
  end;
  Result := _Serializer;
end;

procedure SetOPSerializer(const aSerializer: IOPSerializer);
begin
  _Serializer := aSerializer;
end;

{ TJsonRttiSerializer }

procedure TJsonRttiSerializer.RaiseError(const aOperation, aReason: string);
begin
  OPFRaiseError(EOPFJson, ClassName, aOperation, aReason);
end;

procedure TJsonRttiSerializer.RaiseError(const aOperation, aFmt: string; const aArgs: array of const);
begin
  OPFRaiseError(EOPFJson, ClassName, aOperation, aFmt, aArgs);
end;

function TJsonRttiSerializer.Serialize(aObject: TObject): UTF8String;
var
  S: TJSONStreamer;
  D: TJSONData;
begin
  if aObject = nil then
    RaiseError('Serialize', SErrSerializeNilObject);
  S := TJSONStreamer.Create(nil);
  try
    if aObject is TCollection then
      D := S.StreamCollection(TCollection(aObject))
    else
      D := S.ObjectToJSON(aObject);
    try
      Result := D.FormatJSON;
    finally
      D.Free;
    end;
  finally
    S.Free;
  end;
end;

{ Replace the items of aCollection by those of aArr. When an element cannot
  be read, the collection gets its previous items back. }

procedure FillCollection(aDestreamer: TJSONDestreamer; aArr: TJSONArray; aCollection: TCollection);
var
  S: TJSONStreamer;
  Old: TJSONData;
begin
  S := TJSONStreamer.Create(nil);
  try
    Old := S.StreamCollection(aCollection);
  finally
    S.Free;
  end;
  try
    aCollection.BeginUpdate;
    try
      aCollection.Clear;
      try
        aDestreamer.JSONToCollection(aArr, aCollection);
      except
        aCollection.Clear;
        aDestreamer.JSONToCollection(Old, aCollection);
        raise;
      end;
    finally
      aCollection.EndUpdate;
    end;
  finally
    Old.Free;
  end;
end;

procedure TJsonRttiSerializer.Deserialize(const aJSON: UTF8String; aObject: TObject);
var
  D: TJSONDestreamer;
  J: TJSONData;
begin
  if aObject = nil then
    RaiseError('Deserialize', SErrDeserializeNilObject);
  if aJSON = '' then
  begin
    if aObject is TCollection then
      TCollection(aObject).Clear;
    Exit;
  end;
  D := TJSONDestreamer.Create(nil);
  try
    D.Options := D.Options + [jdoIgnoreNulls];
    try
      J := GetJSON(aJSON);
    except
      on E: Exception do
        RaiseError('Deserialize', SErrDeserializeInvalidJSON, [E.Message]);
    end;
    if J = nil then
      RaiseError('Deserialize', SErrDeserializeInvalidJSON, ['result is nil']);
    try
      if aObject is TCollection then
      begin
        // Check first: a type mismatch leaves the collection as it was
        if not (J is TJSONArray) then
          RaiseError('Deserialize', SErrDeserializeTypeMismatch, ['TJSONArray', J.ClassName]);
        FillCollection(D, TJSONArray(J), TCollection(aObject));
      end
      else
      begin
        if not (J is TJSONObject) then
          RaiseError('Deserialize', SErrDeserializeTypeMismatch, ['TJSONObject', J.ClassName]);
        D.JSONToObject(TJSONObject(J), aObject);
      end;
    finally
      J.Free;
    end;
  finally
    D.Free;
  end;
end;

finalization
  _Serializer := nil;

end.
