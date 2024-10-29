{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    JSON Data structures writer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit jsonwriter;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data;
  {$ELSE}
  Classes, SysUtils, fpjson;
  {$ENDIF}

Type
  EJSONWriter = Class(EJSON);

  { TAbstractJSONWriter }

  { TAbstractJSONWriter }

  TAbstractJSONWriter = class(TObject)
  private
  Public
    procedure WriteValue(aValue: TStrings);
    procedure WriteValue(aValue: TJSONData);
    Procedure WriteProperty(const aName : TJSONStringType);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : Boolean);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : Integer);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : Int64);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : Double);
    Procedure WriteProperty(const aName : TJSONStringType; const aValue : String);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TStrings);
    Procedure WriteProperty(const aName : TJSONStringType; aValue : TJSONData);
    // Override in descendants
    procedure WriteValue; virtual; abstract;
    Procedure WriteValue(aValue : Boolean); virtual; abstract;
    Procedure WriteValue(aValue : Integer); virtual; abstract;
    Procedure WriteValue(aValue : Int64); virtual; abstract;
    Procedure WriteValue(aValue : Double); virtual; abstract;
    Procedure WriteValue(const aValue : String); virtual; abstract;
    Procedure StartProperty(const aName: string); virtual; abstract;
    Procedure EndProperty; virtual; abstract;
    Procedure StartArray; virtual; abstract;
    Procedure EndArray; virtual; abstract;
    Procedure NextElement; virtual; abstract;
    Procedure StartObject; virtual; abstract;
    Procedure EndObject; virtual; abstract;
    procedure Flush; virtual; abstract;
  end;


  { TJSONDataWriter }

  TJSONDataWriter = class(TAbstractJSONWriter)
  Private
    FStack : Array of TJSONData;
    FCount : Integer;
    FPropertyName : String;
  protected
    function CurrentStruct : TJSONData;
    Procedure PushData(Obj : TJSONData);
    Procedure PopData;
  Public
    destructor destroy; override;
    procedure EndArray; override;
    procedure EndObject; override;
    procedure EndProperty; override;
    procedure NextElement; override;
    procedure StartArray; override;
    procedure StartObject; override;
    procedure StartProperty(const aName: string); override;
    procedure WriteValue(aValue: Boolean); override;
    procedure WriteValue(aValue: Double); override;
    procedure WriteValue(aValue: Int64); override;
    procedure WriteValue(aValue: Integer); override;
    procedure WriteValue(const aValue: String); override;
    procedure WriteValue; override;
    procedure Flush; override;
    Function ExtractData : TJSONData;
  end;

  { TJSONStreamWriter }

  { TJSONStreamWriter }

  TJSONStreamWriter = class(TAbstractJSONWriter)
  private
    FStream: TStream;
    FCounts : Array of Integer;
    FLen : Integer;
    FStrictStrings: Boolean;
  Protected
    Procedure PushElCount;
    Procedure PopElCount;
    Procedure IncElCount;
    Function ElCount : Integer;
    procedure WriteString(const aString : TJSONStringType);
    Property Stream : TStream Read FStream;
  Public
    constructor create(aStream: TStream);
    Procedure WriteValue(); override;
    Procedure WriteValue(aValue : Boolean); override;
    Procedure WriteValue(aValue : Integer); override;
    Procedure WriteValue(aValue : Int64); override;
    Procedure WriteValue(aValue : Double); override;
    Procedure WriteValue(const aValue : String); override;
    Procedure StartProperty(const aName: string); override;
    Procedure EndProperty; override;
    Procedure StartArray; override;
    Procedure EndArray; override;
    Procedure NextElement; override;
    Procedure StartObject; override;
    Procedure EndObject; override;
    procedure Flush; override;
    property StrictStrings : Boolean Read FStrictStrings Write FStrictStrings;
  end;


implementation

resourcestring
  SErrNoObjectsOnStack = 'No objects created on stack';
  SPropertyNameAlreadySet = 'Cannot set property name to "%s", it is already set to "%s"';
  SErrNotAtStructuredValue = 'Current value is not a structured value';
  SErrCannotPop = 'Cannot pop, stack empty';
  SErrNoPushOnSimpleValue = 'Cannot push on top of non-structured value';
  SErrNoPropertyNameForPush = 'Cannot push to object without property name';

procedure TAbstractJSONWriter.WriteValue(aValue: TStrings);
var
  S : String;

begin
  StartArray;
  For S in aValue do
    begin
    NextElement;
    WriteValue(S);
    end;
  EndArray;
end;

procedure TAbstractJSONWriter.WriteValue(aValue: TJSONData);

var
  Enum : TJSONEnum;

begin
  Case aValue.JSONType of
    jtNull : WriteValue();
    jtBoolean : WriteValue(aValue.AsBoolean);
    jtString : WriteValue(aValue.AsString);
    jtNumber :
      case TJSONNumber(aValue).NumberType of
       ntInteger : WriteValue(aValue.AsInteger);
       ntInt64 : WriteValue(aValue.AsInt64);
       ntFloat : WriteValue(aValue.AsFloat);
       ntQword : WriteValue(aValue.AsInt64);
      end;
    jtObject :
      begin
      StartObject;
      For Enum in aValue do
        WriteProperty(Enum.Key,enum.Value);
      EndObject;
      end;
    jtArray :
      begin
      StartArray;
      For Enum in aValue do
        begin
        NextElement;
        WriteValue(Enum.Value);
        end;
      EndArray;
      end;
  end;
end;

procedure TAbstractJSONWriter.WriteProperty(const aName: TJSONStringType);
begin
  WriteValue(aName);
end;

procedure TAbstractJSONWriter.WriteProperty(const aName: TJSONStringType; aValue: Boolean);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TAbstractJSONWriter.WriteProperty(const aName: TJSONStringType; aValue: Integer);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TAbstractJSONWriter.WriteProperty(const aName: TJSONStringType; aValue: Int64);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TAbstractJSONWriter.WriteProperty(const aName: TJSONStringType; aValue: Double);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TAbstractJSONWriter.WriteProperty(const aName: TJSONStringType; const aValue: String);
begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;


procedure TAbstractJSONWriter.WriteProperty(const aName: TJSONStringType; aValue: TStrings);

begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty;
end;

procedure TAbstractJSONWriter.WriteProperty(const aName: TJSONStringType; aValue: TJSONData);

begin
  StartProperty(aName);
  WriteValue(aValue);
  EndProperty();
end;




{ TJSONDataWriter }

function TJSONDataWriter.ExtractData: TJSONData;
begin
  Flush;
  Result:=FStack[0];
  FStack[0]:=Nil;
end;

function TJSONDataWriter.CurrentStruct: TJSONData;
begin
  Result:=Nil;
  if FCount>0 then
    begin
    Result:=FStack[FCount-1];
    if not (Result.JSONType in StructuredJSONTypes) then
      Result:=Nil;
    end;
end;

procedure TJSONDataWriter.PushData(Obj: TJSONData);

var
  D : TJSONData;
  O : TJSONObject absolute D;
  A : TJSONArray absolute D;
  AddToStack : Boolean;

begin
  AddToStack:=(Obj.JSONType in StructuredJSONTypes) or (FCount=0);
  D:=CurrentStruct;
  if (D=Nil) then
    begin
    if (FCount>0) then
      Raise EJSONWriter.Create(SErrNoPushOnSimpleValue);
    end
  else
    Case D.JSONType of
    jtObject:
      begin
      if FPropertyName = '' then
        Raise EJSONWriter.Create(SErrNoPropertyNameForPush);
      O.Add(FPropertyName,Obj);
      FPropertyName:='';
      end;
    jtArray:
      begin
      A.Add(Obj);
      FPropertyName:='';
      end;
    end;
  if AddToStack then
    begin
    if FCount=Length(FStack) then
      SetLength(FStack,FCount+10);
    FStack[FCount]:=Obj;
    Inc(FCount);
    end;
end;

procedure TJSONDataWriter.PopData;
begin
 if FCount=0 then
   Raise EJSONWriter.Create(SErrCannotPop);
 Dec(FCount);
end;

destructor TJSONDataWriter.destroy;
begin
  FreeAndNil(FStack[0]);
  inherited destroy;
end;

procedure TJSONDataWriter.EndArray;
begin
  PopData;
end;

procedure TJSONDataWriter.EndObject;
begin
  PopData;
end;

procedure TJSONDataWriter.EndProperty;
begin
  If CurrentStruct=Nil then
    Raise EJSONWriter.Create(SErrNotAtStructuredValue);
end;

procedure TJSONDataWriter.NextElement;
begin
  If CurrentStruct=Nil then
    Raise EJSONWriter.Create(SErrNotAtStructuredValue);
end;

procedure TJSONDataWriter.StartArray;
begin
  PushData(TJSONArray.Create);
end;

procedure TJSONDataWriter.StartObject;
begin
  PushData(TJSONObject.Create);
end;

procedure TJSONDataWriter.StartProperty(const aName: string);
begin
  if FPropertyName<>'' then
    Raise EJSONWriter.CreateFmt(SPropertyNameAlreadySet,[aName,FPropertyName]);
  FPropertyName:=aName;
end;

procedure TJSONDataWriter.WriteValue(aValue: Boolean);
begin
  PushData(TJSONBoolean.Create(aValue));
end;

procedure TJSONDataWriter.WriteValue(aValue: Double);
begin
  PushData(TJSONFloatNumber.Create(aValue));
end;

procedure TJSONDataWriter.WriteValue(aValue: Int64);
begin
  PushData(TJSONInt64Number.Create(aValue));
end;

procedure TJSONDataWriter.WriteValue(aValue: Integer);
begin
  PushData(TJSONIntegerNumber.Create(aValue));
end;

procedure TJSONDataWriter.WriteValue(const aValue: String);
begin
  PushData(TJSONString.Create(aValue));
end;

procedure TJSONDataWriter.WriteValue;
begin
  PushData(TJSONNull.Create);
end;

procedure TJSONDataWriter.Flush;
begin
  if (Length(FStack)=0) or Not Assigned(FStack[0]) then
    Raise EJSONWriter.Create(SErrNoObjectsOnStack);
end;

{ TJSONStreamWriter }

procedure TJSONStreamWriter.PushElCount;
begin
  if FLen=Length(FCounts) then
    SetLength(FCounts,FLen+10);
  FCounts[FLen]:=0;
  Inc(Flen);
end;

procedure TJSONStreamWriter.PopElCount;
begin
  if FLen>0 then
    Dec(FLen);
end;

procedure TJSONStreamWriter.IncElCount;
begin
  if Flen>0 then
    Inc(FCounts[FLen-1]);
end;

function TJSONStreamWriter.ElCount: Integer;
begin
  Result:=FCounts[FLen-1];
end;

procedure TJSONStreamWriter.WriteString(const aString: TJSONStringType);
begin
  if Length(aString)>0 then
    FStream.WriteBuffer(aString[1],Length(aString))
end;

constructor TJSONStreamWriter.create(aStream: TStream);
begin
  FStream:=aStream;
end;

procedure TJSONStreamWriter.WriteValue();
begin
  WriteString('null');
end;

procedure TJSONStreamWriter.WriteValue(aValue: Boolean);
begin
  WriteString(BoolToStr(aValue,'true','false'));
end;

procedure TJSONStreamWriter.WriteValue(aValue: Integer);
begin
  WriteString(IntToStr(aValue));
end;

procedure TJSONStreamWriter.WriteValue(aValue: Int64);
begin
  WriteString(IntToStr(aValue));
end;

procedure TJSONStreamWriter.WriteValue(aValue: Double);

var
  s : String;
begin
  Str(aValue,s);
  WriteString(S);
end;

procedure TJSONStreamWriter.WriteValue(const aValue: String);

begin
  WriteString('"'+StringToJSONString(aValue,StrictStrings)+'"');
end;

procedure TJSONStreamWriter.StartProperty(const aName: string);
begin
  if ElCount>0 then
    NextElement;
  WriteString('"'+StringToJSONString(aName,StrictStrings)+'":');
  IncElCount;
end;

procedure TJSONStreamWriter.EndProperty;
begin
  // Nothing
end;

procedure TJSONStreamWriter.StartArray;
begin
  WriteString('[');
  PushElCount;
end;

procedure TJSONStreamWriter.EndArray;
begin
  PopElCount;
  WriteString(']');
end;

procedure TJSONStreamWriter.NextElement;
begin
  if ElCount>0 then
    WriteString(',');
  IncElCount;
end;


procedure TJSONStreamWriter.StartObject;
begin
  WriteString('{');
  PushElCount;
end;

procedure TJSONStreamWriter.EndObject;
begin
  WriteString('}');
  PopElCount;
end;

procedure TJSONStreamWriter.Flush;
begin
  // Do nothing
end;

end.

