{
    This file is part of the Free Component Library

    Implementation of a TJSONComparer class
    Copyright (c) 2024 Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit jsoncomparer;
{$ENDIF}

{$mode ObjFPC}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data;
{$ELSE}
  Classes, SysUtils, fpjson;
{$ENDIF}

type

  { TJSONComparer }

  TJSONComparer = Class(TObject)
  private
    FEpsilon: Double;
    FErrors : TStrings;
    procedure addDiff(const aPath, aDiff: String);
    procedure addDiff(const aPath: String; const aFmt: String; const aArgs: array of const);
  Protected
    Procedure CompareArray(const aPath : String; aExpected,aActual : TJSONArray);
    Procedure CompareObject(const aPath : String; aExpected,aActual : TJSONObject);
    Procedure doCompare(const aPath : String; aExpected,aActual : TJSONData);
  Public
    constructor create(aErrors : TStrings);
    Procedure Compare(aExpected,aActual : TJSONData);
    class Procedure Compare(aExpected,aActual : TJSONData; aErrors : TStrings);
    property Epsilon : Double Read FEpsilon Write FEpsilon;
  end;

implementation

resourcestring
  SExpectedArrayElements =  'Expected %d array elements, got %d';
  SExpectedObjectKeys = 'Expected %d object keys, got %d';
  SElementExpected = 'Element expected, but not present.';
  SElementUnexpected = 'Element present, but not expected.';
  SElementTypesDiffer = 'Element types differ, expected %s, got: %s';
  SElementValueDiffers = 'Expected %s, got: %s';
  SElementIntegerValueDiffers = 'expected %d, got: %d';

{ TJSONComparer }

procedure TJSONComparer.addDiff(const aPath,aDiff : String);

begin
  FErrors.Add(aPath+': '+aDiff);
end;

procedure TJSONComparer.addDiff(const aPath : String; const aFmt: String; const aArgs: array of const);
begin
  AddDiff(aPath,Format(aFmt,aArgs));
end;

procedure TJSONComparer.CompareArray(const aPath: String; aExpected, aActual: TJSONArray);
var
  i,lCount : integer;
begin
  lCount:=aExpected.Count;
  if lCount<>aActual.Count then
    AddDiff(aPath,SExpectedArrayElements,[aExpected.Count,aActual.Count]);
  if (aActual.Count<lCount) then
    lCount:=aActual.Count;
  for I:=0 to lCount-1 do
    DoCompare(Format('%s[%d]',[aPath,I]),aExpected[i],aActual[i]);
end;

procedure TJSONComparer.CompareObject(const aPath: String; aExpected, aActual: TJSONObject);
var
  i,lCount : integer;
  Names : TStringList;
  lExpect,lActual : TJSONData;
  lName : String;

begin
  lCount:=aExpected.Count;
  if lCount<>aActual.Count then
    AddDiff(aPath,SExpectedObjectKeys,[aExpected.Count,aActual.Count]);
  Names:=TStringList.Create;
  try
    for I:=0 to aExpected.Count-1 do
      Names.Add(aExpected.Names[i]);
    Names.CaseSensitive:=True;
    Names.Sorted:=true;
    For I:=0 to Names.Count-1 do
      begin
      lName:=Names[i];
      lExpect:=aExpected.Elements[lName];
      lActual:=aActual.Find(lName);
      DoCompare(Format('%s.%s',[aPath,lName]),lExpect,lActual);
      end;
    For I:=0 to aActual.Count-1 do
      begin
      lName:=aActual.Names[i];
      if Names.IndexOf(lName)=-1 then
        DoCompare(Format('%s.%s',[aPath,lName]),Nil,aActual.Items[i]);
      end;
  finally
    Names.Free;
  end;
end;

procedure TJSONComparer.doCompare(const aPath: String; aExpected, aActual: TJSONData);
begin
  if (aActual=Nil) and (aExpected=Nil) then
    exit;
  if (aActual=Nil) then
    begin
    AddDiff(aPath,SElementExpected);
    Exit;
    end
  else if (aExpected=Nil) then
    begin
    AddDiff(aPath,SElementUnexpected);
    Exit;
    end;
  if aActual.JSONType<>aExpected.JSONType then
    begin
    AddDiff(aPath,SElementTypesDiffer,[JSONTypeName(aExpected.JSONType),JSONTypeName(aActual.JSONType)]);
    exit;
    end;
  case aActual.JSONType of
    jtNull: ;
    jtBoolean :
      if aActual.AsBoolean<>aExpected.AsBoolean then
        AddDiff(aPath,SElementValueDiffers ,[aExpected.AsJSON,aActual.AsJSON]);
    jtNumber :
      begin
      if TJSONNumber(aExpected).NumberType=TJSONNumber(aActual).NumberType then
        begin
        case TJSONNumber(aExpected).NumberType of
          ntInteger :
            if (aExpected.AsInteger<>aActual.AsInteger) then
              AddDiff(aPath,SElementIntegerValueDiffers,[aExpected.AsInteger<>aActual.AsInteger]);
          ntInt64:
            if (aExpected.AsInt64<>aActual.AsInt64) then
              AddDiff(aPath,SElementIntegerValueDiffers,[aExpected.AsInteger<>aActual.AsInteger]);
          ntQWord:
            if (aExpected.AsQWord<>aActual.AsQWord) then
              AddDiff(aPath,SElementIntegerValueDiffers,[aExpected.AsInteger<>aActual.AsInteger]);
          ntFloat:
          if Abs(aExpected.AsFloat-aActual.AsFloat)>Epsilon then
            AddDiff(aPath,SElementValueDiffers,[aExpected.AsJSON,aActual.AsJSON]);
        end
        end
      else
        if Abs(aExpected.AsFloat-aActual.AsFloat)>Epsilon then
          AddDiff(aPath,SElementValueDiffers,[aExpected.AsJSON,aActual.AsJSON]);
      end;
    jtString :
      begin
      if aExpected.AsString<>aActual.AsString then
        AddDiff(aPath,SElementValueDiffers,[aExpected.AsString,aActual.AsString])
      end;
    jtArray:
      CompareArray(aPath,TJSONArray(aExpected),TJSONArray(aActual));
    jtObject:
      CompareObject(aPath,TJSONObject(aExpected),TJSONObject(aActual));
  end;
end;

constructor TJSONComparer.create(aErrors: TStrings);

begin
  FErrors:=aErrors;
  FEpsilon:=1E-8;
end;

procedure TJSONComparer.Compare(aExpected, aActual: TJSONData);
begin
  DoCompare('#',aExpected,aActual);
end;

class procedure TJSONComparer.Compare(aExpected, aActual: TJSONData; aErrors: TStrings);

var
  cmp: TJSONComparer;
begin
  cmp:=TJSONComparer.Create(aErrors);
  try
    cmp.Compare(aExpected,aActual);
  finally
    cmp.Free;
  end;
end;

end.

