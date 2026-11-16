{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    Variant from/to JSON support

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
 unit Opf.Variants;

{$mode objfpc}{$H+}

interface

uses
  Opf.Types;

var
  (* JSON has no type for Currency, TDateTime and Unassigned, and writes a 1-D
    array of 1-D arrays like a 2-D array. With True, VariantToJSON writes such
    values as a tagged object, {"$vartype": "currency", "value": "1.2345"},
    so JSONToVariant gives back the same variant type (default). False writes
    plain JSON, e.g. for other readers: a currency is then read back as a
    Double, a date as its ISO 8601 text, Unassigned as Null and a rectangular
    array of arrays as a multi-dimensional array. JSONToVariant always reads
    tagged values. *)
  OPFVariantJSONTypeTags: Boolean = True;

// The variant value as JSON text
function VariantToJSON(const aValue: Variant): UTF8String;
// The variant value read back from JSON text
function JSONToVariant(const aJSON: UTF8String): Variant;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.Variants, System.Math, FpJson.Data,
{$ELSE}
  SysUtils, Variants, Math, fpjson,
{$ENDIF}
  Opf.Consts;

const
  // Member names of a tagged value
  STagType = '$vartype';
  STagValue = 'value';
  // ISO 8601 date/time format for varDate
  SISODateTimeFormat = 'yyyy"-"mm"-"dd"T"hh":"nn":"ss"."zzz';

var
  // Invariant decimal separator for numbers written as JSON
  JSONFormatSettings: TFormatSettings;

function StringToJSONLiteral(const aStr: UTF8String): UTF8String;
var
  JS: TJSONString;
begin
  JS := TJSONString.Create(aStr);
  try
    Result := JS.AsJSON;
  finally
    JS.Free;
  end;
end;

(* A tagged value: {"$vartype": aType, "value": aValueJSON} (no value when
  aValueJSON is empty) *)

function TaggedJSON(const aType, aValueJSON: UTF8String): UTF8String;
begin
  Result:='{'+StringToJSONLiteral(STagType)+': '+StringToJSONLiteral(aType);
  if aValueJSON<>'' then
    Result:=Result+', '+StringToJSONLiteral(STagValue)+': '+aValueJSON;
  Result:=Result+'}';
end;

function VariantArrayToJSON(const aValue: Variant; aDim, aDimCount: Integer;
  var aIndices: array of Longint): UTF8String;
var
  Arr: TJSONArray;
  Lo, Hi, I: Longint;
  Elem: Variant;
begin
  Arr := TJSONArray.Create;
  try
    Lo := VarArrayLowBound(aValue, aDim);
    Hi := VarArrayHighBound(aValue, aDim);
    for I := Lo to Hi do
    begin
      aIndices[aDim - 1] := I;
      if aDim < aDimCount then
        Arr.Add(GetJSON(VariantArrayToJSON(aValue, aDim + 1, aDimCount, aIndices)))
      else
      begin
        Elem := VarArrayGet(aValue, aIndices);
        Arr.Add(GetJSON(VariantToJSON(Elem)));
      end;
    end;
    Result := Arr.AsJSON;
  finally
    Arr.Free;
  end;
end;

function VariantToJSON(const aValue: Variant): UTF8String;
var
  VT: TVarType;
  DimCount, Lo, Hi, I: Integer;
  Arr: TJSONArray;
  Elem: Variant;
  Indices: array of Longint;
  HasArrayElem: Boolean;
begin
  if VarIsArray(aValue) then
  begin
    DimCount := VarArrayDimCount(aValue);
    if DimCount = 1 then
    begin
      HasArrayElem := False;
      Arr := TJSONArray.Create;
      try
        Lo := VarArrayLowBound(aValue, 1);
        Hi := VarArrayHighBound(aValue, 1);
        for I := Lo to Hi do
        begin
          Elem := VarArrayGet(aValue, [I]);
          HasArrayElem := HasArrayElem or VarIsArray(Elem);
          Arr.Add(GetJSON(VariantToJSON(Elem)));
        end;
        Result := Arr.AsJSON;
      finally
        Arr.Free;
      end;
      // Not to be read back as a multi-dimensional array
      if HasArrayElem and OPFVariantJSONTypeTags then
        Result := TaggedJSON('array', Result);
    end
    else
    begin
      SetLength(Indices, DimCount);
      Result := VariantArrayToJSON(aValue, 1, DimCount, Indices);
    end;
    Exit;
  end;
  VT := VarType(aValue);
  case VT of
    varNull:
      Result := 'null';
    varEmpty:
      if OPFVariantJSONTypeTags then
        Result := TaggedJSON('empty', '')
      else
        Result := 'null';
    varBoolean:
      if Boolean(aValue) then
        Result := 'true'
      else
        Result := 'false';
    varSmallInt, varInteger, varShortInt, varByte, varWord, varLongWord, varInt64:
      Result := IntToStr(Int64(aValue));
    varQWord:
      Result := IntToStr(QWord(aValue));
    varSingle, varDouble:
      begin
        // JSON has no NaN or infinity
        if IsNan(Double(aValue)) or IsInfinite(Double(aValue)) then
          OPFRaiseError(EOPFJson, 'Opf.Variants', 'VariantToJSON',
            'NaN and infinite values cannot be written as JSON', []);
        Str(Double(aValue), Result);
        Result := Trim(Result);
      end;
    varCurrency:
      // Exact: through Double a currency can lose its last digits
      if OPFVariantJSONTypeTags then
        Result := TaggedJSON('currency', StringToJSONLiteral(CurrToStr(Currency(aValue), JSONFormatSettings)))
      else
        Result := CurrToStr(Currency(aValue), JSONFormatSettings);
    varDate:
      // ISO 8601 text: JSON has no date type
      if OPFVariantJSONTypeTags then
        Result := TaggedJSON('date', StringToJSONLiteral(FormatDateTime(SISODateTimeFormat, TDateTime(aValue))))
      else
        Result := StringToJSONLiteral(FormatDateTime(SISODateTimeFormat, TDateTime(aValue)));
    varOleStr, varUString:
      Result := StringToJSONLiteral(UTF8Encode(UnicodeString(aValue)));
    varString:
      Result := StringToJSONLiteral(UTF8String(aValue));
  else
    { varString subtypes in FPC }
    if (VT and varTypeMask) = varString then
      Result := StringToJSONLiteral(UTF8String(aValue))
    else
      OPFRaiseError(EOPFJson, 'Opf.Variants', 'VariantToJSON',
        'Unsupported variant type: %d', [VT]);
  end;
end;

type
  TSizeIntArray = array of SizeInt;

{ The dimensions of a JSON array that is rectangular at every level:
  [[1,2],[3,4],[5,6]] gives [3,2]. False for a ragged array (children of
  different length or depth, mixed arrays and scalars) and for an empty
  inner array; such arrays become a 1-D array of variants. }

function ArrayShape(aArr: TJSONArray; out aDims: TSizeIntArray): Boolean;
var
  I, J: Integer;
  First, Other: TSizeIntArray;
  ChildIsArray: Boolean;
begin
  aDims:=Nil;
  Result:=aArr.Count>0;
  if not Result then
    Exit;
  ChildIsArray:=aArr.Items[0] is TJSONArray;
  First:=Nil;
  if ChildIsArray and not ArrayShape(TJSONArray(aArr.Items[0]), First) then
    Exit(False);
  for I:=1 to aArr.Count-1 do
  begin
    if (aArr.Items[I] is TJSONArray)<>ChildIsArray then
      Exit(False);
    if not ChildIsArray then
      Continue;
    if not ArrayShape(TJSONArray(aArr.Items[I]), Other) then
      Exit(False);
    if Length(Other)<>Length(First) then
      Exit(False);
    for J:=0 to High(First) do
      if Other[J]<>First[J] then
        Exit(False);
  end;
  SetLength(aDims, 1+Length(First));
  aDims[0]:=aArr.Count;
  for J:=0 to High(First) do
    aDims[J+1]:=First[J];
end;

function JSONDataToVariant(aData: TJSONData): Variant; forward;

procedure FillMultiDimArray(var aResult: Variant; aArr: TJSONArray;
  aDim, aDimCount: Integer; var aIndices: array of Longint);
var
  I, Lo: Integer;
  Elem: TJSONData;
begin
  Lo := VarArrayLowBound(aResult, aDim);
  for I := 0 to aArr.Count - 1 do
  begin
    aIndices[aDim - 1] := Lo + I;
    Elem := aArr.Items[I];
    if aDim < aDimCount then
    begin
      if not (Elem is TJSONArray) then
        OPFRaiseError(EOPFJson, 'Opf.Variants', 'FillMultiDimArray',
          'Expected JSON array at dimension %d but got scalar', [aDim + 1]);
      FillMultiDimArray(aResult, TJSONArray(Elem), aDim + 1, aDimCount, aIndices);
    end
    else
      VarArrayPut(aResult, JSONDataToVariant(Elem), aIndices);
  end;
end;

{ An ISO 8601 date/time as written by VariantToJSON }

function ISOToDateTime(const aText: String): TDateTime;
var
  Y, M, D, H, N, S, Z: Word;
begin
  if (Length(aText)<>23) or (aText[5]<>'-') or (aText[11]<>'T') then
    OPFRaiseError(EOPFJson, 'Opf.Variants', 'JSONToVariant', 'Invalid date/time value: "%s"', [aText]);
  Y:=StrToInt(Copy(aText, 1, 4));
  M:=StrToInt(Copy(aText, 6, 2));
  D:=StrToInt(Copy(aText, 9, 2));
  H:=StrToInt(Copy(aText, 12, 2));
  N:=StrToInt(Copy(aText, 15, 2));
  S:=StrToInt(Copy(aText, 18, 2));
  Z:=StrToInt(Copy(aText, 21, 3));
  // Also right for dates before 1899-12-30 (negative values)
  Result:=ComposeDateTime(EncodeDate(Y, M, D), EncodeTime(H, N, S, Z));
end;

{ A value written by TaggedJSON }

function TaggedToVariant(aObj: TJSONObject): Variant;
var
  TypeName: String;
  Value: TJSONData;
  I: Integer;
begin
  if (aObj.IndexOfName(STagType)<0) or (aObj.Count>2)
     or ((aObj.Count=2) and (aObj.IndexOfName(STagValue)<0)) then
    OPFRaiseError(EOPFJson, 'Opf.Variants', 'JSONToVariant',
      'Unsupported JSON type for variant conversion');
  TypeName:=aObj.Strings[STagType];
  Value:=aObj.Find(STagValue);
  if TypeName='empty' then
    Exit(Unassigned);
  if Value=Nil then
    OPFRaiseError(EOPFJson, 'Opf.Variants', 'JSONToVariant', 'Tagged value without value: %s', [TypeName]);
  if TypeName='currency' then
    Result:=StrToCurr(Value.AsString, JSONFormatSettings)
  else if TypeName='date' then
    Result:=VarFromDateTime(ISOToDateTime(Value.AsString))
  else if (TypeName='array') and (Value is TJSONArray) then
  begin
    // A 1-D array, also when its elements are arrays of the same length
    Result:=VarArrayCreate([0, Value.Count-1], varVariant);
    for I:=0 to Value.Count-1 do
      VarArrayPut(Result, JSONDataToVariant(Value.Items[I]), [I]);
  end
  else
    OPFRaiseError(EOPFJson, 'Opf.Variants', 'JSONToVariant', 'Unknown tagged value type: %s', [TypeName]);
end;

function JSONDataToVariant(aData: TJSONData): Variant;
var
  Arr: TJSONArray;
  Dims: TSizeIntArray;
  Bounds: array of SizeInt;
  Indices: array of Longint;
  Depth, I: Integer;
  IsRect: Boolean;
begin
  case aData.JSONType of
    jtNull:
      Result := Null;
    jtBoolean:
      Result := aData.AsBoolean;
    jtNumber:
      if aData is TJSONFloatNumber then
        Result := aData.AsFloat
      else if aData is TJSONQWordNumber then
        // Above High(Int64)
        Result := aData.AsQWord
      else if aData is TJSONInt64Number then
        Result := aData.AsInt64
      else
        Result := aData.AsInteger;
    jtObject:
      Result := TaggedToVariant(TJSONObject(aData));
    jtString:
      Result := UTF8String(aData.AsString);
    jtArray:
    begin
      Arr := TJSONArray(aData);
      if Arr.Count = 0 then
      begin
        Result := VarArrayCreate([0, -1], varVariant);
        Exit;
      end;
      IsRect := ArrayShape(Arr, Dims);
      Depth := Length(Dims);
      if IsRect and (Depth > 1) then
      begin
        begin
          { Build bounds array: [Lo1, Hi1, Lo2, Hi2, ...] }
          SetLength(Bounds, Depth * 2);
          for I := 0 to Depth - 1 do
          begin
            Bounds[I * 2] := 0;
            Bounds[I * 2 + 1] := Dims[I] - 1;
          end;
          Result := VarArrayCreate(Bounds, varVariant);
          SetLength(Indices, Depth);
          FillMultiDimArray(Result, Arr, 1, Depth, Indices);
          Exit;
        end;
      end;
      { 1-D array (flat or ragged) }
      Result := VarArrayCreate([0, Arr.Count - 1], varVariant);
      for I := 0 to Arr.Count - 1 do
        VarArrayPut(Result, JSONDataToVariant(Arr.Items[I]), [I]);
    end;
  else
    OPFRaiseError(EOPFJson, 'Opf.Variants', 'JSONToVariant',
      'Unsupported JSON type for variant conversion');
  end;
end;

function JSONToVariant(const aJSON: UTF8String): Variant;
var
  J: TJSONData;
begin
  if (aJSON = '') or (aJSON = 'null') then
  begin
    Result := Null;
    Exit;
  end;
  J := nil;
  try
    J := GetJSON(aJSON);
  except
    on E: Exception do
      OPFRaiseError(EOPFJson, 'Opf.Variants', 'JSONToVariant',
        SErrDeserializeInvalidJSON, [E.Message]);
  end;
  if J = nil then
    OPFRaiseError(EOPFJson, 'Opf.Variants', 'JSONToVariant',
      SErrDeserializeInvalidJSON, ['GetJSON returned nil']);
  try
    Result := JSONDataToVariant(J);
  finally
    J.Free;
  end;
end;

initialization
  JSONFormatSettings:=DefaultFormatSettings;
  JSONFormatSettings.DecimalSeparator:='.';
  JSONFormatSettings.ThousandSeparator:=#0;

end.
