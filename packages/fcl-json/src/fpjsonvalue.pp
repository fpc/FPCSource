{
    This file is part of the Free Component Library

    JSON Data structure to  TValue conversion
    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpjsonvalue;
{$ENDIF FPC_DOTTEDUNITS}

{$mode ObjFPC}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  FpJson.Data, System.Rtti;
{$ELSE FPC_DOTTEDUNITS}
uses
  fpjson, rtti;
{$ENDIF FPC_DOTTEDUNITS}

function ValueToJSON(const aValue: TValue; aType: TRttiType): TJSONData;
function JSONToValue(aData: TJSONData; aType: TRttiType): TValue;

Implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.TypInfo;
{$ELSE FPC_DOTTEDUNITS}
uses typinfo;
{$ENDIF FPC_DOTTEDUNITS}

function ValueToJSON(const aValue: TValue; aType: TRttiType): TJSONData;
var
  i: SizeInt;
  at: TRttiDynamicArrayType;
begin
  Result := Nil;
  try
    case aType.TypeKind of
      tkAString,
      tkUString,
      tkWString,
      tkSString:
        Result := TJSONString.Create(aValue.AsUnicodeString);
      tkInteger:
        Result := TJSONIntegerNumber.Create(aValue.AsInteger);
      tkInt64,
      tkQWord:
        Result := TJSONInt64Number.Create(aValue.AsInt64);
      tkBool:
        Result := TJSONBoolean.Create(aValue.AsBoolean);
      tkDynArray: begin
        Result := TJSONArray.Create;
        at := aType as TRttiDynamicArrayType;
        for i := 0 to aValue.GetArrayLength - 1 do
          TJSONArray(Result).Add(ValueToJSON(aValue.GetArrayElement(i), at.ElementType));
      end;
      { ToDo: further types }
    end;
  except
    Result.Free;
    Raise;
  end;
end;

function JSONToValue(aData: TJSONData; aType: TRttiType): TValue;
var
  _as: AnsiString;
  us: UnicodeString;
  i: Integer;
  i64: Int64;
  b: Boolean;
  arr: TJSONArray;
  varr: array of TValue;
  at: TRttiDynamicArrayType;
begin
  varr:=[];
  Result := TValue.Empty;
  case aType.TypeKind of
    tkAString: begin
      _as := aData.AsString;
      TValue.Make(@_as, PTypeInfo(aType.Handle), Result);
    end;
    tkUString: begin
      us := aData.AsUnicodeString;
      TValue.Make(@us, PTypeInfo(aType.Handle), Result);
    end;
    tkInteger: begin
      i := aData.AsInteger;
      TValue.Make(@i, PTypeInfo(aType.Handle), Result);
    end;
    tkInt64,
    tkQWord: begin
      i64 := aData.AsInt64;
      TValue.Make(@i64, PTypeInfo(aType.Handle), Result);
    end;
    tkBool: begin
      b := aData.AsBoolean;
      TValue.Make(@b, PTypeInfo(aType.Handle), Result);
    end;
    tkDynArray: begin
      arr := aData as TJSONArray;
      at := aType as TRttiDynamicArrayType;
      SetLength(varr, arr.Count);
      for i := 0 to High(varr) do
        varr[i] := JSONToValue(arr[i], at.ElementType);
      Result := TValue.FromArray(aType.Handle, varr);
    end;
    { ToDo: further types }
  end;
end;


end.

