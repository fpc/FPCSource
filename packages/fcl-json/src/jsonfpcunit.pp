{
    This file is part of the Free Component Library

    JSON Data unit test helper
    Copyright (c) 2023 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit jsonfpcunit;
{$ENDIF}

{$mode objfpc}
{$H+}
{$modeswitch typehelpers}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, FpJson.Data, FpcUnit.Test;
{$ELSE}  
  Classes, SysUtils, fpjson, fpcunit;
{$ENDIF}

Type

  { TTestJSONHelper }

  TTestJSONHelper = Class helper for TAssert
    // JSON Tests
    class Procedure AssertEquals(const Msg : String; AExpected,AActual : TJSONType); overload;
    // Check D has the correct type.
    class Procedure AssertJSONType(D: TJSONData; AExpected : TJSONType);
    class Procedure AssertJSONType(const Msg : String; D: TJSONData; AExpected : TJSONType) ;
    // Check D is a Null value and return the value
    class function AssertJSONNull(D: TJSONData) : TJSONNull;
    class function AssertJSONNull(Const Msg : String; D: TJSONData) : TJSONNull;
    // Check D is a TJSONArray value and return the value. If element count is specified, the array element count is checked.
    class Function AssertJSONArray(D: TJSONData; AElementCount : Integer = -1) : TJSONArray;
    class Function AssertJSONArray(Const Msg : String; D: TJSONData; AElementCount : Integer = -1) : TJSONArray;
    // Check Src is an object, and has element of given name and type
    class function AssertJSONElement(Src: TJSONData; const Msg, aName: String; aType: TJSONtype): TJSONData;
    // Check D is a boolean with given value. Return typed json value
    class function AssertJSONBoolean(D: TJSONData; AValue: Boolean) : TJSONBoolean;
    class function AssertJSONBoolean(Const Msg : String; D: TJSONData; AValue: Boolean) : TJSONBoolean;
    // Check D is an integer with given value. Return typed json value
    class function AssertJSONInteger(D: TJSONData; AValue: Integer) : TJSONNumber;
    class function AssertJSONInteger(Const Msg : String; D: TJSONData; AValue: Integer) : TJSONNumber;
    // Check D is a float with given value. Return typed json value
    class function AssertJSONFloat(D: TJSONData; AValue: Double) : TJSONNumber;
    class function AssertJSONFloat(Const Msg : String; D: TJSONData; AValue: Double) : TJSONNumber;
    // Check D is a string with given value. Return typed json value
    class Function AssertJSONString(D: TJSONData; const AValue: TJSONStringType) : TJSONString;
    class Function AssertJSONString(Const Msg : String; D: TJSONData; const AValue: TJSONStringType) : TJSONString;
    // Check D is a string with given value. Return typed json value.
    class Function AssertJSONObject(D: TJSONData) : TJSONObject;
    class Function AssertJSONObject(Const Msg : String; D: TJSONData) : TJSONObject;

    // Checks for object members

    // Check D is an object and contains an object value named aName. Return the value
    class Function AssertJSONObject(D: TJSONData; const AName : string) : TJSONObject;
    class Function AssertJSONObject(Const Msg : String; D: TJSONData; const AName : string) : TJSONObject;
    // Check D is an object and contains a boolean value named aName with given value. Return the value
    class function AssertJSONBoolean(D: TJSONData; const AName: String; AValue: Boolean) : TJSONBoolean;
    class function AssertJSONBoolean(Const Msg : String; D: TJSONData; const AName: String; AValue: Boolean) : TJSONBoolean;
    // Check D is an object and contains a null value named aName. Return the value
    class function AssertJSONNull(D: TJSONData; const AName: String) : TJSONNull;
    class function AssertJSONNull(Const Msg : String; D: TJSONData; const AName: String) : TJSONNull;
    // Check D is an object and contains an integer value named aName with given value. Return the value
    class function AssertJSONInteger(D: TJSONData; const AName: String; AValue: Integer) : TJSONNumber;
    class function AssertJSONInteger(Const Msg : String; D: TJSONData; const AName: String; AValue: Integer) : TJSONNumber;
    // Check D is an object and contains an int64 value named aName with given value. Return the value
    class function AssertJSONInt64(D: TJSONData; const AName: String; AValue: Int64) : TJSONNumber;
    class function AssertJSONInt64(Const Msg : String; D: TJSONData; const AName: String; AValue: Int64) : TJSONNumber;
    // Check D is an object and contains a float value named aName with given value. Return the value
    class function AssertJSONFloat(D: TJSONData;  const AName: String; AValue: Double) : TJSONNumber;
    class function AssertJSONFloat(Const Msg : String; D: TJSONData;  const AName: String; AValue: Double) : TJSONNumber;
    // Check D is an object and contains a string value named aName with given value. Return the value
    class Function AssertJSONString(D: TJSONData; const AName: String; AValue: TJSONStringType) : TJSONString;
    class Function AssertJSONString(Const Msg : String; D: TJSONData; const AName: String; AValue: TJSONStringType) : TJSONString;
    // Check D is an object and contains an array value named aName with given element count. Return the value
    class Function AssertJSONArray(D: TJSONData; const AName: String; AElementCount : Integer = -1) : TJSONArray;
    class Function AssertJSONArray(Const Msg : String; D: TJSONData; const AName: String; AElementCount : Integer = -1) : TJSONArray;
    // Check D is an object and has NO member
    class Procedure AssertJSONNoMember(D: TJSONData; const AName: String);
   end;


implementation

{$IFNDEF FPC_DOTTEDUNITS}
uses typinfo;
{$ELSE}
uses System.TypInfo;
{$ENDIF}

resourcestring
  SDataNotNull = ': Data is not null';
  SDataHasCorrectType = '%s: Data has correct type';
  SJSONTest = 'JSON test';
  SCorrectArrayElementCount = '%s: Correct element count for array';
  SCorrectDataValue = ': Correct value for data';
  SIsJSONObject = ': Source is JSONObject';
  SHaveElementCalled = '%s: Have element called %s';
  SCorrectElementType = '%s: Element %s has correct type';
  SCorrectElementValue = '%s: Correct value for element %s';
  SCorrectArrayNamesElementCount = '%s: Correct array element "%s" count';
  SUnexpectedValue = 'Expected no element called %s but found element of type %s with value %s';

class procedure TTestJSONHelper.AssertEquals(const Msg: String; AExpected,
  AActual: TJSONType);
begin
  AssertEquals(Msg,GetEnumName(Typeinfo(TJSONType),Ord(AExpected)),
                   GetEnumName(Typeinfo(TJSONType),Ord(AActual)));
end;

class procedure TTestJSONHelper.AssertJSONType(const Msg: String; D: TJSONData;
  AExpected: TJSONType);

begin
  AssertNotNull(Msg+SDataNotNull,D);
  AssertEquals(Format(SDataHasCorrectType, [Msg]), AExpected, D.JSONType);
end;

class function TTestJSONHelper.AssertJSONNull(D: TJSONData): TJSONNull;
begin
  Result:=AssertJSONNull('',D);
end;

class function TTestJSONHelper.AssertJSONArray(D: TJSONData;
  AElementCount: Integer): TJSONArray;
begin
  Result:=AssertJSONArray('',D,AElementCount);
end;

class function TTestJSONHelper.AssertJSONBoolean(D: TJSONData; AValue: Boolean
  ): TJSONBoolean;
begin
  Result:=AssertJSONBoolean('',D,AValue);
end;

class function TTestJSONHelper.AssertJSONInteger(D: TJSONData; AValue: Integer
  ): TJSONNumber;
begin
  Result:=AssertJSONInteger('',D,AValue);
end;

class function TTestJSONHelper.AssertJSONFloat(D: TJSONData; AValue: Double
  ): TJSONNumber;
begin
  Result:=AssertJSONFloat('',D,AValue);
end;

class function TTestJSONHelper.AssertJSONString(D: TJSONData;
  const AValue: TJSONStringType): TJSONString;
begin
  Result:=AssertJSONString('',D,AValue);
end;

class function TTestJSONHelper.AssertJSONObject(D: TJSONData): TJSONObject;
begin
  Result:=AssertJSONObject('',D);
end;

class function TTestJSONHelper.AssertJSONObject(D: TJSONData; const AName: string
  ): TJSONObject;
begin
  Result:=AssertJSONObject('',D,AName);
end;

class function TTestJSONHelper.AssertJSONBoolean(D: TJSONData; const AName: String;
  AValue: Boolean): TJSONBoolean;
begin
  Result:=AssertJSONBoolean('',D,AName,AValue);
end;

class function TTestJSONHelper.AssertJSONNull(D: TJSONData; const AName: String
  ): TJSONNull;
begin
  Result:=AssertJSONNull('',D,AName);
end;

class function TTestJSONHelper.AssertJSONInteger(D: TJSONData; const AName: String;
  AValue: Integer): TJSONNumber;
begin
  Result:=AssertJSONInteger('',D,AName,AValue);
end;

class function TTestJSONHelper.AssertJSONInt64(D: TJSONData; const AName: String;
  AValue: Int64): TJSONNumber;
begin
  Result:=AssertJSONInt64('',D,AName,AValue);
end;

class function TTestJSONHelper.AssertJSONFloat(D: TJSONData; const AName: String;
  AValue: Double): TJSONNumber;
begin
  Result:=AssertJSONFloat('',D,AName,AValue);
end;

class function TTestJSONHelper.AssertJSONString(D: TJSONData; const AName: String;
  AValue: TJSONStringType): TJSONString;
begin
  Result:=AssertJSONString('',D,AName,AValue);
end;

class function TTestJSONHelper.AssertJSONArray(D: TJSONData; const AName: String;
  AElementCount: Integer): TJSONArray;
begin
  Result:=AssertJSONArray('',D,AName,AElementCount);
end;

class procedure TTestJSONHelper.AssertJSONType(D: TJSONData;
  AExpected: TJSONType);
begin
  AssertJSONType(SJSONTest, D, AExpected);
end;

class function TTestJSONHelper.AssertJSONNull(const Msg: String; D: TJSONData
  ): TJSONNull;
begin
  AssertJSONType(Msg,D,jtNull);
  Result:=TJSONNull(D);
end;

class function TTestJSONHelper.AssertJSONArray(const Msg: String; D: TJSONData;
  AElementCount: Integer): TJSONArray;

begin
  Result:=Nil;
  AssertJSONType(Msg,D,jtArray);
  Result:=TJSONArray(D);
  if AElementCount>=0 then
    AssertEquals(Format(SCorrectArrayElementCount, [Msg]), AElementCount,
      D.Count);
end;

class function TTestJSONHelper.AssertJSONBoolean(const Msg: String;
  D: TJSONData; AValue: Boolean): TJSONBoolean;

begin
  AssertJSONType(Msg,D,jtBoolean);
  Result:=TJSONBoolean(D);
  AssertEquals(Msg+SCorrectDataValue, AValue, D.AsBoolean);
end;

class function TTestJSONHelper.AssertJSONInteger(const Msg: String;
  D: TJSONData; AValue: Integer): TJSONNumber;

begin
  AssertJSONType(Msg,D,jtNumber);
  Result:=TJSONNumber(D);
  AssertEquals(Msg+SCorrectDataValue,AValue,D.AsInteger);
end;

class function TTestJSONHelper.AssertJSONFloat(const Msg: String; D: TJSONData;
  AValue: Double): TJSONNumber;
begin
  AssertJSONType(Msg,D,jtNumber);
  Result:=TJSONNumber(D);
  AssertEquals(Msg+SCorrectDataValue,AValue,D.AsFloat);
end;

class function TTestJSONHelper.AssertJSONString(const Msg: String;
  D: TJSONData; const AValue: TJSONStringType): TJSONString;

begin
  AssertJSONType(Msg,D,jtString);
  Result:=TJSONString(D);
  AssertEquals(Msg+SCorrectDataValue,AValue,D.AsString);
end;

class function TTestJSONHelper.AssertJSONObject(const Msg: String; D: TJSONData
  ): TJSONObject;
begin
  AssertJSONType(Msg+SIsJSONObject, D, jtObject);
  Result:=TJSONObject(D);
end;

class function TTestJSONHelper.AssertJSONObject(const Msg: String;
  D: TJSONData; const AName: string): TJSONObject;
Var
  SD : TJSONData;

begin
  Result:=Nil;
  SD:=AssertJSONElement(D,Msg,aName,jtObject);
  Result:=TJSONObject(SD);
end;

class function TTestJSONHelper.AssertJSONBoolean(const Msg: String;
  D: TJSONData; const AName: String; AValue: Boolean): TJSONBoolean;

Var
  SD : TJSONData;

begin
  Result:=Nil;
  SD:=AssertJSONElement(D,Msg,aName,jtBoolean);
  Result:=TJSONBoolean(SD);
  AssertEquals(Format(SCorrectElementValue, [Msg, AName]),AValue,SD.AsBoolean);
end;

class function TTestJSONHelper.AssertJSONNull(const Msg: String; D: TJSONData;
  const AName: String): TJSONNull;

Var
  SD : TJSONData;

begin
  Result:=Nil;
  SD:=AssertJSONElement(D,Msg,aName,jtNull);
  Result:=TJSONNull(SD);
end;

class function TTestJSONHelper.AssertJSONInteger(const Msg: String;
  D: TJSONData; const AName: String; AValue: Integer): TJSONNumber;

Var
  SD : TJSONData;

begin
  Result:=Nil;
  SD:=AssertJSONElement(D,Msg,aName,jtNumber);
  Result:=TJSONNumber(SD);
  AssertEquals(Format(SCorrectElementValue, [Msg, AName]),AValue,SD.AsInteger);
end;

class function TTestJSONHelper.AssertJSONInt64(const Msg: String; D: TJSONData;
  const AName: String; AValue: Int64): TJSONNumber;
Var
  SD : TJSONData;

begin
  Result:=Nil;
  SD:=AssertJSONElement(D,Msg,aName,jtNumber);
  Result:=TJSONNumber(SD);
  AssertEquals(Format(SCorrectElementValue, [Msg, AName]),AValue,SD.AsInt64);
end;

class function TTestJSONHelper.AssertJSONFloat(const Msg: String; D: TJSONData;
  const AName: String; AValue: Double): TJSONNumber;
Var
  SD : TJSONData;

begin
  Result:=Nil;
  SD:=AssertJSONElement(D,Msg,aName,jtNumber);
  Result:=TJSONNumber(SD);
  AssertEquals(Format(SCorrectElementValue, [Msg, AName]),AValue,SD.AsFloat);
end;

class function TTestJSONHelper.AssertJSONElement(Src : TJSONData; const Msg,aName : String; aType : TJSONtype) : TJSONData;

begin
  Result:=Nil;
  AssertJSONObject(Msg,Src);
  AssertJSONType(Msg+SIsJSONObject,Src,jtObject);
  Result:=TJSONObject(Src).Find(AName);
  AssertNotNull(Format(SHaveElementCalled, [Msg,AName]),Result);
  AssertJSONType(Format(SCorrectElementType,[Msg,aName]),Result,aType);
end;

class function TTestJSONHelper.AssertJSONString(const Msg: String;
  D: TJSONData; const AName: String; AValue: TJSONStringType): TJSONString;

Var
  SD : TJSONData;

begin
  Result:=Nil;
  SD:=AssertJSONElement(D,Msg,aName,jtString);
  Result:=TJSONString(SD);
  AssertEquals(Format(SCorrectElementValue, [Msg, AName]), AValue, SD.AsString);
end;

class function TTestJSONHelper.AssertJSONArray(const Msg: String; D: TJSONData;
  const AName: String; AElementCount: Integer): TJSONArray;

Var
  SD : TJSONData;

begin
  Result:=Nil;
  SD:=AssertJSONElement(D,Msg,aName,jtArray);
  Result:=TJSONArray(SD);
  if AElementCount>=0 then
    AssertEquals(Format(SCorrectArrayNamesElementCount, [Msg, AName]),
      AElementCount, SD.Count);
end;

class procedure TTestJSONHelper.AssertJSONNoMember(D: TJSONData; const AName: String);

Var
  SD : TJSONData;

begin
  AssertJSONType(SIsJSONObject,D,jtObject);
  SD:=TJSONObject(D).Find(AName);
  if (SD<>Nil) then
    Fail(Format(SUnexpectedValue,[AName,GetEnumName(Typeinfo(TJSONType),Ord(SD.JSONType)),SD.AsJSON]));
end;

end.

