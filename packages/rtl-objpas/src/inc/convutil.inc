{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2004 by Marco van de Voort
   member of the Free Pascal development team.

   An implementation for unit ConvUtils, which converts between
   units and simple combinations of them.

   Based on a guessed interface derived from some programs on the web. (Like
   Marco Cantu's EuroConv example), so things can be a bit Delphi
   incompatible. Also part on Delphibasics.co.uk.

   Quantities are mostly taken from my HP48g/gx or the unix units program

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit ConvUtils;

interface

{$mode objfpc}
{$H+}

{$ifndef FPUNONE}
uses
  sysutils, math;

Type TConvType        = type Word;
     TConvFamily      = type Word;
     TConvFamilyArray = array of TConvFamily;
     TConvTypeArray   = array of TConvType;
     TConversionProc  = function(const AValue: Double): Double;
     TConvUtilFloat   = double;
     EConversionError = class(EConvertError);

const
  CIllegalConvFamily = TConvFamily(0);   // Delphi compatible but makes no sense since 0 is a valid value for a TConvFamily
  CIllegalConvType = TConvType(0);       // Delphi compatible but makes no sense since 0 is a valid value for a TConvType
  GConvUnitToStrFmt: string = '%f %s';

Function RegisterConversionFamily(Const S : String):TConvFamily;
Function RegisterConversionType(Fam:TConvFamily;Const S:String;Value:TConvUtilFloat):TConvType;
Function RegisterConversionType(Fam:TConvFamily;Const S:String;const AToCommonFunc, AFromCommonFunc: TConversionProc): TConvType;

procedure UnregisterConversionFamily(const AFamily: TConvFamily);
procedure UnregisterConversionType(const AType: TConvType);

function Convert ( const Measurement  : Double; const FromType, ToType  : TConvType ) :TConvUtilFloat;
function Convert ( const Measurement  : Double; const FromType1, FromType2, ToType1, ToType2  : TConvType ) :TConvUtilFloat;

function ConvertFrom(const AFrom: TConvType; AValue: Double): TConvUtilFloat;
function ConvertTo(const AValue: Double; const ATo: TConvType): TConvUtilFloat;

function ConvUnitCompareValue(const AValue1: Double; const AType1: TConvType;
  const AValue2: Double; const AType2: TConvType): TValueRelationship;
function ConvUnitSameValue(const AValue1: Double; const AType1: TConvType;
  const AValue2: Double; const AType2: TConvType): Boolean;

function ConvUnitAdd(const AValue1: Double; const AType1: TConvType;
  const AValue2: Double; const AType2, AResultType: TConvType): TConvUtilFloat;
function ConvUnitDiff(const AValue1: Double; const AType1: TConvType;
  const AValue2: Double; const AType2, AResultType: TConvType): TConvUtilFloat;

function ConvUnitDec(const AValue: Double; const AType: TConvType;
  const AAmount: Double; const AAmountType: TConvType): TConvUtilFloat;
function ConvUnitDec(const AValue: Double; const AType, AAmountType: TConvType): TConvUtilFloat;
function ConvUnitInc(const AValue: Double; const AType: TConvType;
  const AAmount: Double; const AAmountType: TConvType): TConvUtilFloat;
function ConvUnitInc(const AValue: Double; const AType, AAmountType: TConvType): TConvUtilFloat;
function ConvUnitWithinNext(const AValue, ATest: Double; const AType: TConvType;
  const AAmount: Double; const AAmountType: TConvType): Boolean;
function ConvUnitWithinPrevious(const AValue, ATest: Double;
  const AType: TConvType; const AAmount: Double; const AAmountType: TConvType): Boolean;

function ConvFamilyToDescription(const AFamily: TConvFamily): string;
function ConvTypeToDescription(const AType: TConvType): string;
function ConvUnitToStr(const AValue: Double; const AType: TConvType): string;
function  DescriptionToConvFamily(const ADescription: String; out AFamily: TConvFamily): Boolean;
function  DescriptionToConvType(const ADescription: String; out AType: TConvType): Boolean; overload;
function  DescriptionToConvType(const AFamily: TConvFamily; const ADescription: String; out AType: TConvType): Boolean; overload;
function TryStrToConvUnit(AText: string; out AValue: Double; out AType: TConvType): Boolean;
function StrToConvUnit(AText: string; out AType: TConvType): Double;

procedure GetConvFamilies(out AFamilies: TConvFamilyArray);
procedure GetConvTypes(const AFamily: TConvFamily; out ATypes: TConvTypeArray);

function ConvTypeToFamily(const AType: TConvType): TConvFamily;
function ConvTypeToFamily(const AFrom, ATo: TConvType): TConvFamily;
function CompatibleConversionType(const AType: TConvType; const AFamily: TConvFamily): Boolean;
function CompatibleConversionTypes(const AFrom, ATo: TConvType): Boolean;

procedure RaiseConversionError(const AText: string);
procedure RaiseConversionError(const AText: string; const AArgs: array of const);
procedure RaiseConversionRegError(AFamily: TConvFamily; const ADescription: string);

Type
  TConvTypeInfo = Class(Tobject)
  private
     FDescription : String;
     FConvFamily  : TConvFamily;
     FConvType	  : TConvType;
  public
     Constructor Create(Const AConvFamily : TConvFamily;const ADescription:String);
     function ToCommon(const AValue: Double) : Double; virtual; abstract;
     function FromCommon(const AValue: Double) : Double; virtual; abstract;
     property ConvFamily : TConvFamily read FConvFamily;
     property ConvType   : TConvType   read FConvType;
     property Description: String      read FDescription;
  end;

  TConvTypeFactor = class(TConvTypeInfo)
  private
    FFactor: Double;
  protected
    property Factor: Double read FFactor;
  public
    constructor Create(const AConvFamily: TConvFamily; const ADescription: string;
      const AFactor: Double);
    function ToCommon(const AValue: Double): Double; override;
    function FromCommon(const AValue: Double): Double; override;
  end;

  TConvTypeProcs = class(TConvTypeInfo)
  private
    FToProc: TConversionProc;
    FFromProc: TConversionProc;
  public
    constructor Create(const AConvFamily: TConvFamily; const ADescription: string;
      const AToProc, AFromProc: TConversionProc);
    function ToCommon(const AValue: Double): Double; override;
    function FromCommon(const AValue: Double): Double; override;
  end;

Implementation

uses
  RtlConsts;

const
  macheps=1E-9;
  zeroRes=1E-17;

Type TResourceData = record
                       Description   : String;
                       Value         : TConvUtilFloat;
                       ToCommonFunc  : TConversionProc;
                       FromCommonFunc: TConversionProc;
                       Fam           : TConvFamily;
                       Deleted       : Boolean;
                     end;
     TFamilyData = record
                     Description: String;
                     Deleted    : Boolean;
                   end;


var TheUnits    : array of TResourceData =nil;
    TheFamilies : array of TFamilyData =nil;


Function CheckFamily(i:TConvFamily):Boolean; inline;

begin
  Result:=(i<Length(TheFamilies)) and (not TheFamilies[i].Deleted);
end;

function FindFamily(const ADescription: String; out AFam: TConvFamily): Boolean;

var
  i: Integer;

begin
  result:=False;
  for i := 0 to Length(TheFamilies)-1 do
  begin
    if (TheFamilies[i].Description=ADescription) and (not TheFamilies[i].Deleted) then
    begin
      result:=True;
      AFam:=TConvFamily(i);
      Exit;
    end;
  end;
end;

function CheckType(AType: TConvType): Boolean; inline;
begin
  result:=(AType<Length(TheUnits)) and (not TheUnits[AType].Deleted);
end;

function FindConvType(AFam: TConvFamily; const ADescription: string; out AResourceData: TResourceData): Boolean;

var
  i: Integer;

begin
  result:=False;
  for i := 0 to Length(TheUnits)-1 do
  begin
    if (TheUnits[i].Fam=AFam) and (TheUnits[i].Description=ADescription) and (not TheUnits[i].Deleted) then
    begin
      result:=True;
      AResourceData:=TheUnits[i];
    end;
  end;
end;

function FindConvType(AFam: TConvFamily; const ADescription: string): Boolean;

var
  Data: TResourceData;

begin
  result:=FindConvType(AFam, ADescription, Data);
end;


function ConvUnitDec(const AValue: Double; const AType: TConvType;
  const AAmount: Double; const AAmountType: TConvType): TConvUtilFloat;

var
  D1: Double;

begin
  D1:=Convert(AAmount,AAmountType,AType);
  result:=AValue-D1;
end;

function ConvUnitDec(const AValue: Double; const AType, AAmountType: TConvType): TConvUtilFloat;
begin
  result:=ConvUnitDec(AValue, AType, 1.0, AAmountType);
end;

function ConvUnitInc(const AValue: Double; const AType: TConvType;
  const AAmount: Double; const AAmountType: TConvType): TConvUtilFloat;

var
  D1: Double;

begin
  D1:=Convert(AAmount,AAmountType,AType);
  result:=AValue+D1;
end;

function ConvUnitInc(const AValue: Double; const AType, AAmountType: TConvType): TConvUtilFloat;
begin
    result:=ConvUnitInc(AValue, AType, 1.0, AAmountType);
end;

function ConvUnitWithinNext(const AValue, ATest: Double;
  const AType: TConvType; const AAmount: Double; const AAmountType: TConvType): Boolean;

var
  D: Double;

begin
  D:=Convert(AAmount, AAMountType, AType);
  //don't use InRange() since it does not have an epsilon parameter
  result:=(CompareValue(ATest,AValue,macheps)<>LessThanValue) and
          (CompareValue(ATest,AValue+D,macheps)<>GreaterThanValue);
end;

function ConvUnitWithinPrevious(const AValue, ATest: Double;
  const AType: TConvType; const AAmount: Double; const AAmountType: TConvType): Boolean;

var
  D: Double;

begin
  D:=Convert(AAmount, AAMountType, AType);
  result:=(CompareValue(ATest,AValue,macheps)<>GreaterThanValue) and
          (CompareValue(ATest,AValue-D,macheps)<>LessThanValue);

end;

function ConvFamilyToDescription(const AFamily: TConvFamily): string;

begin
  if CheckFamily(AFamily) then
    result:=TheFamilies[AFamily].Description
  else
    result:=format(SConvUnknownDescriptionWithPrefix,['$',AFamily]);
end;

function ConvUnitToStr(const AValue: Double; const AType: TConvType): string;

begin
  result:=format(GConvUnitToStrFmt,[AValue,ConvTypeToDescription(AType)]);
end;

function  DescriptionToConvFamily(const ADescription: String; out AFamily: TConvFamily): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(TheFamilies) - 1 do
  begin
    if (TheFamilies[i].Description=ADescription) and not TheFamilies[i].Deleted then
    begin
      AFamily := i;
      Result := true;
      break;
     end;
  end;
end;

procedure GetConvFamilies(out AFamilies: TConvFamilyArray);

var i , count: integer;
begin
 AFamilies:=nil;
 setlength(AFamilies,length(thefamilies));
 count:=0;
 for i:=0 to length(TheFamilies)-1 do
 begin
   if not TheFamilies[i].Deleted then
   begin
     AFamilies[i]:=i;
     Inc(Count);
   end;
 end;
 SetLength(AFamilies,count);
end;

procedure GetConvTypes(const AFamily: TConvFamily; out ATypes: TConvTypeArray);

var i,j,nrTypes:integer;

begin
  nrTypes:=0;
  ATypes:=nil;
  for i:=0 to length(TheUnits)-1 do
    if (TheUnits[i].fam=AFamily) and (not TheUnits[i].Deleted) and (not TheUnits[i].Deleted) Then
      inc(nrTypes);
  setlength(atypes,nrtypes);
  j:=0;
  for i:=0 to length(TheUnits)-1 do
    if (TheUnits[i].fam=AFamily) and (not TheUnits[i].Deleted) and (not TheUnits[i].Deleted) Then
     begin
       atypes[j]:=i;
       inc(j);
     end;
end;

function TryConvTypeToDescription(const AType: TConvType; out S: string): Boolean;

begin
  result:=CheckType(AType);
  if result then
    S:=TheUnits[AType].Description;
end;

function ConvTypeToDescription(const AType: TConvType): string;

Begin
  if not TryConvTypeToDescription(AType, result) then
    result:=format(SConvUnknownDescriptionWithPrefix,['$',AType]);
end;

function  DescriptionToConvType(const ADescription: String; out AType: TConvType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(TheUnits) - 1 do
  begin
    if (TheUnits[i].Description = ADescription) and (not TheUnits[i].Deleted) then
    begin
      AType := i;
      Result := true;
      break;
     end;
  end;
end;

function  DescriptionToConvType(const AFamily: TConvFamily; const ADescription: String; out AType: TConvType): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Length(TheUnits) - 1 do
  begin
    if (AFamily = TheUnits[i].Fam) and
       (TheUnits[i].Description = ADescription) and
       (not TheUnits[i].Deleted) then
    begin
      AType := i;
      Result := true;
      break;
     end;
  end;
end;

function TryStrToConvUnit(AText: string; out AValue: Double; out AType: TConvType): Boolean;

var
  P: SizeInt;
  ValueStr, TypeStr: String;

begin
  Result:=False;
  P:=Pos(#32,AText);
  if P=0 then
    Exit;
  ValueStr:=Copy(AText,1,P);
  if not TryStrToFloat(ValueStr, AValue) then
    Exit;
  if P=Length(AText) then //no desription found
    Exit;
  TypeStr:=Trim(Copy(AText,P+1,MaxInt));
  Result:=DescriptionToConvType(TypeStr, AType);
end;

function StrToConvUnit(AText: string; out AType: TConvType): Double;

begin
  if not TryStrToConvUnit(AText, Result, AType) then
    raise EConversionError.CreateFmt(SConvStrParseError,[AText]);
end;

function TryConvTypeToFamily(const AType: TConvType; out AFam: TConvFamily): Boolean;

begin
  result:=false;
  if CheckType(AType) then begin
    AFam:=TheUnits[AType].Fam;
    result:=true;
  end;
end;

function ConvTypeToFamily(const AType: TConvType): TConvFamily;

begin
  //result:=CIllegalConvFamily; //Delphi Docs says it does this, but actually it raises an exception upon error
  if not TryConvTypeToFamily(AType, result) then
    raise EConversionError.CreateFmt(SConvUnknownType,[format(SConvUnknownDescriptionWithPrefix,['$',AType])]);
end;

function ConvTypeToFamily(const AFrom, ATo: TConvType): TConvFamily;

var
  AFromS, AToS: String;
  Fam1, Fam2: TConvFamily;
begin
  // a bit convoluted but Delphi actually raises exceptions that use the descriptions of AFrom and ATo
  AFromS:=ConvTypeToDescription(AFrom);
  AToS:=ConvTypeToDescription(ATo);
  if  TryConvTypeToFamily(AFrom, Fam1) and
      TryConvTypeToFamily(ATo, Fam2) and
      (Fam1=Fam2) then
    result:=Fam1
  else
    raise EConversionError.CreateFmt(SConvIncompatibleTypes2,[AFromS, AToS]);
end;

function CompatibleConversionType(const AType: TConvType;
  const AFamily: TConvFamily): Boolean;

begin
  result:=ConvTypeToFamily(AType)=AFamily;
end;

function CompatibleConversionTypes(const AFrom, ATo: TConvType): Boolean;

begin
  //ConvTypeToFamily potentially raises an exception, make sure it doesn't here
  result:= CheckType(AFrom) and CheckType(ATo) and
           (ConvTypeToFamily(AFrom)=ConvTypeToFamily(ATo));
end;

Function RegisterConversionFamily(Const S:String):TConvFamily;

var len : Longint;
    fam: TConvFamily;

begin
  if Trim(S)='' then
    //Delphi does not, but since this is non-sensical, we disallow it
    raise EConversionError.Create(SConvEmptyDescription);
  len:=Length(TheFamilies);
  if len>0 then
    if FindFamily(S, fam) then
      raise EConversionError.CreateFmt(SConvDuplicateFamily,[S]);
  if len=Integer(High(TConvFamily))+1 then
    raise EConversionError.CreateFmt(SConvTooManyConvFamilies,[High(TConvFamily)]);
  SetLength(TheFamilies,len+1);
  TheFamilies[len].Description:=Trim(S);
  TheFamilies[len].Deleted:=False;
  result:=len;
end;

procedure UnregisterConversionFamily(const AFamily: TConvFamily);

var
  i: Integer;

begin
  //Apparently this procedure is not supposed to raise exceptions
  if CheckFamily(AFamily) then
  begin
    TheFamilies[AFamily].Deleted:=True;
    for i:=0 to Length(TheUnits)-1 do
    begin
      if TheUnits[i].Fam=AFamily then
        TheUnits[i].Deleted:=True;
    end;
  end;
end;


procedure UnregisterConversionType(const AType: TConvType);

begin
  //Apparently this procedure is not supposed to raise exceptions
  if CheckType(AType) then
    TheUnits[AType].Deleted:=True;
end;

Function InternalRegisterConversionType(Fam:TConvFamily; S:String;Value:TConvUtilFloat;
  const AToCommonFunc, AFromCommonFunc: TConversionProc):TConvType;

var len : Longint;

begin
  If NOT CheckFamily(Fam) Then
    raise EConversionError.CreateFmt(SConvUnknownFamily, [IntToStr(Fam)]);
  S:= Trim(S);
  //Delphi does not, but since this is non-sensical, we disallow it
  if S='' then
    raise EConversionError.Create(SConvEmptyDescription);
  if IsZero(Value,zeroRes) then
    raise EZeroDivide.CreateFmt(SConvFactorZero,[S]);
  len:=length(theunits);
  if len>0 then
    if FindConvType(Fam, S) then
      raise EConversionError.CreateFmt(SConvDuplicateType,[S,ConvFamilyToDescription(Fam)]);
  if len=Integer(High(TConvType))+1 then
    raise EConversionError.CreateFmt(SConvTooManyConvTypes,[High(TConvType)]);
  Setlength(theunits,len+1);
  theunits[len].description:=s;
  theunits[len].value:=value;
  theunits[len].ToCommonFunc:=AToCommonFunc;
  theunits[len].FromCommonFunc:=AFromCommonFunc;
  theunits[len].fam:=fam;
  theunits[len].deleted:=false;
  Result:=len;
end;

Function RegisterConversionType(Fam:TConvFamily;Const S:String;Value:TConvUtilFloat):TConvType;
begin
  result:=InternalRegisterConversionType(Fam,S,Value,nil,nil);
end;

function RegisterConversionType(Fam: TConvFamily; const S: String;
  const AToCommonFunc, AFromCommonFunc: TConversionProc): TConvType;
begin
  result:=InternalRegisterConversionType(Fam,S,(AToCommonFunc(1)-AToCommonFunc(0)),AToCommonFunc,AFromCommonFunc);
end;

function SearchConvert(TheType:TConvType; out r:TResourceData):Boolean;

begin
  if not CheckType(TheType) then
    exit(false);
  r:=theunits[thetype];
  result:=true;
end;

function Convert ( const Measurement  : Double; const FromType, ToType  : TConvType ) :TConvUtilFloat;

var
  fromrec,torec :   TResourceData;
  common: double;

begin
  if not SearchConvert(fromtype,fromrec) then
    raise EConversionError.CreateFmt(SConvUnknownType, [IntToStr(FromType)]);
  if not SearchConvert(totype,torec) then
    raise EConversionError.CreateFmt(SConvUnknownType, [IntToStr(ToType)]);
  if fromrec.fam<>torec.fam then
    raise EConversionError.CreateFmt(SConvIncompatibleTypes2,[
      ConvFamilyToDescription(fromrec.fam),
      ConvFamilyToDescription(torec.fam)
    ]);
  if assigned(fromrec.ToCommonFunc) or assigned(torec.FromCommonFunc) then begin
    if assigned(fromrec.ToCommonFunc) then
      common:=fromrec.ToCommonFunc(MeasureMent)
    else
      common:=Measurement*fromrec.value;
    if assigned(torec.FromCommonFunc) then
      result:=torec.FromCommonFunc(common)
    else begin
      if IsZero(torec.value,zeroRes) then
        raise EZeroDivide.CreateFmt(SConvFactorZero,[torec.Description]);
      result:=common/torec.value;
    end;
  end else begin
    //Note: Delphi 7 raises an EZeroDivide even if fromrec.value=0, which is a bit odd
    if IsZero(torec.value,zeroRes) then
      raise EZeroDivide.CreateFmt(SConvFactorZero,[torec.Description]);
    result:=Measurement*fromrec.value/torec.value;
  end;
end;

function Convert ( const Measurement  : Double; const FromType1, FromType2, ToType1, ToType2  : TConvType ) :TConvUtilFloat;
var
  fromrec1,fromrec2,torec1 ,
  torec2 :   TResourceData;

begin
  if not SearchConvert(fromtype1,fromrec1) then
    raise EConversionError.CreateFmt(SConvUnknownType, [IntToStr(fromtype1)]);
  if not SearchConvert(totype1,torec1) then
    raise EConversionError.CreateFmt(SConvUnknownType, [IntToStr(totype1)]);
  if not SearchConvert(fromtype2,fromrec2) then
    raise EConversionError.CreateFmt(SConvUnknownType, [IntToStr(fromtype2)]);
  if not SearchConvert(totype2,torec2) then
    raise EConversionError.CreateFmt(SConvUnknownType, [IntToStr(totype2)]);
  if (fromrec1.fam<>torec1.fam) or (fromrec2.fam<>torec2.fam) then
    raise EConversionError.CreateFmt(SConvIncompatibleTypes4,[
      ConvFamilyToDescription(fromrec1.fam),
      ConvFamilyToDescription(torec1.fam),
      ConvFamilyToDescription(fromrec2.fam),
      ConvFamilyToDescription(torec2.fam)
    ]);
  //using ToCommonFunc() and FromCommonFunc makes no sense in this context
  if IsZero(fromrec2.value,zeroRes) then
    raise EZeroDivide.CreateFmt(SConvFactorZero,[fromrec2.Description]);
  if IsZero(torec2.value,zeroRes) then
    raise EZeroDivide.CreateFmt(SConvFactorZero,[torec2.Description]);
  result:=Measurement*(fromrec1.value/fromrec2.value)/(torec1.value/torec2.value);
end;

function ConvertFrom(const AFrom: TConvType; AValue: Double): TConvUtilFloat;

var
  fromrec :   TResourceData;

begin
  if not SearchConvert(AFrom, fromrec) then
    raise  EConversionError.CreateFmt(SConvUnknownType, [IntToStr(AFrom)]);
  if Assigned(fromrec.ToCommonFunc) then
    result:=fromrec.ToCommonFunc(AValue)
  else
    result:=fromrec.value*AValue;
end;

function ConvertTo(const AValue: Double; const ATo: TConvType): TConvUtilFloat;

var
  torec :   TResourceData;

begin
  if not SearchConvert(ATo, torec) then
    raise  EConversionError.CreateFmt(SConvUnknownType, [IntToStr(ATo)]);
  if Assigned(torec.FromCommonFunc) then
    result:=torec.FromCommonFunc(AValue)
  else
  begin
    if IsZero(torec.value,zeroRes) then
      raise EZeroDivide.CreateFmt(SConvFactorZero,[torec.Description]);
    result:=Avalue/torec.value;
  end;
end;

function ConvUnitCompareValue(const AValue1: Double; const AType1: TConvType;
  const AValue2: Double; const AType2: TConvType): TValueRelationship;

var
  D1, D2: TConvUtilFloat;

begin
  D1:=ConvertFrom(AType1, AValue1);
  D2:=ConvertFrom(AType2, Avalue2);
  result:=CompareValue(D1,D2,macheps);
end;

function ConvUnitSameValue(const AValue1: Double; const AType1: TConvType;
  const AValue2: Double; const AType2: TConvType): Boolean;
begin
  result:=ConvUnitCompareValue(Avalue1, AType1, AValue2, AType2)=EqualsValue;
end;


function ConvUnitAdd(const AValue1: Double; const AType1: TConvType;
  const AValue2: Double; const AType2, AResultType: TConvType): TConvUtilFloat;

var
  D1, D2: Double;

begin
  D1:=ConvertFrom(AType1, AValue1);
  D2:=ConvertFrom(AType2,AValue2);
  result:=ConvertTo(D1+D2,AResultType);
end;

function ConvUnitDiff(const AValue1: Double; const AType1: TConvType;
  const AValue2: Double; const AType2, AResultType: TConvType): TConvUtilFloat;
begin
  result:=ConvUnitAdd(AVAlue1, ATYpe1, -AValue2, AType2, AResultType);
end;

procedure RaiseConversionError(const AText: string);

begin
  Raise EConversionError.Create(AText);
end;

procedure RaiseConversionError(const AText: string; const AArgs: array of const);

begin
  Raise EConversionError.CreateFmt(AText, AArgs);
end;

procedure RaiseConversionRegError(AFamily: TConvFamily; const ADescription: string);

begin
  Raise EConversionError.CreateFmt(SConvDuplicateType,[ADescription,ConvFamilyToDescription(AFamily)]);
end;

Constructor TConvTypeInfo.Create(Const AConvFamily : TConvFamily;const ADescription:String);

begin
  FDescription:=ADescription;
  FConvFamily :=AConvFamily;
end;


constructor TConvTypeFactor.Create(const AConvFamily: TConvFamily; const ADescription: string;const AFactor: Double);
begin
  inherited create(AConvFamily,ADescription);
  FFactor:=AFactor;
end;

function TConvTypeFactor.ToCommon(const AValue: Double): Double;
begin
  result:=AValue * FFactor;
end;

function TConvTypeFactor.FromCommon(const AValue: Double): Double;
begin
  result:=AValue / FFactor;
end;

constructor TConvTypeProcs.Create(const AConvFamily: TConvFamily; const ADescription: string; const AToProc, AFromProc: TConversionProc);
begin
  inherited create(AConvFamily,ADescription);
  ftoproc:=AToProc;
  ffromproc:=AFromProc;
end;

function TConvTypeProcs.ToCommon(const AValue: Double): Double;
begin
  result:=FTOProc(Avalue);
end;

function TConvTypeProcs.FromCommon(const AValue: Double): Double;
begin
  result:=FFromProc(Avalue);
end;

finalization
  setlength(theunits,0);
  setlength(thefamilies,0);
{$else}
implementation
{$endif}
end.
