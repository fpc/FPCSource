{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2004 by Marco van de Voort
   member of the Free Pascal development team.

   An implementation for unit convutils, which converts between
   units and simple combinations of them.

   Based on a guessed interface derived from some programs on the web. (Like
   Marco Cantu's EuroConv example), so things can be a bit Delphi
   incompatible. Also part on Delphibasics.co.uk.

   Quantities are mostly taken from my HP48g/gx or the unix units program

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit convutils;

interface

{$mode objfpc}
{$H+}

Type TConvType        = type Integer;
     TConvFamily      = type Integer;
     TConvFamilyArray = array of TConvFamily;
     TConvTypeArray   = array of TConvType;
     TConversionProc  = function(const AValue: Double): Double;


Type  TConvUtilFloat = double;

Function RegisterConversionFamily(Const S : String):TConvFamily;
Function RegisterConversionType(Fam:TConvFamily;Const S:String;Value:TConvUtilFloat):TConvType;

function Convert ( const Measurement  : Double; const FromType, ToType  : TConvType ) :TConvUtilFloat;
function Convert ( const Measurement  : Double; const FromType1, FromType2, ToType1, ToType2  : TConvType ) :TConvUtilFloat;

function ConvFamilyToDescription(const AFamily: TConvFamily): string;
function ConvTypeToDescription(const AType: TConvType): string;
procedure GetConvFamilies(out AFamilies: TConvFamilyArray);
procedure GetConvTypes(const AFamily: TConvFamily; out ATypes: TConvTypeArray);

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

Type ResourceData = record
                      Description : String;
                      Value       : TConvUtilFloat;
                      Fam         : TConvFamily;
                     end;


var TheUnits    : array of ResourceData =nil;
    TheFamilies : array of string =nil;

function ConvFamilyToDescription(const AFamily: TConvFamily): string;

begin
  result:='';
  if AFamily<length(TheFamilies) then
    result:=TheFamilies[AFamily];
end;

procedure GetConvFamilies(out AFamilies: TConvFamilyArray);

var i : integer;
begin
 setlength(AFamilies,length(thefamilies));
 for i:=0 to length(TheFamilies)-1 do
   AFamilies[i]:=i;
end;

procedure GetConvTypes(const AFamily: TConvFamily; out ATypes: TConvTypeArray);

var i,j,nrTypes:integer;

begin
  nrTypes:=0;
  for i:=0 to length(TheUnits)-1 do
    if TheUnits[i].fam=AFamily Then
     inc(nrTypes);
  setlength(atypes,nrtypes);
  j:=0;
  for i:=0 to length(TheUnits)-1 do
    if TheUnits[i].fam=AFamily Then
     begin
       atypes[j]:=i;
       inc(j);
     end;	
end;

function ConvTypeToDescription(const AType: TConvType): string;

Begin
  result:='';
  if AType<length(TheUnits) then
    result:=TheUnits[AType].Description;
end;

Function RegisterConversionFamily(Const S:String):TConvFamily;

var i,l : Longint;

begin
  l:=Length(TheFamilies);
  If l=0 Then
    begin
      SetLength(TheFamilies,1);
      TheFamilies[0]:=S;
      Result:=0;
    end
  else
    begin
      i:=0;
      while (i<l) and (s<>TheFamilies[i]) do inc(i);
      if i=l Then
         begin
           SetLength(TheFamilies,l+1);
           TheFamilies[l]:=s;
         end;
       Result:=i;
    end;
end;

Function CheckFamily(i:TConvFamily):Boolean;

begin
  Result:=i<Length(TheFamilies);
end;

const macheps=1E-9;

Function RegisterConversionType(Fam:TConvFamily;Const S:String;Value:TConvUtilFloat):TConvType;

var l1 : Longint;

begin
  If NOT CheckFamily(Fam) Then exit(-1); // family not registered.
  if (value+1.0)<macheps then            // not properly defined yet.
    exit(-1);
  l1:=length(theunits);
  Setlength(theunits,l1+1);
  theunits[l1].description:=s;
  theunits[l1].value:=value;
  theunits[l1].fam:=fam;
  Result:=l1;
end;

function SearchConvert(TheType:TConvType; var r:ResourceData):Boolean;

var l1 : longint;

begin
  l1:=length(TheUnits);
  if thetype>=l1 then
    exit(false);
  r:=theunits[thetype];
  result:=true;
end;

function Convert ( const Measurement  : Double; const FromType, ToType  : TConvType ) :TConvUtilFloat;

var
  fromrec,torec :   resourcedata;

begin
  if not SearchConvert(fromtype,fromrec) then
   exit(-1.0);                                  // raise exception?
  if not SearchConvert(totype,torec) then
   exit(-1.0);                                  // raise except?
  if fromrec.fam<>torec.fam then
   exit(-1.0);
  result:=Measurement*fromrec.value/torec.value;
end;

function Convert ( const Measurement  : Double; const FromType1, FromType2, ToType1, ToType2  : TConvType ) :TConvUtilFloat;
var
  fromrec1,fromrec2,torec1 ,
  torec2 :   resourcedata;

begin
  if not SearchConvert(fromtype1,fromrec1) then
   exit(-1.0);                                  // raise exception?
  if not SearchConvert(totype1,torec1) then
   exit(-1.0);                                  // raise except?
  if not SearchConvert(fromtype2,fromrec2) then
   exit(-1.0);                                  // raise exception?
  if not SearchConvert(totype2,torec2) then
   exit(-1.0);                                  // raise except?
  if (fromrec1.fam<>torec1.fam) or (fromrec1.fam<>torec1.fam) then
   exit(-1.0);
  result:=Measurement*(fromrec1.value/fromrec2.value)/(torec1.value/torec2.value);
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
end.
