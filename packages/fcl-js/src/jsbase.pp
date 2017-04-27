{ ********************************************************************* 
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2016 Michael Van Canneyt.
       
    Javascript base definitions
            
    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
                   
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
                                
  **********************************************************************}
                                 
unit jsbase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

Type
  TJSType = (jstUNDEFINED,jstNull,jstBoolean,jstNumber,jstString,jstObject,jstReference,JSTCompletion);

  TJSString = UnicodeString;
  TJSNumber = Double;

  { TJSValue }

  TJSValue = Class(TObject)
  private
    FValueType: TJSType;
    FValue : Record
      Case Integer of
      0 : (P : Pointer);
      1 : (F : TJSNumber);
      2 : (I : Integer);
    end;
    FCustomValue: TJSString;
    procedure ClearValue(ANewValue: TJSType);
    function GetAsBoolean: Boolean;
    function GetAsCompletion: TObject;
    function GetAsNumber: TJSNumber;
    function GetAsObject: TObject;
    function GetAsReference: TObject;
    function GetAsString: TJSString;
    function GetIsNull: Boolean;
    function GetIsUndefined: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);
    procedure SetAsCompletion(const AValue: TObject);
    procedure SetAsNumber(const AValue: TJSNumber);
    procedure SetAsObject(const AValue: TObject);
    procedure SetAsReference(const AValue: TObject);
    procedure SetAsString(const AValue: TJSString);
    procedure SetIsNull(const AValue: Boolean);
    procedure SetIsUndefined(const AValue: Boolean);
  Public
    Constructor Create;
    Constructor CreateNull;
    Constructor Create(ANumber : TJSNumber);
    Constructor Create(ABoolean : Boolean);
    Constructor Create(AString: TJSString);
    Destructor Destroy; override;
    Property ValueType : TJSType Read FValueType;
    Property CustomValue: TJSString Read FCustomValue Write FCustomValue;
    Property IsUndefined : Boolean Read GetIsUndefined Write SetIsUndefined;
    Property IsNull : Boolean Read GetIsNull Write SetIsNull;
    Property AsNumber : TJSNumber Read GetAsNumber Write SetAsNumber;
    Property AsBoolean : Boolean Read GetAsBoolean Write SetAsBoolean;
    Property AsObject : TObject Read GetAsObject Write SetAsObject;
    Property AsString : TJSString Read GetAsString Write SetAsString;
    Property AsReference : TObject Read GetAsReference Write SetAsReference;
    Property AsCompletion : TObject Read GetAsCompletion Write SetAsCompletion;
  end;

implementation

{ TJSValue }


function TJSValue.GetAsBoolean: Boolean;
begin
  If (ValueType=jstBoolean) then
    Result:=(FValue.I<>0)
  else
    Result:=False;
end;

function TJSValue.GetAsCompletion: TObject;
begin
  Result:=TObject(FValue.P);
end;

function TJSValue.GetAsNumber: TJSNumber;
begin
  If (ValueType=jstNumber) then
    Result:=FValue.F
  else
    Result:=0.0;
end;

function TJSValue.GetAsObject: TObject;
begin
  If (ValueType=jstObject) then
    Result:=TObject(FValue.P)
  else
    Result:=nil;
end;

function TJSValue.GetAsReference: TObject;
begin
  If (ValueType=jstReference) then
    Result:=TObject(FValue.P)
  else
    Result:=nil;
end;

function TJSValue.GetAsString: TJSString;
begin
  If (ValueType=jstString) then
    Result:=TJSString(FValue.P)
  else
    Result:='';
end;

function TJSValue.GetIsNull: Boolean;
begin
  Result:=(ValueType=jstNull);
end;

function TJSValue.GetIsUndefined: Boolean;
begin
  Result:=(fValueType=jstUndefined);
end;

procedure TJSValue.ClearValue(ANewValue : TJSType);

begin
  Case FValueType of
    jstString : String(FValue.P):='';
    jstNumber : FValue.F:=0;
  else
    FValue.I:=0;
  end;
  FValueType:=ANewValue;
  FCustomValue:='';
end;

procedure TJSValue.SetAsBoolean(const AValue: Boolean);
begin
  ClearValue(jstBoolean);
  FValue.I:=Ord(AValue);
end;

procedure TJSValue.SetAsCompletion(const AValue: TObject);
begin
  ClearValue(jstBoolean);
  FValue.P:=AValue;
end;

procedure TJSValue.SetAsNumber(const AValue: TJSNumber);
begin
  ClearValue(jstNumber);
  FValue.F:=AValue;
end;

procedure TJSValue.SetAsObject(const AValue: TObject);
begin
  ClearValue(jstObject);
  FValue.P:=AVAlue;
end;

procedure TJSValue.SetAsReference(const AValue: TObject);
begin
  ClearValue(jstReference);
  FValue.P:=AVAlue;
end;

procedure TJSValue.SetAsString(const AValue: TJSString);
begin
  ClearValue(jstString);
  TJSString(FValue.P):=AValue;
end;

procedure TJSValue.SetIsNull(const AValue: Boolean);
begin
  if AValue then
    ClearValue(jstNull)
  else if IsNull then
    ClearValue(jstUNDEFINED);
end;

procedure TJSValue.SetIsUndefined(const AValue: Boolean);
begin
  if AValue then
    ClearValue(jstUndefined)
  else if IsUndefined then
    ClearValue(jstNull);
end;

constructor TJSValue.CreateNull;
begin
  IsNull:=True;
end;

constructor TJSValue.Create;
begin
  IsUndefined:=True;
end;

constructor TJSValue.Create(ANumber: TJSNumber);
begin
  AsNumber:=ANumber;
end;

constructor TJSValue.Create(ABoolean: Boolean);
begin
  AsBoolean:=ABoolean;
end;

constructor TJSValue.Create(AString: TJSString);
begin
  AsString:=AString;
end;

destructor TJSValue.Destroy;
begin
  ClearValue(jstUndefined);
  inherited Destroy;
end;


end.

