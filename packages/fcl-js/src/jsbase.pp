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

{$ifdef pas2js}
uses js;
{$endif}

const
  MinSafeIntDouble = -$1fffffffffffff; // -9007199254740991 53 bits (52 explicitly stored)
  MaxSafeIntDouble =  $1fffffffffffff; //  9007199254740991
Type
  TJSType = (jstUNDEFINED,jstNull,jstBoolean,jstNumber,jstString,jstObject,jstReference,jstCompletion);

  TJSString = UnicodeString;
  TJSChar = WideChar;
  TJSNumber = Double;
  {$ifdef fpc}
  TJSPChar = PWideChar;
  {$endif}

  { TJSValue }

  TJSValue = Class(TObject)
  private
    FValueType: TJSType;
    {$ifdef pas2js}
    FValue: JSValue;
    {$else}
    FValue : Record
      Case Integer of
      0 : (P : Pointer);
      1 : (F : TJSNumber);
      2 : (I : Integer);
    end;
    {$endif}
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

function IsValidJSIdentifier(Name: TJSString; AllowEscapeSeq: boolean = false): boolean;
function StrToJSString(const S: String): TJSString; inline;
function JSStringToString(const S: TJSString): String; inline;

implementation

function IsValidJSIdentifier(Name: TJSString; AllowEscapeSeq: boolean): boolean;
{$ifdef pas2js}
const
  HexChars = ['0'..'9','a'..'f','A'..'F'];
var
  p, l, i: Integer;
begin
  Result:=false;
  if Name='' then exit;
  l:=length(Name);
  p:=1;
  while p<=l do
    case Name[p] of
    '0'..'9':
      if p=1 then
        exit
      else
        inc(p);
    'a'..'z','A'..'Z','_','$': inc(p);
    '\':
      begin
      if not AllowEscapeSeq then exit;
      inc(p);
      if p>l then exit;
      if Name[p]='x' then
        begin
        // \x00
        inc(p);
        if (p>l) or not (Name[p] in HexChars) then exit;
        inc(p);
        if (p>l) or not (Name[p] in HexChars) then exit;
        end
      else if Name[p]='u' then
        begin
        inc(p);
        if p>l then exit;
        if Name[p]='{' then
          begin
          // \u{00000}
          i:=0;
          repeat
            inc(p);
            if p>l then exit;
            case Name[p] of
            '}': break;
            '0'..'9': i:=i*16+ord(Name[p])-ord('0');
            'a'..'f': i:=i*16+ord(Name[p])-ord('a')+10;
            'A'..'F': i:=i*16+ord(Name[p])-ord('A')+10;
            else exit;
            end;
            if i>$FFFF then exit;
          until false;
          if (i>=$D800) and (i<$E000) then exit;
          inc(p);
          end
        else
          begin
          // \u0000
          for i:=1 to 4 do
            begin
            inc(p);
            if (p>l) or not (Name[p] in HexChars) then exit;
            end;
          end;
        // ToDo: check for invalid values like #$D800 and #$0041
        end
      else
        exit; // unknown sequence
      end;
    #$200C,#$200D: inc(p); // zero width non-joiner/joiner
    #$00AA..#$2000,
    #$200E..#$D7FF:
      inc(p); // ToDo: only those with ID_START/ID_CONTINUE see https://codepoints.net/search?IDC=1
    #$D800..#$DFFF:
      exit; // double code units are not allowed for JS identifiers
    #$E000..#$FFFF:
      inc(p);
    else
      exit;
    end;
  Result:=true;
end;
{$else}
var
  p: TJSPChar;
  i: Integer;
begin
  Result:=false;
  if Name='' then exit;
  p:=TJSPChar(Name);
  repeat
    case p^ of
    #0:
      if p-TJSPChar(Name)=length(Name) then
        exit(true)
      else
        exit;
    '0'..'9':
      if p=TJSPChar(Name) then
        exit
      else
        inc(p);
    'a'..'z','A'..'Z','_','$': inc(p);
    '\':
      begin
      if not AllowEscapeSeq then exit;
      inc(p);
      if p^='x' then
        begin
        // \x00
        for i:=1 to 2 do
          begin
          inc(p);
          if not (p^ in ['0'..'9','a'..'f','A'..'F']) then exit;
          end;
        end
      else if p^='u' then
        begin
        inc(p);
        if p^='{' then
          begin
          // \u{00000}
          i:=0;
          repeat
            inc(p);
            case p^ of
            '}': break;
            '0'..'9': i:=i*16+ord(p^)-ord('0');
            'a'..'f': i:=i*16+ord(p^)-ord('a')+10;
            'A'..'F': i:=i*16+ord(p^)-ord('A')+10;
            else exit;
            end;
            if i>$FFFF then exit;
          until false;
          if (i>=$D800) and (i<$E000) then exit;
          inc(p);
          end
        else
          begin
          // \u0000
          for i:=1 to 4 do
            begin
            inc(p);
            if not (p^ in ['0'..'9','a'..'f','A'..'F']) then exit;
            end;
          end;
        // ToDo: check for invalid values like #$D800 and #$0041
        end
      else
        exit; // unknown sequence
      end;
    #$200C,#$200D: inc(p); // zero width non-joiner/joiner
    #$00AA..#$2000,
    #$200E..#$D7FF:
      inc(p); // ToDo: only those with ID_START/ID_CONTINUE see https://codepoints.net/search?IDC=1
    #$D800..#$DFFF:
      exit; // double code units are not allowed for JS identifiers
    #$E000..#$FFFF:
      inc(p);
    else
      exit;
    end;
  until false;
end;
{$endif}

function StrToJSString(const S: String): TJSString;
begin
  Result:={$ifdef pas2js}S{$else}UTF8Decode(S){$endif};
end;

function JSStringToString(const S: TJSString): String;
begin
  Result:={$ifdef pas2js}S{$else}UTF8Encode(S){$endif};
end;

{ TJSValue }

function TJSValue.GetAsBoolean: Boolean;
begin
  If (ValueType=jstBoolean) then
    Result:={$ifdef pas2js}boolean(FValue){$else}(FValue.I<>0){$endif}
  else
    Result:=False;
end;

function TJSValue.GetAsCompletion: TObject;
begin
  Result:=TObject(FValue{$ifdef fpc}.P{$endif});
end;

function TJSValue.GetAsNumber: TJSNumber;
begin
  If (ValueType=jstNumber) then
    Result:={$ifdef pas2js}TJSNumber(FValue){$else}FValue.F{$endif}
  else
    Result:=0.0;
end;

function TJSValue.GetAsObject: TObject;
begin
  If (ValueType=jstObject) then
    Result:=TObject(FValue{$ifdef fpc}.P{$endif})
  else
    Result:=nil;
end;

function TJSValue.GetAsReference: TObject;
begin
  If (ValueType=jstReference) then
    Result:=TObject(FValue{$ifdef fpc}.P{$endif})
  else
    Result:=nil;
end;

function TJSValue.GetAsString: TJSString;
begin
  If (ValueType=jstString) then
    Result:=TJSString(FValue{$ifdef fpc}.P{$endif})
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
  {$ifdef pas2js}
  Case FValueType of
    jstUNDEFINED: FValue:=JS.Undefined;
    jstString : FValue:='';
    jstNumber : FValue:=0;
    jstBoolean : FValue:=false;
  else
    FValue:=JS.Null;
  end;
  {$else}
  Case FValueType of
    jstString : String(FValue.P):='';
    jstNumber : FValue.F:=0;
  else
    FValue.I:=0;
  end;
  {$endif}
  FValueType:=ANewValue;
  FCustomValue:='';
end;

procedure TJSValue.SetAsBoolean(const AValue: Boolean);
begin
  ClearValue(jstBoolean);
  {$ifdef pas2js}
  FValue:=AValue;
  {$else}
  FValue.I:=Ord(AValue);
  {$endif}
end;

procedure TJSValue.SetAsCompletion(const AValue: TObject);
begin
  ClearValue(jstBoolean);
  FValue{$ifdef fpc}.P{$endif}:=AValue;
end;

procedure TJSValue.SetAsNumber(const AValue: TJSNumber);
begin
  ClearValue(jstNumber);
  FValue{$ifdef fpc}.F{$endif}:=AValue;
end;

procedure TJSValue.SetAsObject(const AValue: TObject);
begin
  ClearValue(jstObject);
  FValue{$ifdef fpc}.P{$endif}:=AVAlue;
end;

procedure TJSValue.SetAsReference(const AValue: TObject);
begin
  ClearValue(jstReference);
  FValue{$ifdef fpc}.P{$endif}:=AVAlue;
end;

procedure TJSValue.SetAsString(const AValue: TJSString);
begin
  ClearValue(jstString);
  {$ifdef pas2js}FValue{$else}TJSString(FValue.P){$endif}:=AValue;
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

