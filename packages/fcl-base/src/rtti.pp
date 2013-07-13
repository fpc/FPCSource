{ Extended RTTI compatibility unit

  Copyright (C) 2013 Joost van der Sluis joost@cnoc.nl

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}
unit Rtti;

{$mode objfpc}{$H+}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  typinfo;

type
  TRttiType = class;
  TRttiProperty = class;
  TRttiInstanceType = class;

  TAttributeArray = array of TCustomAttribute;
  TRttiPropertyArray = array of TRttiProperty;
  TRttiTypeArray = array of TRttiType;

  IValueData = interface
  ['{1338B2F3-2C21-4798-A641-CA2BC5BF2396}']
    procedure ExtractRawData(ABuffer: pointer);
    procedure ExtractRawDataNoCopy(ABuffer: pointer);
    function GetDataSize: integer;
    function GetReferenceToRawData: pointer;
  end;

  TValueData = record
    FTypeInfo: PTypeInfo;
    FValueData: IValueData;
    case integer of
      0:  (FAsUByte: Byte);
      1:  (FAsUWord: Word);
      2:  (FAsULong: LongWord);
      3:  (FAsObject: TObject);
      4:  (FAsClass: TClass);
      5:  (FAsSByte: Shortint);
      9:  (FAsDouble: Double);
      10: (FAsExtenden: Extended);
      12: (FAsCurr: Currency);
      14: (FAsSInt64: Int64);
  end;

  { TValue }

  TValue = object
  private
    FData: TValueData;
    function GetTypeDataProp: PTypeData;
  public
    function AsString: string;
    function AsExtended: Extended;
    function AsObject: TObject;
    function IsObject: boolean;
    function AsBoolean: boolean;
    function AsCurrency: Currency;
    function AsInteger: Integer;
    property TypeData: PTypeData read GetTypeDataProp;
  end;

  { TRttiContext }

  TRttiContext = object
  private
    FContextToken: IInterface;
  public
    class function Create: TRttiContext;
    destructor Free;
    function GetType(ATypeInfo: PTypeInfo): TRttiType;
    function GetType(AClass: TClass): TRttiType;
    function GetTypes: TRttiTypeArray;
  end;

  { TRttiObject }

  TRttiObject = class
  public
    function GetAttributes: TAttributeArray; virtual; abstract;
  end;

  { TRttiNamedObject }

  TRttiNamedObject = class(TRttiObject)
  protected
    function GetName: string; virtual;
  public
    property Name: string read GetName;
  end;

  { TRttiType }

  TRttiType = class(TRttiNamedObject)
  private
    FTypeInfo: PTypeInfo;
    FAttributesResolved: boolean;
    FAttributes: TAttributeArray;
    FPropertiesResolved: boolean;
    FProperties: TRttiPropertyArray;
    function GetAsInstance: TRttiInstanceType;
  protected
    FTypeData: PTypeData;
    function GetName: string; override;
    function GetIsInstance: boolean; virtual;
    function GetTypeKind: TTypeKind; virtual;
  public
    constructor create(ATypeInfo : PTypeInfo);
    function GetAttributes: TAttributeArray; override;
    function GetProperties: TRttiPropertyArray;
    function GetProperty(const AName: string): TRttiProperty; virtual;
    destructor destroy; override;
    property IsInstance: boolean read GetIsInstance;
    property AsInstance: TRttiInstanceType read GetAsInstance;
    property TypeKind: TTypeKind read GetTypeKind;
  end;

  TRttiStructuredType = class(TRttiType)

  end;

  { TRttiFloatType }

  TRttiFloatType = class(TRttiType)
  private
    function GetFloatType: TFloatType;
  public
    property FloatType: TFloatType read GetFloatType;
  end;


  TRttiStringKind = (skShortString, skAnsiString, skWideString, skUnicodeString);

  { TRttiStringType }

  TRttiStringType = class(TRttiType)
  private
    function GetStringKind: TRttiStringKind;
  public
    property StringKind: TRttiStringKind read GetStringKind;
  end;


  { TRttiInstanceType }

  TRttiInstanceType = class(TRttiStructuredType)
  private
    function GetMetaClassType: TClass;
  protected
    function GetIsInstance: boolean; override;
  public
    property MetaClassType: TClass read GetMetaClassType;

  end;

  { TRttiMember }

  TMemberVisibility=(mvPrivate, mvProtected, mvPublic, mvPublished);

  TRttiMember = class(TRttiNamedObject)
  private
    FParent: TRttiType;
  protected
    function GetVisibility: TMemberVisibility; virtual;
  public
    constructor create(AParent: TRttiType);
    property Visibility: TMemberVisibility read GetVisibility;
    property Parent: TRttiType read FParent;

  end;

  { TRttiProperty }

  TRttiProperty = class(TRttiMember)
  private
    FPropInfo: PPropInfo;
    FAttributesResolved: boolean;
    FAttributes: TAttributeArray;
    function GetPropertyType: TRttiType;
  protected
    function GetName: string; override;
    function GetAttributes: TAttributeArray; override;
  public
    constructor create(AParent: TRttiType; APropInfo: PPropInfo);
    function GetValue(Instance: pointer): TValue;
    property PropertyType: TRttiType read GetPropertyType;

  end;

implementation

type

  { TRttiPool }

  TRttiPool = class
  private
    FAllTypesResolved: boolean;
    FTypesList: TRttiTypeArray;
  public
    function GetTypes: TRttiTypeArray;
    function GetType(ATypeInfo: PTypeInfo): TRttiType;
    destructor Destroy; override;
  end;

  IPooltoken = interface
  ['{3CDB3CE9-AB55-CBAA-7B9D-2F3BB1CF5AF8}']
    function RttiPool: TRttiPool;
  end;

  { TPoolToken }

  TPoolToken = class(TInterfacedObject, IPooltoken)
  public
    constructor Create;
    destructor Destroy; override;
    function RttiPool: TRttiPool;
  end;

  { TValueDataIntImpl }

  TValueDataIntImpl = class(TInterfacedObject, IValueData)
  private
    FDataSize: integer;
    FBuffer: pointer;
  public
    constructor Create(ACopyFromBuffer: Pointer; ALen: integer);
    destructor Destroy; override;
    procedure ExtractRawData(ABuffer: pointer);
    procedure ExtractRawDataNoCopy(ABuffer: pointer);
    function GetDataSize: integer;
    function GetReferenceToRawData: pointer;
  end;

resourcestring
  SErrUnableToGetValueForType = 'Unable to get value for type %s';
  SErrInvalidTypecast         = 'Invalid typecast';

var
  PoolRefCount : integer;
  GRttiPool    : TRttiPool;

{ TRttiPool }

function TRttiPool.GetTypes: TRttiTypeArray;
var
  UnitList        : PUnitInfoList;
  UnitInd, TypeInd: longint;
  UnitInfo        : PUnitInfo;
  UnitDataList    : PextRTTIDataList;
  ExtRttiData     : PExtRTTIData;
  TypelistUnitInd : longint;
  ARttiType       : TRttiType;
  UnitStartIndex  : LongInt;
  l               : LongInt;

begin
  if not FAllTypesResolved then
    begin
    UnitList:=GetUnitList;
    TypelistUnitInd:=0;
    SetLength(FTypesList,0);
    for UnitInd:=0 to UnitList^.UnitCount-1 do
      begin
      UnitInfo := UnitList^.Units[UnitInd];
      UnitDataList := GetRTTIDataListForUnit(UnitInfo);
      l := GetRTTIDataCountForUnit(UnitInfo);
      UnitStartIndex := length(FTypesList);
      SetLength(FTypesList,UnitStartIndex+l);
      for TypeInd:=0 to l-1 do
        begin
        ExtRttiData := @UnitDataList^[TypeInd];
        if Assigned(ExtRttiData^.TypeInfo) then
          begin
          case ExtRttiData^.TypeInfo^.Kind of
            tkClass   : ARttiType := TRttiInstanceType.Create(ExtRttiData^.TypeInfo);
            tkSString,
            tkLString,
            tkAString,
            tkUString,
            tkWString : ARttiType := TRttiStringType.Create(ExtRttiData^.TypeInfo);
            tkFloat   : ARttiType := TRttiFloatType.Create(ExtRttiData^.TypeInfo);
          else
            ARttiType := TRttiType.Create(ExtRttiData^.TypeInfo);
          end; {case}
          end
        else
          ARttiType := TRttiType.Create(ExtRttiData^.TypeInfo);
        FTypesList[UnitStartIndex+TypeInd] := ARttiType;
        end;
      end;
    FAllTypesResolved:=true;
    end;
  result := FTypesList;
end;

function TRttiPool.GetType(ATypeInfo: PTypeInfo): TRttiType;
var
  ATypesList: TRttiTypeArray;
  i: integer;
begin
  ATypesList := GetTypes;
  result := nil;
  for i := 0 to length(ATypesList)-1 do
     begin
       if ATypesList[i].FTypeInfo=ATypeInfo then
         begin
         result := ATypesList[i];
         break;
         end;
     end;
end;

destructor TRttiPool.Destroy;
var
  i: LongInt;
begin
  for i := 0 to length(FTypesList)-1 do
    FTypesList[i].Free;
  inherited Destroy;
end;

{ TPoolToken }

constructor TPoolToken.Create;
begin
  inherited Create;
  inc(PoolRefCount);
  if PoolRefCount=1 then
    GRttiPool := TRttiPool.Create;
end;

destructor TPoolToken.Destroy;
begin
  dec(PoolRefCount);
  if PoolRefCount=0 then
    GRttiPool.Free;
  inherited;
end;

function TPoolToken.RttiPool: TRttiPool;
begin
  result := GRttiPool;
end;

{ TValueDataIntImpl }

constructor TValueDataIntImpl.create(ACopyFromBuffer: Pointer; ALen: integer);
begin
  FDataSize:=ALen;
  if ALen>0 then
    begin
      Getmem(FBuffer,FDataSize);
      system.move(ACopyFromBuffer^,FBuffer^,FDataSize);
    end;
end;

destructor TValueDataIntImpl.Destroy;
begin
  if assigned(FBuffer) then
    Freemem(FBuffer);
  inherited Destroy;
end;

procedure TValueDataIntImpl.ExtractRawData(ABuffer: pointer);
begin
  system.move(FBuffer^,ABuffer^,FDataSize);
end;

procedure TValueDataIntImpl.ExtractRawDataNoCopy(ABuffer: pointer);
begin
  system.move(FBuffer^,ABuffer^,FDataSize);
end;

function TValueDataIntImpl.GetDataSize: integer;
begin
  result := FDataSize;
end;

function TValueDataIntImpl.GetReferenceToRawData: pointer;
begin
  result := FBuffer;
end;

{ TRttiFloatType }

function TRttiFloatType.GetFloatType: TFloatType;
begin
  result := FTypeData^.FloatType;
end;

{ TValue }

function TValue.GetTypeDataProp: PTypeData;
begin
  result := GetTypeData(FData.FTypeInfo);
end;

function TValue.AsString: string;
var
  s: string;
begin
  case fdata.FTypeInfo^.Kind of
    tkSString,
    tkAString   : begin
                    setlength(s,FData.FValueData.GetDataSize);
                    system.move(FData.FValueData.GetReferenceToRawData^,s[1],FData.FValueData.GetDataSize);
                  end;
  else
    raise exception.Create(SErrInvalidTypecast);
  end;
  result := s;
end;

function TValue.AsExtended: Extended;
begin
  case TypeData^.FloatType of
    ftDouble   : result := FData.FAsDouble;
    ftExtended : result := FData.FAsExtenden;
  end;
end;

function TValue.AsObject: TObject;
begin
  if IsObject then
    result := FData.FAsObject
  else
    raise exception.Create(SErrInvalidTypecast);
end;

function TValue.IsObject: boolean;
begin
  result := fdata.FTypeInfo^.Kind = tkClass;
end;

function TValue.AsBoolean: boolean;
begin
  result := boolean(FData.FAsSInt64)
end;

function TValue.AsCurrency: Currency;
begin
  result := FData.FAsCurr;
end;

function TValue.AsInteger: Integer;
begin
  result := Integer(FData.FAsSInt64)
end;

{ TRttiStringType }

function TRttiStringType.GetStringKind: TRttiStringKind;
begin
  case TypeKind of
    tkSString : result := skShortString;
    tkLString : result := skAnsiString;
    tkAString : result := skAnsiString;
    tkUString : result := skUnicodeString;
    tkWString : result := skWideString;
  end;
end;

{ TRttiInstanceType }

function TRttiInstanceType.GetMetaClassType: TClass;
begin
  result := FTypeData^.ClassType;
end;

function TRttiInstanceType.GetIsInstance: boolean;
begin
  Result:=True;
end;

{ TRttiMember }

function TRttiMember.GetVisibility: TMemberVisibility;
begin
  result := mvPublished;
end;

constructor TRttiMember.create(AParent: TRttiType);
begin
  inherited create();
  FParent := AParent;
end;

{ TRttiProperty }

function TRttiProperty.GetPropertyType: TRttiType;
begin
  GRttiPool.GetType(FPropInfo^.PropType);
end;

function TRttiProperty.GetName: string;
begin
  Result:=FPropInfo^.Name;
end;

function TRttiProperty.GetAttributes: TAttributeArray;
var
  i: Integer;
begin
  if not FAttributesResolved then
    begin
      setlength(FAttributes,FPropInfo^.AttributeCount);
      for i := 0 to FPropInfo^.AttributeCount-1 do
        begin
          FAttributes[i]:=TCustomAttribute(GetPropAttribute(FPropInfo,i));
        end;
      FAttributesResolved:=true;
    end;
  result := FAttributes;
end;

constructor TRttiProperty.create(AParent: TRttiType; APropInfo: PPropInfo);
begin
  inherited create(AParent);
  FPropInfo := APropInfo;
end;

function TRttiProperty.GetValue(Instance: pointer): TValue;
var
  S: ansistring;
begin
  result.FData.FTypeInfo:=FPropInfo^.PropType;

  case Result.FData.FTypeInfo^.Kind of
    tkSString,
    tkAString  : begin
                   s := GetStrProp(TObject(Instance),FPropInfo);
                   result.FData.FValueData := TValueDataIntImpl.Create(@s[1],length(s));
                 end;
    tkClass    : result.FData.FAsObject := GetObjectProp(TObject(Instance),FPropInfo);
    tkInteger,
    tkBool     : result.FData.FAsSInt64 := GetOrdProp(TObject(Instance),FPropInfo);
    tkFloat    : begin
                   case GetTypeData(FPropInfo^.PropType)^.FloatType of
                     ftCurr   : result.FData.FAsCurr := GetFloatProp(TObject(Instance),FPropInfo);
                     ftDouble : result.FData.FAsDouble := GetFloatProp(TObject(Instance),FPropInfo);
                   end;
                 end;
  else
    raise Exception.CreateFmt(SErrUnableToGetValueForType,[FPropInfo^.Name]);
  end;
end;

function TRttiType.GetIsInstance: boolean;
begin
  result := false;
end;

function TRttiType.GetAsInstance: TRttiInstanceType;
begin
  // This is a ridicoulous design, but Delphi-compatible...
  result := TRttiInstanceType(self);
end;

function TRttiType.GetTypeKind: TTypeKind;
begin
  result := FTypeInfo^.Kind;
end;

function TRttiType.GetName: string;
begin
  Result:=FTypeInfo^.Name;
end;

constructor TRttiType.create(ATypeInfo: PTypeInfo);
begin
  inherited create();
  FTypeInfo:=ATypeInfo;
  if assigned(FTypeInfo) then
    FTypeData:=GetTypeData(ATypeInfo);
end;

function TRttiType.GetAttributes: TAttributeArray;
var
  i: Integer;
  ad: PAttributeData;
begin
  if not FAttributesResolved then
    begin
    ad := GetAttributeData(FTypeInfo);
    setlength(FAttributes,ad^.AttributeCount);
    for i := 0 to ad^.AttributeCount-1 do
      FAttributes[i]:=GetAttribute(ad,i);
    FAttributesResolved:=true;
    end;
  result := FAttributes;
end;

function aligntoptr(p : pointer) : pointer;inline;
   begin
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=align(p,sizeof(p));
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
     result:=p;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
   end;

function TRttiType.GetProperties: TRttiPropertyArray;
var
  propcount: integer;
  PropList: PPropList;
  APropInfo: PPropInfo;
  TypeInfo: PTypeInfo;
  TypeRttiType: TRttiType;
  TD: PTypeData;
  PPD: PPropData;
  TP: PPropInfo;
  Count: longint;
  AD: PAttributeData;
begin
  if not FPropertiesResolved then
    begin
      TypeInfo := FTypeInfo;

      // Get the total properties count
      SetLength(FProperties,FTypeData^.PropCount);
      // Clear list
      FillChar(FProperties[0],FTypeData^.PropCount*sizeof(TRttiProperty),0);
      TypeRttiType:= self;
      repeat
        TD:=GetTypeData(TypeInfo);

        // published properties count for this object
        // skip the attribute-info if available
        AD := GetAttributeData(TypeInfo);
        PPD := PPropData(pointer(AD)+SizeOf(AD^.AttributeCount)+(AD^.AttributeCount*SizeOf(TAttributeProc)));
        Count:=PPD^.PropCount;
        // Now point TP to first propinfo record.
        TP:=PPropInfo(@PPD^.PropList);
        While Count>0 do
          begin
            // Don't overwrite properties with the same name
            if FProperties[TP^.NameIndex]=nil then
              FProperties[TP^.NameIndex]:=TRttiProperty.Create(TypeRttiType, TP);

            // Point to TP next propinfo record.
            // Located at Name[Length(Name)+1] !
            TP:=aligntoptr(PPropInfo(pointer(@TP^.Name)+PByte(@TP^.Name)^+(TP^.AttributeCount*SizeOf(TAttributeProc))+1));
            Dec(Count);
          end;
        TypeInfo:=TD^.Parentinfo;
        TypeRttiType:= GRttiPool.GetType(TypeInfo);
      until TypeInfo=nil;
    end;

  result := FProperties;
end;

function TRttiType.GetProperty(const AName: string): TRttiProperty;
var
  FPropList: TRttiPropertyArray;
  i: Integer;
begin
  result := nil;
  FPropList := GetProperties;
  for i := 0 to length(FPropList)-1 do
    if sametext(FPropList[i].Name,AName) then
      begin
        result := FPropList[i];
        break;
      end;
end;

destructor TRttiType.Destroy;
var
  i: Integer;
begin
  for i := 0 to high(FAttributes) do
    FAttributes[i].Free;
  for i := 0 to high(FProperties) do
    FProperties[i].Free;
  inherited destroy;
end;

{ TRttiNamedObject }

function TRttiNamedObject.GetName: string;
begin
  result := '';
end;

{ TRttiContext }

class function TRttiContext.Create: TRttiContext;
begin
  result.FContextToken := nil;
end;

destructor TRttiContext.Free;
begin
  FContextToken := nil;
end;

function TRttiContext.GetType(ATypeInfo: PTypeInfo): TRttiType;
begin
  if not assigned(FContextToken) then
    FContextToken := TPoolToken.Create;
  result := (FContextToken as IPooltoken).RttiPool.GetType(ATypeInfo);
end;


function TRttiContext.GetType(AClass: TClass): TRttiType;
begin
  if assigned(AClass) then
    result := GetType(PTypeInfo(AClass.ClassInfo))
  else
    result := nil;
end;

function TRttiContext.GetTypes: TRttiTypeArray;

begin
  if not assigned(FContextToken) then
    FContextToken := TPoolToken.Create;
  result := (FContextToken as IPooltoken).RttiPool.GetTypes;
end;

initialization
  PoolRefCount := 0;
end.

