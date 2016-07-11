unit odatabase;

{$mode objfpc}{$H+}

interface

uses
  TypInfo,Classes, SysUtils, fpjson, restbase;

Type
  TInt16 = Type Smallint;
  TInt32 = Type Integer;
  SByte = Type Shortint;
  TTimeOfDay = Type TDateTime;
  TDateTimeOffset = Type TDateTime;
  TGUIDString =  Type string;
  TBinary = Array of Byte;

  TDuration = type string;

  { TGeography }

  TGeography = Class(TBaseObject)
  private
    FType: String;
  Public
    Class function AllowAdditionalProperties: Boolean; override;
  Published
    Property _type : String Read FType Write FType;
  end;

  { TGeographyPoint }

  TGeographyPoint = Class(TGeography)
  private
    FCoordinates: TDoubleArray;
    Procedure SetCoordinates(AIndex : integer; AValue : TDoubleArray);
  Published
    Property coordinates : TDoubleArray Index 8 Read FCoordinates Write SetCoordinates;
  end;

  TDoubleArrayArray = Array of TDoubleArray;

  { TGeographyLineString }

  TGeographyLineString = Class(TGeography)
  private
    FCoordinates: TDoubleArrayArray;
  Published
    Property coordinates : TDoubleArrayArray Read FCoordinates Write FCoordinates;
  end;

  TGeographyPolygon = Class(TGeography)
  end;
  TGeographyMultiPoint = Class(TGeography)
  end;
  TGeographyMultiLineString = Class(TGeography)
  end;

  TSByteArray = Array of SByte;
  TByteArray = Array of Byte;
  TInt32Array = Array of TInt32;
  TInt16Array = Array of TInt16;

  TDurationArray = Array of TDuration;
  TDateArray = Array of TDate;
  TTimeArray = Array of TTime;
  TTimeOfDayArray = Array of TTimeOfDay;
  TDateTimeOffsetArray = Array of TDateTimeOffset;
  TGUIDStringArray = Array of TGUIDString;
  TBinaryArray = Array of TBinary;
  TGeographyArray = Array of TGeography;
  TGeographyPointArray = Array of TGeographyPoint;
  TGeographyLineStringArray = Array of TGeographyLineString;
  TGeographyPolygonArray = Array of TGeographyPolygon;
  TGeographyMultiPointArray = Array of TGeographyMultiPoint;
  TGeographyMultiLineStringArray = Array of TGeographyMultiLineString;

  TAnnotation = TJSONEnum;

  { TODataObject }

  TODataObject = Class(TBaseObject)
  Private
    FAns : TJSONObject;
    function GetAnnotation(Index : Integer): TAnnotation;
    function GetAnnotationValue(AName : String): TJSONData;
    function GetDataAnnotationCount: Integer;
  Protected
    Procedure AddAnnotation(Const AName : String; AValue : TJSONData);
    Class Function DynArrayToJSONArray(A : Pointer; AType  : string; AClassType : TBaseObjectClass = Nil) : TJSONArray;
    Class Function JSONArrayToDynArray(A : TJSONArray; AType  : string; AClassType : TBaseObjectClass = Nil) : Pointer;
  Public
    Destructor Destroy; override;
    Procedure LoadPropertyFromJSON(Const AName : String; JSON : TJSONData); override;
    Class Function MakeKeyString(Const AKey : String) : String;
    Class Function AllowAdditionalProperties : Boolean; override;
    Property ODataAnnotations[Index : Integer] : TAnnotation Read GetAnnotation;
    Property ODataAnnotationValues[AName : String] : TJSONData Read GetAnnotationValue;
    Property ODataAnnotationCount : Integer Read GetDataAnnotationCount;
  end;
  TODataObjectClass = Class of TODataObject;
  TODataObjectArray = Array of TODataObject;

  TODataComplexType = Class(TODataObject);
  TODataComplexTypeClass = Class of TODataComplexType;

  { TODataError }
  TODataErrorDetail = Record
    Code : String;
    Message : String;
    Target : String;
  end;
  TODataErrorDetails = Array of TODataErrorDetail;

  TODataError = Class(TObject)
  private
    FCode: String;
    FDetails: TODataErrorDetails;
    FInnerError: String;
    FMessage: String;
    FTargetCode: String;
  Public
    Property Code : String Read FCode Write FCode;
    Property Message : String Read FMessage Write FMessage;
    Property Target : String Read FTargetCode Write FTargetCode;
    Property Details : TODataErrorDetails Read FDetails Write FDetails;
    // JSON
    Property InnerError : String Read FInnerError Write FInnerError;
  end;

  { EOData }

  EOData =Class(Exception)
  private
    FError: TODataError;
    FStatusCode: Integer;
    FStatusText: String;
  Public
    Destructor Destroy; override;
    Property StatusCode : Integer Read FStatusCode Write FStatusCode;
    Property StatusText : String Read FStatusText Write FStatusText;
    Property Error : TODataError Read FError Write FError;
  end;

Function BinaryToString(B : TBinary) : String;

implementation

Function BinaryToString(B : TBinary) : String;

Var
  E : Byte;

begin
  Result:='';
  For E in B do
    Result:=Result+HexStr(E,2);
end;

{ TGeographyPoint }

Procedure TGeographyPoint.SetCoordinates(AIndex: integer; AValue: TDoubleArray);

Var
  D : Double;

begin
{  Writeln('Setting coordinates');
  For d in AValue do
    writeln('Got ',D);}
  FCoordinates:=AValue;
end;

{ TGeography }

Class function TGeography.AllowAdditionalProperties: Boolean;
begin
  Result:=True;
end;

{ EOData }

Destructor EOData.Destroy;
begin
  FreeAndNil(FError);
  inherited Destroy;
end;

{ TODataObject }

function TODataObject.GetAnnotation(Index : Integer): TAnnotation;
begin
  If Not Assigned(FAns) or (Index<0) or (Index>=FAns.Count) then
    begin
    Result.Key:='';
    Result.KeyNum:=-1;
    Result.Value:=Nil;
    end
  else
    begin
    Result.Key:=FAns.Names[Index];
    Result.KeyNum:=Index;
    Result.Value:=FAns.Items[Index];
    end;
end;

function TODataObject.GetAnnotationValue(AName : String): TJSONData;

Var
  I : Integer;

begin
  Result:=Nil;
  if Assigned(FAns) then
    begin
    I:=FAns.IndexOfName(AName);
    if I<>-1 then
      Result:=FAns.Items[i];
    end;
end;

function TODataObject.GetDataAnnotationCount: Integer;
begin
  if Assigned(FAns) then
    Result:=FAns.Count
  else
    Result:=0;
end;

Procedure TODataObject.AddAnnotation(Const AName: String; AValue: TJSONData);
begin
  If Not Assigned(FAns) then
    FAns:=TJSONObject.Create();
  FAns.Add(AName,AValue.Clone);
end;

Type
   TShortIntArray = Array of ShortInt;
   TSmallIntArray = Array of SmallInt;
   TWordArray = Array of Word;
   TCardinalArray = Array of Cardinal;
   TQWordArray= Array of QWord;
   TSingleArray = Array of Single;

Class Function TODataObject.DynArrayToJSONArray(A: Pointer; AType: string; AClassType : TBaseObjectClass = Nil): TJSONArray;


Var
  I,L : Integer;

begin
  Result:=TJSONArray.Create;
  L:=Length(TByteArray(A));
  Case LowerCase(aType) of
    'boolean':
       For I:=0 to L-1 do
         Result.Add(TBooleanArray(A)[i]);
    'byte',
    'tsbyte':
      For I:=0 to L-1 do
        Result.Add(TByteArray(A)[i]);
    'shortint':
      For I:=0 to L-1 do
        Result.Add(TShortIntArray(A)[i]);
    'int16',
    'tint16',
    'smallint':
      For I:=0 to L-1 do
        Result.Add(TSmallIntArray(A)[i]);
    'word':
      For I:=0 to L-1 do
        Result.Add(TWordArray(A)[i]);
    'tint32',
    'int32',
    'integer':
       For I:=0 to L-1 do
          Result.Add(TIntegerArray(A)[i]);
    'cardinal',
    'dword':
       For I:=0 to L-1 do
          Result.Add(TCardinalArray(A)[i]);
    'tint64',
    'int64':
       For I:=0 to L-1 do
         Result.Add(TInt64Array(A)[i]);
    'qword':
       For I:=0 to L-1 do
{$IFNDEF VER2_6}
         Result.Add(TQWordArray(A)[i]);
{$else}
         Result.Add(TInt64Array(A)[i]);
{$ENDIF}
    'string':
       For I:=0 to L-1 do
         Result.Add(TStringArray(A)[i]);
    'tguidstring':
       For I:=0 to L-1 do
         Result.Add(TStringArray(A)[i]);
    'double':
       For I:=0 to L-1 do
        Result.Add(TDoubleArray(A)[i]);
    'single':
       For I:=0 to L-1 do
         Result.Add(TSingleArray(A)[i]);
    else
      if Pos('array',lowerCase(atype))<>0 then
        Raise EOData.Create('Cannot convert array of array: '+atype);
      if (AClassType=Nil) then
        Raise EOData.Create('Cannot convert array of object without class type');
      For I:=0 to L-1 do
        if (TObjectArray(A)[i].InheritsFrom(AClassType)) then
          Result.Add(TObjectArray(A)[i].SaveToJSON);
    end;
end;

Class Function TODataObject.JSONArrayToDynArray(A: TJSONArray; AType: string; AClassType : TBaseObjectClass = Nil  ): Pointer;
Var
  I,L : Integer;

begin
  Result:=TJSONArray.Create;
  L:=A.Count;
  Case LowerCase(aType) of
    'boolean':
       begin
       SetLength(TBooleanArray(Result),L);
       For I:=0 to L-1 do
         TBooleanArray(Result)[i]:=A.Booleans[i];
       end;
    'byte',
    'tsbyte':
       begin
       SetLength(TByteArray(Result),L);
       For I:=0 to L-1 do
         TByteArray(Result)[i]:=A.Integers[i];
       end;
    'shortint':
       begin
       SetLength(TShortIntArray(Result),L);
       For I:=0 to L-1 do
         TShortIntArray(Result)[i]:=A.Integers[i];
       end;
    'int16',
    'tint16',
    'smallint':
       begin
       SetLength(TSmallIntArray(Result),L);
       For I:=0 to L-1 do
         TSmallIntArray(Result)[i]:=A.Integers[i];
       end;
    'word':
       begin
       SetLength(TWordArray(Result),L);
       For I:=0 to L-1 do
         TWordArray(Result)[i]:=A.Integers[i];
       end;
    'tint32',
    'int32',
    'integer':
       begin
       SetLength(TIntegerArray(Result),L);
       For I:=0 to L-1 do
         TIntegerArray(Result)[i]:=A.Integers[i];
       end;
    'cardinal',
    'dword':
       begin
       SetLength(TCardinalArray(Result),L);
       For I:=0 to L-1 do
         TCardinalArray(Result)[i]:=A.Integers[i];
       end;
    'tint64',
    'int64':
       begin
       SetLength(TInt64Array(Result),L);
       For I:=0 to L-1 do
         TInt64Array(Result)[i]:=A.Int64s[i];
       end;
    'qword':
       begin
       SetLength(TQWordArray(Result),L);
       For I:=0 to L-1 do
{$IFDEF VER2_6}
       TInt64Array(Result)[i]:=A.Int64s[i];
{$ELSE}
       TQWordArray(Result)[i]:=A.QWords[i];
{$ENDIF}
       end;
    'tstring',
    'string':
       begin
       SetLength(TStringArray(Result),L);
       For I:=0 to L-1 do
         TStringArray(Result)[i]:=A.Strings[i];
       end;
    'guidstring',
    'tguidstring':
       begin
       SetLength(TStringArray(Result),L);
       For I:=0 to L-1 do
         TStringArray(Result)[i]:=A.Strings[i];
       end;
    'double':
       begin
       SetLength(TDoubleArray(Result),L);
       For I:=0 to L-1 do
         TDoubleArray(Result)[i]:=A.Floats[i];
       end;
    'single':
       begin
       SetLength(TSingleArray(Result),L);
       For I:=0 to L-1 do
         TSingleArray(Result)[i]:=A.Floats[i];
       end;
    else
      if (Pos('array',lowercase(atype))<>0) then
        Raise EOData.Create('Cannot convert array of array: '+atype);
      if (AClassType=Nil) then
        Raise EOData.Create('Cannot convert array of object without class type');
      SetLength(TObjectArray(Result),L);
      For I:=0 to L-1 do
        begin
        if A.Types[i]<>jtObject then
          Raise EOData.CreateFmt('Element %d of array is not an object: %s',[I,A.Items[i].AsJSON]);
        TObjectArray(Result)[i]:=AClassType.Create;
        TObjectArray(Result)[i].LoadFromJSON(A.Objects[i]);
        end;
    end;
end;

Destructor TODataObject.Destroy;
begin
  FreeAndNil(FAns);
  Inherited;
end;

Procedure TODataObject.LoadPropertyFromJSON(Const AName: String; JSON: TJSONData
  );
begin
  if (AName<>'') and (AName[1]='@') then
    AddAnnotation(AName,JSON)
  else
    inherited LoadPropertyFromJSON(AName, JSON);
end;

Class Function TODataObject.MakeKeyString(Const AKey: String): String;
begin
  Result:=''''+AKey+'''';
end;

Class Function TODataObject.AllowAdditionalProperties: Boolean;
begin
  Result:=True; // So we catch annnotations
end;

end.

