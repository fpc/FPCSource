{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt, member of the
    Free Pascal development team

    TFields and related components implementations.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
PRocedure DumpMem (P : Pointer;Size : Longint);

Type PByte = ^Byte;

Var i : longint;

begin
  Write ('Memory dump : ');
  For I:=0 to Size-1 do
    Write (Pbyte(P)[i],' ');
  Writeln;
end;

{ ---------------------------------------------------------------------
    TFieldDef
  ---------------------------------------------------------------------}

Constructor TFieldDef.Create(AOwner: TFieldDefs; const AName: string;
      ADataType: TFieldType; ASize: Word; ARequired: Boolean; AFieldNo: Longint);

begin
  Inherited Create(AOwner);
{$ifdef dsdebug }
  Writeln('TFieldDef.Create : ',Aname,'(',AFieldNo,')');
{$endif}
  FName:=Aname;
  FDatatype:=ADatatype;
  FSize:=ASize;
  FRequired:=ARequired;
  FPrecision:=-1;
  // Correct sizes.
  If FDataType=ftFloat then
    begin
    If Not (FSize in [4,8,10]) then FSize:=10
    end
  else If FDataType in [ftWord,ftsmallint,ftinteger] Then
    If Not (FSize in [1,2,4]) then FSize:=4;

  FFieldNo:=AFieldNo;
  AOwner.FItems.Add(Self);
end;

Destructor TFieldDef.Destroy;

Var I : longint;

begin
  Inherited destroy;
end;

Function TFieldDef.CreateField(AOwner: TComponent): TField;

Var TheField : TFieldClass;

begin
{$ifdef dsdebug}
  Writeln ('Creating field '+FNAME);
{$endif dsdebug}
  TheField:=GetFieldClass;
  if TheField=Nil then
    DatabaseErrorFmt(SUnknownFieldType,[FName]);
  Result:=Thefield.Create(AOwner);
  Try
    Result.Size:=FSize;
    Result.Required:=FRequired;
    Result.FieldName:=FName;
    Result.FFieldNo:=Self.FieldNo;
    Result.SetFieldType(DataType);
{$ifdef dsdebug}
    Writeln ('TFieldDef.CReateField : Trying to set dataset');
{$endif dsdebug}
{$ifdef dsdebug}
    Writeln ('TFieldDef.CReateField : Result Fieldno : ',Result.FieldNo,' Self : ',FieldNo);
{$endif dsdebug}
    Result.Dataset:=TFieldDefs(Owner).FDataset;
    If Result is TFloatField then
      TFloatField(Result).Precision:=FPrecision;
  except
    Result.Free;
    Raise;
  end;

end;

Function TFieldDef.GetFieldClass : TFieldClass;

begin
  //!! Should be owner as tdataset but that doesn't work ??

  If Assigned(Owner) then
    Result:=TFieldDefs(Owner).FDataSet.GetFieldClass(FDataType)
  else
    Result:=Nil;
end;

{ ---------------------------------------------------------------------
    TFieldDefs
  ---------------------------------------------------------------------}

destructor TFieldDefs.Destroy;

begin
  FItems.Free;
  // This will destroy all fielddefs since we own them...
  Inherited Destroy;
end;

procedure TFieldDefs.Add(const AName: string; ADataType: TFieldType; ASize: Word;
  ARequired: Boolean);

begin
  If Length(AName)=0 Then
    DatabaseError(SNeedFieldName);
  // the fielddef will register itself here as a owned component.
  // fieldno is 1 based !
  TFieldDef.Create(Self,AName,ADataType,ASize,Arequired,FItems.Count+1);
end;

function TFieldDefs.GetCount: Longint;

begin
  Result:=FItems.Count;
end;

function TFieldDefs.GetItem(Index: Longint): TFieldDef;

begin
  Result:=TFieldDef(FItems[Index]);
end;

constructor TFieldDefs.Create(ADataSet: TDataSet);

begin
  Inherited Create(ADataSet);
  FItems:=TList.Create;
  FDataset:=ADataset;
end;

procedure TFieldDefs.Assign(FieldDefs: TFieldDefs);

Var I : longint;

begin
  Clear;
  For i:=1 to FieldDefs.Count-1 do
    With FieldDefs[i] do
      Add(Name,DataType,Size,Required);
end;

procedure TFieldDefs.Clear;

Var I : longint;

begin
  For I:=FItems.Count-1 downto 0 do
    TFieldDef(Fitems[i]).Free;
  FItems.Clear;
end;

function TFieldDefs.Find(const AName: string): TFieldDef;

Var I : longint;

begin
  I:=IndexOf(AName);
  If I=-1 Then
    DataBaseErrorFmt(SUnknownField,[AName,FDataSet.Name]);
  Result:=TFieldDef(Fitems[i]);
end;

function TFieldDefs.IndexOf(const AName: string): Longint;

Var I : longint;

begin
  For I:=0 to Fitems.Count-1 do
    If AnsiCompareText(TFieldDef(FItems[I]).Name,AName)=0 then
      begin
      Result:=I;
      Exit;
      end;
  Result:=-1;
end;

procedure TFieldDefs.Update;

begin
  FDataSet.UpdateFieldDefs;
end;

{ ---------------------------------------------------------------------
    TField
  ---------------------------------------------------------------------}

Const
  SBoolean = 'Boolean';
  SDateTime = 'TDateTime';
  SFloat = 'Float';
  SInteger = 'Integer';
  SString = 'String';

constructor TField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FVisible:=True;
  FValidChars:=[#0..#155];
end;

destructor TField.Destroy;

begin
  IF Assigned(FDataSet) then
    begin
    FDataSet.Active:=False;
    FDataSet.RemoveField(Self);
    end;
  Inherited Destroy;
end;

function TField.AccessError(const TypeName: string): EDatabaseError;

begin
  Result:=EDatabaseError.CreateFmt(SinvalidTypeConversion,[TypeName,FFieldName]);
end;

procedure TField.Assign(Source: TPersistent);

begin
  //!! To be implemented
end;

procedure TField.Change;

begin
  If Assigned(FOnChange) Then
    FOnChange(Self);
end;

procedure TField.CheckInactive;

begin
  If Assigned(FDataSet) then
    FDataset.CheckInactive;
end;

procedure TField.Clear;

begin
  SetData(Nil);
end;

procedure TField.DataChanged;

begin
  FDataset.DataEvent(deFieldChange,longint(Self));
end;

procedure TField.FocusControl;

begin
  FDataSet.DataEvent(deFocusControl,longint(Self));
end;

procedure TField.FreeBuffers;

begin
  // Empty. Provided for backward compatibiliy;
  // TDataset manages the buffers.
end;

function TField.GetAsBoolean: Boolean;

begin
  AccessError(SBoolean);
end;

function TField.GetAsDateTime: TDateTime;

begin
  AccessError(SdateTime);
end;

function TField.GetAsFloat: Extended;

begin
  AccessError(SDateTime);
end;

function TField.GetAsLongint: Longint;

begin
  AccessError(SInteger);
end;

function TField.GetAsString: string;

begin
  AccessError(SString);
end;

function TField.GetCanModify: Boolean;

begin
  Result:=Not ReadOnly;
  If Result then
    begin
    Result:=Assigned(DataSet);
    If Result then
      Result:= DataSet.CanModify;
    end;
end;

function TField.GetData(Buffer: Pointer): Boolean;

begin
  IF FDataset=Nil then
    DatabaseErrorFmt(SNoDataset,[FieldName]);
  If FVAlidating then
    begin
    result:=Not(FValueBuffer=Nil);
    If Result then
      Move (FValueBuffer^,Buffer^ ,DataSize);
    end
  else
    Result:=FDataset.GetFieldData(Self,Buffer);
end;

function TField.GetDataSize: Word;

begin
  Result:=0;
end;

function TField.GetDefaultWidth: Longint;

begin
  Result:=10;
end;

function TField.GetDisplayName  : String;

begin
  If FDisplayLabel<>'' then
    result:=FDisplayLabel
  else
    Result:=FFieldName;
end;

function TField.getIndex : longint;

begin
  If Assigned(FDataset) then
    Result:=FDataset.FFieldList.IndexOf(Self)
  else
    Result:=-1;
end;

function TField.GetIsNull: Boolean;

begin
  Result:=Not(GetData (Nil));
end;

function TField.GetParentComponent: TComponent;

begin
  //!! To be implemented
end;

procedure TField.GetText(var AText: string; ADisplayText: Boolean);

begin
  AText:=GetAsString;
end;

function TField.HasParent: Boolean;

begin
  HasParent:=True;
end;

function TField.IsValidChar(InputChar: Char): Boolean;

begin
  // FValidChars must be set in Create.
  Result:=InputChar in FValidChars;
end;

procedure TField.Notification(AComponent: TComponent; Operation: TOperation);

begin
  Inherited Notification(AComponent,Operation);
end;

procedure TField.PropertyChanged(LayoutAffected: Boolean);

begin
  If (FDataset<>Nil) and (FDataset.Active) then
    If LayoutAffected then
      FDataset.DataEvent(deLayoutChange,0)
    else
      FDataset.DataEvent(deDatasetchange,0);
end;

procedure TField.ReadState(Reader: TReader);

begin
  //!! To be implemented
end;

procedure TField.SetAsBoolean(AValue: Boolean);

begin
  AccessError(SBoolean);
end;

procedure TField.SetAsDateTime(AValue: TDateTime);

begin
  AccessError(SDateTime);
end;

procedure TField.SetAsFloat(AValue: Extended);

begin
  AccessError(SFloat);
end;

procedure TField.SetAsLongint(AValue: Longint);

begin
  AccessError(SInteger);
end;

procedure TField.SetAsString(const AValue: string);

begin
  AccessError(SString);
end;

procedure TField.SetData(Buffer: Pointer);

begin
  If Not Assigned(FDataset) then
    EDatabaseError.CreateFmt(SNoDataset,[FieldName]);
  FDataSet.SetFieldData(Self,Buffer);
end;

Procedure TField.SetDataset (Value : TDataset);

begin
{$ifdef dsdebug}
  Writeln ('Setting dataset');
{$endif}
  If Value=FDataset then exit;
  If Assigned(FDataset) Then FDataset.CheckInactive;
  If Assigned(Value) then
    begin
    Value.CheckInactive;
// ?? Identifier idents no member ??
    Value.FFieldList.CheckFieldName(FFieldName);
    end;
  If Assigned(FDataset) then
    FDataset.FFieldList.Remove(Self);
  If Assigned(Value) then
    Value.FFieldList.Add(Self);
  FDataset:=Value;
end;

procedure TField.SetDataType(AValue: TFieldType);

begin
  FDataType := AValue;
end;

procedure TField.SetFieldType(AValue: TFieldType);

begin
  //!! To be implemented
end;

procedure TField.SetParentComponent(AParent: TComponent);

begin
  //!! To be implemented
end;

procedure TField.SetSize(AValue: Word);

begin
  CheckInactive;
  CheckTypeSize(AValue);
  FSize:=AValue;
end;

procedure TField.SetText(const AValue: string);

begin
  AsString:=AValue;
end;

procedure TField.Validate(Buffer: Pointer);

begin
  If assigned(OnValidate) Then
    begin
    FValueBuffer:=Buffer;
    FValidating:=True;
    Try
      OnValidate(Self);
    finally
      FValidating:=False;
    end;
    end;
end;

class function Tfield.IsBlob: Boolean;

begin
  Result:=False;
end;

class procedure TField.CheckTypeSize(AValue: Longint);

begin
  If (AValue<>0) and Not IsBlob Then
    DatabaseErrorFmt(SInvalidFieldSize,[AValue]);
end;

// TField private methods

function TField.GetDisplayText: String;
begin
  SetLength(Result, 0);
  if Assigned(OnGetText) then
    OnGetText(Self, Result, True)
  else
    GetText(Result, True);
end;


{ ---------------------------------------------------------------------
    TStringField
  ---------------------------------------------------------------------}


constructor TStringField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftString);
  Size:=20;
end;

class procedure TStringField.CheckTypeSize(AValue: Longint);

begin
  If (AValue<1) or (AValue>dsMaxStringSize) Then
    databaseErrorFmt(SInvalidFieldSize,[AValue])
end;

function TStringField.GetAsBoolean: Boolean;

Var S : String;

begin
  S:=GetAsString;
  result := (Length(S)>0) and (Upcase(S[1]) in ['T',YesNoChars[True]]);
end;

function TStringField.GetAsDateTime: TDateTime;

begin
  Result:=StrToDateTime(GetAsString);
end;

function TStringField.GetAsFloat: Extended;

begin
  Result:=StrToFloat(GetAsString);
end;

function TStringField.GetAsLongint: Longint;

begin
  Result:=StrToInt(GetAsString);
end;

function TStringField.GetAsString: string;

begin
  If Not GetValue(Result) then
    Result:='';
end;

function TStringField.GetDataSize: Word;

begin
  Result:=Size+1;
end;

function TStringField.GetDefaultWidth: Longint;

begin
  result:=Size;
end;

Procedure TStringField.GetText(var AText: string; ADisplayText: Boolean);

begin
    AText:=GetAsString;
end;

function TStringField.GetValue(var AValue: string): Boolean;

Var Buf : TStringFieldBuffer;

begin
  Result:=GetData(@Buf);
  If Result then
    AValue:=Buf;
end;

procedure TStringField.SetAsBoolean(AValue: Boolean);

begin
  If AValue Then
    SetAsString('T')
  else
    SetAsString('F');
end;

procedure TStringField.SetAsDateTime(AValue: TDateTime);

begin
  SetAsString(DateTimeToStr(AValue));
end;

procedure TStringField.SetAsFloat(AValue: Extended);

begin
  SetAsString(FloatToStr(AValue));
end;

procedure TStringField.SetAsLongint(AValue: Longint);

begin
  SetAsString(intToStr(AValue));
end;

procedure TStringField.SetAsString(const AValue: string);

Const NullByte : char = #0;

begin
  IF Length(AValue)=0 then
    SetData(@NullByte)
  else
    SetData(@AValue[1]);
end;

{ ---------------------------------------------------------------------
    TNumericField
  ---------------------------------------------------------------------}


constructor TNumericField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  AlignMent:=taRightJustify;
end;

procedure TNumericField.RangeError(AValue, Min, Max: Extended);

begin
  DatabaseErrorFMT(SRangeError,[AValue,Min,Max,FieldName]);
end;

procedure TNumericField.SetDisplayFormat(const AValue: string);

begin
 If FDisplayFormat<>AValue then
   begin
   FDisplayFormat:=AValue;
   PropertyChanged(True);
   end;
end;

procedure TNumericField.SetEditFormat(const AValue: string);

begin
  If FEDitFormat<>AValue then
    begin
    FEDitFormat:=AVAlue;
    PropertyChanged(True);
    end;
end;

{ ---------------------------------------------------------------------
    TLongintField
  ---------------------------------------------------------------------}


constructor TLongintField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDatatype(ftinteger);
  FMinRange:=Low(LongInt);
  FMaxRange:=High(LongInt);
  FValidchars:=['+','-','0'..'9'];
end;

function TLongintField.GetAsFloat: Extended;

begin
  Result:=GetAsLongint;
end;

function TLongintField.GetAsLongint: Longint;

begin
  If Not GetValue(Result) then
    Result:=0;
end;

function TLongintField.GetAsString: string;

Var L : Longint;

begin
  If GetValue(L) then
    Result:=IntTostr(L)
  else
    Result:='';
end;

function TLongintField.GetDataSize: Word;

begin
  Result:=SizeOf(Longint);
end;

procedure TLongintField.GetText(var AText: string; ADisplayText: Boolean);

var l : longint;
    fmt : string;

begin
  Atext:='';
  If Not GetData(@l) then exit;
  If ADisplayText or (FEditFormat='') then
    fmt:=FDisplayFormat
  else
    fmt:=FEditFormat;
{  // no formatFloat yet
  If length(fmt)<>0 then
    AText:=FormatFloat(fmt,L)
  else
}
    Str(L,AText);
end;

function TLongintField.GetValue(var AValue: Longint): Boolean;

Type
  PSmallint = ^SmallInt;
  PLongint = ^Longint;
  PWord = ^Word;

Var L : Longint;
    P : PLongint;

begin
  P:=@L;
  Result:=GetData(P);
  If Result then
    Case Datatype of
      ftInteger,ftautoinc  : AValue:=Plongint(P)^;
      ftword               : Avalue:=Pword(P)^;
      ftsmallint           : AValue:=PSmallint(P)^;
    end;
end;

procedure TLongintField.SetAsFloat(AValue: Extended);

begin
  SetAsLongint(Round(Avalue));
end;

procedure TLongintField.SetAsLongint(AValue: Longint);

begin
  If CheckRange(AValue) then
    SetData(@AValue)
  else
    RangeError(Avalue,FMinrange,FMaxRange);
end;

procedure TLongintField.SetAsString(const AValue: string);

Var L,Code : longint;

begin
  If length(AValue)=0 then
    Clear
  else
    begin
    Val(AVAlue,L,Code);
    If Code=0 then
      SetAsLongint(L)
    else
      DatabaseErrorFMT(SNotAnInteger,[Avalue]);
    end;
end;

Function TLongintField.CheckRange(AValue : longint) : Boolean;

begin
  if FMaxValue=0 Then
    Result:=(AValue<=FMaxRange) and (AValue>=FMinRange)
  else
    Result:=(AValue<=FMaxValue) and (AValue>=FMinValue);
end;

Procedure TLongintField.SetMaxValue (AValue : longint);

begin
  If (AValue>=FMinRange) and (AValue<=FMaxRange) then
    FMaxValue:=AValue
  else
    RangeError(AValue,FMinRange,FMaxRange);
end;

Procedure TLongintField.SetMinValue (AValue : longint);

begin
  If (AValue>=FMinRange) and (AValue<=FMaxRange) then
    FMinValue:=AValue
  else
    RangeError(AValue,FMinRange,FMaxRange);
end;

{ TSmallintField }

function TSmallintField.GetDataSize: Word;

begin
  Result:=SizeOf(SmallInt);
end;

constructor TSmallintField.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  SetDataType(ftSmallInt);
  FMinRange:=-32768;
  FMaxRange:=32767;
end;


{ TWordField }

function TWordField.GetDataSize: Word;

begin
  Result:=SizeOf(Word);
end;

constructor TWordField.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  SetDataType(ftWord);
  FMinRange:=0;
  FMaxRange:=65535;
  FValidchars:=['+','0'..'9'];
end;

{ TAutoIncField }

constructor TAutoIncField.Create(AOwner: TComponent);

begin
  Inherited Create(AOWner);
  SetDataType(ftAutoInc);
end;

Procedure TAutoIncField.SetAsLongint(AValue : Longint);

begin
  DataBaseError(SCantSetAutoIncfields);
end;

{ TFloatField }

function TFloatField.GetAsFloat: Extended;

begin
  If Not GetData(@Result) Then
    Result:=0.0;
end;

function TFloatField.GetAsLongint: Longint;

begin
  Result:=Round(GetAsFloat);
end;

function TFloatField.GetAsString: string;

Var R : Extended;

begin
  If GetData(@R) then
    Result:=FloatToStr(R)
  else
    Result:='';
end;

function TFloatField.GetDataSize: Word;

begin
  Result:=SizeOf(Extended);
end;

procedure TFloatField.GetText(var TheText: string; ADisplayText: Boolean);

Var
    fmt : string;
    E : Extended;

begin
  text:='';
  If Not GetData(@E) then exit;
  If ADisplayText or (Length(FEditFormat) = 0) Then
    Fmt:=FDisplayFormat
  else
    Fmt:=FEditFormat;
{  // No formatfloat yet
  If fmt<>'' then
    TheText:=FormatFloat(fmt,E)
  else
}
    Text:=FloatToStrF(E,ffgeneral,FPrecision,0);
end;

procedure TFloatField.SetAsFloat(AValue: Extended);

begin
  If CheckRange(AValue) then
    SetData(@Avalue)
  else
    RangeError(AValue,FMinValue,FMaxValue);
end;

procedure TFloatField.SetAsLongint(AValue: Longint);

begin
  SetAsFloat(Avalue);
end;

procedure TFloatField.SetAsString(const AValue: string);

Var R : Extended;
    Code : longint;

begin
  Val(AVAlue,R,Code);
  If Code<>0 then
    DatabaseErrorFmt(SNotAFloat,[AVAlue])
  Else
    SetAsFloat(R);
end;

constructor TFloatField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDatatype(ftfloat);
end;

Function TFloatField.CheckRange(AValue : Extended) : Boolean;

begin
  If (FMinValue<>0) or (FmaxValue<>0) then
    Result:=(AValue>=FMinValue) and (AVAlue<=FMAxValue)
  else
    Result:=True;
end;


{ TBooleanField }

function TBooleanField.GetAsBoolean: Boolean;

begin
  If not GetData(@Result) then
    Result:=False;
end;

function TBooleanField.GetAsString: string;

Var B : boolean;

begin
  If Getdata(@B) then
    Result:=FDisplays[False,B]
  else
    result:='';
end;

function TBooleanField.GetDataSize: Word;

begin
  Result:=SizeOf(Boolean);
end;

function TBooleanField.GetDefaultWidth: Longint;

begin
  Result:=Length(FDisplays[false,false]);
  If Result<Length(FDisplays[false,True]) then
    Result:=Length(FDisplays[false,True]);
end;

procedure TBooleanField.SetAsBoolean(AValue: Boolean);

begin
  SetData(@AValue);
end;

procedure TBooleanField.SetAsString(const AValue: string);

Var Temp : string;

begin
  Temp:=UpperCase(AValue);
  If Temp=FDisplays[True,True] Then
    SetAsBoolean(True)
  else If Temp=FDisplays[True,False] then
    SetAsBoolean(False)
  else
    DatabaseErrorFmt(SNotABoolean,[AValue]);
end;

constructor TBooleanField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftBoolean);
  DisplayValues:='True;False';
end;

Procedure TBooleanField.SetDisplayValues(AValue : String);

Var I : longint;

begin
  If FDisplayValues<>AValue then
    begin
    I:=Pos(';',AValue);
    If (I<2) or (I=Length(AValue)) then
      DatabaseErrorFmt(SInvalidDisplayValues,[AValue]);
    FdisplayValues:=AValue;
    // Store display values and their uppercase equivalents;
    FDisplays[False,True]:=Copy(AValue,1,I-1);
    FDisplays[True,True]:=UpperCase(FDisplays[False,True]);
    FDisplays[False,False]:=Copy(AValue,I+1,Length(AValue)-i);
    FDisplays[True,False]:=UpperCase(FDisplays[False,False]);
    PropertyChanged(True);
    end;
end;

{ TDateTimeField }


function TDateTimeField.GetAsDateTime: TDateTime;

begin
  If Not GetData(@Result) then
    Result:=0;
end;


function TDateTimeField.GetAsFloat: Extended;

begin
  Result:=GetAsdateTime;
end;


function TDateTimeField.GetAsString: string;

begin
  GetText(Result,False);
end;


function TDateTimeField.GetDataSize: Word;

begin
  Result:=SizeOf(TDateTime);
end;


procedure TDateTimeField.GetText(var TheText: string; ADisplayText: Boolean);

Var R : TDateTime;
    F : String;

begin
  If Not Getdata(@R) then
    TheText:=''
  else
    begin
    If (ADisplayText) and (Length(FDisplayFormat)<>0) then
      F:=FDisplayFormat
    else
      Case DataType of
       ftTime : F:=ShortTimeFormat;
       ftDate : F:=ShortDateFormat;
      else
       F:='c'
      end;
    TheText:=FormatDateTime(F,R);
    end;
end;


procedure TDateTimeField.SetAsDateTime(AValue: TDateTime);

begin
  SetData(@Avalue);
end;


procedure TDateTimeField.SetAsFloat(AValue: Extended);

begin
  SetAsDateTime(AValue);
end;


procedure TDateTimeField.SetAsString(const AValue: string);

Var R : TDateTime;

begin
  R:=StrToDateTime(AVAlue);
  SetData(@R);
end;


constructor TDateTimeField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftDateTime);
end;

{ TDateField }

function TDateField.GetDataSize: Word;

begin
  Result:=SizeOf(TDateTime);
end;


constructor TDateField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftDate);
end;



{ TTimeField }

function TTimeField.GetDataSize: Word;

begin
  Result:=SizeOf(TDateTime);
end;


constructor TTimeField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftTime);
end;



{ TBinaryField }

class procedure TBinaryField.CheckTypeSize(AValue: Longint);

begin
  // Just check for really invalid stuff; actual size is
  // dependent on the record...
  If AValue<1 then
    DatabaseErrorfmt(SInvalidFieldSize,[Avalue]);
end;


function TBinaryField.GetAsString: string;

begin
  Setlength(Result,DataSize);
  GetData(Pointer(Result));
end;


procedure TBinaryField.GetText(var TheText: string; ADisplayText: Boolean);

begin
  TheText:=GetAsString;
end;


procedure TBinaryField.SetAsString(const AValue: string);

Var Buf : PChar;
    Allocated : Boolean;

begin
  Allocated:=False;
  If Length(AVAlue)=DataSize then
    Buf:=PChar(Avalue)
  else
    begin
    GetMem(Buf,DataSize);
    Move(Pchar(Avalue)[0],Buf^,DataSize);
    Allocated:=True;
    end;
  SetData(Buf);
  If Allocated then
    FreeMem(Buf,DataSize);
end;


procedure TBinaryField.SetText(const AValue: string);

begin
  SetAsString(Avalue);
end;


constructor TBinaryField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
end;



{ TBytesField }

function TBytesField.GetDataSize: Word;

begin
  Result:=Size;
end;


constructor TBytesField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftBytes);
  Size:=16;
end;



{ TVarBytesField }

function TVarBytesField.GetDataSize: Word;

begin
  Result:=Size+2;
end;


constructor TVarBytesField.Create(AOwner: TComponent);

begin
  INherited Create(AOwner);
  SetDataType(ftvarbytes);
  Size:=16;
end;



{ TBCDField }

class procedure TBCDField.CheckTypeSize(AValue: Longint);

begin
  //!! To be implemented
end;


function TBCDField.GetAsFloat: Extended;

begin
  //!! To be implemented
end;


function TBCDField.GetAsLongint: Longint;

begin
  //!! To be implemented
end;


function TBCDField.GetAsString: string;

begin
  //!! To be implemented
end;


function TBCDField.GetDataSize: Word;

begin
  //!! To be implemented
end;


function TBCDField.GetDefaultWidth: Longint;

begin
  //!! To be implemented
end;


procedure TBCDField.GetText(var TheText: string; ADisplayText: Boolean);

begin
  //!! To be implemented
end;


procedure TBCDField.SetAsFloat(AValue: Extended);

begin
  //!! To be implemented
end;


procedure TBCDField.SetAsLongint(AValue: Longint);

begin
  //!! To be implemented
end;


procedure TBCDField.SetAsString(const AValue: string);

begin
  //!! To be implemented
end;


constructor TBCDField.Create(AOwner: TComponent);

begin
  DatabaseError('BCD fields not supported yet. Sorry !');
end;



{ TBlobField }


procedure TBlobField.AssignTo(Dest: TPersistent);

begin
  //!! To be implemented
end;

Function TBlobField.GetBlobStream(Mode : TBlobStreamMode) : TStream;

begin
  Result:=FDataset.CreateBlobStream(Self,Mode);
end;

procedure TBlobField.FreeBuffers;

begin
end;


function TBlobField.GetAsString: string;

begin
  With GetBlobStream(bmRead) do
    try
      SetLength(Result,Size);
      ReadBuffer(Pointer(Result)^,Size);
    finally
      Free
    end;
end;


function TBlobField.GetBlobSize: Longint;

begin
  With GetBlobStream(bmread) do
    try
      Result:=Size;
    finally
      Free;
    end;
end;


function TBlobField.GetIsNull: Boolean;

begin
  If Not Modified then
    result:= inherited GetIsnull
  else
    With GetBlobStream(bmread) do
      try
        Result:=(Size=0);
      Finally
        Free;
      end;
end;


procedure TBlobField.GetText(var TheText: string; ADisplayText: Boolean);

begin
  TheText:=GetAsString;
end;


procedure TBlobField.SetAsString(const AValue: string);

begin
  With GetBlobStream(bmwrite) do
    try
      WriteBuffer(Pointer(Avalue)^,Length(Avalue));
    finally
      Free;
    end;
end;


procedure TBlobField.SetText(const AValue: string);

begin
  SetAsString(AValue);
end;


constructor TBlobField.Create(AOwner: TComponent);

begin
  Inherited Create(AOWner);
  SetDataType(ftBlob);
end;


procedure TBlobField.Assign(Source: TPersistent);

begin
  //!! To be implemented
end;


procedure TBlobField.Clear;

begin
  GetBlobStream(bmWrite).free;
end;


class function TBlobField.IsBlob: Boolean;

begin
  Result:=True;
end;


procedure TBlobField.LoadFromFile(const FileName: string);

Var S : TFileStream;

begin
  S:=TFileStream.Create(FileName,fmOpenRead);
  try
    LoadFromStream(S);
  finally
    S.Free;
  end;
end;


procedure TBlobField.LoadFromStream(Stream: TStream);

begin
  With GetBlobStream(bmWrite) do
    Try
      CopyFrom(Stream,0);
    finally
      Free;
    end;
end;


procedure TBlobField.SaveToFile(const FileName: string);

Var S : TFileStream;

begin
  S:=TFileStream.Create(FileName,fmCreate);
  try
    SaveToStream(S);
  finally
    S.Free;
  end;
end;


procedure TBlobField.SaveToStream(Stream: TStream);

Var S : TStream;

begin
  S:=GetBlobStream(bmRead);
  Try
    Stream.CopyFrom(S,0);
  finally
    S.Free;
  end;
end;


procedure TBlobField.SetFieldType(AValue: TFieldType);

begin
  If AValue in [Low(TBlobType)..High(TBlobType)] then
    SetDatatype(Avalue);
end;



{ TMemoField }

constructor TMemoField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftMemo);
end;


{ TGraphicField }

constructor TGraphicField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftGraphic);
end;


{ TFields }

Constructor TFields.Create(ADataset : TDataset);

begin
  FDataSet:=ADataset;
  FFieldList:=TList.Create;
  FValidFieldKinds:=[fkData..fkInternalcalc];
end;

Destructor TFields.Destroy;

begin
  FFieldList.Free;
end;

Procedure Tfields.Changed;

begin
  If Assigned(FOnChange) then
    FOnChange(Self);
end;

Procedure TFields.CheckfieldKind(Fieldkind : TFieldKind; Field : TField);

begin
  If Not (FieldKind in ValidFieldKinds) Then
    DatabaseErrorFmt(SInvalidFieldKind,[Field.FieldName]);
end;

Function Tfields.GetCount : Longint;

begin
  Result:=FFieldList.Count;
end;


Function TFields.GetField (Index : longint) : TField;

begin
  Result:=Tfield(FFieldList[Index]);
end;

Procedure TFields.SetFieldIndex (Field : TField;Value : Integer);

Var Old : Longint;

begin
  Old := FFieldList.indexOf(Field);
  If Old=-1 then
    Exit;
  // Check value
  If Value<FFieldList.Count Then Value:=0;
  If Value>=Count then Value:=Count-1;
  If Value<>Old then
    begin
    FFieldList.Delete(Old);
    FFieldList.Insert(Value,Field);
    Field.PropertyChanged(True);
    Changed;
    end;
end;

Procedure TFields.Add(Field : TField);

begin
  CheckFieldName(Field.FieldName);
  FFieldList.Add(Field);
  Field.FFields:=Self;
  Changed;
end;

Procedure TFields.CheckFieldName (Const Value : String);

Var I : longint;
    S : String;

begin
  If FindField(Value)<>Nil then
    begin
    S:=UpperCase(Value);
    For I:=0 To FFieldList.Count-1 do
      If S=UpperCase(TField(FFieldList[i]).FieldName) Then
        DataBaseErrorFmt(SDuplicateFieldName,[Value],FDataset);
    end;
end;

Procedure TFields.CheckFieldNames (Const Value : String);


Var I : longint;
    S,T : String;
begin
  T:=Value;
  Repeat
    I:=Pos(T,';');
    If I=0 Then I:=Length(T);
    S:=Copy(T,1,I-1);
    Delete(T,1,I);
    // Will raise an error if no such field...
    FieldByName(S);
  Until (T='');
end;

Procedure TFields.Clear;

begin
end;

Function TFields.FindField (Const Value : String) : TField;

Var S : String;
    I : longint;

begin
  Result:=Nil;
  S:=UpperCase(Value);
  For I:=0 To FFieldList.Count-1 do
    If S=UpperCase(TField(FFieldList[i]).FieldName) Then
      Begin
      {$ifdef dsdebug}
      Writeln ('Found field ',Value);
      {$endif}
      Result:=TField(FFieldList[I]);
      Exit;
      end;
end;

Function TFields.FieldByName (Const Value : String) : TField;

begin
  Result:=FindField(Value);
  If result=Nil then
    DatabaseErrorFmt(SFieldNotFound,[Value],FDataset);
end;

Function TFields.FieldByNumber(FieldNo : Integer) : TField;

Var i : Longint;

begin
  Result:=Nil;
  For I:=0 to FFieldList.Count-1 do
    If FieldNo=TField(FFieldList[I]).FieldNo then
      begin
      Result:=TField(FFieldList[i]);
      Exit;
      end;
end;

Procedure TFields.GetFieldNames (Values : TStrings);

Var i : longint;

begin
  Values.Clear;
  For I:=0 to FFieldList.Count-1 do
    Values.Add(Tfield(FFieldList[I]).FieldName);
end;

Function TFields.IndexOf(Field : TField) : Longint;

Var i : longint;

begin
  Result:=-1;
  For I:=0 To FFieldList.Count-1 do
    If Pointer(Field)=FFieldList[i] Then
      Exit(I);
end;

procedure TFields.Remove(Value : TField);

Var I : longint;

begin
  I:=IndexOf(Value);
  If I<>0 then
    FFieldList.Delete(I);
end;

{
  $Log$
  Revision 1.8  2003-09-14 13:22:14  michael
  + Fixed error in TField.GetCanModify reported by Andrew Johnson

  Revision 1.7  2002/09/07 15:15:23  peter
    * old logs removed and tabs fixed

}
