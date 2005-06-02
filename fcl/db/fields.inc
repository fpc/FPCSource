{
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
Procedure DumpMem (P : Pointer;Size : Longint);

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
  FDisplayName := '';
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
end;

Destructor TFieldDef.Destroy;

Var I : longint;

begin
  Inherited destroy;
end;

procedure TFieldDef.Assign(APersistent: TPersistent);
var fd: TFieldDef;
begin
  fd := nil;
  if APersistent is TFieldDef then
    fd := APersistent as TFieldDef;
  if Assigned(fd) then begin
    Collection.BeginUpdate;
    try
      Name := fd.Name;
      DataType := fd.DataType;
      Size := fd.Size;
      Precision := fd.Precision;
      FRequired := fd.Required;
    finally
      Collection.EndUpdate;
    end;
  end else
  inherited Assign(APersistent);
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
    Result.FFieldName:=FName;
    Result.FDisplayLabel:=FDisplayName;
    Result.FFieldNo:=Self.FieldNo;
    Result.SetFieldType(DataType);
    Result.FReadOnly:= (faReadOnly in Attributes);
{$ifdef dsdebug}
    Writeln ('TFieldDef.CReateField : Trying to set dataset');
{$endif dsdebug}
{$ifdef dsdebug}
    Writeln ('TFieldDef.CReateField : Result Fieldno : ',Result.FieldNo,' Self : ',FieldNo);
{$endif dsdebug}
    Result.Dataset:=TFieldDefs(Collection).Dataset;
    If Result is TFloatField then
      TFloatField(Result).Precision:=FPrecision;
  except
    Result.Free;
    Raise;
  end;

end;

procedure TFieldDef.SetAttributes(AValue: TFieldAttributes);
begin
  FAttributes := AValue;
  Changed(False);
end;

procedure TFieldDef.SetDataType(AValue: TFieldType);
begin
  FDataType := AValue;
  Changed(False);
end;

procedure TFieldDef.SetPrecision(const AValue: Longint);
begin
  FPrecision := AValue;
  Changed(False);
end;

procedure TFieldDef.SetSize(const AValue: Word);
begin
  FSize := AValue;
  Changed(False);
end;

function TFieldDef.GetDisplayName: string;
begin
  Result := FDisplayName;
  if Result = '' then
    Result := Fname;
end;

procedure TFieldDef.SetDisplayName(const AValue: string);
begin
  if (AValue <> '') and (AnsiCompareText(AValue, DisplayName) <> 0) and
    (Collection is TOwnedCollection) and
    (TFieldDefs(Collection).IndexOf(AValue) >= 0) then
    DatabaseErrorFmt(SDuplicateName, [AValue, Collection.ClassName]);
  FName := AValue;
end;

Function TFieldDef.GetFieldClass : TFieldClass;

begin
  //!! Should be owner as tdataset but that doesn't work ??

  If Assigned(Collection) And
     (Collection is TFieldDefs) And
     Assigned(TFieldDefs(Collection).Dataset) then
    Result:=TFieldDefs(Collection).Dataset.GetFieldClass(FDataType)
  else
    Result:=Nil;
end;

{ ---------------------------------------------------------------------
    TFieldDefs
  ---------------------------------------------------------------------}

{
destructor TFieldDefs.Destroy;

begin
  FItems.Free;
  // This will destroy all fielddefs since we own them...
  Inherited Destroy;
end;
}

procedure TFieldDefs.Add(const AName: string; ADataType: TFieldType);

begin
  Add(AName,ADatatype,0,False);
end;

procedure TFieldDefs.Add(const AName: string; ADataType: TFieldType; ASize : Word);

begin
  Add(AName,ADatatype,ASize,False);
end;

procedure TFieldDefs.Add(const AName: string; ADataType: TFieldType; ASize: Word;
  ARequired: Boolean);

begin
  If Length(AName)=0 Then
    DatabaseError(SNeedFieldName);
  // the fielddef will register itself here as a owned component.
  // fieldno is 1 based !
  BeginUpdate;
  try
    TFieldDef.Create(Self,AName,ADataType,ASize,Arequired,Count+1);
  finally
    EndUpdate;
  end;
end;

function TFieldDefs.GetItem(Index: Longint): TFieldDef;

begin
  Result := TFieldDef(inherited Items[Index]);;
end;

function TFieldDefs.GetDataset: TDataset;
begin
  Result := TDataset(GetOwner);
end;

procedure TFieldDefs.SetItem(Index: Longint; const AValue: TFieldDef);
begin
  inherited Items[Index] := AValue;
end;

procedure TFieldDefs.SetItemName(AItem: TCollectionItem);
begin
  if AItem is TFieldDef then
    with AItem as TFieldDef do
      if Name = '' then
        Name := Dataset.Name + Copy(ClassName, 2, 5) + IntToStr(ID+1)
  else inherited SetItemName(AItem);
end;

constructor TFieldDefs.Create(ADataset: TDataset);
begin
  Inherited Create(TPersistent(ADataset), TFieldDef);
end;

procedure TFieldDefs.Assign(FieldDefs: TFieldDefs);

Var I : longint;

begin
  Clear;
  For i:=0 to FieldDefs.Count-1 do
    With FieldDefs[i] do
      Add(Name,DataType,Size,Required);
end;

{
procedure TFieldDefs.Clear;

Var I : longint;

begin
  For I:=FItems.Count-1 downto 0 do
    TFieldDef(Fitems[i]).Free;
  FItems.Clear;
end;
}

function TFieldDefs.Find(const AName: string): TFieldDef;

Var I : longint;

begin
  I:=IndexOf(AName);
  If I=-1 Then
    DataBaseErrorFmt(SUnknownField,[AName,DataSet.Name]);
  Result:=Items[i];
end;

function TFieldDefs.IndexOf(const AName: string): Longint;

Var I : longint;

begin
  For I:=0 to Count-1 do
    If AnsiCompareText(Items[I].Name,AName)=0 then
      begin
      Result:=I;
      Exit;
      end;
  Result:=-1;
end;

procedure TFieldDefs.Update;

begin
  DataSet.InitFieldDefs;
end;

Function TFieldDefs.AddFieldDef : TFieldDef;

begin
  Result:=TFieldDef.Create(Self,'',ftUnknown,0,False,Count+1);
end;

{ ---------------------------------------------------------------------
    TField
  ---------------------------------------------------------------------}

Const
  SBoolean = 'Boolean';
  SDateTime = 'TDateTime';
  SFloat = 'Float';
  SInteger = 'Integer';
  SVariant = 'Variant';
  SString = 'String';

constructor TField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FVisible:=True;
  FValidChars:=[#0..#255];

  FProviderFlags := [pfInUpdate,pfInWhere];
end;

destructor TField.Destroy;

begin
  IF Assigned(FDataSet) then
    begin
    FDataSet.Active:=False;
    if Assigned(FFields) then
      FFields.Remove(Self);
    end;
  Inherited Destroy;
end;

function TField.AccessError(const TypeName: string): EDatabaseError;

begin
  Result:=EDatabaseError.CreateFmt(SinvalidTypeConversion,[TypeName,FFieldName]);
end;

procedure TField.Assign(Source: TPersistent);

begin
  if Source = nil then Clear
  else if Source is TField then begin
    Value := TField(Source).Value;
  end else
    inherited Assign(Source);
end;

procedure TField.AssignValue(const Value: TVarRec);
  procedure Error;
  begin
    DatabaseErrorFmt(SFieldValueError, [DisplayName]);
  end;

begin
  with Value do
    case VType of
      vtInteger:
        AsInteger := VInteger;
      vtBoolean:
        AsBoolean := VBoolean;
      vtChar:
        AsString := VChar;
      vtExtended:
        AsFloat := VExtended^;
      vtString:
        AsString := VString^;
      vtPointer:
        if VPointer <> nil then Error;
      vtPChar:
        AsString := VPChar;
      vtObject:
        if (VObject = nil) or (VObject is TPersistent) then
          Assign(TPersistent(VObject))
        else
          Error;
      vtAnsiString:
        AsString := string(VAnsiString);
//      vtCurrency:
//        AsCurrency := VCurrency^;
      vtVariant:
        if not VarIsClear(VVariant^) then Self.Value := VVariant^;
      vtWideString:
        AsString := WideString(VWideString);
      vtInt64:
        Self.Value := VInt64^;
    else
      Error;
    end;
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
  if FieldKind in [fkData, fkInternalCalc] then
    SetData(Nil);
end;

procedure TField.DataChanged;

begin
  FDataset.DataEvent(deFieldChange,ptrint(Self));
end;

procedure TField.FocusControl;

begin
  FDataSet.DataEvent(deFocusControl,ptrint(Self));
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

function TField.GetAsFloat: Double;

begin
  AccessError(SDateTime);
end;

function TField.GetAsLongint: Longint;

begin
  AccessError(SInteger);
end;

function TField.GetAsVariant: Variant;

begin
  AccessError(SVariant);
end;


function TField.GetAsInteger: Integer;

begin
  Result:=GetAsLongint;
end;

function TField.GetAsString: string;

begin
  AccessError(SString);
end;

function TField.GetOldValue: Variant;

var SaveState : TDatasetState;

begin
  SaveState := FDataset.State;
  try
    FDataset.SetTempState(dsOldValue);
    Result := GetAsVariant;
  finally
    FDataset.RestoreState(SaveState);
  end;
end;

function TField.GetNewValue: Variant;

var SaveState : TDatasetState;

begin
  SaveState := FDataset.State;
  try
    FDataset.SetTempState(dsNewValue);
    Result := GetAsVariant;
  finally
    FDataset.RestoreState(SaveState);
  end;
end;

procedure TField.SetNewValue(const AValue: Variant);

var SaveState : TDatasetState;

begin
  SaveState := FDataset.State;
  try
    FDataset.SetTempState(dsNewValue);
    SetAsVariant(AValue);
  finally
    FDataset.RestoreState(SaveState);
  end;
end;

function TField.GetCurValue: Variant;

var SaveState : TDatasetState;

begin
  SaveState := FDataset.State;
  try
    FDataset.SetTempState(dsCurValue);
    Result := GetAsVariant;
  finally
    FDataset.RestoreState(SaveState);
  end;
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
    Result:=FDataset.GetFieldData(Self,Buffer,False);
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

Function TField.IsDisplayStored : Boolean;

begin
  Result:=(DisplayLabel<>FieldName);
end;

function TField.getIndex : longint;

begin
  If Assigned(FDataset) then
    Result:=FDataset.FFieldList.IndexOf(Self)
  else
    Result:=-1;
end;

function TField.GetAsCurrency: Currency;
begin
  Result := GetAsFloat;
end;

procedure TField.SetAlignment(const AValue: TAlignMent);
begin
  if FAlignment <> AValue then
    begin
    FAlignment := Avalue;
    PropertyChanged(false);
    end;
end;

procedure TField.SetIndex(AValue: Integer);
begin
  if FFields <> nil then FFields.SetFieldIndex(Self, AValue)
end;

procedure TField.SetAsCurrency(AValue: Currency);
begin
  SetAsFloat(AValue);
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

procedure TField.SetAsFloat(AValue: Double);

begin
  AccessError(SFloat);
end;

procedure TField.SetAsVariant(AValue: Variant);

begin
  if VarIsNull(AValue) then
    Clear
  else
    try
      SetVarValue(AValue);
    except
      on EVariantError do DatabaseErrorFmt(SFieldValueError, [DisplayName]);
    end;
end;


procedure TField.SetAsLongint(AValue: Longint);

begin
  AccessError(SInteger);
end;

procedure TField.SetAsInteger(AValue: Integer);

begin
  SetAsLongint(AValue);
end;

procedure TField.SetAsString(const AValue: string);

begin
  AccessError(SString);
end;

procedure TField.SetData(Buffer: Pointer);

begin
  If Not Assigned(FDataset) then
    EDatabaseError.CreateFmt(SNoDataset,[FieldName]);
  FDataSet.SetFieldData(Self,Buffer, False);
end;

Procedure TField.SetDataset (Value : TDataset);

begin
{$ifdef dsdebug}
  Writeln ('Setting dataset');
{$endif}
  If Value=FDataset then exit;
  If Assigned(FDataset) Then
    begin
    FDataset.CheckInactive;
    FDataset.FFieldList.Remove(Self);
    end;
  If Assigned(Value) then
    begin
    Value.CheckInactive;
    Value.FFieldList.Add(Self);
    end;
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

procedure TField.SetVarValue(const AValue: Variant);
begin
  AccessError(SVariant);
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

procedure TField.SetDisplayLabel(const AValue: string);
begin
  if FDisplayLabel<>Avalue then
    begin
    FDisplayLabel:=Avalue;
    PropertyChanged(true);
    end;
end;

procedure TField.SetDisplayWidth(const AValue: Longint);
begin
  if FDisplayWidth<>AValue then
    begin
    FDisplayWidth:=AValue;
    PropertyChanged(True);
    end;
end;

function TField.GetDisplayWidth: integer;
begin
  if FDisplayWidth=0 then
    result:=GetDefaultWidth
  else
    result:=FDisplayWidth;
end;

procedure TField.SetReadOnly(const AValue: Boolean);
begin
  if (FReadOnly<>Avalue) then
    begin
    FReadOnly:=AValue;
    PropertyChanged(True);
    end;
end;

procedure TField.SetVisible(const AValue: Boolean);
begin
  if FVisible<>Avalue then
    begin
    FVisible:=AValue;
    PropertyChanged(True);
    end;
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

function TStringField.GetAsFloat: Double;

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

function TStringField.GetAsVariant: Variant;

Var s : string;

begin
  If GetValue(s) then
    Result:=s
  else
    Result:=Null;
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

procedure TStringField.SetAsFloat(AValue: Double);

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

procedure TStringField.SetVarValue(const AValue: Variant);
begin
  SetAsString(AValue);
end;

{ ---------------------------------------------------------------------
    TNumericField
  ---------------------------------------------------------------------}


constructor TNumericField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  AlignMent:=taRightJustify;
end;

procedure TNumericField.RangeError(AValue, Min, Max: Double);

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

function TLongintField.GetAsFloat: Double;

begin
  Result:=GetAsLongint;
end;

function TLongintField.GetAsLongint: Longint;

begin
  If Not GetValue(Result) then
    Result:=0;
end;

function TLongintField.GetAsVariant: Variant;

Var L : Longint;

begin
  If GetValue(L) then
    Result:=L
  else
    Result:=Null;
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
  If Not GetValue(l) then exit;
  If ADisplayText or (FEditFormat='') then
    fmt:=FDisplayFormat
  else
    fmt:=FEditFormat;
  If length(fmt)<>0 then
    AText:=FormatFloat(fmt,L)
  else
    Str(L,AText);
end;

function TLongintField.GetValue(var AValue: Longint): Boolean;

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

procedure TLongintField.SetAsFloat(AValue: Double);

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

procedure TLongintField.SetVarValue(const AValue: Variant);
begin
  SetAsLongint(AValue);
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
  result := true;
  if (FMaxValue=0) then
    begin
    if (AValue>FMaxRange) Then result := false;
    end
  else
    if AValue>FMaxValue then result := false;

  if (FMinValue=0) then
    begin
    if (AValue<FMinRange) Then result := false;
    end
  else
    if AValue<FMinValue then result := false;
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

{ ---------------------------------------------------------------------
    TLargeintField
  ---------------------------------------------------------------------}


constructor TLargeintField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDatatype(ftLargeint);
  FMinRange:=Low(Largeint);
  FMaxRange:=High(Largeint);
  FValidchars:=['+','-','0'..'9'];
end;

function TLargeintField.GetAsFloat: Double;

begin
  Result:=GetAsLargeint;
end;

function TLargeintField.GetAsLargeint: Largeint;

begin
  If Not GetValue(Result) then
    Result:=0;
end;

function TLargeIntField.GetAsVariant: Variant;

Var L : Largeint;

begin
  If GetValue(L) then
    Result:=L
  else
    Result:=Null;
end;

function TLargeintField.GetAsLongint: Longint;

begin
  Result:=GetAsLargeint;
end;

function TLargeintField.GetAsString: string;

Var L : Largeint;

begin
  If GetValue(L) then
    Result:=IntTostr(L)
  else
    Result:='';
end;

function TLargeintField.GetDataSize: Word;

begin
  Result:=SizeOf(Largeint);
end;

procedure TLargeintField.GetText(var AText: string; ADisplayText: Boolean);

var l : largeint;
    fmt : string;

begin
  Atext:='';
  If Not GetValue(l) then exit;
  If ADisplayText or (FEditFormat='') then
    fmt:=FDisplayFormat
  else
    fmt:=FEditFormat;
  If length(fmt)<>0 then
    AText:=FormatFloat(fmt,L)
  else
    Str(L,AText);
end;

function TLargeintField.GetValue(var AValue: Largeint): Boolean;

type
  PLargeint = ^Largeint;

Var P : PLargeint;

begin
  P:=@AValue;
  Result:=GetData(P);
end;

procedure TLargeintField.SetAsFloat(AValue: Double);

begin
  SetAsLargeint(Round(Avalue));
end;

procedure TLargeintField.SetAsLargeint(AValue: Largeint);

begin
  If CheckRange(AValue) then
    SetData(@AValue)
  else
    RangeError(Avalue,FMinrange,FMaxRange);
end;

procedure TLargeintField.SetAsLongint(AValue: Longint);

begin
  SetAsLargeint(Avalue);
end;

procedure TLargeintField.SetAsString(const AValue: string);

Var L     : largeint;
    code  : longint;

begin
  If length(AValue)=0 then
    Clear
  else
    begin
    Val(AVAlue,L,Code);
    If Code=0 then
      SetAsLargeint(L)
    else
      DatabaseErrorFMT(SNotAnInteger,[Avalue]);
    end;
end;

procedure TLargeintField.SetVarValue(const AValue: Variant);
begin
  SetAsLargeint(AValue);
end;

Function TLargeintField.CheckRange(AValue : largeint) : Boolean;

begin
  result := true;
  if (FMaxValue=0) then
    begin
    if (AValue>FMaxRange) Then result := false;
    end
  else
    if AValue>FMaxValue then result := false;

  if (FMinValue=0) then
    begin
    if (AValue<FMinRange) Then result := false;
    end
  else
    if AValue<FMinValue then result := false;
end;

Procedure TLargeintField.SetMaxValue (AValue : largeint);

begin
  If (AValue>=FMinRange) and (AValue<=FMaxRange) then
    FMaxValue:=AValue
  else
    RangeError(AValue,FMinRange,FMaxRange);
end;

Procedure TLargeintField.SetMinValue (AValue : largeint);

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
  FReadOnly:=True;
end;

Procedure TAutoIncField.SetAsLongint(AValue : Longint);

begin
  DataBaseError(SCantSetAutoIncfields);
end;

{ TFloatField }

function TFloatField.GetAsFloat: Double;

begin
  If Not GetData(@Result) Then
    Result:=0.0;
end;

function TFloatField.GetAsVariant: Variant;

Var f : Double;

begin
  If GetData(@f) then
    Result := f
  else
    Result:=Null;
end;

function TFloatField.GetAsLongint: Longint;

begin
  Result:=Round(GetAsFloat);
end;

function TFloatField.GetAsString: string;

Var R : Double;

begin
  If GetData(@R) then
    Result:=FloatToStr(R)
  else
    Result:='';
end;

function TFloatField.GetDataSize: Word;

begin
  Result:=SizeOf(Double);
end;

procedure TFloatField.GetText(var TheText: string; ADisplayText: Boolean);

Var
    fmt : string;
    E : Double;

begin
  TheText:='';
  If Not GetData(@E) then exit;
  If ADisplayText or (Length(FEditFormat) = 0) Then
    Fmt:=FDisplayFormat
  else
    Fmt:=FEditFormat;

  If fmt<>'' then
    TheText:=FormatFloat(fmt,E)
  else
    TheText:=FloatToStrF(E,ffgeneral,FPrecision,0);
end;

procedure TFloatField.SetAsFloat(AValue: Double);

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

Var R : Double;

begin
  try
    R := StrToFloat(AValue);
    SetAsFloat(R);
  except
    DatabaseErrorFmt(SNotAFloat, [AValue]);
  end;
end;

procedure TFloatField.SetVarValue(const AValue: Variant);
begin
  SetAsFloat(Avalue);
end;

constructor TFloatField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDatatype(ftfloat);
  FPrecision:=15;
  FValidChars := [DecimalSeparator, '+', '-', '0'..'9', 'E', 'e'];
end;

Function TFloatField.CheckRange(AValue : Double) : Boolean;

begin
  If (FMinValue<>0) or (FmaxValue<>0) then
    Result:=(AValue>=FMinValue) and (AVAlue<=FMAxValue)
  else
    Result:=True;
end;

{ TCurrencyField }

Constructor TCurrencyField.Create(AOwner: TComponent);

begin
  inherited Create(AOwner);
  SetDataType(ftCurrency);
end;

procedure TCurrencyField.GetText(var TheText: string; ADisplayText: Boolean);

Var
    fmt : string;
    ff: TFloatFormat;
    E : Double;

begin
  TheText:='';
  If Not GetData(@E) then exit;
  If ADisplayText or (Length(FEditFormat) = 0) Then
    Fmt:=FDisplayFormat
  else
    Fmt:=FEditFormat;

  if ADisplayText then 
    ff := ffCurrency 
  else 
    ff := ffFixed;
        
  If fmt<>'' then
    TheText:=FormatFloat(fmt, E)
  else
    TheText:=FloatToStrF(E, ff, FPrecision, CurrencyDecimals);
end;


{ TBooleanField }

function TBooleanField.GetAsBoolean: Boolean;

var b : wordbool;

begin
  If GetData(@b) then
    result := b
  else
    Result:=False;
end;

function TBooleanField.GetAsVariant: Variant;

Var b : wordbool;

begin
  If GetData(@b) then
    Result := b
  else
    Result:=Null;
end;

function TBooleanField.GetAsString: string;

Var B : wordbool;

begin
  If Getdata(@B) then
    Result:=FDisplays[False,B]
  else
    result:='';
end;

function TBooleanField.GetDataSize: Word;

begin
  Result:=SizeOf(wordBool);
end;

function TBooleanField.GetDefaultWidth: Longint;

begin
  Result:=Length(FDisplays[false,false]);
  If Result<Length(FDisplays[false,True]) then
    Result:=Length(FDisplays[false,True]);
end;

procedure TBooleanField.SetAsBoolean(AValue: Boolean);

var b : wordbool;

begin
  b := AValue;
  SetData(@b);
end;

procedure TBooleanField.SetAsString(const AValue: string);

Var Temp : string;

begin
  Temp:=UpperCase(AValue);
  if Temp='' then
    Clear
  else if pos(Temp, FDisplays[True,True])=1 then
    SetAsBoolean(True)
  else if pos(Temp, FDisplays[True,False])=1 then
    SetAsBoolean(False)
  else
    DatabaseErrorFmt(SNotABoolean,[AValue]);
end;

procedure TBooleanField.SetVarValue(const AValue: Variant);
begin
  SetAsBoolean(AValue);
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

procedure TDateTimeField.SetDisplayFormat(const AValue: string);
begin
  if FDisplayFormat<>AValue then begin
    FDisplayFormat:=AValue;
    PropertyChanged(True);
  end;
end;

function TDateTimeField.GetAsDateTime: TDateTime;

begin
  If Not GetData(@Result) then
    Result:=0;
end;

procedure TDateTimeField.SetVarValue(const AValue: Variant);
begin
  SetAsDateTime(AValue);
end;

function TDateTimeField.GetAsVariant: Variant;

Var d : tDateTime;

begin
  If Getdata(@d) then
    Result := d
  else
    Result:=Null;
end;

function TDateTimeField.GetAsFloat: Double;

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


procedure TDateTimeField.SetAsFloat(AValue: Double);

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

constructor TDateField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftDate);
end;


{ TTimeField }

constructor TTimeField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  SetDataType(ftTime);
end;

procedure TTimeField.SetAsString(const AValue: string);
Var R : TDateTime;
begin
  R:=StrToTime(AVAlue);
  SetData(@R);
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

procedure TBinaryField.SetVarValue(const AValue: Variant);
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
  If not (AValue in [1..4]) then
    DatabaseErrorfmt(SInvalidFieldSize,[Avalue]);
end;

function TBCDField.GetAsCurrency: Currency;

Var C : system.Currency;

begin
  if GetData(@C) then
    result := C;
end;

function TBCDField.GetAsVariant: Variant;

Var c : system.Currency;

begin
  If GetData(@c) then
    Result := c
  else
    Result:=Null;
end;

function TBCDField.GetAsFloat: Double;

begin
  result := GetAsCurrency;
end;


function TBCDField.GetAsLongint: Longint;

begin
  result := round(GetAsCurrency);
end;


function TBCDField.GetAsString: string;

var c : system.currency;

begin
  If GetData(@C) then
    Result:=CurrToStr(C)
  else
    Result:='';
end;

function TBCDField.GetValue(var AValue: Currency): Boolean;

begin
  Result := GetData(@AValue);
end;
  
function TBCDField.GetDataSize: Word;

begin
  result := sizeof(currency);
end;

function TBCDField.GetDefaultWidth: Longint;

begin
  if precision > 0 then result := precision
    else result := 10;
end;

procedure TBCDField.GetText(var TheText: string; ADisplayText: Boolean);
var
  c : system.currency;
  fmt: String;
begin
  if GetData(@C) then begin
    if aDisplayText or (FEditFormat='') then
      fmt := FDisplayFormat
    else
      fmt := FEditFormat;
    if fmt<>'' then
      TheText := FormatFloat(fmt,C)
    else if fCurrency then begin
      if aDisplayText then
        TheText := FloatToStrF(C, ffCurrency, FPrecision, 2{digits?})
      else
        TheText := FloatToStrF(C, ffFixed, FPrecision, 2{digits?});
    end else
      TheText := FloatToStrF(C, ffGeneral, FPrecision, 0{digits?});
  end else
    TheText := '';
end;

procedure TBCDField.SetAsCurrency(AValue: Currency);

begin
  If CheckRange(AValue) then
    setdata(@AValue)
  else
    RangeError(AValue,FMinValue,FMaxvalue);
end;

procedure TBCDField.SetVarValue(const AValue: Variant);
begin
  SetAsCurrency(AValue);
end;

Function TBCDField.CheckRange(AValue : Currency) : Boolean;

begin
  If (FMinValue<>0) or (FmaxValue<>0) then
    Result:=(AValue>=FMinValue) and (AVAlue<=FMaxValue)
  else
    Result:=True;
end;

procedure TBCDField.SetAsFloat(AValue: Double);

begin
  SetAsCurrency(AValue);
end;


procedure TBCDField.SetAsLongint(AValue: Longint);

begin
  SetAsCurrency(AValue);
end;


procedure TBCDField.SetAsString(const AValue: string);

begin
  SetAsCurrency(strtocurr(AValue));
end;

constructor TBCDField.Create(AOwner: TComponent);

begin
  Inherited Create(AOwner);
  FMaxvalue := 0;
  FMinvalue := 0;
  SetDataType(ftBCD);
  FPrecision := 15;
  Size:=4;
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
var
  Stream: TStream;
begin
  Stream := GetBlobStream(bmRead);
  if Stream <> nil then
    With Stream do
      try
        SetLength(Result,Size);
        ReadBuffer(Pointer(Result)^,Size);
      finally
        Free
      end
  else
    Result := '(blob)';
end;


function TBlobField.GetBlobSize: Longint;
var
  Stream: TStream;
begin
  Stream := GetBlobStream(bmread);
  if Stream <> nil then
    With Stream do
      try
        Result:=Size;
      finally
        Free;
      end
  else
    result := 0;
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

procedure TBlobField.SetVarValue(const AValue: Variant);
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
  if FFieldList <> nil then Clear;
  FFieldList.Free;
  inherited Destroy;
end;

Procedure Tfields.Changed;

begin
  if (FDataSet <> nil) and not (csDestroying in FDataSet.ComponentState) then
    FDataSet.DataEvent(deFieldListChange, 0);
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

procedure Tfields.SetField(Index: Integer; Value: TField);
begin
  Fields[Index].Assign(Value);
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
    DataBaseErrorFmt(SDuplicateFieldName,[Value],FDataset);
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
  with FFieldList do
    while Count > 0 do begin
      TField(Last).FDataSet := Nil;
      TField(Last).Free;
      FFieldList.Delete(Count - 1);
    end;
  Changed;
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

begin
  Result:=FFieldList.IndexOf(Field);
end;

procedure TFields.Remove(Value : TField);

begin
  FFieldList.Remove(Value);
  Value.FFields := nil;
  Changed;
end;

{
  $Log: fields.inc,v $
  Revision 1.34  2005/05/12 18:52:32  michael
    - Patch from Jesus Reyes:
    * TFloatField.SetAsString doesn't look at DecimalSeparator for
      converting a string into a Float
    * TBooleanField.SetAsString won't accept an empty string as a valid
      value, also the patch allows strings as T, F or any starting
      substring of DisplayValues to be accepted.

  Revision 1.33  2005/04/26 16:48:58  michael
   * Some patches from Uberto Barbini
     + TLoginEvent more Delphi compatible (D5 and bigger).
     * AsCurrency Property for TField.

  Revision 1.32  2005/04/26 15:45:30  michael
  + Patch from Sergey Smirnov to fix TTimeField.AsString

  Revision 1.31  2005/04/16 10:02:13  michael
  + Patch to show/enter alternate charsets in grid editor.

  Revision 1.30  2005/04/10 22:18:43  joost
  Patch from Alexandrov Alexandru
  - implemented TDataset.BindFields
  - master-detail relation implemented
  - improved variant-support for fields
  - implemented TField.Assign and TField.AssignValue

  Revision 1.29  2005/04/04 07:30:51  michael
  + Patch from Jesus reyes to notify changes to DisplayFormat

  Revision 1.28  2005/03/23 08:17:51  michael
  + Several patches from Jose A. Rimon
  # Prevents "field not found" error, when use a query without the primary key
  Set SQLlen of different data types
   Use AliasName instead of SQLname to avoid "duplicate field name" error, for
  example when using "coalesce" more than once
  use SQLScale in ftLargeInt to get actual values
   Send query to server with different lines. Provides line info in sqlErrors
  and allows single line comments

  Revision 1.27  2005/03/15 22:44:22  michael
    * Patch from Luiz Americo
      - fixes a memory leak in TBlobField.GetAsString

  Revision 1.26  2005/03/01 14:00:53  joost
  - Fix to avoid scientific format in TDBCFields from Jesus Reyes

  Revision 1.25  2005/02/28 16:19:07  joost
  - Boolean fields are wordbool's now

  Revision 1.24  2005/02/16 09:31:58  michael
  - Remove TTimeField and TDateField GetDataSize functions since both are exactly
    equal to their ancestor: TDateTimeField.GetDataSize
  - TAutoInc fields are set to ReadyOnly on create
  - In TFieldDef.CreateField the presence of faReadyOnly in Attributes is respected

  Revision 1.23  2005/02/14 17:13:12  peter
    * truncate log

  Revision 1.22  2005/02/07 11:22:23  joost
    - Implemented TField.DisplayName

  Revision 1.21  2005/01/12 10:29:54  michael
   * Patch from Joost Van der Sluis:
     - removed some duplicate definitions
     - restructured SetDataset
     - implemented UpdateMode, ProviderFlags

}
