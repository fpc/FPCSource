
{ TParams }

Function TParams.GetItem(Index: Integer): TParam;
begin
  Result:=(Inherited GetItem(Index)) as TParam;
end;

Function TParams.GetParamValue(const ParamName: string): Variant;
begin
  Result:=ParamByName(ParamName).Value;
end;

Procedure TParams.SetItem(Index: Integer; Value: TParam);
begin
  Inherited SetItem(Index,Value);
end;

Procedure TParams.SetParamValue(const ParamName: string; const Value: Variant);
begin
  ParamByName(ParamName).Value:=Value;
end;

Procedure TParams.AssignTo(Dest: TPersistent);
begin
 if (Dest is TParams) then
   TParams(Dest).Assign(Self)
 else
   inherited AssignTo(Dest);
end;

Function TParams.GetDataSet: TDataSet;
begin
  If (FOwner is TDataset) Then
    Result:=TDataset(FOwner)
  else
    Result:=Nil;
end;

Function TParams.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;


constructor TParams.Create(AOwner: TPersistent);
begin
  Inherited Create(TParam);
  Fowner:=AOwner;
end;

constructor TParams.Create;
begin
  Create(TPersistent(Nil));
end;

Procedure TParams.AddParam(Value: TParam);
begin
  Value.Collection:=Self;
end;

Procedure TParams.AssignValues(Value: TParams);

Var
  I : Integer;
  P,PS : TParam;

begin
  For I:=0 to Value.Count-1 do
    begin
    PS:=Value[i];
    P:=FindParam(PS.Name);
    If Assigned(P) then
      P.Assign(PS);
    end;
end;

Function TParams.CreateParam(FldType: TFieldType; const ParamName: string;
  ParamType: TParamType): TParam;

begin
  Result:=Add as TParam;
  With Result do
    begin
    Name:=ParamName;
    DataType:=FldType;
    ParamType:=ParamType;
    end;
end;

Function TParams.FindParam(const Value: string): TParam;

Var
  I : Integer;

begin
  Result:=Nil;
  I:=Count-1;
  While (Result=Nil) and (I>=0) do
    If (CompareText(Value,Items[i].Name)=0) then
      Result:=Items[i]
    else
      Dec(i);
end;

Procedure TParams.GetParamList(List: TList; const ParamNames: string);

  Function NextName(Var S : String) : String;
  Var
    P : Integer;
  begin
    P:=Pos(';',S);
    If (P=0) then
      P:=Length(S)+1;
    Result:=Copy(S,1,P-1);
    system.Delete(S,1,P);
  end;

Var
  L,N : String;

begin
  L:=ParamNames;
  While (Length(L)>0) do
    begin
    N:=NextName(L);
    List.Add(ParamByName(N));
    end;
end;

Function TParams.IsEqual(Value: TParams): Boolean;

Var
  I : Integer;

begin
  Result:=(Value.Count=Count);
  I:=Count-1;
  While Result and (I>=0) do
    begin
    Result:=Items[I].IsEqual(Value[i]);
    Dec(I);
    end;
end;

Function TParams.ParamByName(const Value: string): TParam;
begin
  Result:=FindParam(Value);
  If (Result=Nil) then
    DatabaseErrorFmt(SParameterNotFound,[Value],Dataset);
end;

Function TParams.ParseSQL(SQL: String; DoCreate: Boolean): String;

var pb : TParamBinding;

begin
  Result := ParseSQL(SQL,DoCreate,psInterbase, pb);
end;

Function TParams.ParseSQL(SQL: String; DoCreate: Boolean; ParameterStyle : TParamStyle): String;

var pb : TParamBinding;

begin
  Result := ParseSQL(SQL,DoCreate,ParameterStyle,pb);
end;


Function TParams.ParseSQL(SQL: String; DoCreate: Boolean; ParameterStyle : TParamStyle; var ParamBinding: TParambinding): String;

type
  // used for ParamPart
  TStringPart = record
    Start,Stop:integer;
  end;
  
const
  ParamAllocStepSize = 8;
  
var
  p,ParamNameStart,BufStart:PChar;
  ParamName:string;
  QuestionMarkParamCount,ParameterIndex,NewLength:integer;
  ParamCount:integer; // actual number of parameters encountered so far;
                      // always <= Length(ParamPart) = Length(Parambinding)
                      // Parambinding will have length ParamCount in the end
  ParamPart:array of TStringPart; // describe which parts of buf are parameters
  NewQueryLength:integer;
  NewQuery:string;
  NewQueryIndex,BufIndex,CopyLen,i:integer;                     // Parambinding will have length ParamCount in the end

begin
  if DoCreate then Clear;
  // Parse the SQL and build ParamBinding
  ParamCount:=0;
  NewQueryLength:=Length(SQL);
  SetLength(ParamPart,ParamAllocStepSize);
  SetLength(Parambinding,ParamAllocStepSize);
  QuestionMarkParamCount:=0; // number of ? params found in query so far

  p:=PChar(SQL);
  BufStart:=p; // used to calculate ParamPart.Start values
  repeat
    case p^ of
      '''': // single quote delimited string
        begin
          Inc(p);
          while not (p^ in [#0, '''']) do
          begin
            if p^='\' then Inc(p,2) // make sure we handle \' and \\ correct
            else Inc(p);
          end;
          if p^='''' then Inc(p); // skip final '
        end;
      '"':  // double quote delimited string
        begin
          Inc(p);
          while not (p^ in [#0, '"']) do
          begin
            if p^='\'  then Inc(p,2) // make sure we handle \" and \\ correct
            else Inc(p);
          end;
          if p^='"' then Inc(p); // skip final "
        end;
      '-': // possible start of -- comment
        begin
          Inc(p);
          if p='-' then // -- comment
          begin
            repeat // skip until at end of line
              Inc(p);
            until p^ in [#10, #0];
          end
        end;
      '/': // possible start of /* */ comment
        begin
          Inc(p);
          if p^='*' then // /* */ comment
          begin
            repeat
              Inc(p);
              if p^='*' then // possible end of comment
              begin
                Inc(p);
                if p^='/' then Break; // end of comment
              end;
            until p^=#0;
            if p^='/' then Inc(p); // skip final /
          end;
        end;
      ':','?': // parameter
        begin
          Inc(ParamCount);
          if ParamCount>Length(ParamPart) then
          begin
            NewLength:=Length(ParamPart)+ParamAllocStepSize;
            SetLength(ParamPart,NewLength);
            SetLength(ParamBinding,NewLength);
          end;

          if p^=':' then
          begin // find parameter name
            Inc(p);
            ParamNameStart:=p;
            while not (p^ in (SQLDelimiterCharacters+[#0])) do
              Inc(p);
            ParamName:=Copy(ParamNameStart,1,p-ParamNameStart);
          end
          else
          begin
            Inc(p);
            ParamNameStart:=p;
            ParamName:='';
          end;

          // create Parameter and assign ParameterIndex
          if DoCreate then
            ParameterIndex := CreateParam(ftUnknown, ParamName, ptInput).Index
          // else find ParameterIndex
          else
            begin
              if ParamName<>'' then
                ParameterIndex:=ParamByName(ParamName).Index
              else
              begin
                ParameterIndex:=QuestionMarkParamCount;
                Inc(QuestionMarkParamCount);
              end;
            end;

          // store ParameterIndex in FParamIndex, ParamPart data
          ParamBinding[ParamCount-1]:=ParameterIndex;
          ParamPart[ParamCount-1].Start:=ParamNameStart-BufStart;
          ParamPart[ParamCount-1].Stop:=p-BufStart+1;

          // update NewQueryLength
          Dec(NewQueryLength,p-ParamNameStart);
        end;
      #0:Break;
    else
      Inc(p);
    end;
  until false;
  
  SetLength(ParamPart,ParamCount);
  SetLength(ParamBinding,ParamCount);

  if ParamCount>0 then
  begin
    // replace :ParamName by ? (using ParamPart array and NewQueryLength)
    if ParameterStyle = psPostgreSQL then
      if paramcount < 10 then
        inc(NewQueryLength,paramcount)
      else
        inc(NewQueryLength,(paramcount-9)*2+9);

    SetLength(NewQuery,NewQueryLength);
    NewQueryIndex:=1;
    BufIndex:=1;
    for i:=0 to High(ParamPart) do
    begin
      CopyLen:=ParamPart[i].Start-BufIndex;
      Move(SQL[BufIndex],NewQuery[NewQueryIndex],CopyLen);
      Inc(NewQueryIndex,CopyLen);
      case ParameterStyle of
        psInterbase : NewQuery[NewQueryIndex]:='?';
        psPostgreSQL: begin
                        ParamName := IntToStr(i+1);
                        NewQuery[NewQueryIndex]:='$';
                        Inc(NewQueryIndex);
                        NewQuery[NewQueryIndex]:= paramname[1];
                        if i>10 then
                          begin
                          Inc(NewQueryIndex);
                          NewQuery[NewQueryIndex]:= paramname[2]
                          end;
                      end;
      end;
      Inc(NewQueryIndex);
      BufIndex:=ParamPart[i].Stop;
    end;
    CopyLen:=Length(SQL)+1-BufIndex;
    Move(SQL[BufIndex],NewQuery[NewQueryIndex],CopyLen);
  end
  else
    NewQuery:=SQL;
  Result := NewQuery;
end;


Procedure TParams.RemoveParam(Value: TParam);
begin
   Value.Collection:=Nil;
end;

{ TParam }

Function TParam.GetDataSet: TDataSet;
begin
  If Assigned(Collection) and (Collection is TParams) then
    Result:=TParams(Collection).GetDataset
  else
    Result:=Nil;
end;

Function TParam.IsParamStored: Boolean;
begin
  Result:=Bound;
end;

Procedure TParam.AssignParam(Param: TParam);
begin
  if Not Assigned(Param) then
    begin
    Clear;
    FDataType:=ftunknown;
    FParamType:=ptUnknown;
    Name:='';
    Size:=0;
    Precision:=0;
    NumericScale:=0;
    end
  else
    begin
    FDataType:=Param.DataType;
    if Param.IsNull then
      Clear
    else
      FValue:=Param.FValue;
    FBound:=Param.Bound;
    Name:=Param.Name;
    if (ParamType=ptUnknown) then
      ParamType:=Param.ParamType;
    Size:=Param.Size;
    Precision:=Param.Precision;
    NumericScale:=Param.NumericScale;
    end;
end;

Procedure TParam.AssignTo(Dest: TPersistent);
begin
  if (Dest is TField) then
    AssignToField(TField(Dest))
  else
    inherited AssignTo(Dest);
end;

Function TParam.GetAsBoolean: Boolean;
begin
  If IsNull then
    Result:=False
  else
    Result:=FValue;
end;

Function TParam.GetAsCurrency: Currency;
begin
  If IsNull then
    Result:=0.0
  else
    Result:=FValue;
end;

Function TParam.GetAsDateTime: TDateTime;
begin
  If IsNull then
    Result:=0.0
  else
    Result:=FValue;
end;

Function TParam.GetAsFloat: Double;
begin
  If IsNull then
    Result:=0.0
  else
    Result:=FValue;
end;

Function TParam.GetAsInteger: Longint;
begin
  If IsNull then
    Result:=0
  else
    Result:=FValue;
end;

Function TParam.GetAsMemo: string;
begin
  If IsNull then
    Result:=''
  else
    Result:=FValue;
end;

Function TParam.GetAsString: string;
begin
  If IsNull then
    Result:=''
  else
    Result:=FValue;
end;

Function TParam.GetAsVariant: Variant;
begin
  if IsNull then
    Result:=Null
  else
    Result:=FValue;
end;

Function TParam.GetDisplayName: string;
begin
  if (FName<>'') then
    Result:=FName
  else
    Result:=inherited GetDisplayName
end;

Function TParam.GetIsNull: Boolean;
begin
  Result:= VarIsNull(FValue) or VarIsClear(FValue);
end;

Function TParam.IsEqual(AValue: TParam): Boolean;
begin
  Result:=(Name=AValue.Name)
          and (IsNull=AValue.IsNull)
          and (Bound=AValue.Bound)
          and (DataType=AValue.DataType)
          and (ParamType=AValue.ParamType)
          and (VarType(FValue)=VarType(AValue.FValue))
          and (FValue=AValue.FValue);
end;

Procedure TParam.SetAsBlob(const AValue: TBlobData);
begin
  FValue:=AValue;
  FDataType:=ftBlob;
end;

Procedure TParam.SetAsBoolean(AValue: Boolean);
begin
  FValue:=AValue;
  FDataType:=ftBoolean;
end;

Procedure TParam.SetAsCurrency(const AValue: Currency);
begin
  FValue:=Avalue;
  FDataType:=ftCurrency;
end;

Procedure TParam.SetAsDate(const AValue: TDateTime);
begin
  FValue:=Avalue;
  FDataType:=ftDate;
end;

Procedure TParam.SetAsDateTime(const AValue: TDateTime);
begin
  FValue:=AValue;
  FDataType:=ftDateTime;
end;

Procedure TParam.SetAsFloat(const AValue: Double);
begin
  FValue:=AValue;
  FDataType:=ftFloat;
end;

Procedure TParam.SetAsInteger(AValue: Longint);
begin
  FValue:=AValue;
  FDataType:=ftInteger;
end;

Procedure TParam.SetAsMemo(const AValue: string);
begin
  FValue:=AValue;
  FDataType:=ftMemo;
end;


Procedure TParam.SetAsSmallInt(AValue: LongInt);
begin
  FValue:=AValue;
  FDataType:=ftSmallInt;
end;

Procedure TParam.SetAsString(const AValue: string);
begin
  FValue:=AValue;
  FDataType:=ftString;
end;

Procedure TParam.SetAsTime(const AValue: TDateTime);
begin
  FValue:=AValue;
  FDataType:=ftTime;
end;

Procedure TParam.SetAsVariant(const AValue: Variant);
begin
  FValue:=AValue;
  FBound:=not VarIsClear(Value);
  if FDataType = ftUnknown then
    case VarType(Value) of
      varBoolean  : FDataType:=ftBoolean;
      varSmallint,
      varShortInt,
      varByte     : FDataType:=ftSmallInt;
      varWord,
      varInteger  : FDataType:=ftInteger;
      varCurrency : FDataType:=ftCurrency;
      varLongWord,
      varSingle,
      varDouble   : FDataType:=ftFloat;
      varDate     : FDataType:=ftDateTime;
      varString,
      varOleStr   : if (FDataType<>ftFixedChar) then
                      FDataType:=ftString;
      varInt64    : FDataType:=ftLargeInt;
    else
      FDataType:=ftUnknown;
    end;
end;

Procedure TParam.SetAsWord(AValue: LongInt);
begin
  FValue:=AValue;
  FDataType:=ftWord;
end;


Procedure TParam.SetDataType(AValue: TFieldType);

Var
  VT : Integer;

begin
  FDataType:=AValue;
  VT:=FieldTypetoVariantMap[AValue];
  If (VT=varError) then
    clear
  else
    Try
      FValue:=VarAsType(AValue,VT)
    except
      Clear;
    end;
end;

Procedure TParam.SetText(const AValue: string);
begin
  Value:=AValue;
end;

constructor TParam.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  ParamType:=ptUnknown;
  DataType:=ftUnknown;
  FValue:=Unassigned;
end;

constructor TParam.Create(AParams: TParams; AParamType: TParamType);
begin
  Create(AParams);
  ParamType:=AParamType;
end;

Procedure TParam.Assign(Source: TPersistent);
begin
  if (Source is TParam) then
    AssignParam(TParam(Source))
  else if (Source is TField) then
    AssignField(TField(Source))
  else if (source is TStrings) then
    AsMemo:=TStrings(Source).Text
  else
    inherited Assign(Source);
end;

Procedure TParam.AssignField(Field: TField);
begin
  if Assigned(Field) then
    begin
    // Need TField.Value
    // AssignFieldValue(Field,Field.Value);
    Name:=Field.FieldName;
    end
  else
    begin
    Clear;
    Name:='';
    end
end;

procedure TParam.AssignToField(Field : TField);

begin
  if Assigned(Field) then
    case FDataType of
      ftUnknown  : DatabaseErrorFmt(SUnknownParamFieldType,[Name],DataSet);
      // Need TField.AsSmallInt
      ftSmallint : Field.AsInteger:=AsSmallInt;
      // Need TField.AsWord
      ftWord     : Field.AsInteger:=AsWord;
      ftInteger,
      ftAutoInc  : Field.AsInteger:=AsInteger;
      // Need TField.AsCurrency
      ftCurrency : Field.asFloat:=AsCurrency;
      ftFloat    : Field.asFloat:=AsFloat;
      ftBoolean  : Field.AsBoolean:=AsBoolean;
      ftBlob,
      ftGraphic..ftTypedBinary,
      ftOraBlob,
      ftOraClob,
      ftString,
      ftMemo,
      ftAdt,
      ftFixedChar: Field.AsString:=AsString;
      ftTime,
      ftDate,
      ftDateTime : Field.AsDateTime:=AsDateTime;
      ftBytes,
      ftVarBytes : ; // Todo.
    else
      If not (DataType in [ftCursor, ftArray, ftDataset,ftReference]) then
        DatabaseErrorFmt(SBadParamFieldType, [Name], DataSet);
    end;
end;

Procedure TParam.AssignFieldValue(Field: TField; const AValue: Variant);
begin
  If Assigned(Field) then
    begin
    // Need TField.FixedChar property.
    if (Field.DataType = ftString) {and TStringField(Field).FixedChar} then
      DataType:=ftFixedChar
    else if (Field.DataType = ftMemo) and (Field.Size > 255) then
      DataType:=ftString
    else
      DataType:=Field.DataType;
    if VarIsNull(AValue) then
      Clear
    else
      Value:=AValue;
    Size:=Field.DataSize;
    FBound:=True;
    end;
end;

Procedure TParam.Clear;
begin
  FValue:=UnAssigned;
end;

Procedure TParam.GetData(Buffer: Pointer);

Var
  P : Pointer;
  S : String;

begin
  case FDataType of
    ftUnknown  : DatabaseErrorFmt(SUnknownParamFieldType,[Name],DataSet);
    ftSmallint : PSmallint(Buffer)^:=AsSmallInt;
    ftWord     : PWord(Buffer)^:=AsWord;
    ftInteger,
    ftAutoInc  : PInteger(Buffer)^:=AsInteger;
    ftCurrency : PDouble(Buffer)^:=AsCurrency;
    ftFloat    : PDouble(Buffer)^:=AsFloat;
    ftBoolean  : PWordBool(Buffer)^:=AsBoolean;
    ftString,
    ftMemo,
    ftAdt,
    ftFixedChar:
      begin
      S:=AsString;
      StrMove(PChar(Buffer),Pchar(S),Length(S)+1);
      end;
    ftTime     : PInteger(Buffer)^:=DateTimeToTimeStamp(AsTime).Time;
    ftDate     : PInteger(Buffer)^:=DateTimeToTimeStamp(AsTime).Date;
    ftDateTime : PDouble(Buffer)^:=TimeStampToMSecs(DateTimeToTimeStamp(AsDateTime));
    ftBlob,
    ftGraphic..ftTypedBinary,
    ftOraBlob,
    ftOraClob  :
      begin
      S:=GetAsString;
      Move(PChar(S)^, Buffer^, Length(S));
      end;
    ftBytes, ftVarBytes:
      begin
      if VarIsArray(FValue) then
        begin
        P:=VarArrayLock(FValue);
        try
          Move(P^, Buffer^, VarArrayHighBound(FValue, 1) + 1);
        finally
          VarArrayUnlock(FValue);
        end;
        end;
      end;
  else
    If not (DataType in [ftCursor, ftArray, ftDataset,ftReference]) then
      DatabaseErrorFmt(SBadParamFieldType, [Name], DataSet);
  end;
end;

Function TParam.GetDataSize: Integer;
begin
  Result:=0;
  case DataType of
    ftUnknown  : DatabaseErrorFmt(SUnknownParamFieldType,[Name],DataSet);
    ftBoolean  : Result:=SizeOf(WordBool);
    ftInteger,
    ftAutoInc  : Result:=SizeOf(Integer);
    ftSmallint : Result:=SizeOf(SmallInt);
    ftWord     : Result:=SizeOf(Word);
    ftTime,
    ftDate     : Result:=SizeOf(Integer);
    ftDateTime,
    ftCurrency,
    ftFloat    : Result:=SizeOf(Double);
    ftString,
    ftFixedChar,
    ftMemo,
    ftADT      : Result:=Length(AsString)+1;
    ftBytes,
    ftVarBytes : if VarIsArray(FValue) then
                   Result:=VarArrayHighBound(FValue,1)+1
                 else
                   Result:=0;
    ftBlob,
    ftGraphic..ftTypedBinary,
    ftOraClob,
    ftOraBlob  : Result:=Length(AsString);
    ftArray,
    ftDataSet,
    ftReference,
    ftCursor   : Result:=0;
  else
    DatabaseErrorFmt(SBadParamFieldType,[Name],DataSet);
  end;


end;

Procedure TParam.LoadFromFile(const FileName: string; BlobType: TBlobType);

Var
  S : TFileStream;

begin
  S:=TFileStream.Create(FileName,fmOpenRead);
  Try
    LoadFromStream(S,BlobType);
  Finally
    FreeAndNil(S);
  end;
end;

Procedure TParam.LoadFromStream(Stream: TStream; BlobType: TBlobType);

Var
  Temp : String;

begin
  FDataType:=BlobType;
  With Stream do
    begin
    Position:=0;
    SetLength(Temp,Size);
    ReadBuffer(Pointer(Temp)^,Size);
    FValue:=Temp;
    end;
end;

Procedure TParam.SetBlobData(Buffer: Pointer; Size: Integer);

Var
  Temp : String;

begin
  SetLength(Temp,Size);
  Move(Buffer^,Temp,Size);
  AsBlob:=Temp;
end;

Procedure TParam.SetData(Buffer: Pointer);

  Function FromTimeStamp(T,D : Integer) : TDateTime;

  Var TS : TTimeStamp;

  begin
    TS.Time:=T;
    TS.Date:=D;
    Result:=TimeStampToDateTime(TS);
  end;

begin
  case FDataType of
    ftUnknown  : DatabaseErrorFmt(SUnknownParamFieldType,[Name],DataSet);
    ftSmallint : AsSmallInt:=PSmallint(Buffer)^;
    ftWord     : AsWord:=PWord(Buffer)^;
    ftInteger,
    ftAutoInc  : AsInteger:=PInteger(Buffer)^;
    ftCurrency : AsCurrency:= PDouble(Buffer)^;
    ftFloat    : AsFloat:=PDouble(Buffer)^;
    ftBoolean  : AsBoolean:=PWordBool(Buffer)^;
    ftString,
    ftFixedChar: AsString:=StrPas(Buffer);
    ftMemo     : AsMemo:=StrPas(Buffer);
    ftTime     : AsTime:=FromTimeStamp(PInteger(Buffer)^,DateDelta);
    ftDate     : Asdate:=FromTimeStamp(0,PInteger(Buffer)^);
    ftDateTime : AsDateTime:=TimeStampToDateTime(MSecsToTimeStamp(trunc(PDouble(Buffer)^)));
    ftCursor   : FValue:=0;
    ftBlob,
    ftGraphic..ftTypedBinary,
    ftOraBlob,
    ftOraClob  : SetBlobData(Buffer, StrLen(PChar(Buffer)));
  else
    DatabaseErrorFmt(SBadParamFieldType,[Name],DataSet);
  end;
end;

