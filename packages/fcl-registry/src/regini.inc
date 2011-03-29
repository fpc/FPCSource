
{******************************************************************************
                                TRegIniFile
 ******************************************************************************}

constructor TRegIniFile.Create(const FN: String);
begin
  inherited Create;
  fFileName := FN;
  if fFileName<>'' then
   fPath := fFileName + '\'
  else
   fPath := '';
end;

constructor TRegIniFile.Create(const FN: String;aaccess:longword);
begin
  inherited Create(aaccess);
  fFileName := FN;
  if fFileName<>'' then
   fPath := fFileName + '\'
  else
   fPath := '';
end;

procedure TRegIniFile.DeleteKey(const Section, Ident: String);
begin
	if not OpenKey(fPath+Section,true) then Exit;
	try
	 DeleteValue(Ident);
	finally
	 CloseKey;
	end;
end;

procedure TRegIniFile.EraseSection(const Section: string);
begin
 inherited DeleteKey(fPath+Section);
end;

procedure TRegIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
	if not OpenKey(fPath+Section,false) then Exit;
	try
	 GetValueNames(Strings);
	finally
	 CloseKey;
	end;
end;

procedure TRegIniFile.ReadSections(Strings: TStrings);
begin
	if not OpenKey(fFileName,false) then Exit;
	try
	 GetKeyNames(Strings);
	finally
	 CloseKey;
	end;
end;

procedure TRegIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
 ValList : TStringList;
 V : String;
 i : Integer;
begin
	if not OpenKey(fPath+Section,false) then Exit;
	ValList := TStringList.Create;
	try
	 GetValueNames(ValList);
	 for i:=0 to ValList.Count-1 do
	 begin
	   V := inherited ReadString(ValList.Strings[i]);
	   Strings.Add(ValList.Strings[i] + '=' + V);
	 end;
	finally
	 ValList.Free;
	 CloseKey;
	end;
end;

procedure TRegIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
	if not OpenKey(fPath+Section,true) then Exit;
	try
    if not fPreferStringValues then
  	  inherited WriteBool(Ident,Value)
    else begin
      if ValueExists(Ident) and (GetDataType(Ident)=rdInteger) then
    	  inherited WriteBool(Ident,Value)
      else
        inherited WriteString(Ident,BoolToStr(Value));
    end;
  finally
	  CloseKey;
	end;
end;

procedure TRegIniFile.WriteInteger(const Section, Ident: string; Value: LongInt);
begin
  if not OpenKey(fPath+Section,true) then Exit;
  try
    if not fPreferStringValues then
      inherited WriteInteger(Ident,Value)
    else begin
      if ValueExists(Ident) and (GetDataType(Ident)=rdInteger) then
    	  inherited WriteInteger(Ident,Value)
      else
        inherited WriteString(Ident,IntToStr(Value));
    end;
  finally
    CloseKey;
  end;
end;

procedure TRegIniFile.WriteString(const Section, Ident, Value: String);
begin
  if not OpenKey(fPath+Section,true) then Exit;
  try
   inherited WriteString(Ident,Value);
  finally
   CloseKey;
  end;
end;

function TRegIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
	Result := Default;
	if not OpenKey(fPath+Section,false) then Exit;
	try
    if ValueExists(Ident) then
      if (not fPreferStringValues) or (GetDataType(Ident)=rdInteger) then
  	    Result := inherited ReadBool(Ident)
      else
        Result := StrToBool(inherited ReadString(Ident));
	finally
	  CloseKey;
	end;
end;

function TRegIniFile.ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
begin
  Result := Default;
  if not OpenKey(fPath+Section,false) then Exit;
  try
    if ValueExists(Ident) then
      if (not fPreferStringValues) or (GetDataType(Ident)=rdInteger) then
        Result := inherited ReadInteger(Ident)
      else
        Result := StrToInt(inherited ReadString(Ident));
  finally
    CloseKey;
  end;
end;

function TRegIniFile.ReadString(const Section, Ident, Default: String): String;
begin
  Result := Default;
  if not OpenKey(fPath+Section,false) then Exit;
  try
    if ValueExists(Ident) then
      Result := inherited ReadString(Ident);
  finally
    CloseKey;
  end;
end;
