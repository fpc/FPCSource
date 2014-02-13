
{******************************************************************************
                                TRegIniFile
 ******************************************************************************}

constructor TRegIniFile.Create(const FN: String);
begin
  Create(FN, KEY_ALL_ACCESS);
end;

constructor TRegIniFile.Create(const FN: String;aaccess:longword);
begin
  inherited Create(aaccess);
  fFileName := FN;
  if fFileName<>'' then begin
    fPath := fFileName + '\';
    OpenKey(fFileName, aaccess <> KEY_READ);
  end
  else
    fPath := '';
  fPreferStringValues:=True; // Delphi compatibility
end;

procedure TRegIniFile.DeleteKey(const Section, Ident: String);
begin
  if OpenSection(Section) then
  try
    DeleteValue(Ident);
  finally
    CloseSection;
  end;
end;

procedure TRegIniFile.EraseSection(const Section: string);
begin
  inherited DeleteKey(Section);
end;

procedure TRegIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
  if OpenSection(Section) then
  try
    GetValueNames(Strings);
  finally
    CloseSection;
  end;
end;

procedure TRegIniFile.ReadSections(Strings: TStrings);
begin
  GetKeyNames(Strings);
end;

procedure TRegIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
 ValList : TStringList;
 V : String;
 i : Integer;
begin
  if OpenSection(Section) then
  try
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
 	  end;
  finally
    CloseSection;
  end;
end;

procedure TRegIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin
  if OpenSection(Section,True) then
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
    CloseSection;
	end;
end;

procedure TRegIniFile.WriteInteger(const Section, Ident: string; Value: LongInt);
begin
  if OpenSection(Section,True) then
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
    CloseSection;
  end;
end;

procedure TRegIniFile.WriteString(const Section, Ident, Value: String);
begin
  if OpenSection(Section,True) then
  try
    inherited WriteString(Ident,Value);
  finally
    CloseSection;
  end;
end;

function TRegIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
	Result := Default;
  if OpenSection(Section) then
	try
    if ValueExists(Ident) then
      if (not fPreferStringValues) or (GetDataType(Ident)=rdInteger) then
  	    Result := inherited ReadBool(Ident)
      else
        Result := StrToBool(inherited ReadString(Ident));
	finally
    CloseSection;
	end;
end;

function TRegIniFile.ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
begin
  Result := Default;
  if OpenSection(Section) then
  try
    if ValueExists(Ident) then
      if (not fPreferStringValues) or (GetDataType(Ident)=rdInteger) then
        Result := inherited ReadInteger(Ident)
      else
        Result := StrToInt(inherited ReadString(Ident));
  finally
    CloseSection;
  end;
end;

function TRegIniFile.ReadString(const Section, Ident, Default: String): String;
begin
  Result := Default;
  if OpenSection(Section) then
  try
    if ValueExists(Ident) then
      Result := inherited ReadString(Ident);
  finally
    CloseSection;
  end;
end;

function TRegIniFile.OpenSection(const Section: string; CreateSection : Boolean = false): boolean;
var
  k: HKEY;
begin
  if CreateSection then
    CreateKey(Section);
  ASSERT(fOldCurKey = 0);
  if Section <> '' then begin
    k:=GetKey(Section);
    if k = 0 then begin
      Result:=False;
      exit;
    end;
    fOldCurKey:=CurrentKey;
    SetCurrentKey(k);
  end;
  Result:=True;
end;

procedure TRegIniFile.CloseSection;
begin
  if fOldCurKey <> 0 then begin
    CloseKey(CurrentKey);
    SetCurrentKey(fOldCurKey);
    fOldCurKey:=0;
  end;
end;

