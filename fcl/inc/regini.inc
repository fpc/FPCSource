
{******************************************************************************
                                TRegIniFile
 ******************************************************************************}

constructor TRegIniFile.Create(const FN: String);
begin
  inherited Create;
  fFileName := FN;
end;

procedure TRegIniFile.DeleteKey(const Section, Ident: String);
begin

end;

procedure TRegIniFile.EraseSection(const Section: string);
begin

end;

procedure TRegIniFile.ReadSection(const Section: string; Strings: TStrings);
begin

end;

procedure TRegIniFile.ReadSections(Strings: TStrings);
begin

end;

procedure TRegIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
begin

end;

procedure TRegIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
begin

end;

procedure TRegIniFile.WriteInteger(const Section, Ident: string; Value: LongInt);
begin

end;

procedure TRegIniFile.WriteString(const Section, Ident, Value: String);
begin

end;

function TRegIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result := Default;
end;

function TRegIniFile.ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
begin
  Result := Default;
end;

function TRegIniFile.ReadString(const Section, Ident, Default: String): String;
begin
  Result := Default;
end;
