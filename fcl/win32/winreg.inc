{******************************************************************************
                                  TRegistry
 ******************************************************************************}

Procedure TRegistry.SysRegCreate;
begin
  FStringSizeIncludesNull:=True;
end;

Procedure TRegistry.SysRegfree;

begin
end;

Function PrepKey(Const S : String) : pChar;

begin
  If (S[1]<>'\') then
    Result:=@S[1]
  else
    Result:=@S[2];
end;

Function RelativeKey(Const S : String) : Boolean;

begin
  Result:=(S[1]<>'\')
end;


function TRegistry.sysCreateKey(const Key: String): Boolean;
Var
  P: PChar;
  Disposition: Dword;
  Handle: HKEY;
  SecurityAttributes: Pointer; //LPSECURITY_ATTRIBUTES;

begin
  SecurityAttributes := Nil;
  P:=PrepKey(Key);
  Result:=RegCreateKeyEx(GetBaseKey(RelativeKey(Key)),
                         P,
                         0,
                         '',
                         REG_OPTION_NON_VOLATILE,
                         KEY_ALL_ACCESS,
                         SecurityAttributes,
                         Handle,
                         @Disposition) = ERROR_SUCCESS;
  RegCloseKey(Handle);
end;

function TRegistry.DeleteKey(const Key: String): Boolean;

Var
  P: PChar;

begin
  P:=PRepKey(Key);
  Result:=RegDeleteKey(GetBaseKey(RelativeKey(Key)),P)=ERROR_SUCCESS;
end;

function TRegistry.DeleteValue(const Name: String): Boolean;
begin
  Result := RegDeleteValue(fCurrentKey, @Name[1]) = ERROR_SUCCESS;
end;

function TRegistry.SysGetData(const Name: String; Buffer: Pointer;
          BufSize: Integer; var RegData: TRegDataType): Integer;
Var
  P: PChar;
  RD : DWord;

begin
  P := @Name[1];
  If RegQueryValueEx(fCurrentKey,P,Nil,
                     @RD,Buffer,@BufSize)<>ERROR_SUCCESS Then
    Result:=-1
  else
    begin
    If (RD=REG_SZ) then
      RegData:=rdString
    else if (RD=REG_EXPAND_SZ) then
      Regdata:=rdExpandString
    else if (RD=REG_DWORD) then
      RegData:=rdInteger
    else if (RD=REG_BINARY) then
      RegData:=rdBinary
    else
      RegData:=rdUnknown;
    Result:=BufSize;
    end;
end;

function TRegistry.GetDataInfo(const ValueName: String; var Value: TRegDataInfo): Boolean;

Var
  P: PChar;

begin
  P:=@ValueName[1];
  With Value do
    Result:=RegQueryValueEx(fCurrentKey,P,Nil,@RegData,Nil,@DataSize)=ERROR_SUCCESS;
  If Not Result Then
    begin
    Value.RegData := rdUnknown;
    Value.DataSize := 0
    end
end;

function TRegistry.GetKey(const Key: String): HKEY;
begin
  Result := FCurrentKey;
end;

function TRegistry.GetKeyInfo(var Value: TRegKeyInfo): Boolean;

begin
  FillChar(Value, SizeOf(Value), 0);
  With Value do
    Result:=RegQueryInfoKey(CurrentKey,nil,nil,nil,@NumSubKeys,
                            @MaxSubKeyLen,nil,@NumValues,@MaxValueLen,
                            @MaxDataLen,nil,@FileTime)=ERROR_SUCCESS;
end;

function TRegistry.KeyExists(const Key: string): Boolean;

Var
  Value : TRegKeyInfo;

begin
  Result :=GetKeyInfo(Value);
end;

function TRegistry.LoadKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.OpenKey(const Key: string; CanCreate: Boolean): Boolean;

Var
  P: PChar;
  Handle: HKEY;
  Disposition: Integer;
  SecurityAttributes: Pointer; //LPSECURITY_ATTRIBUTES;

begin
  SecurityAttributes := Nil;
  P:=PrepKey(Key);
  If CanCreate then
    begin
    Handle:=0;
    Result:=RegCreateKeyEx(GetBaseKey(RelativeKey(Key)),P,0,'',

                           REG_OPTION_NON_VOLATILE,
                           fAccess,SecurityAttributes,Handle,
                           @Disposition)=ERROR_SUCCESS

    end
  else
    Result:=RegOpenKeyEx(GetBaseKey(RelativeKey(Key)),
                         P,0,fAccess,Handle)=ERROR_SUCCESS;
  If Result then
    fCurrentKey:=Handle;
end;

function TRegistry.OpenKeyReadOnly(const Key: string): Boolean;

Var
  P: PChar;
  Handle: HKEY;

begin
  P:=PrepKey(Key);
  Result := RegOpenKeyEx(GetBaseKey(RelativeKey(Key)),P,0,KEY_READ,Handle) = 0;
  If Result Then
    fCurrentKey := Handle;
end;

function TRegistry.RegistryConnect(const UNCName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.ReplaceKey(const Key, FileName, BackUpFileName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.RestoreKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.SaveKey(const Key, FileName: string): Boolean;
begin
  Result := False;
end;

function TRegistry.UnLoadKey(const Key: string): Boolean;
begin
  Result := false;
end;

function TRegistry.ValueExists(const Name: string): Boolean;

var
  Info : TRegDataInfo;

begin
  Result:=GetDataInfo(Name,Info);
end;

procedure TRegistry.CloseKey;
begin
  If (CurrentKey<>0) then
    begin
    if LazyWrite then
      RegCloseKey(CurrentKey)
    else
      RegFlushKey(CurrentKey);
    fCurrentKey:=0;
    end
end;

procedure TRegistry.ChangeKey(Value: HKey; const Path: String);
begin

end;

procedure TRegistry.GetKeyNames(Strings: TStrings);

Var
  L : Cardinal;
  I: Integer;
  Info: TRegKeyInfo;
  P : PChar;

begin
   Strings.Clear;
   if GetKeyInfo(Info) then
     begin
     L:=Info.MaxSubKeyLen+1;
     GetMem(P,L);
     Try
       for I:=0 to Info.NumSubKeys-1 do
         begin
         L:=Info.MaxSubKeyLen+1;
         RegEnumKeyEx(CurrentKey,I,P,L,Nil,Nil,Nil,Nil);
         Strings.Add(StrPas(P));
         end;
     Finally
       FreeMem(P);
     end;
     end;
end;

procedure TRegistry.GetValueNames(Strings: TStrings);

Var
  L : Cardinal;
  I: Integer;
  Info: TRegKeyInfo;
  P : PChar;

begin
   Strings.Clear;
   if GetKeyInfo(Info) then
     begin
     L:=Info.MaxValueLen+1;
     GetMem(P,L);
     Try
       for I:=0 to Info.NumValues-1 do
         begin
         L:=Info.MaxValueLen+1;
         RegEnumValue(CurrentKey,I,P,L,Nil,Nil,Nil,Nil);
         Strings.Add(StrPas(P));
         end;
     Finally
       FreeMem(P);
     end;
     end;

end;

Function TRegistry.SysPutData(const Name: string; Buffer: Pointer;
  BufSize: Integer; RegData: TRegDataType) : Boolean;

Var
  P: PChar;
  RegDataType: DWORD;

begin
  Case RegData of
    rdUnknown      : RegDataType:=REG_NONE;
    rdString       : RegDataType:=REG_SZ;
    rdExpandString : RegDataType:=REG_EXPAND_SZ;
    rdInteger      : RegDataType:=REG_DWORD;
    rdBinary       : RegDataType:=REG_BINARY;
  end;
  P:=@Name[1];
  Result:=RegSetValueEx(fCurrentKey,P,0,RegDataType,Buffer,BufSize)=ERROR_SUCCESS;
end;

procedure TRegistry.RenameValue(const OldName, NewName: string);

var
  L: Integer;
  InfoO,InfoN : TRegDataInfo;
  D : TRegDataType;
  P: PChar;

begin
  If GetDataInfo(OldName,InfoO) and Not GetDataInfo(NewName,InfoN) then
    begin
    L:=InfoO.DataSize;
    if L>0 then
      begin
      GetMem(P,L);
      try
        L:=GetData(OldName,P,L,D);
        If SysPutData(NewName,P,L,D) then
          DeleteValue(OldName);
      finally
        FreeMem(P);
      end;
      end;
    end;
end;

procedure TRegistry.SetCurrentKey(Value: HKEY);
begin
  fCurrentKey := Value;
end;

procedure TRegistry.SetRootKey(Value: HKEY);
begin
  fRootKey := Value;
end;

