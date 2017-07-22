program testunicode;

{$mode objfpc}{$H+}
{$codepage utf8}
{$IFNDEF UNIX}
{$APPTYPE CONSOLE}
{$ENDIF}
uses
  sysutils, classes, registry;

Var
  EditKey : UTF8String = 'ASCII;这是一个测试';
  labeledEditName : UTF8String = 'ASCII;പേര് ഇതാണ്ASCII;这是一个测试';
  labeledEditValue : UTF8String = 'これは値です;ASCII';
  labelkeycaption : string = 'HKCU\Software\zzz_test\';
  reg: TRegistry;
  Results : TStrings;



function TestKey (const AKey: utf8string): boolean;
begin
  Result:=false;
  try
    reg.CloseKey;
    if reg.KeyExists(AKey) then
      reg.DeleteKey(AKey);
    if reg.KeyExists(AKey) then
    begin
      Results.Add('TestKey-01 failed: DeleteKey(%s);',[AKey]);
      exit;
    end;
    if not reg.OpenKey(AKey,true) then
    begin
      Results.Add('TestKey-02 failed: OpenKey(%s,true)',[AKey]);
      exit;
    end;
    reg.CloseKey;
    if not reg.KeyExists(AKey) then
    begin
      Results.Add('TestKey-03 failed: OpenKey(%s,true)',[AKey]);
      exit;
    end;
    reg.DeleteKey(AKey);
    if not reg.CreateKey(AKey) then
    begin
      Results.Add('TestKey-04 failed: CreateKey(%s)',[AKey]);
      exit;
    end;
    if not reg.KeyExists(AKey) then
    begin
      Results.Add('TestKey-05 failed: CreateKey(%s,true)',[AKey]);
      exit;
    end;
    if not reg.OpenKeyReadOnly(AKey) then
    begin
      Results.Add('TestKey-06 failed: OpenKeyReadOnly(%s)',[AKey]);
      exit;
    end;
    reg.CloseKey;
    if not reg.OpenKey(AKey,false) then
    begin
      Results.Add('TestKey-07 failed: OpenKey(%s,false)',[AKey]);
      exit;
    end;

    Results.Add('TestKey           passed: %s',[AKey]);

  except
    on e:Exception do
      Results.Add('TestKey-08 failed: %s; %s;',[AKey,e.Message]);
  end;

  Result:=true;

end;

procedure TestValue (const AName, AValue: utf8string);
var
  wrong,s: string;
begin
  try
    wrong:=AName+'_wrong';
    if reg.ValueExists(wrong) then
      reg.DeleteValue(wrong);
    if reg.ValueExists(wrong) then
    begin
      Results.Add('TestValue-01 failed: DeleteValue(%s)',[wrong]);
      exit;
    end;
    reg.WriteString(wrong,AValue);
    s:=reg.ReadString(wrong);
    if s<>AValue then
    begin
      Results.Add('TestValue-02 failed: WriteString(%s,%s)',[wrong,AValue]);
      exit;
    end;

    if reg.ValueExists(AName) then
      reg.DeleteValue(AName);
    if reg.ValueExists(AName) then
    begin
      Results.Add('TestValue-03 failed: DeleteValue(%s)',[AName]);
      exit;
    end;

    reg.RenameValue(wrong,AName);
    s:=reg.ReadString(AName);
    if s<>AValue then
    begin
      Results.Add('TestValue-04 failed: RenameValue(%s,%s)',[wrong,AName]);
      exit;
    end;

    Results.Add('TestValue         passed: %s; %s;',[AName,AValue]);

  except
    on e:Exception do
      Results.Add('TestValue-08 failed: %s; %s; %s;',[AName,AValue,e.Message]);
  end;
end;

procedure TestGetKeyNames (const AKey, AExpected: utf8string);
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  sl.Delimiter:=';';
  reg.CloseKey;
  try
    if not reg.OpenKeyReadOnly(AKey) then
    begin
      Results.Add('TestGetKeyNames-01 failed: Key "%s";',[AKey]);
      exit;
    end;
    reg.GetKeyNames(sl);
    if sl.DelimitedText=AExpected then
      Results.Add('TestGetKeyNames   passed: Key: "%s"; Expected: "%s";',[AKey,AExpected])
    else
      Results.Add('TestGetKeyNames-02 failed: Key: "%s"; got: "%s"; expected: "%s";',
                           [AKey,sl.DelimitedText,AExpected]);
  except
    on e:Exception do
      Results.Add('TestGetKeyNames-03 failed exception: Key: "%s"; Got: "%s"; Expected: "%s"; Exception: "%s";',
                           [AKey,sl.DelimitedText,AExpected,e.Message]);
  end;
  sl.Free;
end;

procedure TestGetValueNames (const AKey, AExpected: UTF8string);
var
  sl: TStringList;
begin
  sl:=TStringList.Create;
  sl.Delimiter:=';';
  try
    reg.GetValueNames(sl);
    if sl.DelimitedText=AExpected then
      Results.Add('TestGetValueNames passed: Key: "%s"; Expected "%s";',[AKey,AExpected])
    else
      Results.Add('TestGetValueNames-01 failed: Key "%s"; Got: "%s"; Expected: "%s";',
                           [AKey,sl.DelimitedText,AExpected]);
  except
    on e:Exception do
      Results.Add('TestGetValueNames-02 failed exception: Key: "%s"; Got: "%s"; expected: "%s"; exception: "%s";',
                           [AKey,sl.DelimitedText,AExpected,e.Message]);
  end;
  sl.Free;
end;

procedure Test;
var
  sKey:        string;
  slKeys,
  slNames,
  slValues:    TStringList;
  sValueNames,
  s:           string;
  k,n,v:       integer;
  l:           longint;
begin
  sKey:=LabelKeyCaption;
  l:=pos('\',LabelKeyCaption);
  if l>0 then
    delete(sKey,1,l);
  if sKey[Length(sKey)]='\' then
    SetLength(sKey,Length(sKey)-1);

  slKeys:=TStringList.Create;
  slKeys.Delimiter:=';';
  slKeys.DelimitedText:=EditKey;

  slNames:=TStringList.Create;
  slNames.Delimiter:=';';
  slNames.DelimitedText:=LabeledEditName;

  slValues:=TStringList.Create;
  slValues.Delimiter:=';';
  slValues.DelimitedText:=LabeledEditValue;
  
  for k:=0 to slKeys.Count-1 do
    if TestKey(sKey+'\'+slKeys[k]) then
    begin
      sValueNames:='';
      for n:=0 to slNames.Count-1 do
        for v:=0 to slValues.Count-1 do
        begin
          s:=Format('%d%d%d_%s',[k,n,v,slNames[n]]);
          if sValueNames='' then
            sValueNames:=s
          else
            sValueNames:=sValueNames+slNames.Delimiter+s;
          TestValue(s,slValues[v]);
        end;
      TestGetValueNames(reg.CurrentPath,sValueNames);
    end;

  TestGetKeyNames(sKey,slKeys.DelimitedText);

  reg.CloseKey;

  slKeys.Free;
  slNames.Free;
  slValues.Free;
end;

Procedure WN;
Var
  F : Text;


begin
  Assign(F,'names.txt');
  Rewrite(F);
  Writeln(F,EditKey);
  Writeln(F,labeledEditName);
  Writeln(F,LabeledEditValue);
  Writeln(F,LabelKeyCaption);
  Close(F);
end;

begin
  defaultsystemcodepage:=CP_UTF8;
  if (ParamStr(1)='-s') then
    WN;
  reg:=TRegistry.Create;
  reg.lazywrite:=false;
  Results:=TStringList.Create;
  Test;
  Reg.Free;
  if (ParamStr(1)='-s') then
    Results.SaveToFile('result.txt');
  Writeln(Results.Text);
  Results.Free;
  {$IFDEF WINDOWS}Readln;{$ENDIF}
end.

