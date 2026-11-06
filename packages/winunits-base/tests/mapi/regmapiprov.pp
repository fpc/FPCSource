{ Registers fpcmapiprov.dll as the default Simple MAPI mail client: regmapiprov install [DLL] | uninstall | status.
  install writes HKLM (needs administrator rights) and makes it the default in HKCU and HKLM; uninstall restores
  the previous defaults. A 32-bit build registers in the 32-bit registry view, a 64-bit build in the 64-bit view. }
program regmapiprov;

{$mode objfpc}{$H+}

uses
  Windows, SysUtils, mapi;

const
  cClientName = 'FPC MAPI Test';
  cMailClients = 'Software\Clients\Mail';
  cClientKey = cMailClients+'\'+cClientName;
  cPreviousDefault = 'PreviousDefault';
  cPreviousMachineDefault = 'PreviousMachineDefault';


// Set string value aName ('' for the default value) of key aRoot\aKey, creating the key; True on success
function SetRegString(aRoot: HKEY; const aKey, aName, aValue: UnicodeString): Boolean;

var
  lKey: HKEY;
  lName: PWideChar;

begin
  Result:=RegCreateKeyExW(aRoot, PWideChar(aKey), 0, nil, 0, KEY_WRITE, nil, lKey, nil)=ERROR_SUCCESS;
  if not Result then
    Exit;
  try
    if aName='' then
      lName:=nil
    else
      lName:=PWideChar(aName);
    Result:=RegSetValueExW(lKey, lName, 0, REG_SZ, PByte(PWideChar(aValue)), (Length(aValue)+1)*SizeOf(WideChar))=ERROR_SUCCESS;
  finally
    RegCloseKey(lKey);
  end;
end;


// The string value aName of key aRoot\aKey, '' if absent
function GetRegString(aRoot: HKEY; const aKey, aName: UnicodeString): UnicodeString;

var
  lKey: HKEY;
  lType, lSize: DWORD;
  lName: PWideChar;

begin
  Result:='';
  if RegOpenKeyExW(aRoot, PWideChar(aKey), 0, KEY_READ, lKey)<>ERROR_SUCCESS then
    Exit;
  try
    if aName='' then
      lName:=nil
    else
      lName:=PWideChar(aName);
    lSize:=0;
    if (RegQueryValueExW(lKey, lName, nil, @lType, nil, @lSize)<>ERROR_SUCCESS) or (lType<>REG_SZ) or (lSize<2) then
      Exit;
    SetLength(Result, lSize div SizeOf(WideChar));
    if RegQueryValueExW(lKey, lName, nil, @lType, PByte(PWideChar(Result)), @lSize)<>ERROR_SUCCESS then
      Exit('');
    SetLength(Result, Length(PWideChar(Result)));
  finally
    RegCloseKey(lKey);
  end;
end;


// Delete the default value of key aRoot\aKey
procedure DeleteDefaultValue(aRoot: HKEY; const aKey: UnicodeString);

var
  lKey: HKEY;

begin
  if RegOpenKeyExW(aRoot, PWideChar(aKey), 0, KEY_WRITE, lKey)<>ERROR_SUCCESS then
    Exit;
  RegDeleteValueW(lKey, nil);
  RegCloseKey(lKey);
end;


// Print the registration state
procedure Status;

begin
  Writeln('default mail client (HKCU): "', GetRegString(HKEY_CURRENT_USER, cMailClients, ''), '"');
  Writeln('default mail client (HKLM): "', GetRegString(HKEY_LOCAL_MACHINE, cMailClients, ''), '"');
  Writeln('default mail client library: "', DefaultMailClientLibrary, '"');
  Writeln(cClientName, ' DLLPath: "', GetRegString(HKEY_LOCAL_MACHINE, cClientKey, 'DLLPath'), '"');
end;


// Register aDll and make it the default mail client; False on failure
function Install(const aDll: UnicodeString): Boolean;

var
  lPrevious, lPreviousMachine: UnicodeString;

begin
  Result:=False;
  if not FileExists(aDll) then
    begin
    Writeln('no such file: ', aDll);
    Exit;
    end;
  lPrevious:=GetRegString(HKEY_CURRENT_USER, cMailClients, '');
  if lPrevious=cClientName then
    lPrevious:=GetRegString(HKEY_LOCAL_MACHINE, cClientKey, cPreviousDefault);
  lPreviousMachine:=GetRegString(HKEY_LOCAL_MACHINE, cMailClients, '');
  if lPreviousMachine=cClientName then
    lPreviousMachine:=GetRegString(HKEY_LOCAL_MACHINE, cClientKey, cPreviousMachineDefault);
  if not (SetRegString(HKEY_LOCAL_MACHINE, cClientKey, '', cClientName)
    and SetRegString(HKEY_LOCAL_MACHINE, cClientKey, 'DLLPath', aDll)
    and SetRegString(HKEY_LOCAL_MACHINE, cClientKey, 'DLLPathEx', aDll)
    and SetRegString(HKEY_LOCAL_MACHINE, cClientKey, cPreviousDefault, lPrevious)
    and SetRegString(HKEY_LOCAL_MACHINE, cClientKey, cPreviousMachineDefault, lPreviousMachine)
    and SetRegString(HKEY_LOCAL_MACHINE, cMailClients, '', cClientName)) then
    begin
    Writeln('cannot write HKLM\', cMailClients, ' (run as administrator)');
    Exit;
    end;
  Result:=SetRegString(HKEY_CURRENT_USER, cMailClients, '', cClientName);
  if Result then
    Writeln('registered ', aDll, ' as "', cClientName, '", previous defaults "', lPrevious, '" (HKCU), "',
      lPreviousMachine, '" (HKLM)');
end;


// Restore the previous default mail client and remove the registration; False on failure
function Uninstall: Boolean;

var
  lPrevious, lPreviousMachine: UnicodeString;

begin
  lPrevious:=GetRegString(HKEY_LOCAL_MACHINE, cClientKey, cPreviousDefault);
  lPreviousMachine:=GetRegString(HKEY_LOCAL_MACHINE, cClientKey, cPreviousMachineDefault);
  if GetRegString(HKEY_CURRENT_USER, cMailClients, '')=cClientName then
    if lPrevious<>'' then
      SetRegString(HKEY_CURRENT_USER, cMailClients, '', lPrevious)
    else
      DeleteDefaultValue(HKEY_CURRENT_USER, cMailClients);
  if GetRegString(HKEY_LOCAL_MACHINE, cMailClients, '')=cClientName then
    if lPreviousMachine<>'' then
      SetRegString(HKEY_LOCAL_MACHINE, cMailClients, '', lPreviousMachine)
    else
      DeleteDefaultValue(HKEY_LOCAL_MACHINE, cMailClients);
  Result:=RegDeleteKeyW(HKEY_LOCAL_MACHINE, cClientKey)=ERROR_SUCCESS;
  Writeln('unregistered "', cClientName, '", defaults restored to "', lPrevious, '" (HKCU), "', lPreviousMachine, '" (HKLM)');
end;


var
  lDll: UnicodeString;

begin
  if ParamStr(1)='install' then
    begin
    if ParamCount>=2 then
      lDll:=ExpandFileName(ParamStr(2))
    else
      lDll:=ExtractFilePath(ParamStr(0))+'fpcmapiprov.dll';
    if not Install(lDll) then
      ExitCode:=1;
    end
  else if ParamStr(1)='uninstall' then
    begin
    if not Uninstall then
      ExitCode:=1;
    end
  else if ParamStr(1)='status' then
    Status
  else
    begin
    Writeln('usage: regmapiprov install [DLL] | uninstall | status');
    ExitCode:=2;
    end;
end.
