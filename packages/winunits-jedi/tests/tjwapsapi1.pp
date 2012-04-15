program winmoduleinfo_test;

{$mode objfpc}{$H+}

uses
  SysUtils,
  Classes,
  windows,
  jwapsapi;

  function GetModuleName(
    const pHandle : HANDLE;
    const mHandle : HMODULE)
    : string;
  var
    name : array [0..1023] of char;
  begin
    result := 'ERROR';
    if GetModuleFileNameEx(pHandle,mHandle,LPTSTR(@name[0]),sizeof(name)) > 0 then begin
      result := name;
    end;
  end;

  procedure WriteModuleInfo(
    const pHandle : HANDLE;
    const mHandle : HANDLE);
  var
    moduleHandle : HANDLE;
    modInfo: MODULEINFO;
    moduleName : string;
    tmpBeginning : ptruint;
    tmpEnding : ptruint;
    success : boolean;
    lastError : DWORD;
  begin
    moduleHandle := mHandle;

    success := (GetModuleInformation(pHandle,moduleHandle,modInfo,sizeof(MODULEINFO)) = WINBOOL(true));
    if success then begin
      lastError := 0;
    end else begin
      lastError := GetLastError();
      modInfo.lpBaseOfDll := nil;
      modInfo.SizeOfImage := 0;
    end;

    tmpBeginning := ptruint(modInfo.lpBaseOfDll);
    tmpEnding := tmpBeginning + modInfo.SizeOfImage;
    moduleName := GetModuleName(pHandle,mHandle);
    writeln(ExtractFileName(moduleName), ' error=', lastError,
            '; from 0x', IntToHex(tmpBeginning, sizeof(tmpBeginning)*2),
            ' to 0x', IntToHex(tmpBeginning, sizeof(tmpEnding)*2));
    if lastError<>0 then
      halt(1);
  end;

var
  pHandle : HANDLE;
  mHandles : array [0..1023] of HMODULE;
  cbNeeded : DWORD;
  n : integer;
  i : integer;
begin
  pHandle := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,
                         WINBOOL(false),
                         GetCurrentProcessID);
  if pHandle <> 0 then begin
    try
      if EnumProcessModules(pHandle,@mHandles[0],sizeof(mHandles),cbNeeded) = WINBOOL(true) then begin
        n := cbNeeded div sizeof(HMODULE);
        for i := 0 to n-1 do begin
          WriteModuleInfo(pHandle, mHandles[i]);
        end;
      end;
    finally
      CloseHandle(pHandle);
    end;
  end;
  writeln('ok');
end.
