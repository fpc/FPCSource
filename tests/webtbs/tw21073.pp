{ %norun }

{$mode delphi}

program gpabugtest;

TYPE TGetCurrentProcess = function : THandle; stdcall;
     TGetProcAddress = function(const hModule : THandle; const lpProcName : PAnsiChar) : Pointer; stdcall;

function GetProcAddress(const hModule : THandle;const lpProcName : PAnsiChar) : Pointer; stdcall;
begin
  result:=nil;
end;

function GetModuleHandle(const lpModuleName : PWideChar) : THandle; stdcall;
begin
  result:=thandle(-1);
end;

var proc_GetCurrentProcess : TGetCurrentProcess;
    proc_GetProcAddress : TGetProcAddress;

begin
 {no error} proc_GetCurrentProcess:=GetProcAddress(GetModuleHandle('Kernel32'),'GetCurrentProcess');
 {error ??} proc_GetProcAddress:=   GetProcAddress(GetModuleHandle('Kernel32'),'GetProcAddress');
end.
