{ %target=win32 }

uses Windows;

function Enum_FindTaskWindow (hWindow:HWND; lpar:LPARAM) : boolean; export; stdcall;
begin
  Enum_FindTaskWindow := FALSE;
end;

var dwThread:DWORD;
begin
  dwThread := GetCurrentThreadId;
  EnumTaskWindows (dwThread, @Enum_FindTaskWindow, LPARAM(nil));
end.
