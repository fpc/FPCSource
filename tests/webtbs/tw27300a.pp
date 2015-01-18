{ %OS=win32,win64,wince}
{ %norun }
program Project1;

uses
  Classes;

const
  kernel32 = 'kernel32.dll';

type
  BOOL = Boolean;
  HANDLE = THandle;

function OpenThread(
  {_In_} dwDesiredAccess: DWORD;
  {_In_} bInheritHandle: BOOL;
  {_In_} dwThreadId: DWORD
): HANDLE; WINAPI; external kernel32;

begin
end.
