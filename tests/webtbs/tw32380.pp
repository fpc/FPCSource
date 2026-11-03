{ %target=win32,win64,wince }
{ %norun }
{ The Windows unit has what a mouse and keyboard input sender needs, on WinCE as well }
program tw32380;

{$mode objfpc}

uses
  Windows;

{$if (MOUSEEVENTF_WHEEL <> $800) or (MOUSEEVENTF_XDOWN <> $80) or (MOUSEEVENTF_XUP <> $100) or (MOUSEEVENTF_VIRTUALDESK <> $4000)}
  {$error wrong MOUSEEVENTF_* value}
{$endif}
{$if WHEEL_DELTA <> 120}
  {$error wrong WHEEL_DELTA value}
{$endif}
{$ifdef wince}
{$if (MOUSEEVENTF_TOUCH <> $100000) or (MOUSEEVENTF_ALLOWROTATION <> $200000)}
  {$error wrong WinCE MOUSEEVENTF_* value}
{$endif}
{$endif}

// Send a mouse event with flags aFlag and wheel or button data aData.
procedure SendMouse(aFlag, aData: DWORD);

var
  lInput: TInput;

begin
  lInput := Default(TInput);
  lInput._Type := INPUT_MOUSE;
  lInput.mi.mouseData := aData;
  lInput.mi.dwFlags := aFlag;
  SendInput(1, @lInput, SizeOf(lInput));
end;


// Move the mouse to screen position aX, aY of a aWidth by aHeight screen.
procedure SendMouseMove(aX, aY, aWidth, aHeight: Integer);

var
  lInput: TInput;

begin
  lInput := Default(TInput);
  lInput._Type := INPUT_MOUSE;
  lInput.mi.dx := MulDiv(aX, 65535, aWidth - 1);
  lInput.mi.dy := MulDiv(aY, 65535, aHeight - 1);
  lInput.mi.dwFlags := MOUSEEVENTF_MOVE or MOUSEEVENTF_ABSOLUTE;
  SendInput(1, @lInput, SizeOf(lInput));
end;


// Send a key event with flags aFlag for virtual key aKey.
procedure SendKey(aFlag: DWORD; aKey: Word);

var
  lInput: TInput;

begin
  lInput := Default(TInput);
  lInput._Type := INPUT_KEYBOARD;
  lInput.ki.wVk := aKey;
  lInput.ki.dwFlags := aFlag;
  SendInput(1, @lInput, SizeOf(lInput));
end;


begin
  SendMouseMove(10, 10, 640, 480);
  SendMouse(MOUSEEVENTF_LEFTDOWN, 0);
  SendMouse(MOUSEEVENTF_LEFTUP, 0);
  SendMouse(MOUSEEVENTF_WHEEL, WHEEL_DELTA);
  SendMouse(MOUSEEVENTF_WHEEL, DWORD(-WHEEL_DELTA));
  SendKey(0, VK_SHIFT);
  SendKey(KEYEVENTF_KEYUP, VK_SHIFT);
end.
