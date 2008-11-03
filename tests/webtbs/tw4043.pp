{ %target=win32 }
{ %OPT=-Sew -vw }

uses
  windows;

var
  WindowHandle : HWND;

begin
  WindowHandle:=0;
  SetWindowLong(WindowHandle, GWL_STYLE, WS_POPUP or WS_CLIPCHILDREN);
  halt(0);
end.
