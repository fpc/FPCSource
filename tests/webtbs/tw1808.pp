{$ifdef win32}
uses
  windows;
{$endif win32}

var
  x : DWORD;
begin
{$ifdef win32}
  x:=CommDlgExtendedError;
{$else not win32}
  x:=0;
{$endif win32}
end.
