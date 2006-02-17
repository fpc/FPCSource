{ %target=win32,wince }

{ Source provided for Free Pascal Bug Report 1375 }
{ Submitted by "Bill Rayer" on  2001-02-01 }
{ e-mail: lingolanguage@hotmail.com }
{
Should be able to use null ptr as 2nd param of InvalidateRect()
Compiles in Delphi 4:
  dcc32 fpc1
Does not compile in FPC:
  ppc386 -Sd fpc1
}

program test1;
uses windows;
begin
  InvalidateRect (HWND(0), pointer(0), TRUE);
end.
