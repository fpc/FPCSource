{ %target=win32 }

{ Source provided for Free Pascal Bug Report 4290 }
{ Submitted by "rimga" on  2005-08-18 }
{ e-mail: rimga@ktl.mii.lt }
function SysAllocStringLen(psz:pointer;len:dword):pointer;stdcall;
 external 'oleaut32.dll' name 'SysAllocStringLen';

procedure SysFreeString(bstr:pointer);stdcall;
 external 'oleaut32.dll' name 'SysFreeString';

var
  s: PWideChar;
  w: widestring;
begin
  setlength(w, 7);;
  s:= SysAllocStringLen(nil, 7);
  WriteLn(plongint(pointer(s)-4)^);
  WriteLn(plongint(pointer(w)-4)^);
  if plongint(pointer(s)-4)^ <> plongint(pointer(w)-4)^ then
    Writeln('Not equal: problem (widestring not compatible to COM BSTR)')
  else
    Writeln('OK');
  SysFreeString(s);
end.
