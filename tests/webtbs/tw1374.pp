{ Source provided for Free Pascal Bug Report 1374 }
{ Submitted by "Christian Keck" on  2001-02-01 }
{ e-mail: C.Keck@gmx.net }
program realtest;

var temp        : longint;
    low         : byte;
    high        : byte;
    DLL_Version : extended{real};

begin
  temp:= 14340; { This value is normaly retrieved form an external DLL }

  low:= ((temp shr 8) and $FF);
  high:= (temp and $FF);          { Some calculation...   }
  DLL_Version:= high + (low/100); { to get an real result }

  { ... }

  if DLL_Version < 4.56 then      { 4.560000000000000E+000 < 4.56 ?!?! }
    writeln ('Error! 4.56 < 4.56');
end.
