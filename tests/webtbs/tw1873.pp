{ %target=win32 }

uses Windows,ub1873;
var
  s : SC_handle;
  d : dword;
begin
  GetServiceDisplayNameA(s,nil,nil,d);
end.
