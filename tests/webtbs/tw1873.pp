{$ifdef win32}
uses Windows,ub1873;
var
  s : SC_handle;
  d : dword;
begin
  GetServiceDisplayNameA(s,nil,nil,d);
{$else}
begin
  writeln('win32 only');
{$endif}
end.

