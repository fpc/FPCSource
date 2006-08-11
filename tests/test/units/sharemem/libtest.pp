{ %target=win32,win64,wince}
library LibTest;

uses
  sharemem;

function LibFunction : pointer; stdcall;
begin
  getmem(result,10000);
end;


exports
  LibFunction;
end.
