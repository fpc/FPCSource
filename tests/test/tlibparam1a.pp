{ %target=linux }
{ %skiptarget=$nosharedlib }
{ %norun }

{ library part of tlibparam1b, issue #20752 }
library tlibparam1a;

{$mode objfpc}{$h+}

uses
  SysUtils;

// Return the parameter count seen by the library
function LibParamCount: Longint; cdecl;

begin
  Result:=ParamCount;
end;


// Copy ObjPas.ParamStr(aIndex) as seen by the library into aBuf
procedure LibParamStr(aIndex: Longint; aBuf: PAnsiChar; aSize: Longint); cdecl;

begin
  StrPLCopy(aBuf,ParamStr(aIndex),aSize-1);
end;


// Copy System.ParamStr(aIndex) as seen by the library into aBuf
procedure LibSysParamStr(aIndex: Longint; aBuf: PAnsiChar; aSize: Longint); cdecl;

begin
  StrPLCopy(aBuf,System.ParamStr(aIndex),aSize-1);
end;


exports
  LibParamCount, LibParamStr, LibSysParamStr;

end.
