{ %TARGET=win32,win64 }
{ EnumResourceTypesEx A/W/generic find the same resource types as EnumResourceTypesA, issue #35110 }
program tenumrestypesex;

{$mode objfpc}{$h+}

uses
  Windows;

var
  lCount: Integer;

function EnumTypeA(aModule: HMODULE; aType: LPSTR; aParam: LONG_PTR): WINBOOL; stdcall;

begin
  Inc(lCount);
  Result:=True;
end;


function EnumTypeW(aModule: HMODULE; aType: LPWSTR; aParam: LONG_PTR): WINBOOL; stdcall;

begin
  Inc(lCount);
  Result:=True;
end;


procedure Check(aCode: Integer; aOk: Boolean; aExpected: Integer);

begin
  if not aOk then
    begin
    writeln(aCode,': failed, error ',GetLastError);
    halt(aCode);
    end;
  if lCount<>aExpected then
    begin
    writeln(aCode,': ',lCount,' types, expected ',aExpected);
    halt(aCode+1);
    end;
  lCount:=0;
end;


var
  lModule: HMODULE;
  lExpected: Integer;

begin
  lModule:=LoadLibrary('user32.dll');
  if lModule=0 then
    halt(1);
  lCount:=0;
  if not EnumResourceTypesA(lModule,@EnumTypeA,0) or (lCount=0) then
    halt(2);
  lExpected:=lCount;
  lCount:=0;
  Check(10,EnumResourceTypesExA(lModule,@EnumTypeA,0,RESOURCE_ENUM_LN,0),lExpected);
  Check(20,EnumResourceTypesExW(lModule,@EnumTypeW,0,RESOURCE_ENUM_LN or RESOURCE_ENUM_VALIDATE,0),lExpected);
  Check(30,EnumResourceTypesEx(lModule,ENUMRESTYPEPROC(@EnumTypeA),0,RESOURCE_ENUM_LN,0),lExpected);
  FreeLibrary(lModule);
  writeln('ok');
end.
