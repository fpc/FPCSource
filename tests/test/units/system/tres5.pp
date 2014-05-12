{ Test for resources support from .rc files. }

{%TARGET=win32,win64}

{$mode objfpc}

uses sysutils;

{$R tres5.rc}

procedure Fail(const Msg: string);
begin
  writeln(Msg);
  Halt(1);
end;

function GetResource(ResourceName, ResourceType: PChar; PResSize: PLongInt = nil): pointer;
var
  hRes: TFPResourceHandle;
  gRes: TFPResourceHGLOBAL;
begin
  hRes:=FindResource(HINSTANCE, ResourceName, ResourceType);
  if hRes = 0 then
    Fail('FindResource failed.');
  gRes:=LoadResource(HINSTANCE, hRes);
  if gRes = 0 then
    Fail('LoadResource failed.');
  if PResSize <> nil then begin
    PResSize^:=SizeofResource(HINSTANCE, hRes);
    if PResSize^ = 0 then
      Fail('SizeofResource failed.');
  end;
  Result:=LockResource(gRes);
  if Result = nil then
    Fail('LockResource failed.');
end;

procedure DoTest;
var
  s: string;
  p: PChar;
  sz: longint;
begin
  p:=GetResource('TestFile', 'FILE', @sz);
  SetString(s, p, sz);
  if s <> 'test file.' then
    Fail('Invalid resource loaded.');
  writeln(s);

  p:=GetResource('Test', 'TEXT', @sz);
  SetString(s, p, sz);
  if s <> 'Another test file.' then
    Fail('Invalid resource loaded.');
  writeln(s);
end;

begin
  writeln('Resources test.');
  DoTest;
  writeln('Done.');
end.
