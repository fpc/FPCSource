{ Test for resources support. }

{%TARGET=win32,win64,wince,linux}

{$mode objfpc}

{$R tres1.res}

procedure Fail(const Msg: string);
begin
  writeln(Msg);
  Halt(1);
end;

function GetResource(ResourceName, ResourceType: PChar; PResSize: PLongInt = nil): pointer;
var
  hRes: TResourceHandle;
  gRes: HGLOBAL;
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
