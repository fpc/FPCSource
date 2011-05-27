unit TestBasics;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  fpcunit, testutils, testregistry, testdecorator,
  Classes, SysUtils;

type

  { TTestBasics }

  TTestBasics = class(TTestCase)
  private
  protected
  published
    procedure TestSimpleWinRegistry;
    procedure TestDoubleWrite;
  end;

implementation

uses
  registry;

{ TTestBasics }

procedure TTestBasics.TestSimpleWinRegistry;
var
  Registry : TRegistry;
begin
  Registry := TRegistry.Create(KEY_READ);
  Registry.RootKey:=HKEY_LOCAL_MACHINE;

  // use a hopefully non existing key
  AssertFalse(Registry.KeyExists('FPC1234'));
{$ifdef windows}
  AssertTrue(Registry.KeyExists('SOFTWARE'));
{$endif}  

  Registry.Free;
end;

procedure TTestBasics.TestDoubleWrite;

{$ifndef windows}
Var
  FN : String;
{$endif}

begin
{$ifndef windows}
  FN:=includetrailingpathdelimiter(GetAppConfigDir(False))+'reg.xml';
  if FileExists(FN) then
    AssertTrue(DeleteFile(FN));
{$endif}
  with TRegistry.Create do
    try
      OpenKey('test', true);
      WriteString('LAYOUT', '');
      CloseKey;
    finally
      Free;
    end;
  with TRegistry.Create do
    try
      OpenKey('test', true);
      WriteString('LAYOUT', '');
      CloseKey;
    finally
      Free;
    end;
{$ifndef windows}
  FN:=includetrailingpathdelimiter(GetAppConfigDir(False))+'reg.xml';
  if FileExists(FN) then
    AssertTrue(DeleteFile(FN));
{$endif}
end;

initialization
  RegisterTest(TTestBasics);
end.
