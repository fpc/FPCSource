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

  AssertTrue(Registry.KeyExists('SOFTWARE'));

  // Registry.OpenKey('FPC', False);
  // Result:=Registry.ReadString('VALUE1');

  Registry.Free;
end;

initialization
  RegisterTest(TTestBasics);
end.
