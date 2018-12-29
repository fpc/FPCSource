{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }
{ %SKIPTARGET=embedded,nativent,msdos,win16,macos,palmos }

program testrunner.rtlobjpas;

{$mode objfpc}{$H+}
{ Invoke needs a function call manager }
{.$define useffi}
{$if defined(CPUX64) and defined(WINDOWS)}
{$define testinvoke}
{$else}
{$ifdef useffi}
{$define testinvoke}
{$endif}
{$endif}

uses
{$ifdef useffi}
  ffi.manager,
{$endif}
  consoletestrunner,
{$ifdef testinvoke}
  tests.rtti.invoke,
{$endif}
{$ifdef testimpl}
  tests.rtti.impl,
{$endif}
  tests.rtti;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'RTL-ObjPas unit tests';
  Application.Run;
  Application.Free;
end.
