{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }
{ %SKIPTARGET=embedded,nativent,msdos,win16,macos,palmos }

program testrunner.rtlobjpas;

{$mode objfpc}{$H+}
{ Invoke needs a function call manager }
{.$define useffi}
{$if defined(CPUX64) and defined(WINDOWS)}
{$define testinvoke}
{$define testimpl}
{$elseif defined(CPUI386)}
{$define testinvoke}
{$define testimpl}
{$else}
{$ifdef useffi}
{$define testinvoke}
{$define testimpl}
{$endif}
{$endif}

{$IF DEFINED(WINDOWS) or DEFINED(UNIX)}
{$DEFINE HAS_MONITOR}
{$ENDIF}

uses
{$ifdef unix}
  cthreads,
  cwstring,
{$endif}
{$ifdef useffi}
  ffi.manager,
{$endif}
  consoletestrunner,
  tests.rtti.util,
{$ifdef testinvoke}
  tests.rtti.invoke,
{$endif}
{$ifdef testimpl}
  tests.rtti.impl,
{$endif}
  tests.rtti,
  tests.rtti.value,
  tests.rtti.types,
  utmathvectorbase,
  utcmatrix,
  utcpoint,
  utcvector,
  utcquaternion,
{$IFDEF HAS_MONITOR}
  utcfpmonitor, tests.rtti.attrtypes
{$ENDIF}

;


var
  Application: TTestRunner;

begin
  DefaultFormat:=fPlain;
  DefaultRunAllTests:=True;
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'RTL-ObjPas unit tests';
  Application.Run;
  Application.Free;
end.
