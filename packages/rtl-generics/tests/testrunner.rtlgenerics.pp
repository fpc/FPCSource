{ %CONFIGFILE=fpcunit-console-defaults.ini testdefaults.ini }
{ %SKIPTARGET=embedded,nativent,msdos,win16,macos,palmos }

program testrunner.rtlgenerics;

{$mode objfpc}{$H+}

uses
  consoletestrunner,
  tests.generics.bugs,
  tests.generics.hashmaps,
  tests.generics.arrayhelper,
  tests.generics.trees,
  tests.generics.stdcollections,
  tests.generics.sets,
  tests.generics.queue,
  tests.generics.stack,
  tests.generics.dictionary
  ;

var
  Application: TTestRunner;

begin
  Application := TTestRunner.Create(nil);
  Application.Initialize;
  Application.Title := 'RTL-Generics unit tests';
  Application.Run;
  Application.Free;
{$IFDEF WAIT_FOR_ENTER}
  WriteLn('Press enter...');
  ReadLn;
{$ENDIF}
end.
