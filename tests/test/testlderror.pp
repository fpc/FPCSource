{ %FAIL }
{ %OPT= -Xe -k--dummy-unsupported-option }
{ This is a test to check that passing
  wrong options to the external linker
  causes the compilation to fail }

{ Targets not supporting external linker
  should be listed in skiptarget.
  I don't know if we have any... }

{ Note: -k option does not force external
  linker use, which means that using
  -k--dummy-unsupported-option
  alone doesn't generate an error
  Don't know if this is a feature or a bug...
  PM 2011-09-01 }


program Test_external_linker_error_report;

begin
  Writeln('Test of external linker');
end.
