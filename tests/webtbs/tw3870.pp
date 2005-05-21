{ Source provided for Free Pascal Bug Report 3870 }
{ Submitted by "Tom Verhoeff" on  2005-04-04 }
{ e-mail: T.Verhoeff@tue.nl }
program AssertFormatBug;
  { Illustrates bug with using Format in Assert }

{$mode objfpc}
{$assertions on}

uses
  SysUtils { for IntToStr, Format };

begin
  try
    Assert(False, IntToStr(2));  { incorrectly raises EAccessViolation }
  except
    on E: EAssertionFailed do
      begin
        Writeln('Caught Assert: ',E.Message);
      end;
  end;
end.
