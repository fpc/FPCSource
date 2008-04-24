{ %interactive }

uses
{$ifdef unix}
  cwstring,
  clocale,
{$endif}
  SysUtils;


procedure PrintSettings;
var
 i: integer;
begin
 for i := 1 to 12 do
   begin
   writeln(ShortMonthNames[i]);
   writeln(LongMonthNames[i]);
   end;
 for i := 1 to 7 do
   begin
   writeln(ShortDayNames[i]);
   writeln(LongDayNames[i]);
   end;
 writeln(DateSeparator);
 writeln(ShortDateFormat);
 writeln(LongDateFormat);
 { Time stuff }
 writeln(TimeSeparator);
 writeln(TimeAMString);
 writeln(TimePMString);
 // No support for 12 hour stuff at the moment...
 writeln(ShortTimeFormat);
 writeln(LongTimeFormat);
 { Currency stuff }
 writeln(CurrencyString);
 writeln(CurrencyFormat);
 writeln(NegCurrFormat);
 { Number stuff }
 writeln(ThousandSeparator);
 writeln(DecimalSeparator);
 writeln(CurrencyDecimals);
end;

begin
  printsettings;
end.
