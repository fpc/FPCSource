{ %interactive }

{$ifdef go32v2}
  {$define USE_INTERNAL_UNICODE}
{$endif}

{$ifdef USE_INTERNAL_UNICODE}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
  {$define USE_CPALL_UNIT}
{$endif}
uses
{$ifdef unix}
  clocale,
{$endif}
{$ifndef USE_INTERNAL_UNICODE}
 {$ifdef unix}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
 {$endif unix}
{$endif not USE_INTERNAL_UNICODE}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
  SysUtils;

procedure PrintSettings;
var
 i: integer;
begin
  Writeln('Month names:');
  for i := 1 to 12 do
    Writeln('month ',i:5,' ',shortmonthnames[i]:25,' - ',longmonthnames[i]);
  Writeln;
  Writeln('Day names  :');
  for i := 1 to 7 do
     writeln('short day ',i:5,' ',shortdaynames[i]:25,' - ',longdaynames[i]);

  writeln('Dateseparator : ', dateseparator);
  {$ifdef localedebug}
  writeln('orgshortdate  : ', orgformatsettings.shortdateformat);
  {$endif}
  writeln('short date    : ', shortdateformat);
  {$ifdef localedebug}
  writeln('orglongdate   : ', orgformatsettings.longdateformat);
  {$endif}
  writeln('long  date    : ', longdateformat);
 { Time stuff }
  writeln('TimeSeparator : ', timeseparator);
  writeln('TimeAMstring  : ', timeamstring);
  writeln('TimePMstring  : ', timepmstring);  
  {$ifdef localedebug}
  writeln('orgshorttime  : ', orgformatsettings.shorttimeformat);
  {$endif}

 // No support for 12 hour stuff at the moment...
  writeln('short time    : ', shorttimeformat);
  {$ifdef localedebug}
  writeln('orglongtime   : ', orgformatsettings.longtimeformat);
  {$endif}
  writeln('long  time    : ', longtimeformat);

 { Currency stuff }

  {$ifdef localedebug}
  writeln('currency1     : ', orgformatsettings.currencystring1);
  writeln('currency2     : ', orgformatsettings.currencystring2);
  {$endif}

  writeln('currencystring  : ', currencystring);

  writeln('currencyformat  : ', currencyformat);
  writeln('negcurrformat   : ', negcurrformat);

  writeln('decimalseparator : ', decimalseparator);
  writeln('thousandseparator: ', thousandseparator);
  writeln('currencydecimals : ', currencydecimals);
end;

begin
  printsettings;
end.
