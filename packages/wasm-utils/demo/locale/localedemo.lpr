program localedemo;

uses sysutils, dateutils,wasm.locale.objects, wasiutil;

var
  I : integer;

procedure testwasiutil;

var
  epoch,epoch2 : int64;
  y,m,d,h,n,s : word;

begin
  epoch:=dateutils.DateTimeToUnix(sysutils.now);
  wasiutil.EpochToLocal(epoch,y,m,d,h,n,s);
  epoch2:=wasiutil.localtoepoch(y,m,d,h,n,s);
  if epoch<>epoch2 then
    Writeln('Epoch time error: ',epoch,'<>',epoch2)
  else
    Writeln('Epoch time OK: ',epoch,'=',epoch2);
end;

begin
  With TWasmHostLocale.Create do
    try
      Writeln('Host locale object:');
      Writeln('-------------------');
      for I:=1 to 7 do
        Writeln('Day[',i,'] long: ',LongDayNames[i],', Short: ',ShortDayNames[i]);
      Writeln;
      for I:=1 to 12 do
        Writeln('Month[',i,'] long: ',LongMonthNames[i],', Short: ',ShortMonthNames[i]);
      Writeln;
      Writeln('Date separator   : ',DateSeparator);
      Writeln('Time separator   : ',TimeSeparator);
      Writeln('Decimal separator   : ',DecimalSeparator);
      Writeln('Thousands separator : ',ThousandSeparator);
      Writeln('Currency symbol     : ',CurrencyString);
      Writeln('TimezoneOffsetl     : ',TimeZoneOffset);
      TransferToFormatSettings;
      wasiutil.UTCTimeOffset:=-TimeZoneOffset;
    finally
      Free;
    end;
  Writeln('');
  Writeln('');
  Writeln('Format settings:');
  Writeln('----------------');
  With DefaultFormatSettings do
    begin
    for I:=1 to 7 do
      Writeln('Day[',i,'] long: ',LongDayNames[i],', Short: ',ShortDayNames[i]);
    Writeln;
    for I:=1 to 12 do
      Writeln('Month[',i,'] long: ',LongMonthNames[i],', Short: ',ShortMonthNames[i]);
    Writeln;
    Writeln('Date separator   : ',DateSeparator);
    Writeln('Time separator   : ',TimeSeparator);
    Writeln('Decimal separator   : ',DecimalSeparator);
    Writeln('Thousands separator : ',ThousandSeparator);
    Writeln('Currency symbol     : ',CurrencyString);
    end;
  Writeln('TZ environment variable: ',GetEnvironmentVariable('TZ'));
  Writeln('Local Timezone offset: ',GetLocalTimeOffset);
  Writeln('Local Time: ',FormatDateTime('hh:nn:ss',Now));
  Writeln('UTC Time: ',FormatDateTime('hh:nn:ss',NowUTC));
  Writeln('LocalToUniversal Time: ',FormatDateTime('hh:nn:ss',DateUtils.LocalTimeToUniversal(Now)));
  testwasiutil;
end.

