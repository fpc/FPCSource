{ %version=1.1 }

{$ifdef go32v2}
  {$define USE_FPWIDESTRING_UNIT}
  {$define USE_UNICODEDUCET_UNIT}
{$endif}

{$ifdef fpc}
{$ifdef unix}
  {$ifdef darwin}
uses
  iosxwstr;
  {$else}
uses
  cwstring;
  {$endif}
{$endif}
{$ifdef go32v2}
uses
    fpwidestring,
    unicodeducet;
{$endif}
{$endif}

type
  RR = record
    RA : WideString;
  end;

const
  Z : RR = (RA: 'B');

begin
  if z.ra<>'B' then
    begin
      writeln('error');
      halt(1);
    end;
end.
