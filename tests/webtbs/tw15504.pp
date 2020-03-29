program strtest;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

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
 {$ifndef USE_INTERNAL_UNICODE}
  {$ifdef darwin}iosxwstr{$else}cwstring{$endif},
 {$endif ndef USE_INTERNAL_UNICODE}
{$endif unix}
 {$ifdef USE_UNICODEDUCET_UNIT}
  unicodeducet,
 {$endif}
 {$ifdef USE_FPWIDESTRING_UNIT}
  fpwidestring,
 {$endif}
 {$ifdef USE_CPALL_UNIT}
  cpall,
 {$endif}
  Strings;

type
    tEnum = (North, East, South, West);

var s0: shortstring;
    s1: widestring;
    s2: ansistring;
    s3: array[3..7] of char;
    e: tEnum;

begin
    e := West; Str(e, s0);
    if s0<>'West' then
      halt(1);
    e := East; Str(e, s1);
    if s1<>'East' then
      halt(2);
    e := South; Str(e, s2);
    if s2<>'South' then
      halt(3);
    e:= North; Str(e, s3);
    if s3<>'North' then
      halt(4);
end.
