program strtest;

{$MODE OBJFPC}
{$LONGSTRINGS ON}

{$ifdef unix}
uses CWstring;
{$endif}

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
