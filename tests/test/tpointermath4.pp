{ %NORUN }

program tpointermath4;

{$mode delphi}

type
{$POINTERMATH ON}
  PMyCardinal = ^Cardinal;
  PMyByte = ^Byte;
  PMyString = ^String;
{$POINTERMATH OFF}

procedure Test;
var
  pc: PMyCardinal;
  pb: PMyByte;
  ps: PMyString;
  b: Boolean;
begin
  b := pc > pb;
  b := pc < pb;
  b := ps < pb;
  b := ps > pb;
  b := pc < ps;
  b := pc > ps;

  b := pc >= pb;
  b := pc <= pb;
  b := ps <= pb;
  b := ps >= pb;
  b := pc <= ps;
  b := pc >= ps;
end;

{$pointermath on}

var
  pc: PCardinal = Nil;
  pb: PByte = Nil;
  ps: PString = Nil;
  b: Boolean;
begin
  b := pc > pb;
  b := pc < pb;
  b := ps < pb;
  b := ps > pb;
  b := pc < ps;
  b := pc > ps;

  b := pc >= pb;
  b := pc <= pb;
  b := ps <= pb;
  b := ps >= pb;
  b := pc <= ps;
  b := pc >= ps;
end.
