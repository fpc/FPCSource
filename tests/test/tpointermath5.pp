{ %FAIL }

program tpointermath5;

{$mode delphi}

{ both pointer types need to have POINTERMATH enabled for comparisons to be
  supported between them }

type
  PMyCardinal = ^Cardinal;
{$POINTERMATH ON}
  PMyByte = ^Byte;
{$POINTERMATH OFF}

procedure Test;
var
  pc: PMyCardinal;
  pb: PMyByte;
  b: Boolean;
begin
  b := pc > pb;
  b := pc < pb;
end;

begin
end.
