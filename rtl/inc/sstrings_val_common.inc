{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Val() Functions
*****************************************************************************}

type
  ValCommon=record
    negative:boolean;
    base,baseIndex:byte; { baseIndex is flattened base for lookups: 0 — base 10, 1 — base 16, 2 — base 2, 3 — base 8. }
    minusPos:SizeInt;
  end;

Function InitVal(const s:shortstring;out vc:ValCommon):ValSInt;
var
  ns : SizeInt;
begin
  result:=1;
  vc.negative:=false;
  vc.base:=10;
  vc.baseIndex:=0;
  ns:=length(s);
{Skip Spaces and Tab}
  while (result<=ns) and (s[result] in [' ',#9]) do
   inc(result);
{Sign}
  if result<=ns then
   case s[result] of
    '-' : begin
            vc.negative:=true;
            vc.minusPos:=result;
            inc(result);
          end;
    '+' : inc(result);
   end;
{Base}
  if result<=ns then
   case s[result] of
    '$',
    'X',
    'x' : begin
            vc.base:=16;
            vc.baseIndex:=1;
            inc(result);
          end;
    '%' : begin
            vc.base:=2;
            vc.baseIndex:=2;
            inc(result);
          end;
    '&' : begin
            vc.base:=8;
            vc.baseIndex:=3;
            inc(result);
          end;
    '0' : if (result<ns) and (s[result+1] in ['x', 'X']) then
          begin
            vc.base:=16;
            vc.baseIndex:=1;
            inc(result, 2);
          end;
   end;
  { strip leading zeros }
  while (result<ns) and (s[result]='0') and (s[result+1]<>#0) do
    inc(result);
end;

const
  ValData: record
    ValueArray: array[0..ord('f')-ord('0')] of byte;
    MaxDigits: array[0 .. 2 { unsigned / signed positive / signed negative }, 0 .. 3 { base index }, 0 .. 4 { Bsr(DestSize) }] of byte;
  end =
  (
    ValueArray:
    (
      0,1,2,3,4,5,6,7,8,9,$FF,$FF,$FF,$FF,$FF,$FF,$FF,10,11,12,13,14,15,
      $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,
      10,11,12,13,14,15
    );

    { If VAL input has exactly this many digits (without leading zeros), then it may overflow.
      If it has more digits, it definitely overflows.
      If it has less, it definitely doesn’t overflow. }
    MaxDigits:
    (
      ( { unsigned }
        (3, 5, 10, 20, 39), { base 10: 255 / 65535 / 4_294_967_295 / 18_446_744_073_709_551_615 / 340_282_366_920_938_463_463_374_607_431_768_211_455 }
        (2, 4, 8, 16, 32), { base 16: $FF / $FFFF / $FFFF_FFFF / $FFFF_FFFF_FFFF_FFFF /  $FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF }
        (8, 16, 32, 64, 128), { base 2: %1111_1111 / %1111_1111_1111_1111 / %1111_1111_1111_1111_1111_1111_1111_1111 / %1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 / %1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 }
        (3, 6, 11, 22, 43) { base 8: &377 / &17_7777 / &377_7777_7777 / &17_7777_7777_7777_7777_7777 / &377_7777_7777_7777_7777_7777_7777_7777_7777_7777_7777 }
      ),
      ( { signed positive }
        (3, 5, 10, 19, 39), { base 10: 127 / 32767 / 2_147_483_647 / 9_223_372_036_854_775_807 / 170_141_183_460_469_231_731_687_303_715_884_105_727 }
        (2, 4, 8, 16, 32), { base 16: $7F / $7FFF / $7FFF_FFFF / $7FFF_FFFF_FFFF_FFFF / $7FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF }
        (7, 15, 31, 63, 127), { base 2: %111_1111 / %111_1111_1111_1111 / %111_1111_1111_1111_1111_1111_1111_1111 / %111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 / %111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111_1111 }
        (3, 5, 11, 21, 43) { base 8: &177 / &7_7777 / &177_7777_7777 / &7_7777_7777_7777_7777_7777 / &177_7777_7777_7777_7777_7777_7777_7777_7777_7777_7777 }
      ),
      ( { signed negative }
        (3, 5, 10, 19, 39), { base 10: 128 / 32768 / 2_147_483_648 / 9_223_372_036_854_775_808 / 170_141_183_460_469_231_731_687_303_715_884_105_728 }
        (2, 4, 8, 16, 32), { base 16: $80 / $8000 / $8000_0000 / $8000_0000_0000_0000 / $8000_0000_0000_0000_0000_0000_0000_0000 }
        (8, 16, 32, 64, 128), { base 2: %1000_0000 / %1000_0000_0000_0000 / %1000_0000_0000_0000_0000_0000_0000_0000 / %1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 / %1000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 }
        (3, 6, 11, 22, 43) { base 8: &200 / &10_0000 / &200_0000_0000 / &10_0000_0000_0000_0000_0000 / &200_0000_0000_0000_0000_0000_0000_0000_0000_0000_0000 }
      )
    )
  );

type
  ValNonZeroBase = 1 .. ord(High(ValCommon.base));
  ValNonZeroDestSize = 1 .. ord(High(uint32));
