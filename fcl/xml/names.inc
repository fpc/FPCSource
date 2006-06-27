{
    This file is part of the Free Component Library

    XML naming character tables, built upon w3.org specifications

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


type
  TByteSet = set of Byte;
  TNbPage = record
  case Boolean of
    False: (Init: array[0..7] of Cardinal);
  	True: (Work: TByteSet);
  end;

const
  namingBitmap: array[0..$27] of TNbPage = (
// #00 - nothing allowed
(Init: ($00000000, $00000000, $00000000, $00000000,
        $00000000, $00000000, $00000000, $00000000)),
// #01 - all allowed
(Init: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
        $FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF)),
// #02 - $0000, NameStart
(Init: ($00000000, $04000000, $87FFFFFE, $07FFFFFE,
        $00000000, $00000000, $FF7FFFFF, $FF7FFFFF)),
// #03 - $0100, both Name and NameStart
(Init: ($FFFFFFFF, $7FF3FFFF, $FFFFFDFE, $7FFFFFFF,
        $FFFFFFFF, $FFFFFFFF, $FFFFE00F, $FC31FFFF)),
// #04 - $0200, NameStart
(Init: ($00FFFFFF, $00000000, $FFFF0000, $FFFFFFFF,
        $FFFFFFFF, $F80001FF, $00000003, $00000000)),
// #05 - $0300, NameStart
(Init: ($00000000, $00000000, $00000000, $00000000,
        $FFFFD740, $FFFFFFFB, $547F7FFF, $000FFFFD)),
// #06 - $0400, NameStart
(Init: ($FFFFDFFE, $FFFFFFFF, $DFFEFFFF, $FFFFFFFF,
        $FFFF0003, $FFFFFFFF, $FFFF199F, $033FCFFF)),
// #07 - $0500, NameStart
(Init: ($00000000, $FFFE0000, $027FFFFF, $FFFFFFFE,
        $0000007F, $00000000, $FFFF0000, $000707FF)),
// #08 - $0600, NameStart
(Init: ($00000000, $07FFFFFE, $000007FE, $FFFE0000,
        $FFFFFFFF, $7CFFFFFF, $002F7FFF, $00000060)),
// #09 - $0900, NameStart
(Init: ($FFFFFFE0, $23FFFFFF, $FF000000, $00000003,
        $FFF99FE0, $03C5FDFF, $B0000000, $00030003)),
// #0A - $0A00, NameStart
(Init: ($FFF987E0, $036DFDFF, $5E000000, $001C0000,
        $FFFBAFE0, $23EDFDFF, $00000000, $00000001)),
// #0B - $0B00, NameStart
(Init: ($FFF99FE0, $23CDFDFF, $B0000000, $00000003,
        $D63DC7E0, $03BFC718, $00000000, $00000000)),
// #0C - $0C00, NameStart
(Init: ($FFFDDFE0, $03EFFDFF, $00000000, $00000003,
        $FFFDDFE0, $03EFFDFF, $40000000, $00000003)),
// #0D - $0D00, NameStart
(Init: ($FFFDDFE0, $03FFFDFF, $00000000, $00000003,
        $00000000, $00000000, $00000000, $00000000)),
// #0E - $0E00, NameStart
(Init: ($FFFFFFFE, $000D7FFF, $0000003F, $00000000,
        $FEF02596, $200D6CAE, $0000001F, $00000000)),
// #0F - $0F00, NameStart
(Init: ($00000000, $00000000, $FFFFFEFF, $000003FF,
        $00000000, $00000000, $00000000, $00000000)),
// #10 - $1000, both Name and NameStart
(Init: ($00000000, $00000000, $00000000, $00000000,
        $00000000, $FFFFFFFF, $FFFF003F, $007FFFFF)),
// #11 - $1100, both Name and NameStart
(Init: ($0007DAED, $50000000, $82315001, $002C62AB,
        $40000000, $F580C900, $00000007, $02010800)),
// #12 - $1E00, both Name and NameStart
(Init: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
        $0FFFFFFF, $FFFFFFFF, $FFFFFFFF, $03FFFFFF)),
// #13 - $1F00, both Name and NameStart
(Init: ($3F3FFFFF, $FFFFFFFF, $AAFF3F3F, $3FFFFFFF,
        $FFFFFFFF, $5FDFFFFF, $0FCF1FDC, $1FDC1FFF)),
// #14 - $2100, NameStart
(Init: ($00000000, $00004C40, $00000000, $00000000,
        $00000007, $00000000, $00000000, $00000000)),
// #15 - $3000, NameStart
(Init: ($00000080, $000003FE, $FFFFFFFE, $FFFFFFFF,
        $001FFFFF, $FFFFFFFE, $FFFFFFFF, $07FFFFFF)),
// #16 - $3100, NameStart
(Init: ($FFFFFFE0, $00001FFF, $00000000, $00000000,
        $00000000, $00000000, $00000000, $00000000)),
// #17 - $9F00, NameStart
(Init: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
        $FFFFFFFF, $0000003F, $00000000, $00000000)),
// #18 - $D700, NameStart
(Init: ($FFFFFFFF, $FFFFFFFF, $FFFFFFFF, $FFFFFFFF,
        $FFFFFFFF, $0000000F, $00000000, $00000000)),

// #19 - $0000, Names
(Init: ($00000000, $07FF6000, $87FFFFFE, $07FFFFFE,
        $00000000, $00800000, $FF7FFFFF, $FF7FFFFF)),
// #1A - $0200, Names
(Init: ($00FFFFFF, $00000000, $FFFF0000, $FFFFFFFF,
        $FFFFFFFF, $F80001FF, $00030003, $00000000)),
// #1B - $0300, Names
(Init: ($FFFFFFFF, $FFFFFFFF, $0000003F, $00000003,
        $FFFFD7C0, $FFFFFFFB, $547F7FFF, $000FFFFD)),
// #1C $0400 - Names
(Init: ($FFFFDFFE, $FFFFFFFF, $DFFEFFFF, $FFFFFFFF,
        $FFFF007B, $FFFFFFFF, $FFFF199F, $033FCFFF)),
// #1D $0500 - Names
(Init: ($00000000, $FFFE0000, $027FFFFF, $FFFFFFFE,
        $FFFE007F, $BBFFFFFB, $FFFF0016, $000707FF)),
// #1E $0600 - Names
(Init: ($00000000, $07FFFFFE, $0007FFFF, $FFFF03FF,
        $FFFFFFFF, $7CFFFFFF, $FFEF7FFF, $03FF3DFF)),
// #1F $0900 - Names
(Init: ($FFFFFFEE, $F3FFFFFF, $FF1E3FFF, $0000FFCF,
        $FFF99FEE, $D3C5FDFF, $B080399F, $0003FFCF)),
// #20 $0A00 - Names
(Init: ($FFF987E4, $D36DFDFF, $5E003987, $001FFFC0,
        $FFFBAFEE, $F3EDFDFF, $00003BBF, $0000FFC1)),
// #21 $0B00 - Names
(Init: ($FFF99FEE, $F3CDFDFF, $B0C0398F, $0000FFC3,
        $D63DC7EC, $C3BFC718, $00803DC7, $0000FF80)),
// #22 $0C00 - Names
(Init: ($FFFDDFEE, $C3EFFDFF, $00603DDF, $0000FFC3,
        $FFFDDFEC, $C3EFFDFF, $40603DDF, $0000FFC3)),
// #23 $0D00 - Names
(Init: ($FFFDDFEC, $C3FFFDFF, $00803DCF, $0000FFC3,
        $00000000, $00000000, $00000000, $00000000)),
// #24 $0E00 - Names
(Init: ($FFFFFFFE, $07FF7FFF, $03FF7FFF, $00000000,
        $FEF02596, $3BFF6CAE, $03FF3F5F, $00000000)),
// #25 $0F00 - Names
(Init: ($03000000, $C2A003FF, $FFFFFEFF, $FFFE03FF,
        $FEBF0FDF, $02FE3FFF, $00000000, $00000000)),
// #26 $2000 - Names
(Init: ($00000000, $00000000, $00000000, $00000000,
        $00000000, $00000000, $1FFF0000, $00000002)),
// #27 $3000 - Names
(Init: ($000000A0, $003EFFFE, $FFFFFFFE, $FFFFFFFF,
        $661FFFFF, $FFFFFFFE, $FFFFFFFF, $77FFFFFF))
);


  NameStartPages: array[0..255] of Byte = (
$02, $03, $04, $05, $06, $07, $08, $00,
$00, $09, $0A, $0B, $0C, $0D, $0E, $0F,
$10, $11, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $12, $13,
$00, $14, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$15, $16, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $17,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $18,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00);

  namePages: array[0..255] of Byte = (
$19, $03, $1A, $1B, $1C, $1D, $1E, $00,
$00, $1F, $20, $21, $22, $23, $24, $25,
$10, $11, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $12, $13,
$26, $14, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$27, $16, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $17,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $01,
$01, $01, $01, $01, $01, $01, $01, $18,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00,
$00, $00, $00, $00, $00, $00, $00, $00);

