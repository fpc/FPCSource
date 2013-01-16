{
    Free Pascal port of the Hermes C library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}


{  Placeholder converter. This is for things like 8 bit converters that have
   one main loop and NO scanline conversion function as opposed to most others
   which use the generic loop.
   There needs to be a function assigned to the scanline loop however,
   otherwise the converter will fail a test in Convert.c. This is the easiest
   way. do NOT use NotApplicable as NotYetImplemented ! }
procedure NotApplicable(source, dest: PUint8; count, inc_source: DWord); cdecl;
begin
  if source <> nil Then;
  if dest <> nil Then;
  if count <> 0 Then;
  if inc_source <> 0 Then;
end;

{  Factory converter array, holds ALL converters available from HERMES. Note
   that converters have to be assigned IN ORDER of priority for processors
   that can run different converters.
   Thus, for an Intel MMX Pentium, the order would be:

      - MMX converters
      - X86 converters
      - Pascal converters

   if someone wrote a P2 or P3 converter, if would be added even further up. }

const
  Factory_NumConverters = 45
  {$IFDEF I386_ASSEMBLER}+27{$ENDIF I386_ASSEMBLER}
  {$IFDEF X86_64_ASSEMBLER}+1{$ENDIF X86_64_ASSEMBLER}
  ;

  Factory_Converters: array [0..Factory_NumConverters - 1] of THermesFactoryStruct =
(
{$IFDEF X86_64_ASSEMBLER}
  { ------ From 8 bit INDEXED ------- }
  (s_bits:8;s_idx:True;s_r:0;s_g:0;s_b:0;s_a:0;
   d_bits:32;d_idx:False;d_r:0;d_g:0;d_b:0;d_a:0;
   loopnormal:@ConvertX86_64_index8_32;loopstretch:nil;
   normal:@NotApplicable;stretch:nil;
   dither:nil;ditherstretch:nil;processor:PROC_X86_64),
{$ENDIF X86_64_ASSEMBLER}
{$IFDEF I386_ASSEMBLER}
  { ------ From 32 RGB 888 - MMX PENTIUM II ---- }
  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertMMX;loopstretch:nil;normal:@ConvertMMXpII32_24RGB888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_MMX_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$f800;d_g:$7e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertMMX;loopstretch:nil;normal:@ConvertMMXpII32_16RGB565;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_MMX_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$7e0;d_b:$f800;d_a:0;
   loopnormal:@ConvertMMX;loopstretch:nil;normal:@ConvertMMXpII32_16BGR565;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_MMX_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$7c00;d_g:$3e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertMMX;loopstretch:nil;normal:@ConvertMMXpII32_16RGB555;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_MMX_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$3e0;d_b:$7c00;d_a:0;
   loopnormal:@ConvertMMX;loopstretch:nil;normal:@ConvertMMXpII32_16BGR555;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_MMX_PENTIUM),

  { ------ From 32 RGB 888 - MMX PENTIUM ------- }
  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$7c00;d_g:$3e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertMMX;loopstretch:nil;normal:@ConvertMMXpII32_16RGB555;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_MMX_PENTIUM),

  { ------ From 32 RGB 888 - X86 PENTIUM ------- }
  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_32BGR888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff000000;d_g:$ff0000;d_b:$ff00;d_a:$ff;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_32RGBA888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff00;d_g:$ff0000;d_b:$ff000000;d_a:$ff;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_32BGRA888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_24RGB888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_24BGR888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$f800;d_g:$7e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertX86;loopstretch:@ConvertX86Stretch;
   normal:@ConvertX86p32_16RGB565;stretch:@ConvertX86p32_16RGB565_S;
   dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$7e0;d_b:$f800;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_16BGR565;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$7c00;d_g:$3e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_16RGB555;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$3e0;d_b:$7c00;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_16BGR555;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:8;d_idx:False;d_r:$e0;d_g:$1c;d_b:$3;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p32_8RGB332;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  { ------ From 16 RGB 565 - X86 PENTIUM ------- }
  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_32RGB888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_32BGR888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff000000;d_g:$ff0000;d_b:$ff00;d_a:$ff;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_32RGBA888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff00;d_g:$ff0000;d_b:$ff000000;d_a:$ff;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_32BGRA888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_24RGB888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_24BGR888;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$7e0;d_b:$f800;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_16BGR565;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:16;d_idx:False;d_r:$7c00;d_g:$3e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_16RGB555;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$3e0;d_b:$7c00;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_16BGR555;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:8;d_idx:False;d_r:$e0;d_g:$1c;d_b:$3;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86p16_8RGB332;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),

  (s_bits:8;s_idx:True;s_r:0;s_g:0;s_b:0;s_a:0;
   d_bits:16;d_idx:False;d_r:0;d_g:0;d_b:0;d_a:0;
   loopnormal:@ConvertX86;loopstretch:nil;normal:@ConvertX86pI8_16;
   stretch:nil;dither:nil;ditherstretch:nil;processor:PROC_X86_PENTIUM),
{$ENDIF I386_ASSEMBLER}

  { ------ From 32 RGBA 8888 ---- }
  {
  (s_bits:32;s_idx:False;s_r:$ff000000;s_g:$ff0000;s_b:$ff00;s_a:$ff;
   d_bits:32;d_idx:False;d_r:$ff000000;d_g:$ff0000;d_b:$ff00;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_32bgr888;stretch:@ConvertP_32rgb888_32bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),
  }

  { ------ From 32 RGB 888 ------- }

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_32bgr888;stretch:@ConvertP_32rgb888_32bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff000000;d_g:$ff0000;d_b:$ff00;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_32rgba888;stretch:@ConvertP_32rgb888_32rgba888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff00;d_g:$ff0000;d_b:$ff000000;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_32bgra888;stretch:@ConvertP_32rgb888_32bgra888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_24rgb888;stretch:@ConvertP_32rgb888_24rgb888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_24bgr888;stretch:@ConvertP_32rgb888_24bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$f800;d_g:$7e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_16rgb565;stretch:@ConvertP_32rgb888_16rgb565_S;
   dither:@ConvertP_32rgb888_16rgb565_dither;ditherstretch:nil;
   processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$7e0;d_b:$f800;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_16bgr565;stretch:@ConvertP_32rgb888_16bgr565_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$7c00;d_g:$3e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_16rgb555;stretch:@ConvertP_32rgb888_16rgb555_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$3e0;d_b:$7c00;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_16bgr555;stretch:@ConvertP_32rgb888_16bgr555_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:8;d_idx:False;d_r:$e0;d_g:$1c;d_b:$3;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_32rgb888_8rgb332;stretch:@ConvertP_32rgb888_8rgb332_S;
   dither:@ConvertP_32rgb888_8rgb332_dither;
   ditherstretch:nil;processor:PROC_GENERIC),

 { ------ From 32 RGB MUHMU ------- }
  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_32rgb888;stretch:@ConvertP_muhmu32_32rgb888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_32bgr888;stretch:@ConvertP_muhmu32_32bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff000000;d_g:$ff0000;d_b:$ff00;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_32rgba888;stretch:@ConvertP_muhmu32_32rgba888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff00;d_g:$ff0000;d_b:$ff000000;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_32bgra888;stretch:@ConvertP_muhmu32_32bgra888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_24rgb888;stretch:@ConvertP_muhmu32_24rgb888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_24bgr888;stretch:@ConvertP_muhmu32_24bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$f800;d_g:$7e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_16rgb565;stretch:@ConvertP_muhmu32_16rgb565_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$7e0;d_b:$f800;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_16bgr565;stretch:@ConvertP_muhmu32_16bgr565_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$7c00;d_g:$3e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_16rgb555;stretch:@ConvertP_muhmu32_16rgb555_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$3e0;d_b:$7c00;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_16bgr555;stretch:@ConvertP_muhmu32_16bgr555_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:32;s_idx:False;s_r:$ff shl 20;s_g:$ff shl 10;s_b:$ff;s_a:0;
   d_bits:8;d_idx:False;d_r:$e0;d_g:$1c;d_b:$3;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_muhmu32_8rgb332;stretch:@ConvertP_muhmu32_8rgb332_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  { ------ From 24 RGB 888 ------- }
  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_32rgb888;stretch:@ConvertP_24rgb888_32rgb888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_32bgr888;stretch:@ConvertP_24rgb888_32bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff000000;d_g:$ff0000;d_b:$ff00;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_32rgba888;stretch:@ConvertP_24rgb888_32rgba888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff00;d_g:$ff0000;d_b:$ff000000;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_32bgra888;stretch:@ConvertP_24rgb888_32bgra888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_24bgr888;stretch:@ConvertP_24rgb888_24bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$f800;d_g:$7e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_16rgb565;stretch:@ConvertP_24rgb888_16rgb565_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$7e0;d_b:$f800;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_16bgr565;stretch:@ConvertP_24rgb888_16bgr565_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$7c00;d_g:$3e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_16rgb555;stretch:@ConvertP_24rgb888_16rgb555_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$3e0;d_b:$7c00;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_16bgr555;stretch:@ConvertP_24rgb888_16bgr555_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:24;s_idx:False;s_r:$ff0000;s_g:$ff00;s_b:$ff;s_a:0;
   d_bits:8;d_idx:False;d_r:$e0;d_g:$1c;d_b:$3;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_24rgb888_8rgb332;stretch:@ConvertP_24rgb888_8rgb332_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  { ------ From 16 RGB 565 ------- }
  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_32rgb888;stretch:@ConvertP_16rgb565_32rgb888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_32bgr888;stretch:@ConvertP_16rgb565_32bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff000000;d_g:$ff0000;d_b:$ff00;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_32rgba888;stretch:@ConvertP_16rgb565_32rgba888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:32;d_idx:False;d_r:$ff00;d_g:$ff0000;d_b:$ff000000;d_a:$ff;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_32bgra888;stretch:@ConvertP_16rgb565_32bgra888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff0000;d_g:$ff00;d_b:$ff;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_24rgb888;stretch:@ConvertP_16rgb565_24rgb888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:24;d_idx:False;d_r:$ff;d_g:$ff00;d_b:$ff0000;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_24bgr888;stretch:@ConvertP_16rgb565_24bgr888_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$7e0;d_b:$f800;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_16bgr565;stretch:@ConvertP_16rgb565_16bgr565_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:16;d_idx:False;d_r:$7c00;d_g:$3e0;d_b:$1f;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_16rgb555;stretch:@ConvertP_16rgb565_16rgb555_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:16;d_idx:False;d_r:$1f;d_g:$3e0;d_b:$7c00;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_16bgr555;stretch:@ConvertP_16rgb565_16bgr555_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:16;s_idx:False;s_r:$f800;s_g:$7e0;s_b:$1f;s_a:0;
   d_bits:8;d_idx:False;d_r:$e0;d_g:$1c;d_b:$3;d_a:0;
   loopnormal:@ConvertP;loopstretch:@ConvertPStretch;
   normal:@ConvertP_16rgb565_8rgb332;stretch:@ConvertP_16rgb565_8rgb332_S;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  { ------ From 8 bit INDEXED ------- }
  (s_bits:8;s_idx:True;s_r:0;s_g:0;s_b:0;s_a:0;
   d_bits:32;d_idx:False;d_r:0;d_g:0;d_b:0;d_a:0;
   loopnormal:@ConvertP_index8_32;loopstretch:@ConvertP_index8_32_S;
   normal:@NotApplicable;stretch:@NotApplicable;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:8;s_idx:True;s_r:0;s_g:0;s_b:0;s_a:0;
   d_bits:24;d_idx:False;d_r:0;d_g:0;d_b:0;d_a:0;
   loopnormal:@ConvertP_index8_24;loopstretch:@ConvertP_index8_24_S;
   normal:@NotApplicable;stretch:@NotApplicable;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:8;s_idx:True;s_r:0;s_g:0;s_b:0;s_a:0;
   d_bits:16;d_idx:False;d_r:0;d_g:0;d_b:0;d_a:0;
   loopnormal:@ConvertP_index8_16;loopstretch:@ConvertP_index8_16_S;
   normal:@NotApplicable;stretch:@NotApplicable;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC),

  (s_bits:8;s_idx:True;s_r:0;s_g:0;s_b:0;s_a:0;
   d_bits:8;d_idx:False;d_r:0;d_g:0;d_b:0;d_a:0;
   loopnormal:@ConvertP_index8_8;loopstretch:@ConvertP_index8_8_S;
   normal:@NotApplicable;stretch:@NotApplicable;
   dither:nil;ditherstretch:nil;processor:PROC_GENERIC)
);
