{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Header conversions (with FpIoctl macro expansion) for FreeBSD 4.2's
   sys/fbio.h sys/consio.h sys/kbdio.h (together these three form
        machine/console.h) and
   machine/mouse.h

   Converted to use in a future FreeBSD API to get the IDE running on
   the physical console with mousesupport.

   As soon as cross unit inlining is ready, all functions should be made
    inline. (so the FpIoctl and the other very small macro's)

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

UNIT Console;

{$packrecords C}

interface

{I tried to keep original types as much as possible, only "int" is converted
to longint because INT is a standard function in TP/FPC}

TYPE
     uchar = char;
     uint  = dword;
     u_int = uint;
     ushort= word;
     short = integer;
     long  = dword;             {?}
     size_t= longint;           {Dunno sure, but it is 32-bit}
     caddr_t= longint;          {idem}
     vm_offset_t=dword;         {idem}

{----------------------------- sys/fbio.h ----------------------------------}

{
 * Copyright (c) 1992, 1993
 *      The Regents of the University of California.  All rights reserved.
 *
 * This code is derived from software developed by the Computer Systems
 * Engineering group at Lawrence Berkeley Laboratory under DARPA
 * contract BG 91-66 and contributed to Berkeley.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. All advertising materials mentioning features or use of this software
 *    must display the following acknowledgement:
 *      This product includes software developed by the University of
 *      California, Berkeley and its contributors.
 * 4. Neither the name of the University nor the names of its contributors
 *    may be used to endorse or promote products derived from this software
 *    without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 *      @(#)fbio.h      8.2 (Berkeley) 10/30/93
 *
 * $FreeBSD: src/sys/sys/fbio.h,v 1.9.2.1 2000/05/05 09:16:16 nyan Exp $
 }

{
 * Frame buffer FpIoctls (from Sprite, trimmed to essentials for X11).
 }

{
 * Frame buffer type codes.
 }

CONST

                FBTYPE_SUN1BW           =0;     { multibus mono }
                FBTYPE_SUN1COLOR        =1;     { multibus color }
                FBTYPE_SUN2BW           =2;     { memory mono }
                FBTYPE_SUN2COLOR        =3;     { color w/rasterop chips }
                FBTYPE_SUN2GP           =4;     { GP1/GP2 }
                FBTYPE_SUN5COLOR        =5;     { RoadRunner accelerator }
                FBTYPE_SUN3COLOR        =6;     { memory color }
                FBTYPE_MEMCOLOR         =7;     { memory 24-bit }
                FBTYPE_SUN4COLOR        =8;     { memory color w/overlay }

                FBTYPE_NOTSUN1          =9;     { reserved for customer }
                FBTYPE_NOTSUN2          =10;    { reserved for customer }
                FBTYPE_NOTSUN3          =11;    { reserved for customer }

                FBTYPE_SUNFAST_COLOR    =12;    { accelerated 8bit }
                FBTYPE_SUNROP_COLOR     =13;    { MEMCOLOR with rop h/w }
                FBTYPE_SUNFB_VIDEO      =14;    { Simple video mixing }
                FBTYPE_RESERVED5        =15;    { reserved, do not use }
                FBTYPE_RESERVED4        =16;    { reserved, do not use }
                FBTYPE_RESERVED3        =17;    { reserved, do not use }
                FBTYPE_RESERVED2        =18;    { reserved, do not use }
                FBTYPE_RESERVED1        =19;    { reserved, do not use }

                FBTYPE_MDA              =20;
                FBTYPE_HERCULES         =21;
                FBTYPE_CGA              =22;
                FBTYPE_EGA              =23;
                FBTYPE_VGA              =24;
                FBTYPE_PC98             =25;
                FBTYPE_TGA              =26;

                FBTYPE_LASTPLUSONE      =27;    { max number of fbs (change as add) }

{
 * Frame buffer descriptor as returned by FBIOGTYPE.
 }

type fbtype = record
                fb_type   : longint;    { as defined above }
                fb_height : longint;    { in pixels }
                fb_width  : longint;    { in pixels }
                fb_depth  : longint;    { bits per pixel }
                fb_cmsize : longint;    { size of color map (entries) }
                fb_size   : longint;    { total size in bytes }
               end;

Function FBIOGTYPE(fd:longint;var param1 : fbtype):boolean;

{
 * General purpose structure for passing info in and out of frame buffers
 * (used for gp1) -- unsupported.
 }
type  fbinfo = record
                fb_physaddr   : longint;        { physical frame buffer address }
                fb_hwwidth    : longint;        { fb board width }
                fb_hwheight   : longint;        { fb board height }
                fb_addrdelta  : longint;        { phys addr diff between boards }
                fb_ropaddr    : ^uchar;         { fb virtual addr }
                fb_unit       : longint;        { minor devnum of fb }
                end;

Function FBIOGINFO(fd:longint;var param1 : fbinfo):boolean;

type
{
 * Color map I/O.
 }
       fbcmap = record
                index   : longint;              { first element (0 origin) }
                count   : longint;              { number of elements }
                red     : ^uchar;               { red color map elements }
                green   : ^uchar;               { green color map elements }
                blue    : ^uchar;               { blue color map elements }
                end;

Function FBIOPUTCMAP(fd:longint;var param1 : fbcmap):boolean;
Function FBIOGETCMAP(fd:longint;var param1 : fbcmap):boolean;

{
 * Set/get attributes.
 }
const
                FB_ATTR_NDEVSPECIFIC    =8;     { no. of device specific values }
                FB_ATTR_NEMUTYPES       =4;     { no. of emulation types }

type  fbsattr = record
                flags:longint;                  { flags; see below }
                emu_type : longint;             { emulation type (-1 if unused) }
                dev_specific : array[0..FB_ATTR_NDEVSPECIFIC-1] of longint;     { catchall }
               end;
const
                FB_ATTR_AUTOINIT        =1;     { emulation auto init flag }
                FB_ATTR_DEVSPECIFIC     =2;     { dev. specific stuff valid flag }

type   fbgattr = record
                real_type : longint;            { real device type }
                owner     : longint;                    { PID of owner, 0 if myself }
                _fbtype   : fbtype;             { fbtype info for real device }
                sattr     : fbsattr;            { see above }
                emu_types : array [0..FB_ATTR_NEMUTYPES-1] OF Longint;  { possible emulations }
                                                { (-1 if unused) }
                end;

{       FBIOSATTR       _IOW('F', 5, struct fbsattr) -- unsupported }

Function FBIOGATTR(fd:longint;var param1 : fbgattr):boolean;

{
 * Video control.
 }

const
                FBVIDEO_OFF             =0;
                FBVIDEO_ON              =1;

Function FBIOSVIDEO(fd:longint;var param1 : longint):boolean;
Function FBIOGVIDEO(fd:longint;var param1 : longint):boolean;

{
 * Hardware cursor control (for, e.g., CG6).  A rather complex and icky
 * interface that smells like VMS, but there it is....
 }
type fbcurpos = record
                x : short;
                y : short;
                end;



     fbcursor = record
                _set     : short;               { flags; see below }
                enable  : short;                { nonzero => cursor on, 0 => cursor off }
                _pos     : fbcurpos;    { position on display }
                hot     : fbcurpos;     { hot-spot within cursor }
                cmap    : fbcmap;       { cursor color map }
                _size   : fbcurpos;     { number of valid bits in image & mask }
                image   : caddr_t;              { cursor image bits }
                mask    : caddr_t;              { cursor mask bits }
               end;

const
                FB_CUR_SETCUR   =$01;   { set on/off (i.e., obey fbcursor.enable) }
                FB_CUR_SETPOS   =$02;   { set position }
                FB_CUR_SETHOT   =$04;   { set hot-spot }
                FB_CUR_SETCMAP  =$08;   { set cursor color map }
                FB_CUR_SETSHAPE =$10;   { set size & bits }
                FB_CUR_SETALL   =(FB_CUR_SETCUR OR FB_CUR_SETPOS OR FB_CUR_SETHOT OR
                         FB_CUR_SETCMAP OR FB_CUR_SETSHAPE);

{ controls for cursor attributes & shape (including position) }
Function FBIOSCURSOR(fd:longint;var param1 : fbcursor):boolean;
Function FBIOGCURSOR(fd:longint;var param1 : fbcursor):boolean;

{ controls for cursor position only }
Function FBIOSCURPOS(fd:longint;var param1 : fbcurpos):boolean;
Function FBIOGCURPOS(fd:longint;var param1 : fbcurpos):boolean;

{ get maximum cursor size }
Function FBIOGCURMAX(fd:longint;var param1 : fbcurpos):boolean;

{ The new style frame buffer FpIoctls. }

CONST
         V_INFO_COLOR   =(1 SHL 0);
         V_INFO_GRAPHICS        =(1 SHL 1);
         V_INFO_LINEAR  =(1 SHL 2);
         V_INFO_VESA    =(1 SHL 3);
         V_INFO_MM_OTHER  =(-1);
         V_INFO_MM_TEXT  =0;
         V_INFO_MM_PLANAR =1;
         V_INFO_MM_PACKED =2;
         V_INFO_MM_DIRECT =3;
         V_INFO_MM_CGA   =100;
         V_INFO_MM_HGC   =101;
         V_INFO_MM_VGAX  =102;

TYPE
{ video mode information block }
  video_info = record
                        vi_mode         : longint;      { mode number, see below }
                        vi_flags        : longint;
                        vi_width        : longint;
                        vi_height       : longint;
                        vi_cwidth       : longint;
                        vi_cheight      : longint;
                        vi_depth        : longint;
                        vi_planes       : longint;
                        vi_window       : uint; { physical address }
                        vi_window_size  : size_t;
                        vi_window_gran  : size_t;
                        vi_buffer       : uint; { physical address }
                        vi_buffer_size  : size_t;
                        vi_mem_model    : longint;
    { for MM_PACKED and MM_DIRECT only }
                        vi_pixel_size   : longint;      { in bytes }
    { for MM_DIRECT only }
                        vi_pixel_fields : array[0..3] of longint;       { RGB and reserved fields }
                        vi_pixel_fsizes : array[0..3] of longint;
    { reserved }
                        vi_reserved     : array[0..63] of uchar;
                        end;

        video_info_t = video_info;
const
         KD_OTHER       =0;             { unknown }
         KD_MONO                =1;             { monochrome adapter }
         KD_HERCULES    =2;             { hercules adapter }
         KD_CGA         =3;             { color graphics adapter }
         KD_EGA         =4;             { enhanced graphics adapter }
         KD_VGA         =5;             { video graphics adapter }
         KD_PC98                =6;             { PC-98 display }
         KD_TGA         =7;             { TGA }
         V_ADP_COLOR    =(1 SHL 0);
         V_ADP_MODECHANGE=(1 SHL 1);
         V_ADP_STATESAVE        =(1 SHL 2);
         V_ADP_STATELOAD        =(1 SHL 3);
         V_ADP_FONT     =(1 SHL 4);
         V_ADP_PALETTE  =(1 SHL 5);
         V_ADP_BORDER   =(1 SHL 6);
         V_ADP_VESA     =(1 SHL 7);
         V_ADP_PROBED   =(1 SHL 16);
         V_ADP_INITIALIZED=(1 SHL 17);
         V_ADP_REGISTERED =(1 SHL 18);

{ adapter infromation block }
type  video_adapter  = record
                        va_index                : longint;
                        va_type                 : longint;
                        va_name                 : pchar;
                        va_unit                 : longint;
                        va_minor                : longint;
                        va_flags                : longint;
                        va_io_base              : longint;
                        va_io_size              : longint;
                        va_crtc_addr            : longint;
                        va_mem_base             : longint;
                        va_mem_size             : longint;
                        va_window               : vm_offset_t;  { virtual address }
                        va_window_size          : size_t;
                        va_window_gran          : size_t;
                        va_window_orig          : uint;
                        va_buffer               : vm_offset_t;  { virtual address }
                        va_buffer_size          : size_t;
                        va_initial_mode         : longint;
                        va_initial_bios_mode    : longint;
                        va_mode                 : longint;
                        va_info                 : video_info;
                        va_line_width           : longint;
                        va_disp_start : record
                                          x : longint;
                                          y : longint;
                                         end;
                        va_token      : pointer;
                        end;

        video_adapter_t = video_adapter;

       video_adapter_info = record
                        va_index                : longint;
                        va_type                 : longint;
                        va_name                 : array[0..15] of char;
                        va_unit                 : longint;
                        va_flags                : longint;
                        va_io_base              : longint;
                        va_io_size              : longint;
                        va_crtc_addr            : longint;
                        va_mem_base             : longint;
                        va_mem_size             : longint;
                        va_window               : uint;         { virtual address }
                        va_window_size          : size_t;
                        va_window_gran          : size_t;
                        va_unused0              : uint;
                        va_buffer_size          : size_t;
                        va_initial_mode         : longint;
                        va_initial_bios_mode    : longint;
                        va_mode                 : longint;
                        va_line_width           : longint;
                        va_disp_start : record
                                          x : longint;
                                          y : longint;
                                         end;
                        va_window_orig :  uint;
    { reserved }
                        va_reserved : array[0..63] OF uchar;
                        end;
        video_adapter_info_t = video_adapter_info;

CONST
{ some useful video adapter index }
         V_ADP_PRIMARY  =0;
         V_ADP_SECONDARY        =1;

{ video mode numbers }

         M_B40x25       =0;     { black & white 40 columns }
         M_C40x25       =1;     { color 40 columns }
         M_B80x25       =2;     { black & white 80 columns }
         M_C80x25       =3;     { color 80 columns }
         M_BG320                =4;     { black & white graphics 320x200 }
         M_CG320                =5;     { color graphics 320x200 }
         M_BG640                =6;     { black & white graphics 640x200 hi-res }
         M_EGAMONO80x25  =7;       { ega-mono 80x25 }
         M_CG320_D      =13;    { ega mode D }
         M_CG640_E      =14;    { ega mode E }
         M_EGAMONOAPA   =15;    { ega mode F }
         M_CG640x350    =16;    { ega mode 10 }
         M_ENHMONOAPA2  =17;    { ega mode F with extended memory }
         M_ENH_CG640    =18;    { ega mode 10* }
         M_ENH_B40x25    =19;      { ega enhanced black & white 40 columns }
         M_ENH_C40x25    =20;      { ega enhanced color 40 columns }
         M_ENH_B80x25    =21;      { ega enhanced black & white 80 columns }
         M_ENH_C80x25    =22;      { ega enhanced color 80 columns }
         M_VGA_C40x25   =23;    { vga 8x16 font on color }
         M_VGA_C80x25   =24;    { vga 8x16 font on color }
         M_VGA_M80x25   =25;    { vga 8x16 font on mono }

         M_VGA11                =26;    { vga 640x480 2 colors }
         M_BG640x480    =26;
         M_VGA12                =27;    { vga 640x480 16 colors }
         M_CG640x480    =27;
         M_VGA13                =28;    { vga 320x200 256 colors }
         M_VGA_CG320    =28;

         M_VGA_C80x50   =30;    { vga 8x8 font on color }
         M_VGA_M80x50   =31;    { vga 8x8 font on color }
         M_VGA_C80x30   =32;    { vga 8x16 font on color }
         M_VGA_M80x30   =33;    { vga 8x16 font on color }
         M_VGA_C80x60   =34;    { vga 8x8 font on color }
         M_VGA_M80x60   =35;    { vga 8x8 font on color }
         M_VGA_CG640    =36;    { vga 640x400 256 color }
         M_VGA_MODEX    =37;    { vga 320x240 256 color }

         M_VGA_C90x25   =40;    { vga 8x16 font on color }
         M_VGA_M90x25   =41;    { vga 8x16 font on mono }
         M_VGA_C90x30   =42;    { vga 8x16 font on color }
         M_VGA_M90x30   =43;    { vga 8x16 font on mono }
         M_VGA_C90x43   =44;    { vga 8x8 font on color }
         M_VGA_M90x43   =45;    { vga 8x8 font on mono }
         M_VGA_C90x50   =46;    { vga 8x8 font on color }
         M_VGA_M90x50   =47;    { vga 8x8 font on mono }
         M_VGA_C90x60   =48;    { vga 8x8 font on color }
         M_VGA_M90x60   =49;    { vga 8x8 font on mono }

         M_ENH_B80x43   =$70;   { ega black & white 80x43 }
         M_ENH_C80x43   =$71;   { ega color 80x43 }

         M_PC98_80x25           =98;    { PC98 text 80x25 }
         M_PC98_80x30           =99;    { PC98 text 80x30 }
         M_PC98_EGC640x400      =100;   { PC98 graphic 640x400 16 colors }
         M_PC98_PEGC640x400     =101;   { PC98 graphic 640x400 256 colors }
         M_PC98_PEGC640x480     =102;   { PC98 graphic 640x480 256 colors }

         M_HGC_P0       =$e0;   { hercules graphics - page 0 @ B0000 }
         M_HGC_P1       =$e1;   { hercules graphics - page 1 @ B8000 }
         M_MCA_MODE     =$ff;   { monochrome adapter mode }

         M_TEXT_80x25   =200;   { generic text modes }
         M_TEXT_80x30   =201;
         M_TEXT_80x43   =202;
         M_TEXT_80x50   =203;
         M_TEXT_80x60   =204;
         M_TEXT_132x25  =205;
         M_TEXT_132x30  =206;
         M_TEXT_132x43  =207;
         M_TEXT_132x50  =208;
         M_TEXT_132x60  =209;

         M_VESA_BASE            =$100;  { VESA mode number base }
         M_VESA_CG640x400       =$100;  { 640x400, 256 color }
         M_VESA_CG640x480       =$101;  { 640x480, 256 color }
         M_VESA_800x600         =$102;  { 800x600, 16 color }
         M_VESA_CG800x600       =$103;  { 800x600, 256 color }
         M_VESA_1024x768                =$104;  { 1024x768, 16 color }
         M_VESA_CG1024x768      =$105;  { 1024x768, 256 color }
         M_VESA_1280x1024       =$106;  { 1280x1024, 16 color }
         M_VESA_CG1280x1024     =$107;  { 1280x1024, 256 color }
         M_VESA_C80x60          =$108;  { 8x8 font }
         M_VESA_C132x25         =$109;  { 8x16 font }
         M_VESA_C132x43         =$10a;  { 8x14 font }
         M_VESA_C132x50         =$10b;  { 8x8 font }
         M_VESA_C132x60         =$10c;  { 8x8 font }
         M_VESA_32K_320         =$10d;  { 320x200, 5:5:5 }
         M_VESA_64K_320         =$10e;  { 320x200, 5:6:5 }
         M_VESA_FULL_320                =$10f;  { 320x200, 8:8:8 }
         M_VESA_32K_640         =$110;  { 640x480, 5:5:5 }
         M_VESA_64K_640         =$111;  { 640x480, 5:6:5 }
         M_VESA_FULL_640                =$112;  { 640x480, 8:8:8 }
         M_VESA_32K_800         =$113;  { 800x600, 5:5:5 }
         M_VESA_64K_800         =$114;  { 800x600, 5:6:5 }
         M_VESA_FULL_800                =$115;  { 800x600, 8:8:8 }
         M_VESA_32K_1024                =$116;  { 1024x768, 5:5:5 }
         M_VESA_64K_1024                =$117;  { 1024x768, 5:6:5 }
         M_VESA_FULL_1024       =$118;  { 1024x768, 8:8:8 }
         M_VESA_32K_1280                =$119;  { 1280x1024, 5:5:5 }
         M_VESA_64K_1280                =$11a;  { 1280x1024, 5:6:5 }
         M_VESA_FULL_1280       =$11b;  { 1280x1024, 8:8:8 }
         M_VESA_MODE_MAX                =$1ff;

type
        video_display_start = record
                        x  :longint;
                        y : longint;
                        end;

        video_display_start_t= video_display_start;

        video_color_palette = record
                        index : longint;                { first element (zero-based) }
                        count : longint;                { number of elements }
                        red   : ^uchar;         { red }
                        green : ^uchar;         { green }
                        blue  : ^uchar;         { blue }
                        transparent : ^uchar;   { may be NULL }
                        end;

        video_color_palette_t = video_color_palette;

{ adapter info. }
Function FBIO_ADAPTER(fd:longint;var param1 : longint):boolean;
Function FBIO_ADPTYPE(fd:longint;var param1 : longint):boolean;
Function FBIO_ADPINFO(fd:longint;var param1 : video_adapter_info):boolean;

{ video mode control }
Function FBIO_MODEINFO(fd:longint;var param1 : video_info):boolean;
Function FBIO_FINDMODE(fd:longint;var param1 : video_info):boolean;
Function FBIO_GETMODE(fd:longint;var param1 : longint):boolean;
Function FBIO_SETMODE(fd:longint;var param1 : longint):boolean;

{ get/set frame buffer window origin }
Function FBIO_GETWINORG(fd:longint;var param1 : u_int):boolean;
Function FBIO_SETWINORG(fd:longint;var param1 : u_int):boolean;

{ get/set display start address }
Function FBIO_GETDISPSTART(fd:longint;var param1 : video_display_start_t):boolean;
Function FBIO_SETDISPSTART(fd:longint;var param1 : video_display_start_t):boolean;

{ get/set scan line width }
Function FBIO_GETLINEWIDTH(fd:longint;var param1 : u_int):boolean;
Function FBIO_SETLINEWIDTH(fd:longint;var param1 : u_int):boolean;

{ color palette control }
Function FBIO_GETPALETTE(fd:longint;var param1 : video_color_palette_t):boolean;
Function FBIO_SETPALETTE(fd:longint;var param1 : video_color_palette_t):boolean;

{----------------------------- sys/consio.h ----------------------------------}

{ version packaged with FreeBSD 4.2-RELEASE
Translation to FreePascal by Marco van de Voort. (2000-2001), original
copyright follows:

 * Copyright (c) 1991-1996 Søren Schmidt
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer
 *    in this position and unchanged.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $FreeBSD: src/sys/sys/consio.h,v 1.5.2.1 2000/05/05 09:16:15 nyan Exp $

}

{$define definconsole}

{
 * Console FpIoctl commands.  Some commands are named as KDXXXX, GIO_XXX, and
 * PIO_XXX, rather than CONS_XXX, for historical and compatibility reasons.
 * Some other CONS_XXX commands are works as wrapper around frame buffer
 * FpIoctl commands FBIO_XXX.  Do not try to change all these commands,
 * otherwise we shall have compatibility problems.
 }

const

{ get/set video mode }
        KD_TEXT     =0;         { set text mode restore fonts  }
        KD_TEXT0    =0;         { ditto            }
        KD_GRAPHICS =1;         { set graphics mode        }
        KD_TEXT1    =2;         { set text mode !restore fonts }
        KD_PIXEL    =3;         { set pixel mode       }

Function KDGETMODE(fd:longint;var param1 : longint):boolean;
Function KDSETMODE(fd:longint;param1 : longint):boolean;

{ set border color }
Function KDSBORDER(fd:longint;param1 : longint):boolean;

{ set up raster(pixel) text mode }
type
        scr_size        = record
                          _scrsize : array[0..2] of longint;
                          end;
        scr_size_t      = scr_size;

Function KDRASTER(fd:longint;var param1 : scr_size_t):boolean;

type

{ get/set screen char map }

        scrmap   = record
                    _scrmap : array[0..255] of char;
                    end;
        scrmap_t =  scrmap;

Function GIO_SCRNMAP(fd:longint;var param1 : scrmap_t):boolean;
Function PIO_SCRNMAP(fd:longint;var param1 : scrmap_t):boolean;

{ get the current text attribute }
Function GIO_ATTR(fd:longint;var param1 : longint):boolean;

{ get the current text color }
Function GIO_COLOR(fd:longint;var param1 : longint):boolean;

{ get the adapter type (equivalent to FBIO_ADPTYPE) }
Function CONS_CURRENT(fd:longint;var param1 : longint):boolean;

{ get the current video mode (equivalent to FBIO_GETMODE) }
Function CONS_GET(fd:longint;var param1 : longint):boolean;

{ not supported? }
Function CONS_IO(fd:longint):boolean;

{ set blank time interval }
Function CONS_BLANKTIME(fd:longint;var param1 : longint):boolean;

{ set/get the screen saver (these FpIoctls are current noop) }
CONST        maxsaver=16;

type ssaver =record
                name : array[0..maxsaver-1] of char;
                num  : Longint;
                time : Long;
                end;
     ssaver_t =   ssaver;

Function CONS_SSAVER(fd:longint;var param1 : ssaver_t):boolean;
Function CONS_GSAVER(fd:longint;var param1 : ssaver_t):boolean;

{ set the text cursor shape }

CONST
        CONS_BLINK_CURSOR  = (1  shl  0);
        CONS_CHAR_CURSOR   = (1  shl  1);

Function CONS_CURSORTYPE(fd:longint;var param1 : longint):boolean;

{ set the bell type to audible or visual }
CONST
        CONS_VISUAL_BELL   =(1  shl  0);
        CONS_QUIET_BELL    =(1  shl  1);

Function CONS_BELLTYPE(fd:longint;var param1 : longint):boolean;

{ set the history (scroll back) buffer size (in lines) }
Function CONS_HISTORY(fd:longint;var param1 : longint):boolean;

{ mouse cursor FpIoctl }
type
        mouse_data      = record
                            x       : longint;
                            y       : Longint;
                            z       : longint;
                            buttons : longint;
                           end;

        mouse_data_t    = mouse_data;

        mouse_mode      = record
                            mode    : longint;
                            signal  : longint;
                           end;

        mouse_mode_t    = mouse_mode;

        mouse_event     = record
                            id    : Longint;   { one based }
                            value : longint;
                           end;

        mouse_event_t   = mouse_event;

CONST
        MOUSE_SHOW           =$01;
        MOUSE_HIDE           =$02;
        MOUSE_MOVEABS        =$03;
        MOUSE_MOVEREL        =$04;
        MOUSE_GETINFO        =$05;
        _MOUSE_MODE           =$06;
        MOUSE_ACTION         =$07;
        MOUSE_MOTION_EVENT   =$08;
        MOUSE_BUTTON_EVENT   =$09;
        MOUSE_MOUSECHAR      =$0a;

TYPE
        mouse_info = record
                       operation : longint;
                       u : record
                            case integer of
                               0:  (data : mouse_data_t);
                               1:  (mode : mouse_mode_t);
                               2:  (event: mouse_event_t);
                               3:  (mouse_char : longint);
                               end;
                      end;
        mouse_info_t = mouse_info;

Function CONS_MOUSECTL(fd:longint;var param1 : mouse_info_t):boolean;

{ see if the vty has been idle }
Function CONS_IDLE(fd:longint;var param1 : longint):boolean;

{ set the screen saver mode }
CONST
        CONS_LKM_SAVER = 0;
        CONS_USR_SAVER =  1;

Function CONS_SAVERMODE(fd:longint;var param1 : longint):boolean;

{ start the screen saver }
Function CONS_SAVERSTART(fd:longint;var param1 : longint):boolean;

TYPE
{ set/get font data }
        fnt8        = record
                        fnt8x8 : array[0..8*256-1] of char;
                      end;

        fnt8_t      = fnt8;

        fnt14       = record
                        fnt8x14: array[0..14*256-1] of char;
                      end;

        fnt14_t     = fnt14;

        fnt16       = record
                        fnt8x16: array[0..16*256-1] of char;
                       end;
        fnt16_t     = fnt16;

Function PIO_FONT8x8(fd:longint;var param1 : fnt8_t):boolean;
Function GIO_FONT8x8(fd:longint;var param1 : fnt8_t):boolean;
Function PIO_FONT8x14(fd:longint;var param1 : fnt14_t):boolean;
Function GIO_FONT8x14(fd:longint;var param1 : fnt14_t):boolean;
Function PIO_FONT8x16(fd:longint;var param1 : fnt16_t):boolean;
Function GIO_FONT8x16(fd:longint;var param1 : fnt16_t):boolean;


{ get video mode information }
type        colors = record
                       fore : char;
                       back : char;
                      end;

            vid_info = record
                        _size         : short;
                        m_num         : short;
                        mv_row,
                        mv_col        : ushort;
                        mv_rsz,
                        mv_csz        : ushort;
                        mv_norm,
                        mv_rev,
                        mv_grfc       : colors;
                        mv_ovscan     : uchar;
                        mk_keylock    : uchar;
                       end;
        vid_info_t   = vid_info;

Function CONS_GETINFO(fd:longint;var param1 : vid_info_t):boolean;

{ get version }

Function CONS_GETVERS(fd:longint;var param1 : longint):boolean;

{ get the video adapter index (equivalent to FBIO_ADAPTER) }
Function CONS_CURRENTADP(fd:longint;var param1 : longint):boolean;

{ get the video adapter information (equivalent to FBIO_ADPINFO) }
Function CONS_ADPINFO(fd:longint;var param1 : video_adapter_info_t):boolean;

{ get the video mode information (equivalent to FBIO_MODEINFO) }
Function CONS_MODEINFO(fd:longint;var param1 : video_info_t):boolean;

{ find a video mode (equivalent to FBIO_FINDMODE) }
Function CONS_FINDMODE(fd:longint;var param1 : video_info_t):boolean;

{ set the frame buffer window origin (equivalent to FBIO_SETWINORG) }
Function CONS_SETWINORG(fd:longint;param1 : longint):boolean;

{ use the specified keyboard }
Function CONS_SETKBD(fd:longint;param1 : longint):boolean;

{ release the current keyboard }
Function CONS_RELKBD(fd:longint):boolean;

{ get/set the current terminal emulator info. }
CONST
        TI_NAME_LEN   = 32;
        TI_DESC_LEN   = 64;

TYPE
        term_info     = record
                          ti_index    : Longint;
                          ti_flags    : longint;
                          ti_name     : array[0..TI_NAME_LEN-1] of uchar;
                          ti_desc     : array[0..TI_DESC_LEN-1] of uchar;
                         end;
        term_info_t   = term_info;

Function CONS_GETTERM(fd:longint;var param1 : term_info_t):boolean;
Function CONS_SETTERM(fd:longint;var param1 : term_info_t):boolean;

{$ifdef PC98}
Function ADJUST_CLOCK(fd:longint):boolean;
{$endif}

{
* Vty switching FpIoctl commands.
}

{ get the next available vty }
Function VT_OPENQRY(fd:longint;var param1 : longint):boolean;

{ set/get vty switching mode }
const
        VT_AUTO     =0;       { switching is automatic   }
        VT_PROCESS  =1;       { switching controlled by prog }
        VT_KERNEL   =255;     { switching controlled in kernel }

TYPE
        vt_mode    = record
                       mode     :   Char;
                       waitv    :   char;    { not implemented yet  SOS }
                       relsig   :   short;
                       acqsig   :   short;
                       frsig    :   short;   { not implemented yet  SOS }
                      end;

        vtmode_t  = vt_mode;


Function VT_SETMODE(fd:longint;var param1 : vtmode_t):boolean;
Function VT_GETMODE(fd:longint;var param1 : vtmode_t):boolean;


{ acknowledge release or acquisition of a vty }
const
        VT_FALSE      = 0;
        VT_TRUE       = 1;
        VT_ACKACQ     = 2;

Function VT_RELDISP(fd:longint;param1 : longint):boolean;

{ activate the specified vty }
Function VT_ACTIVATE(fd:longint;param1 : longint):boolean;

{ wait until the specified vty is activate }
Function VT_WAITACTIVE(fd:longint;param1 : longint):boolean;

{ get the currently active vty }
Function VT_GETACTIVE(fd:longint;var param1 : longint):boolean;

{ get the index of the vty }
Function VT_GETINDEX(fd:longint;var param1 : longint):boolean;

{
* Video mode switching FpIoctl.  See sys/fbio.h for mode numbers.
}

Function SW_B40x25(fd:longint):boolean;
Function SW_C40x25(fd:longint):boolean;
Function SW_B80x25(fd:longint):boolean;
Function SW_C80x25(fd:longint):boolean;
Function SW_BG320(fd:longint):boolean;
Function SW_CG320(fd:longint):boolean;
Function SW_BG640(fd:longint):boolean;
Function SW_EGAMONO80x25(fd:longint):boolean;
Function SW_CG320_D(fd:longint):boolean;
Function SW_CG640_E(fd:longint):boolean;
Function SW_EGAMONOAPA(fd:longint):boolean;
Function SW_CG640x350(fd:longint):boolean;
Function SW_ENH_MONOAPA2(fd:longint):boolean;
Function SW_ENH_CG640(fd:longint):boolean;
Function SW_ENH_B40x25(fd:longint):boolean;
Function SW_ENH_C40x25(fd:longint):boolean;
Function SW_ENH_B80x25(fd:longint):boolean;
Function SW_ENH_C80x25(fd:longint):boolean;
Function SW_ENH_B80x43(fd:longint):boolean;
Function SW_ENH_C80x43(fd:longint):boolean;
Function SW_MCAMODE(fd:longint):boolean;
Function SW_VGA_C40x25(fd:longint):boolean;
Function SW_VGA_C80x25(fd:longint):boolean;
Function SW_VGA_C80x30(fd:longint):boolean;
Function SW_VGA_C80x50(fd:longint):boolean;
Function SW_VGA_C80x60(fd:longint):boolean;
Function SW_VGA_M80x25(fd:longint):boolean;
Function SW_VGA_M80x30(fd:longint):boolean;
Function SW_VGA_M80x50(fd:longint):boolean;
Function SW_VGA_M80x60(fd:longint):boolean;
Function SW_VGA11(fd:longint):boolean;
Function SW_BG640x480(fd:longint):boolean;
Function SW_VGA12(fd:longint):boolean;
Function SW_CG640x480(fd:longint):boolean;
Function SW_VGA13(fd:longint):boolean;
Function SW_VGA_CG320(fd:longint):boolean;
Function SW_VGA_CG640(fd:longint):boolean;
Function SW_VGA_MODEX(fd:longint):boolean;
Function SW_PC98_80x25(fd:longint):boolean;
Function SW_PC98_80x30(fd:longint):boolean;
Function SW_PC98_EGC640x400(fd:longint):boolean;
Function SW_PC98_PEGC640x400(fd:longint):boolean;
Function SW_PC98_PEGC640x480(fd:longint):boolean;
Function SW_VGA_C90x25(fd:longint):boolean;
Function SW_VGA_M90x25(fd:longint):boolean;
Function SW_VGA_C90x30(fd:longint):boolean;
Function SW_VGA_M90x30(fd:longint):boolean;
Function SW_VGA_C90x43(fd:longint):boolean;
Function SW_VGA_M90x43(fd:longint):boolean;
Function SW_VGA_C90x50(fd:longint):boolean;
Function SW_VGA_M90x50(fd:longint):boolean;
Function SW_VGA_C90x60(fd:longint):boolean;
Function SW_VGA_M90x60(fd:longint):boolean;
Function SW_TEXT_80x25(fd:longint):boolean;
Function SW_TEXT_80x30(fd:longint):boolean;
Function SW_TEXT_80x43(fd:longint):boolean;
Function SW_TEXT_80x50(fd:longint):boolean;
Function SW_TEXT_80x60(fd:longint):boolean;
Function SW_TEXT_132x25(fd:longint):boolean;
Function SW_TEXT_132x30(fd:longint):boolean;
Function SW_TEXT_132x43(fd:longint):boolean;
Function SW_TEXT_132x50(fd:longint):boolean;
Function SW_TEXT_132x60(fd:longint):boolean;
Function SW_VESA_CG640x400(fd:longint):boolean;
Function SW_VESA_CG640x480(fd:longint):boolean;
Function SW_VESA_800x600(fd:longint):boolean;
Function SW_VESA_CG800x600(fd:longint):boolean;
Function SW_VESA_1024x768(fd:longint):boolean;
Function SW_VESA_CG1024x768(fd:longint):boolean;
Function SW_VESA_1280x1024(fd:longint):boolean;
Function SW_VESA_CG1280x1024(fd:longint):boolean;
Function SW_VESA_C80x60(fd:longint):boolean;
Function SW_VESA_C132x25(fd:longint):boolean;
Function SW_VESA_C132x43(fd:longint):boolean;
Function SW_VESA_C132x50(fd:longint):boolean;
Function SW_VESA_C132x60(fd:longint):boolean;
Function SW_VESA_32K_320(fd:longint):boolean;
Function SW_VESA_64K_320(fd:longint):boolean;
Function SW_VESA_FULL_320(fd:longint):boolean;
Function SW_VESA_32K_640(fd:longint):boolean;
Function SW_VESA_64K_640(fd:longint):boolean;
Function SW_VESA_FULL_640(fd:longint):boolean;
Function SW_VESA_32K_800(fd:longint):boolean;
Function SW_VESA_64K_800(fd:longint):boolean;
Function SW_VESA_FULL_800(fd:longint):boolean;
Function SW_VESA_32K_1024(fd:longint):boolean;
Function SW_VESA_64K_1024(fd:longint):boolean;
Function SW_VESA_FULL_1024(fd:longint):boolean;
Function SW_VESA_32K_1280(fd:longint):boolean;
Function SW_VESA_64K_1280(fd:longint):boolean;
Function SW_VESA_FULL_1280(fd:longint):boolean;

{----------------------------- sys/kbio.h ----------------------------------}

{ version packaged with FreeBSD 4.2-RELEASE
Translation to FreePascal by Marco van de Voort. (2000-2001), original
copyright follows: ( I assume BSD licensed)

Based on
 * $FreeBSD: src/sys/sys/kbio.h,v 1.5.2.1 2000/10/29 16:59:32 dwmalone Exp $
}

{ get/set keyboard I/O mode}
const   K_RAW           =0;             { keyboard returns scancodes}
        K_XLATE         =1;             { keyboard returns ascii}
        K_CODE          =2;             { keyboard returns keycodes}

{After each FpIoctl value, I've put the type of the parameters to be passed:
     @int -> pass a pointer to an int.
      int -> pass pointer(int)
      -   -> nothing
@keymap_t -> pass a pointer to a keymap_t
 etc.
}

Function KDGKBMODE(fd:longint;var param1 : longint):boolean;
Function KDSKBMODE(fd:longint;param1 : longint):boolean;


{ make tone}
Function KDMKTONE(fd:longint;param1 : longint):boolean;

{ see console.h for the definitions of the following FpIoctls}
{$ifndef definconsole}
Function KDGETMODE(fd:longint;var param1 : longint):boolean;
Function KDSETMODE(fd:longint;param1 : longint):boolean;
Function KDSBORDER(fd:longint;param1 : longint):boolean;

{$endif}
const
{ get/set keyboard lock state}
        CLKED           =1;             { Caps locked}
        NLKED           =2;             { Num locked}
        SLKED           =4;             { Scroll locked}
        ALKED           =8;             { AltGr locked}
        LOCK_MASK       =CLKED or NLKED or SLKED or ALKED;

Function KDGKBSTATE(fd:longint;var param1 : longint):boolean;
Function KDSKBSTATE(fd:longint;param1 : longint):boolean;

{ enable/disable I/O access}
Function KDENABIO(fd:longint):boolean;
Function KDDISABIO(fd:longint):boolean;

{ make sound}
Function KIOCSOUND(fd:longint;param1 : longint):boolean;

Const
{ get keyboard model}
        KB_OTHER        =0;             { keyboard not known}
        KB_84           =1;             { 'old' 84 key AT-keyboard}
        KB_101          =2;             { MF-101 or MF-102 keyboard}
Function KDGKBTYPE(fd:longint;var param1 : longint):boolean;

const
{ get/set keyboard LED state}
        LED_CAP         =1;             { Caps lock LED}
        LED_NUM         =2;             { Num lock LED}
        LED_SCR         =4;             { Scroll lock LED}
        LED_MASK        =LED_CAP or LED_NUM or LED_SCR;
Function KDGETLED(fd:longint;var param1 : longint):boolean;
Function KDSETLED(fd:longint;param1 : longint):boolean;

{ set keyboard repeat rate (obsolete, use KDSETREPEAT below)}
Function KDSETRAD(fd:longint;param1 : longint):boolean;

{ see console.h for the definition of the following FpIoctl}
{$ifndef definconsole}
Function KDRASTER(fd:longint;var param1 : scr_size_t):boolean;

{$endif}

TYPE
{ get keyboard information}
  keyboard_info = Record
                        kb_index : longint;     { kbdio index#}
                        kb_name  : array[0..15] of char;        { driver name}
                        kb_unit  : longint;     { unit#}
                        kb_type  : longint;     { KB_84, KB_101, KB_OTHER,...}
                        kb_config: longint;     { device configuration flags}
                        kb_flags : longint;     { internal flags}
                 end;
  keyboard_info_t=keyboard_info;

Function KDGKBINFO(fd:longint;var param1 : keyboard_info_t):boolean;

Type
{ set/get keyboard repeat rate (new interface)}
 keyboard_repeat = record
                    kb_repeat: array[0..1] of longint;
                   end;

keyboard_repeat_t = keyboard_repeat;

Function KDSETREPEAT(fd:longint;var param1 : keyboard_repeat_t):boolean;
Function KDGETREPEAT(fd:longint;var param1 : keyboard_repeat_t):boolean;

{ get/set key map/accent map/function key strings}

const
        NUM_KEYS        =256;           { number of keys in table}
        NUM_STATES      =8;             { states per key}
        ALTGR_OFFSET    =128;           { offset for altlock keys}

        NUM_DEADKEYS    =15;            { number of accent keys}
        NUM_ACCENTCHARS =52;            { max number of accent chars}

        NUM_FKEYS       =96;            { max number of function keys}
        MAXFK           =16;            { max length of a function key str}

type
        keyent_t = record
                        map  : array[0..NUM_STATES-1] of uchar;
                        spcl : uchar;
                        flgs : uchar;
                   end;

const
        FLAG_LOCK_O     =0;
        FLAG_LOCK_C     =1;
        FLAG_LOCK_N     =2;

type keymap = record
                        n_keys : ushort;
                        key    : array[0..NUM_KEYS-1] OF keyent_t;
              end;

     keymap_t= keymap;

CONST
{ defines for "special" keys (spcl bit set in keymap)}
        NOP             =$00;           { nothing (dead key)}
        LSH             =$02;           { left shift key}
        RSH             =$03;           { right shift key}
        CLK             =$04;           { caps lock key}
        NLK             =$05;           { num lock key}
        SLK             =$06;           { scroll lock key}
        LALT            =$07;           { left alt key}
        BTAB            =$08;           { backwards tab}
        LCTR            =$09;           { left control key}
        NEXT            =$0a;           { switch to next screen}
        F_SCR           =$0b;           { switch to first screen}
        L_SCR           =$1a;           { switch to last screen}
        F_FN            =$1b;           { first function key}
        L_FN            =$7a;           { last function key}
{                        $7b-$7f          reserved do not use !}
        RCTR            =$80;           { right control key}
        RALT            =$81;           { right alt (altgr) key}
        ALK             =$82;           { alt lock key}
        ASH             =$83;           { alt shift key}
        META            =$84;           { meta key}
        RBT             =$85;           { boot machine}
        DBG             =$86;           { call debugger}
        SUSP            =$87;           { suspend power (APM)}
        SPSC            =$88;           { toggle splash/text screen}

        DGRA            =$89;           { grave}
        F_ACC           =DGRA;          { first accent key}

        DACU            =$8a;           { acute}
        DCIR            =$8b;           { circumflex}
        DTIL            =$8c;           { tilde}
        DMAC            =$8d;           { macron}
        DBRE            =$8e;           { breve}
        DDOT            =$8f;           { dot}
        DUML            =$90;           { umlaut/diaresis}
        DDIA            =$90;           { diaresis}
        DSLA            =$91;           { slash}
        DRIN            =$92;           { ring}
        DCED            =$93;           { cedilla}
        DAPO            =$94;           { apostrophe}
        DDAC            =$95;           { double acute}
        DOGO            =$96;           { ogonek}
        DCAR            =$97;           { caron}
        L_ACC           =DCAR;          { last accent key}

        STBY            =$98;           { Go into standby mode (apm)}
        PREV            =$99;           { switch to previous screen}
        PNC             =$9a;           { force system panic}
        LSHA            =$9b;           { left shift key / alt lock}
        RSHA            =$9c;           { right shift key / alt lock}
        LCTRA           =$9d;           { left ctrl key / alt lock}
        RCTRA           =$9e;           { right ctrl key / alt lock}
        LALTA           =$9f;           { left alt key / alt lock}
        RALTA           =$a0;           { right alt key / alt lock}
        HALT            =$a1;           { halt machine}
        PDWN            =$a2;           { halt machine and power down}

function kbio_F(x:longint):longint;
function kbio_S(x:longint):longint;
function kbio_ACC(x:longint):longint;

type acc_t           = record
                        accchar : uchar;
                        map : array[0..NUM_ACCENTCHARS-1,0..1] of uchar;
                       end;

   accentmap       = record
                        n_accs : ushort;
                        acc    : array[0..NUM_DEADKEYS-1] of acc_t
                       end;

      accentmap_t     =  accentmap ;

     keyarg         = record
                        keynum : ushort;
                        key : keyent_t;
                        end;

       keyarg_t = keyarg;

             fkeytab = record
                        str : array [0..MAXFK-1] of uchar;
                        len : uchar;
                       end;
             fkeytab_t = fkeytab;

             fkeyarg =record
                        keynum : ushort;
                        keydef : array[0..MAXFK-1] of char;
                        flen :char;
                        end;

         fkeyarg_t       = fkeyarg;

Function GETFKEY(fd:longint;var param1 : fkeyarg_t):boolean;
Function SETFKEY(fd:longint;var param1 : fkeyarg_t):boolean;

{$ifndef definconsole}
Function GIO_SCRNMAP(fd:longint;var param1 : scrmap_t):boolean;
Function PIO_SCRNMAP(fd:longint;var param1 : scrmap_t):boolean;
{$endif}
Function GIO_KEYMAP(fd:longint;var param1 : keymap_t):boolean;
Function PIO_KEYMAP(fd:longint;var param1 : keymap_t):boolean;
Function GIO_DEADKEYMAP(fd:longint;var param1 : accentmap_t):boolean;
Function PIO_DEADKEYMAP(fd:longint;var param1 : accentmap_t):boolean;
Function GIO_KEYMAPENT(fd:longint;var param1 : keyarg_t):boolean;
Function PIO_KEYMAPENT(fd:longint;var param1 : keyarg_t):boolean;

{ flags set to the return value in the KD_XLATE mode}
Const
        NOKEY           =$100;          { no key pressed marker}
        FKEY            =$200;          { function key marker}
        MKEY            =$400;          { meta key marker (prepend ESC)}
        BKEY            =$800;          { backtab (ESC [ Z)}

        SPCLKEY         =$8000;         { special key}
        RELKEY          =$4000;         { key released}
        ERRKEY          =$2000;         { error}


function KEYCHAR(c:longint):longint;

function KEYFLAGS(c:longint):longint;

{----------------------------- machine/mouse.h -------------------------------}

{ Based on machine/mouse.h from FreeBSD release 4.2

 * Copyright (c) 1992, 1993 Erik Forsberg.
 * Copyright (c) 1996, 1997 Kazutaka YOKOTA
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * THIS SOFTWARE IS PROVIDED BY ``AS IS'' AND ANY EXPRESS OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN
 * NO EVENT SHALL I BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * $FreeBSD: src/sys/i386/include/mouse.h,v 1.15.2.1 2000/03/21 14:44:10 yokota Exp $
 }


{ FpIoctls }


{ mouse status block }

type
 mousestatus = record
                 flags    : longint;            { state change flags }
                 button   : longint;            { button status }
                 obutton  : longint;            { previous button status }
                 dx       : longint;            { x movement }
                 dy       : longint;            { y movement }
                 dz       : longint;            { z movement }
                end;

 mousestatus_t = mousestatus;

CONST

{ button }
       MOUSE_BUTTON1DOWN        =$0001; { left }
       MOUSE_BUTTON2DOWN        =$0002; { middle }
       MOUSE_BUTTON3DOWN        =$0004; { right }
       MOUSE_BUTTON4DOWN        =$0008;
       MOUSE_BUTTON5DOWN        =$0010;
       MOUSE_BUTTON6DOWN        =$0020;
       MOUSE_BUTTON7DOWN        =$0040;
       MOUSE_BUTTON8DOWN        =$0080;
       MOUSE_MAXBUTTON          =31;
       MOUSE_STDBUTTONS         =$0007;         { buttons 1-3 }
       MOUSE_EXTBUTTONS         =$7ffffff8;     { the others (28 of them!) }
       MOUSE_BUTTONS            =(MOUSE_STDBUTTONS or MOUSE_EXTBUTTONS);

{ flags }
       MOUSE_STDBUTTONSCHANGED  =MOUSE_STDBUTTONS;
       MOUSE_EXTBUTTONSCHANGED  =MOUSE_EXTBUTTONS;
       MOUSE_BUTTONSCHANGED     =MOUSE_BUTTONS;
       MOUSE_POSCHANGED         =$80000000;

type
  mousehw =record
         buttons : longint;             { -1 if unknown }
         iftype  : longint;             { MOUSE_IF_XXX }
         _type    : longint;            { mouse/track ball/pad... }
         model   : longint;             { I/F dependent model ID: MOUSE_MODEL_XXX }
         hwid    : longint;             { I/F dependent hardware ID}
                                 { for the PS/2 mouse, it will be PSM_XXX_ID  }
          end;

   mousehw_t  = mousehw;

const

{ iftype }
       MOUSE_IF_UNKNOWN         =(-1);
       MOUSE_IF_SERIAL          =0;
       MOUSE_IF_BUS             =1;
       MOUSE_IF_INPORT          =2;
       MOUSE_IF_PS2             =3;
       MOUSE_IF_SYSMOUSE        =4;
       MOUSE_IF_USB             =5;

{ type }
       MOUSE_UNKNOWN            =(-1);  { should be treated as a mouse }
       MOUSE_MOUSE              =0;
       MOUSE_TRACKBALL          =1;
       MOUSE_STICK              =2;
       MOUSE_PAD                =3;

{ model }
       MOUSE_MODEL_UNKNOWN              =(-1);
       MOUSE_MODEL_GENERIC              =0;
       MOUSE_MODEL_GLIDEPOINT           =1;
       MOUSE_MODEL_NETSCROLL            =2;
       MOUSE_MODEL_NET                  =3;
       MOUSE_MODEL_INTELLI              =4;
       MOUSE_MODEL_THINK                =5;
       MOUSE_MODEL_EASYSCROLL           =6;
       MOUSE_MODEL_MOUSEMANPLUS         =7;
       MOUSE_MODEL_KIDSPAD              =8;
       MOUSE_MODEL_VERSAPAD             =9;
       MOUSE_MODEL_EXPLORER             =10;
       MOUSE_MODEL_4D                   =11;
       MOUSE_MODEL_4DPLUS               =12;

type  mousemode = record
                    protocol    : longint;              { MOUSE_PROTO_XXX }
                    rate        : longint;              { report rate (per sec), -1 if unknown }
                    resolution  : longint;              { MOUSE_RES_XXX, -1 if unknown }
                    accelfactor : longint;              { accelation factor (must be 1 or greater) }
                    level       : longint;              { driver operation level }
                    packetsize  : longint;              { the length of the data packet }
                    syncmask    : array[0..1] of uchar; { sync. data bits in the header byte }
                  end;

type mousemode_t = mousemode;

{ protocol }
{
 * Serial protocols:
 *   Microsoft, MouseSystems, Logitech, MM series, MouseMan, Hitachi Tablet,
 *   GlidePoint, IntelliMouse, Thinking Mouse, MouseRemote, Kidspad,
 *   VersaPad
 * Bus mouse protocols:
 *   bus, InPort
 * PS/2 mouse protocol:
 *   PS/2
 }
 const

       MOUSE_PROTO_UNKNOWN      =(-1);
       MOUSE_PROTO_MS           =0;     { Microsoft Serial, 3 bytes }
       MOUSE_PROTO_MSC          =1;     { Mouse Systems, 5 bytes }
       MOUSE_PROTO_LOGI         =2;     { Logitech, 3 bytes }
       MOUSE_PROTO_MM           =3;     { MM series, 3 bytes }
       MOUSE_PROTO_LOGIMOUSEMAN =4;     { Logitech MouseMan 3/4 bytes }
       MOUSE_PROTO_BUS          =5;     { MS/Logitech bus mouse }
       MOUSE_PROTO_INPORT       =6;     { MS/ATI InPort mouse }
       MOUSE_PROTO_PS2          =7;     { PS/2 mouse, 3 bytes }
       MOUSE_PROTO_HITTAB       =8;     { Hitachi Tablet 3 bytes }
       MOUSE_PROTO_GLIDEPOINT   =9;     { ALPS GlidePoint, 3/4 bytes }
       MOUSE_PROTO_INTELLI      =10;    { MS IntelliMouse, 4 bytes }
       MOUSE_PROTO_THINK        =11;    { Kensignton Thinking Mouse, 3/4 bytes }
       MOUSE_PROTO_SYSMOUSE     =12;    { /dev/sysmouse }
       MOUSE_PROTO_X10MOUSEREM  =13;    { X10 MouseRemote, 3 bytes }
       MOUSE_PROTO_KIDSPAD      =14;    { Genius Kidspad }
       MOUSE_PROTO_VERSAPAD     =15;    { Interlink VersaPad, 6 bytes }

       MOUSE_RES_UNKNOWN        =(-1);
       MOUSE_RES_DEFAULT        =0;
       MOUSE_RES_LOW            =(-2);
       MOUSE_RES_MEDIUMLOW      =(-3);
       MOUSE_RES_MEDIUMHIGH     =(-4);
       MOUSE_RES_HIGH           =(-5);

type  mousedata = record
        len : longint;          { # of data in the buffer }
        buf : array [0..15] of longint;         { data buffer }
        end;

 mousedata_t=mousedata;

 mousevar  = record
               _var : array[0..15] of longint;
               end;

type mousevar_t = mousevar;

Function MOUSE_GETSTATUS(fd:longint;var param1 : mousestatus_t):boolean;
Function MOUSE_GETHWINFO(fd:longint;var param1 : mousehw_t):boolean;
Function MOUSE_GETMODE(fd:longint;var param1 : mousemode_t):boolean;
Function MOUSE_SETMODE(fd:longint;var param1 : mousemode_t):boolean;
Function MOUSE_GETLEVEL(fd:longint;var param1 : longint):boolean;
Function MOUSE_SETLEVEL(fd:longint;var param1 : longint):boolean;
Function MOUSE_GETVARS(fd:longint;var param1 : mousevar_t):boolean;
Function MOUSE_SETVARS(fd:longint;var param1 : mousevar_t):boolean;
Function MOUSE_READSTATE(fd:longint;var param1 : mousedata_t):boolean;
Function MOUSE_READDATA(fd:longint;var param1 : mousedata_t):boolean;

Function MOUSE_SETRESOLUTION(fd:longint;var param1 : longint):boolean;
Function MOUSE_SETSCALING(fd:longint;var param1 : longint):boolean;
Function MOUSE_SETRATE(fd:longint;var param1 : longint):boolean;
Function MOUSE_GETHWID(fd:longint;var param1 : longint):boolean;



const

{ magic numbers in var[0] }
       MOUSE_VARS_PS2_SIG       = $00325350;    { 'PS2' }
       MOUSE_VARS_BUS_SIG       = $00535542;    { 'BUS' }
       MOUSE_VARS_INPORT_SIG    = $00504e49;    { 'INP' }

{ Microsoft Serial mouse data packet }
       MOUSE_MSS_PACKETSIZE     = 3;
       MOUSE_MSS_SYNCMASK       = $40;
       MOUSE_MSS_SYNC           = $40;
       MOUSE_MSS_BUTTONS        = $30;
       MOUSE_MSS_BUTTON1DOWN    = $20;  { left }
       MOUSE_MSS_BUTTON2DOWN    = $00;  { no middle button }
       MOUSE_MSS_BUTTON3DOWN    = $10;  { right }

{ Logitech MouseMan data packet (M+ protocol) }
       MOUSE_LMAN_BUTTON2DOWN   = $20;  { middle button, the 4th byte }

{ ALPS GlidePoint extension (variant of M+ protocol) }
       MOUSE_ALPS_BUTTON2DOWN   = $20;  { middle button, the 4th byte }
       MOUSE_ALPS_TAP           = $10;  { `tapping' action, the 4th byte }

{ Kinsington Thinking Mouse extension (variant of M+ protocol) }
       MOUSE_THINK_BUTTON2DOWN = $20;   { lower-left button, the 4th byte }
       MOUSE_THINK_BUTTON4DOWN = $10;   { lower-right button, the 4th byte }

{ MS IntelliMouse (variant of MS Serial) }
       MOUSE_INTELLI_PACKETSIZE  = 4;
       MOUSE_INTELLI_BUTTON2DOWN = $10; { middle button in the 4th byte }

{ Mouse Systems Corp. mouse data packet }
       MOUSE_MSC_PACKETSIZE     = 5;
       MOUSE_MSC_SYNCMASK       = $f8;
       MOUSE_MSC_SYNC           = $80;
       MOUSE_MSC_BUTTONS        = $07;
       MOUSE_MSC_BUTTON1UP      = $04;  { left }
       MOUSE_MSC_BUTTON2UP      = $02;  { middle }
       MOUSE_MSC_BUTTON3UP      = $01;  { right }
       MOUSE_MSC_MAXBUTTON      = 3;

{ MM series mouse data packet }
       MOUSE_MM_PACKETSIZE      = 3;
       MOUSE_MM_SYNCMASK        = $e0;
       MOUSE_MM_SYNC            = $80;
       MOUSE_MM_BUTTONS         = $07;
       MOUSE_MM_BUTTON1DOWN     = $04;  { left }
       MOUSE_MM_BUTTON2DOWN     = $02;  { middle }
       MOUSE_MM_BUTTON3DOWN     = $01;  { right }
       MOUSE_MM_XPOSITIVE       = $10;
       MOUSE_MM_YPOSITIVE       = $08;

{ PS/2 mouse data packet }
       MOUSE_PS2_PACKETSIZE     = 3;
       MOUSE_PS2_SYNCMASK       = $c8;
       MOUSE_PS2_SYNC           = $08;
       MOUSE_PS2_BUTTONS        = $07;  { = $03 for 2 button mouse }
       MOUSE_PS2_BUTTON1DOWN    = $01;  { left }
       MOUSE_PS2_BUTTON2DOWN    = $04;  { middle }
       MOUSE_PS2_BUTTON3DOWN    = $02;  { right }
       MOUSE_PS2_TAP            = MOUSE_PS2_SYNC; { GlidePoint (PS/2) `tapping'
                                                * Yes! this is the same bit
                                                * as SYNC!
                                                }

       MOUSE_PS2_XNEG           = $10;
       MOUSE_PS2_YNEG           = $20;
       MOUSE_PS2_XOVERFLOW      = $40;
       MOUSE_PS2_YOVERFLOW      = $80;

{ Logitech MouseMan+ (PS/2) data packet (PS/2++ protocol) }
       MOUSE_PS2PLUS_SYNCMASK   = $48;
       MOUSE_PS2PLUS_SYNC       = $48;
       MOUSE_PS2PLUS_ZNEG       = $08;  { sign bit }
       MOUSE_PS2PLUS_BUTTON4DOWN = $10; { 4th button on MouseMan+ }
       MOUSE_PS2PLUS_BUTTON5DOWN = $20;

{ IBM ScrollPoint (PS/2) also uses PS/2++ protocol }
       MOUSE_SPOINT_ZNEG        = $80;  { sign bits }
       MOUSE_SPOINT_WNEG        = $08;

{ MS IntelliMouse (PS/2) data packet }
       MOUSE_PS2INTELLI_PACKETSIZE = 4;
{ some compatible mice have additional buttons }
       MOUSE_PS2INTELLI_BUTTON4DOWN = $40;
       MOUSE_PS2INTELLI_BUTTON5DOWN = $80;

{ MS IntelliMouse Explorer (PS/2) data packet (variation of IntelliMouse) }
       MOUSE_EXPLORER_ZNEG        = $08;        { sign bit }
{ IntelliMouse Explorer has additional button data in the fourth byte }
       MOUSE_EXPLORER_BUTTON4DOWN = $10;
       MOUSE_EXPLORER_BUTTON5DOWN = $20;

{ Interlink VersaPad (serial I/F) data packet }
       MOUSE_VERSA_PACKETSIZE   = 6;
       MOUSE_VERSA_IN_USE       = $04;
       MOUSE_VERSA_SYNCMASK     = $c3;
       MOUSE_VERSA_SYNC         = $c0;
       MOUSE_VERSA_BUTTONS      = $30;
       MOUSE_VERSA_BUTTON1DOWN  = $20;  { left }
       MOUSE_VERSA_BUTTON2DOWN  = $00;  { middle }
       MOUSE_VERSA_BUTTON3DOWN  = $10;  { right }
       MOUSE_VERSA_TAP          = $08;

{ Interlink VersaPad (PS/2 I/F) data packet }
       MOUSE_PS2VERSA_PACKETSIZE        = 6;
       MOUSE_PS2VERSA_IN_USE            = $10;
       MOUSE_PS2VERSA_SYNCMASK          = $e8;
       MOUSE_PS2VERSA_SYNC              = $c8;
       MOUSE_PS2VERSA_BUTTONS           = $05;
       MOUSE_PS2VERSA_BUTTON1DOWN       = $04;  { left }
       MOUSE_PS2VERSA_BUTTON2DOWN       = $00;  { middle }
       MOUSE_PS2VERSA_BUTTON3DOWN       = $01;  { right }
       MOUSE_PS2VERSA_TAP               = $02;

{ A4 Tech 4D Mouse (PS/2) data packet }
       MOUSE_4D_PACKETSIZE              = 3;
       MOUSE_4D_WHEELBITS               = $f0;

{ A4 Tech 4D+ Mouse (PS/2) data packet }
       MOUSE_4DPLUS_PACKETSIZE          = 3;
       MOUSE_4DPLUS_ZNEG                = $04;  { sign bit }
       MOUSE_4DPLUS_BUTTON4DOWN         = $08;

{ sysmouse extended data packet }
{
 * /dev/sysmouse sends data in two formats, depending on the protocol
 * level.  At the level 0, format is exactly the same as MousSystems'
 * five byte packet.  At the level 1, the first five bytes are the same
 * as at the level 0.  There are additional three bytes which shows
 * `dz' and the states of additional buttons.  `dz' is expressed as the
 * sum of the byte 5 and 6 which contain signed seven bit values.
 * The states of the button 4 though 10 are in the bit 0 though 6 in
 * the byte 7 respectively: 1 indicates the button is up.
 }
       MOUSE_SYS_PACKETSIZE     = 8;
       MOUSE_SYS_SYNCMASK       = $f8;
       MOUSE_SYS_SYNC           = $80;
       MOUSE_SYS_BUTTON1UP      = $04;  { left, 1st byte }
       MOUSE_SYS_BUTTON2UP      = $02;  { middle, 1st byte }
       MOUSE_SYS_BUTTON3UP      = $01;  { right, 1st byte }
       MOUSE_SYS_BUTTON4UP      = $0001;        { 7th byte }
       MOUSE_SYS_BUTTON5UP      = $0002;
       MOUSE_SYS_BUTTON6UP      = $0004;
       MOUSE_SYS_BUTTON7UP      = $0008;
       MOUSE_SYS_BUTTON8UP      = $0010;
       MOUSE_SYS_BUTTON9UP      = $0020;
       MOUSE_SYS_BUTTON10UP     = $0040;
       MOUSE_SYS_MAXBUTTON      = 10;
       MOUSE_SYS_STDBUTTONS     = $07;
       MOUSE_SYS_EXTBUTTONS     = $7f;  { the others }

{ Mouse remote socket }
       _PATH_MOUSEREMOTE        ='/var/run/MouseRemote';


{fbio FpIoctl numbers}
           nr_FBIOGTYPE     =$40184600;
           nr_FBIOGINFO     =$40184602;
           nr_FBIOPUTCMAP     =$80144603;
           nr_FBIOGETCMAP     =$80144604;
           nr_FBIOGATTR     =$40584606;
           nr_FBIOSVIDEO     =$80044607;
           nr_FBIOGVIDEO     =$40044608;
           nr_FBIOSCURSOR     =$802c4618;
           nr_FBIOGCURSOR     =$c02c4619;
           nr_FBIOSCURPOS     =$8004461a;
           nr_FBIOGCURPOS     =$8004461b;
           nr_FBIOGCURMAX     =$4004461c;
           nr_FBIO_ADAPTER     =$40044664;
           nr_FBIO_ADPTYPE     =$40044665;
           nr_FBIO_ADPINFO     =$40a44666;
           nr_FBIO_MODEINFO     =$c09c4667;
           nr_FBIO_FINDMODE     =$c09c4668;
           nr_FBIO_GETMODE     =$40044669;
           nr_FBIO_SETMODE     =$8004466a;
           nr_FBIO_GETWINORG     =$4004466b;
           nr_FBIO_SETWINORG     =$8004466c;
           nr_FBIO_GETDISPSTART     =$4008466d;
           nr_FBIO_SETDISPSTART     =$8008466e;
           nr_FBIO_GETLINEWIDTH     =$4004466f;
           nr_FBIO_SETLINEWIDTH     =$80044670;
           nr_FBIO_GETPALETTE     =$80184671;
           nr_FBIO_SETPALETTE     =$80184672;

{consio FpIoctl numbers}

           nr_KDGETMODE     =$40044b09;
           nr_KDSETMODE     =$20004b0a;
           nr_KDSBORDER     =$20004b0d;
           nr_KDRASTER     =$800c4b64;
           nr_GIO_SCRNMAP     =$41006b02;
           nr_PIO_SCRNMAP     =$81006b03;
           nr_GIO_ATTR     =$40046100;
           nr_GIO_COLOR     =$40046300;
           nr_CONS_CURRENT     =$40046301;
           nr_CONS_GET     =$40046302;
           nr_CONS_IO     =$20006303;
           nr_CONS_BLANKTIME     =$80046304;
           nr_CONS_SSAVER     =$80186305;
           nr_CONS_GSAVER     =$c0186306;
           nr_CONS_CURSORTYPE     =$80046307;
           nr_CONS_BELLTYPE     =$80046308;
           nr_CONS_HISTORY     =$80046309;
           nr_CONS_MOUSECTL     =$c014630a;
           nr_CONS_IDLE     =$4004630b;
           nr_CONS_SAVERMODE     =$8004630c;
           nr_CONS_SAVERSTART     =$8004630d;
           nr_PIO_FONT8x8     =$88006340;
           nr_GIO_FONT8x8     =$48006341;
           nr_PIO_FONT8x14     =$8e006342;
           nr_GIO_FONT8x14     =$4e006343;
           nr_PIO_FONT8x16     =$90006344;
           nr_GIO_FONT8x16     =$50006345;
           nr_CONS_GETINFO     =$c0146349;
           nr_CONS_GETVERS     =$4004634a;
           nr_CONS_CURRENTADP     =$40046364;
           nr_CONS_ADPINFO     =$c0a46365;
           nr_CONS_MODEINFO     =$c09c6366;
           nr_CONS_FINDMODE     =$c09c6367;
           nr_CONS_SETWINORG     =$20006368;
           nr_CONS_SETKBD     =$2000636e;
           nr_CONS_RELKBD     =$2000636f;
           nr_CONS_GETTERM     =$c0686370;
           nr_CONS_SETTERM     =$80686371;
           nr_ADJUST_CLOCK     =$20007464;
           nr_VT_OPENQRY     =$40047601;
           nr_VT_SETMODE     =$80087602;
           nr_VT_GETMODE     =$40087603;
           nr_VT_RELDISP     =$20007604;
           nr_VT_ACTIVATE     =$20007605;
           nr_VT_WAITACTIVE     =$20007606;
           nr_VT_GETACTIVE     =$40047607;
           nr_VT_GETINDEX     =$40047608;
           nr_SW_B40x25     =$20005300;
           nr_SW_C40x25     =$20005301;
           nr_SW_B80x25     =$20005302;
           nr_SW_C80x25     =$20005303;
           nr_SW_BG320     =$20005304;
           nr_SW_CG320     =$20005305;
           nr_SW_BG640     =$20005306;
           nr_SW_EGAMONO80x25     =$20005307;
           nr_SW_CG320_D     =$2000530d;
           nr_SW_CG640_E     =$2000530e;
           nr_SW_EGAMONOAPA     =$2000530f;
           nr_SW_CG640x350     =$20005310;
           nr_SW_ENH_MONOAPA2     =$20005311;
           nr_SW_ENH_CG640     =$20005312;
           nr_SW_ENH_B40x25     =$20005313;
           nr_SW_ENH_C40x25     =$20005314;
           nr_SW_ENH_B80x25     =$20005315;
           nr_SW_ENH_C80x25     =$20005316;
           nr_SW_ENH_B80x43     =$20005370;
           nr_SW_ENH_C80x43     =$20005371;
           nr_SW_MCAMODE     =$200053ff;
           nr_SW_VGA_C40x25     =$20005317;
           nr_SW_VGA_C80x25     =$20005318;
           nr_SW_VGA_C80x30     =$20005320;
           nr_SW_VGA_C80x50     =$2000531e;
           nr_SW_VGA_C80x60     =$20005322;
           nr_SW_VGA_M80x25     =$20005319;
           nr_SW_VGA_M80x30     =$20005321;
           nr_SW_VGA_M80x50     =$2000531f;
           nr_SW_VGA_M80x60     =$20005323;
           nr_SW_VGA11     =$2000531a;
           nr_SW_BG640x480     =$2000531a;
           nr_SW_VGA12     =$2000531b;
           nr_SW_CG640x480     =$2000531b;
           nr_SW_VGA13     =$2000531c;
           nr_SW_VGA_CG320     =$2000531c;
           nr_SW_VGA_CG640     =$20005324;
           nr_SW_VGA_MODEX     =$20005325;
           nr_SW_PC98_80x25     =$20005362;
           nr_SW_PC98_80x30     =$20005363;
           nr_SW_PC98_EGC640x400     =$20005364;
           nr_SW_PC98_PEGC640x400     =$20005365;
           nr_SW_PC98_PEGC640x480     =$20005366;
           nr_SW_VGA_C90x25     =$20005328;
           nr_SW_VGA_M90x25     =$20005329;
           nr_SW_VGA_C90x30     =$2000532a;
           nr_SW_VGA_M90x30     =$2000532b;
           nr_SW_VGA_C90x43     =$2000532c;
           nr_SW_VGA_M90x43     =$2000532d;
           nr_SW_VGA_C90x50     =$2000532e;
           nr_SW_VGA_M90x50     =$2000532f;
           nr_SW_VGA_C90x60     =$20005330;
           nr_SW_VGA_M90x60     =$20005331;
           nr_SW_TEXT_80x25     =$200053c8;
           nr_SW_TEXT_80x30     =$200053c9;
           nr_SW_TEXT_80x43     =$200053ca;
           nr_SW_TEXT_80x50     =$200053cb;
           nr_SW_TEXT_80x60     =$200053cc;
           nr_SW_TEXT_132x25     =$200053cd;
           nr_SW_TEXT_132x30     =$200053ce;
           nr_SW_TEXT_132x43     =$200053cf;
           nr_SW_TEXT_132x50     =$200053d0;
           nr_SW_TEXT_132x60     =$200053d1;
           nr_SW_VESA_CG640x400     =$20005600;
           nr_SW_VESA_CG640x480     =$20005601;
           nr_SW_VESA_800x600     =$20005602;
           nr_SW_VESA_CG800x600     =$20005603;
           nr_SW_VESA_1024x768     =$20005604;
           nr_SW_VESA_CG1024x768     =$20005605;
           nr_SW_VESA_1280x1024     =$20005606;
           nr_SW_VESA_CG1280x1024     =$20005607;
           nr_SW_VESA_C80x60     =$20005608;
           nr_SW_VESA_C132x25     =$20005609;
           nr_SW_VESA_C132x43     =$2000560a;
           nr_SW_VESA_C132x50     =$2000560b;
           nr_SW_VESA_C132x60     =$2000560c;
           nr_SW_VESA_32K_320     =$2000560d;
           nr_SW_VESA_64K_320     =$2000560e;
           nr_SW_VESA_FULL_320     =$2000560f;
           nr_SW_VESA_32K_640     =$20005610;
           nr_SW_VESA_64K_640     =$20005611;
           nr_SW_VESA_FULL_640     =$20005612;
           nr_SW_VESA_32K_800     =$20005613;
           nr_SW_VESA_64K_800     =$20005614;
           nr_SW_VESA_FULL_800     =$20005615;
           nr_SW_VESA_32K_1024     =$20005616;
           nr_SW_VESA_64K_1024     =$20005617;
           nr_SW_VESA_FULL_1024     =$20005618;
           nr_SW_VESA_32K_1280     =$20005619;
           nr_SW_VESA_64K_1280     =$2000561a;
           nr_SW_VESA_FULL_1280     =$2000561b;

{kbdsio FpIoctl numbers}

           nr_KDGKBMODE     =$40044b06;
           nr_KDSKBMODE     =$20004b07;
           nr_KDMKTONE     =$20004b08;
{$ifndef definconsole}
           nr_KDGETMODE     =$40044b09;
           nr_KDSETMODE     =$20004b0a;
           nr_KDSBORDER     =$20004b0d;
{$endif}
           nr_KDGKBSTATE     =$40044b13;
           nr_KDSKBSTATE     =$20004b14;
           nr_KDENABIO     =$20004b3c;
           nr_KDDISABIO     =$20004b3d;
           nr_KIOCSOUND     =$20004b3f;
           nr_KDGKBTYPE     =$40044b40;
           nr_KDGETLED     =$40044b41;
           nr_KDSETLED     =$20004b42;
           nr_KDSETRAD     =$20004b43;
{$ifndef definconsole}
           nr_KDRASTER     =$800c4b64;
{$endif}
           nr_KDGKBINFO     =$40244b65;
           nr_KDSETREPEAT     =$80084b66;
           nr_KDGETREPEAT     =$40084b67;
           nr_GETFKEY     =$c0146b00;
           nr_SETFKEY     =$c0146b01;
{$ifndef definconsole}
           nr_GIO_SCRNMAP     =$41006b02;
           nr_PIO_SCRNMAP     =$81006b03;
{$endif}
           nr_GIO_KEYMAP     =$4a026b06;
           nr_PIO_KEYMAP     =$8a026b07;
           nr_GIO_DEADKEYMAP     =$462a6b08;
           nr_PIO_DEADKEYMAP     =$862a6b09;
           nr_GIO_KEYMAPENT     =$c00c6b0a;
           nr_PIO_KEYMAPENT     =$800c6b0b;



{mouse FpIoctl numbers}
           nr_MOUSE_GETSTATUS     =$40184d00;
           nr_MOUSE_GETHWINFO     =$40144d01;
           nr_MOUSE_GETMODE     =$401c4d02;
           nr_MOUSE_SETMODE     =$801c4d03;
           nr_MOUSE_GETLEVEL     =$40044d04;
           nr_MOUSE_SETLEVEL     =$80044d05;
           nr_MOUSE_GETVARS     =$40404d06;
           nr_MOUSE_SETVARS     =$80404d07;
           nr_MOUSE_READSTATE     =$c0444d08;
           nr_MOUSE_READDATA     =$c0444d09;
           nr_MOUSE_SETRESOLUTION     =$80044d0a;
           nr_MOUSE_SETSCALING     =$80044d0b;
           nr_MOUSE_SETRATE     =$80044d0c;
           nr_MOUSE_GETHWID     =$40044d0d;

{------------- Added procedures ---------------}

function physicalconsole(fd:longint) : boolean;

IMPLEMENTATION

Uses BaseUnix,termio;

function physicalconsole(fd:longint) : boolean;

var name:string;

begin
 if (isatty(fd)<>-1) then
  begin
   name:=ttyname(fd);
   if Copy(name,1,8)<>'/dev/tty' then
    physicalconsole:=false              {isatty is true, but not /dev/tty.
                                           Could be /dev/pts support, but
                                           I reserve the case}
   else
    begin
     if name[9]='v' then                        {ttyv is phys console. see /etc/ttys}
      physicalconsole:=true
     else
      physicalconsole:=false;
    end;
  end
 else
  physicalconsole:=false;       {Not a tty, then I don't know what it is}
end;

{other macros (not FpIoctl)}

function KEYCHAR(c:longint):longint;

begin
  c:=c and $FF;
end;

function KEYFLAGS(c:longint):longint;

begin
  c:=c and NOT $FF;
end;

function kbio_F(x:longint):longint;
begin
 kbio_f:=x+F_FN-1;
end;

function kbio_S(x:longint):longint;
begin
 kbio_S:=x+F_SCR-1;
end;

function kbio_ACC(x:longint):longint;
begin
 kbio_ACC:=x+F_ACC;
end;

{fbio.h FpIoctl's}

Function FBIOGTYPE(fd:longint;var param1 : fbtype):boolean;
{IOR('F',0,sizeof(struct fbtype) }

Begin
 FBIOGTYPE:=FpIoctl(fd,nr_FBIOGTYPE,@param1)=0;
end;

Function FBIOGINFO(fd:longint;var param1 : fbinfo):boolean;
{IOR('F',2,sizeof(struct fbinfo) }

Begin
 FBIOGINFO:=FpIoctl(fd,nr_FBIOGINFO,@param1)=0;
end;

Function FBIOPUTCMAP(fd:longint;var param1 : fbcmap):boolean;
{IOW('F',3,sizeof(struct fbcmap) }

Begin
 FBIOPUTCMAP:=FpIoctl(fd,nr_FBIOPUTCMAP,@param1)=0;
end;

Function FBIOGETCMAP(fd:longint;var param1 : fbcmap):boolean;
{IOW('F',4,sizeof(struct fbcmap) }

Begin
 FBIOGETCMAP:=FpIoctl(fd,nr_FBIOGETCMAP,@param1)=0;
end;

Function FBIOGATTR(fd:longint;var param1 : fbgattr):boolean;
{IOR('F',6,sizeof(struct fbgattr) }

Begin
 FBIOGATTR:=FpIoctl(fd,nr_FBIOGATTR,@param1)=0;
end;

Function FBIOSVIDEO(fd:longint;var param1 : longint):boolean;
{IOW('F',7,sizeof(int) }

Begin
 FBIOSVIDEO:=FpIoctl(fd,nr_FBIOSVIDEO,@param1)=0;
end;

Function FBIOGVIDEO(fd:longint;var param1 : longint):boolean;
{IOR('F',8,sizeof(int) }

Begin
 FBIOGVIDEO:=FpIoctl(fd,nr_FBIOGVIDEO,@param1)=0;
end;

Function FBIOSCURSOR(fd:longint;var param1 : fbcursor):boolean;
{IOW('F',24,sizeof(struct fbcursor) }

Begin
 FBIOSCURSOR:=FpIoctl(fd,nr_FBIOSCURSOR,@param1)=0;
end;

Function FBIOGCURSOR(fd:longint;var param1 : fbcursor):boolean;
{IOWR('F',25,sizeof(struct fbcursor) }

Begin
 FBIOGCURSOR:=FpIoctl(fd,nr_FBIOGCURSOR,@param1)=0;
end;

Function FBIOSCURPOS(fd:longint;var param1 : fbcurpos):boolean;
{IOW('F',26,sizeof(struct fbcurpos) }

Begin
 FBIOSCURPOS:=FpIoctl(fd,nr_FBIOSCURPOS,@param1)=0;
end;

Function FBIOGCURPOS(fd:longint;var param1 : fbcurpos):boolean;
{IOW('F',27,sizeof(struct fbcurpos) }

Begin
 FBIOGCURPOS:=FpIoctl(fd,nr_FBIOGCURPOS,@param1)=0;
end;

Function FBIOGCURMAX(fd:longint;var param1 : fbcurpos):boolean;
{IOR('F',28,sizeof(struct fbcurpos) }

Begin
 FBIOGCURMAX:=FpIoctl(fd,nr_FBIOGCURMAX,@param1)=0;
end;

Function FBIO_ADAPTER(fd:longint;var param1 : longint):boolean;
{IOR('F',100,sizeof(int) }

Begin
 FBIO_ADAPTER:=FpIoctl(fd,nr_FBIO_ADAPTER,@param1)=0;
end;

Function FBIO_ADPTYPE(fd:longint;var param1 : longint):boolean;
{IOR('F',101,sizeof(int) }

Begin
 FBIO_ADPTYPE:=FpIoctl(fd,nr_FBIO_ADPTYPE,@param1)=0;
end;

Function FBIO_ADPINFO(fd:longint;var param1 : video_adapter_info):boolean;
{IOR('F',102,sizeof(struct video_adapter_info) }

Begin
 FBIO_ADPINFO:=FpIoctl(fd,nr_FBIO_ADPINFO,@param1)=0;
end;

Function FBIO_MODEINFO(fd:longint;var param1 : video_info):boolean;
{IOWR('F',103,sizeof(struct video_info) }

Begin
 FBIO_MODEINFO:=FpIoctl(fd,nr_FBIO_MODEINFO,@param1)=0;
end;

Function FBIO_FINDMODE(fd:longint;var param1 : video_info):boolean;
{IOWR('F',104,sizeof(struct video_info) }

Begin
 FBIO_FINDMODE:=FpIoctl(fd,nr_FBIO_FINDMODE,@param1)=0;
end;

Function FBIO_GETMODE(fd:longint;var param1 : longint):boolean;
{IOR('F',105,sizeof(int) }

Begin
 FBIO_GETMODE:=FpIoctl(fd,nr_FBIO_GETMODE,@param1)=0;
end;

Function FBIO_SETMODE(fd:longint;var param1 : longint):boolean;
{IOW('F',106,sizeof(int) }

Begin
 FBIO_SETMODE:=FpIoctl(fd,nr_FBIO_SETMODE,@param1)=0;
end;

Function FBIO_GETWINORG(fd:longint;var param1 : u_int):boolean;
{IOR('F',107,sizeof(u_int) }

Begin
 FBIO_GETWINORG:=FpIoctl(fd,nr_FBIO_GETWINORG,@param1)=0;
end;

Function FBIO_SETWINORG(fd:longint;var param1 : u_int):boolean;
{IOW('F',108,sizeof(u_int) }

Begin
 FBIO_SETWINORG:=FpIoctl(fd,nr_FBIO_SETWINORG,@param1)=0;
end;

Function FBIO_GETDISPSTART(fd:longint;var param1 : video_display_start_t):boolean;
{IOR('F',109,sizeof(video_display_start_t) }

Begin
 FBIO_GETDISPSTART:=FpIoctl(fd,nr_FBIO_GETDISPSTART,@param1)=0;
end;

Function FBIO_SETDISPSTART(fd:longint;var param1 : video_display_start_t):boolean;
{IOW('F',110,sizeof(video_display_start_t) }

Begin
 FBIO_SETDISPSTART:=FpIoctl(fd,nr_FBIO_SETDISPSTART,@param1)=0;
end;

Function FBIO_GETLINEWIDTH(fd:longint;var param1 : u_int):boolean;
{IOR('F',111,sizeof(u_int) }

Begin
 FBIO_GETLINEWIDTH:=FpIoctl(fd,nr_FBIO_GETLINEWIDTH,@param1)=0;
end;

Function FBIO_SETLINEWIDTH(fd:longint;var param1 : u_int):boolean;
{IOW('F',112,sizeof(u_int) }

Begin
 FBIO_SETLINEWIDTH:=FpIoctl(fd,nr_FBIO_SETLINEWIDTH,@param1)=0;
end;

Function FBIO_GETPALETTE(fd:longint;var param1 : video_color_palette_t):boolean;
{IOW('F',113,sizeof(video_color_palette_t) }

Begin
 FBIO_GETPALETTE:=FpIoctl(fd,nr_FBIO_GETPALETTE,@param1)=0;
end;

Function FBIO_SETPALETTE(fd:longint;var param1 : video_color_palette_t):boolean;
{IOW('F',114,sizeof(video_color_palette_t) }

Begin
 FBIO_SETPALETTE:=FpIoctl(fd,nr_FBIO_SETPALETTE,@param1)=0;
end;


{consio.h FpIoctl's}

Function KDGETMODE(fd:longint;var param1 : longint):boolean;
{IOR('K',9,sizeof(int) }

Begin
 KDGETMODE:=FpIoctl(fd,nr_KDGETMODE,@param1)=0;
end;

Function KDSETMODE(fd:longint;param1 : longint):boolean;
{IO('K',10 /* int */));
 }

Begin
 KDSETMODE:=FpIoctl(fd,nr_KDSETMODE,pointer(param1))=0;
end;

Function KDSBORDER(fd:longint;param1 : longint):boolean;
{IO('K',13 /* int */));
 }

Begin
 KDSBORDER:=FpIoctl(fd,nr_KDSBORDER,pointer(param1))=0;
end;

Function KDRASTER(fd:longint;var param1 : scr_size_t):boolean;
{IOW('K',100,sizeof(scr_size_t) }

Begin
 KDRASTER:=FpIoctl(fd,nr_KDRASTER,@param1)=0;
end;

Function GIO_SCRNMAP(fd:longint;var param1 : scrmap_t):boolean;
{IOR('k',2,sizeof(scrmap_t) }

Begin
 GIO_SCRNMAP:=FpIoctl(fd,nr_GIO_SCRNMAP,@param1)=0;
end;

Function PIO_SCRNMAP(fd:longint;var param1 : scrmap_t):boolean;
{IOW('k',3,sizeof(scrmap_t) }

Begin
 PIO_SCRNMAP:=FpIoctl(fd,nr_PIO_SCRNMAP,@param1)=0;
end;

Function GIO_ATTR(fd:longint;var param1 : longint):boolean;
{IOR('a',0,sizeof(int) }

Begin
 GIO_ATTR:=FpIoctl(fd,nr_GIO_ATTR,@param1)=0;
end;

Function GIO_COLOR(fd:longint;var param1 : longint):boolean;
{IOR('c',0,sizeof(int) }

Begin
 GIO_COLOR:=FpIoctl(fd,nr_GIO_COLOR,@param1)=0;
end;

Function CONS_CURRENT(fd:longint;var param1 : longint):boolean;
{IOR('c',1,sizeof(int) }

Begin
 CONS_CURRENT:=FpIoctl(fd,nr_CONS_CURRENT,@param1)=0;
end;

Function CONS_GET(fd:longint;var param1 : longint):boolean;
{IOR('c',2,sizeof(int) }

Begin
 CONS_GET:=FpIoctl(fd,nr_CONS_GET,@param1)=0;
end;

Function CONS_IO(fd:longint):boolean;
{IO('c',3));
 }

Begin
 CONS_IO:=FpIoctl(fd,nr_CONS_IO,nil)=0;
end;

Function CONS_BLANKTIME(fd:longint;var param1 : longint):boolean;
{IOW('c',4,sizeof(int) }

Begin
 CONS_BLANKTIME:=FpIoctl(fd,nr_CONS_BLANKTIME,@param1)=0;
end;

Function CONS_SSAVER(fd:longint;var param1 : ssaver_t):boolean;
{IOW('c',5,sizeof(ssaver_t) }

Begin
 CONS_SSAVER:=FpIoctl(fd,nr_CONS_SSAVER,@param1)=0;
end;

Function CONS_GSAVER(fd:longint;var param1 : ssaver_t):boolean;
{IOWR('c',6,sizeof(ssaver_t) }

Begin
 CONS_GSAVER:=FpIoctl(fd,nr_CONS_GSAVER,@param1)=0;
end;

Function CONS_CURSORTYPE(fd:longint;var param1 : longint):boolean;
{IOW('c',7,sizeof(int) }

Begin
 CONS_CURSORTYPE:=FpIoctl(fd,nr_CONS_CURSORTYPE,@param1)=0;
end;

Function CONS_BELLTYPE(fd:longint;var param1 : longint):boolean;
{IOW('c',8,sizeof(int) }

Begin
 CONS_BELLTYPE:=FpIoctl(fd,nr_CONS_BELLTYPE,@param1)=0;
end;

Function CONS_HISTORY(fd:longint;var param1 : longint):boolean;
{IOW('c',9,sizeof(int) }

Begin
 CONS_HISTORY:=FpIoctl(fd,nr_CONS_HISTORY,@param1)=0;
end;

Function CONS_MOUSECTL(fd:longint;var param1 : mouse_info_t):boolean;
{IOWR('c',10,sizeof(mouse_info_t) }

Begin
 CONS_MOUSECTL:=FpIoctl(fd,nr_CONS_MOUSECTL,@param1)=0;
end;

Function CONS_IDLE(fd:longint;var param1 : longint):boolean;
{IOR('c',11,sizeof(int) }

Begin
 CONS_IDLE:=FpIoctl(fd,nr_CONS_IDLE,@param1)=0;
end;

Function CONS_SAVERMODE(fd:longint;var param1 : longint):boolean;
{IOW('c',12,sizeof(int) }

Begin
 CONS_SAVERMODE:=FpIoctl(fd,nr_CONS_SAVERMODE,@param1)=0;
end;

Function CONS_SAVERSTART(fd:longint;var param1 : longint):boolean;
{IOW('c',13,sizeof(int) }

Begin
 CONS_SAVERSTART:=FpIoctl(fd,nr_CONS_SAVERSTART,@param1)=0;
end;

Function PIO_FONT8x8(fd:longint;var param1 : fnt8_t):boolean;
{IOW('c',64,sizeof(fnt8_t) }

Begin
 PIO_FONT8x8:=FpIoctl(fd,nr_PIO_FONT8x8,@param1)=0;
end;

Function GIO_FONT8x8(fd:longint;var param1 : fnt8_t):boolean;
{IOR('c',65,sizeof(fnt8_t) }

Begin
 GIO_FONT8x8:=FpIoctl(fd,nr_GIO_FONT8x8,@param1)=0;
end;

Function PIO_FONT8x14(fd:longint;var param1 : fnt14_t):boolean;
{IOW('c',66,sizeof(fnt14_t) }

Begin
 PIO_FONT8x14:=FpIoctl(fd,nr_PIO_FONT8x14,@param1)=0;
end;

Function GIO_FONT8x14(fd:longint;var param1 : fnt14_t):boolean;
{IOR('c',67,sizeof(fnt14_t) }

Begin
 GIO_FONT8x14:=FpIoctl(fd,nr_GIO_FONT8x14,@param1)=0;
end;

Function PIO_FONT8x16(fd:longint;var param1 : fnt16_t):boolean;
{IOW('c',68,sizeof(fnt16_t) }

Begin
 PIO_FONT8x16:=FpIoctl(fd,nr_PIO_FONT8x16,@param1)=0;
end;

Function GIO_FONT8x16(fd:longint;var param1 : fnt16_t):boolean;
{IOR('c',69,sizeof(fnt16_t) }

Begin
 GIO_FONT8x16:=FpIoctl(fd,nr_GIO_FONT8x16,@param1)=0;
end;

Function CONS_GETINFO(fd:longint;var param1 : vid_info_t):boolean;
{IOWR('c',73,sizeof(vid_info_t) }

Begin
 CONS_GETINFO:=FpIoctl(fd,nr_CONS_GETINFO,@param1)=0;
end;

Function CONS_GETVERS(fd:longint;var param1 : longint):boolean;
{IOR('c',74,sizeof(int) }

Begin
 CONS_GETVERS:=FpIoctl(fd,nr_CONS_GETVERS,@param1)=0;
end;

Function CONS_CURRENTADP(fd:longint;var param1 : longint):boolean;
{IOR('c',100,sizeof(int) }

Begin
 CONS_CURRENTADP:=FpIoctl(fd,nr_CONS_CURRENTADP,@param1)=0;
end;

Function CONS_ADPINFO(fd:longint;var param1 : video_adapter_info_t):boolean;
{IOWR('c',101,sizeof(video_adapter_info_t) }

Begin
 CONS_ADPINFO:=FpIoctl(fd,nr_CONS_ADPINFO,@param1)=0;
end;

Function CONS_MODEINFO(fd:longint;var param1 : video_info_t):boolean;
{IOWR('c',102,sizeof(video_info_t) }

Begin
 CONS_MODEINFO:=FpIoctl(fd,nr_CONS_MODEINFO,@param1)=0;
end;

Function CONS_FINDMODE(fd:longint;var param1 : video_info_t):boolean;
{IOWR('c',103,sizeof(video_info_t) }

Begin
 CONS_FINDMODE:=FpIoctl(fd,nr_CONS_FINDMODE,@param1)=0;
end;

Function CONS_SETWINORG(fd:longint;param1 : longint):boolean;
{IO('c',104 /* int */));
 }

Begin
 CONS_SETWINORG:=FpIoctl(fd,nr_CONS_SETWINORG,pointer(param1))=0;
end;

Function CONS_SETKBD(fd:longint;param1 : longint):boolean;
{IO('c',110 /* int */));
 }

Begin
 CONS_SETKBD:=FpIoctl(fd,nr_CONS_SETKBD,pointer(param1))=0;
end;

Function CONS_RELKBD(fd:longint):boolean;
{IO('c',111));
 }

Begin
 CONS_RELKBD:=FpIoctl(fd,nr_CONS_RELKBD,nil)=0;
end;

Function CONS_GETTERM(fd:longint;var param1 : term_info_t):boolean;
{IOWR('c',112,sizeof(term_info_t) }

Begin
 CONS_GETTERM:=FpIoctl(fd,nr_CONS_GETTERM,@param1)=0;
end;

Function CONS_SETTERM(fd:longint;var param1 : term_info_t):boolean;
{IOW('c',113,sizeof(term_info_t) }

Begin
 CONS_SETTERM:=FpIoctl(fd,nr_CONS_SETTERM,@param1)=0;
end;

Function ADJUST_CLOCK(fd:longint):boolean;
{IO('t',100));
 }

Begin
 ADJUST_CLOCK:=FpIoctl(fd,nr_ADJUST_CLOCK,nil)=0;
end;

Function VT_OPENQRY(fd:longint;var param1 : longint):boolean;
{IOR('v',1,sizeof(int) }

Begin
 VT_OPENQRY:=FpIoctl(fd,nr_VT_OPENQRY,@param1)=0;
end;

Function VT_SETMODE(fd:longint;var param1 : vtmode_t):boolean;
{IOW('v',2,sizeof(vtmode_t) }

Begin
 VT_SETMODE:=FpIoctl(fd,nr_VT_SETMODE,@param1)=0;
end;

Function VT_GETMODE(fd:longint;var param1 : vtmode_t):boolean;
{IOR('v',3,sizeof(vtmode_t) }

Begin
 VT_GETMODE:=FpIoctl(fd,nr_VT_GETMODE,@param1)=0;
end;

Function VT_RELDISP(fd:longint;param1 : longint):boolean;
{IO('v',4 /* int */));
 }

Begin
 VT_RELDISP:=FpIoctl(fd,nr_VT_RELDISP,pointer(param1))=0;
end;

Function VT_ACTIVATE(fd:longint;param1 : longint):boolean;
{IO('v',5 /* int */));
 }

Begin
 VT_ACTIVATE:=FpIoctl(fd,nr_VT_ACTIVATE,pointer(param1))=0;
end;

Function VT_WAITACTIVE(fd:longint;param1 : longint):boolean;
{IO('v',6 /* int */));
 }

Begin
 VT_WAITACTIVE:=FpIoctl(fd,nr_VT_WAITACTIVE,pointer(param1))=0;
end;

Function VT_GETACTIVE(fd:longint;var param1 : longint):boolean;
{IOR('v',7,sizeof(int) }

Begin
 VT_GETACTIVE:=FpIoctl(fd,nr_VT_GETACTIVE,@param1)=0;
end;

Function VT_GETINDEX(fd:longint;var param1 : longint):boolean;
{IOR('v',8,sizeof(int) }

Begin
 VT_GETINDEX:=FpIoctl(fd,nr_VT_GETINDEX,@param1)=0;
end;

Function SW_B40x25(fd:longint):boolean;
{IO('S',M_B40x25));
 }

Begin
 SW_B40x25:=FpIoctl(fd,nr_SW_B40x25,nil)=0;
end;

Function SW_C40x25(fd:longint):boolean;
{IO('S',M_C40x25));
 }

Begin
 SW_C40x25:=FpIoctl(fd,nr_SW_C40x25,nil)=0;
end;

Function SW_B80x25(fd:longint):boolean;
{IO('S',M_B80x25));
 }

Begin
 SW_B80x25:=FpIoctl(fd,nr_SW_B80x25,nil)=0;
end;

Function SW_C80x25(fd:longint):boolean;
{IO('S',M_C80x25));
 }

Begin
 SW_C80x25:=FpIoctl(fd,nr_SW_C80x25,nil)=0;
end;

Function SW_BG320(fd:longint):boolean;
{IO('S',M_BG320));
 }

Begin
 SW_BG320:=FpIoctl(fd,nr_SW_BG320,nil)=0;
end;

Function SW_CG320(fd:longint):boolean;
{IO('S',M_CG320));
 }

Begin
 SW_CG320:=FpIoctl(fd,nr_SW_CG320,nil)=0;
end;

Function SW_BG640(fd:longint):boolean;
{IO('S',M_BG640));
 }

Begin
 SW_BG640:=FpIoctl(fd,nr_SW_BG640,nil)=0;
end;

Function SW_EGAMONO80x25(fd:longint):boolean;
{IO('S',M_EGAMONO80x25));
 }

Begin
 SW_EGAMONO80x25:=FpIoctl(fd,nr_SW_EGAMONO80x25,nil)=0;
end;

Function SW_CG320_D(fd:longint):boolean;
{IO('S',M_CG320_D));
 }

Begin
 SW_CG320_D:=FpIoctl(fd,nr_SW_CG320_D,nil)=0;
end;

Function SW_CG640_E(fd:longint):boolean;
{IO('S',M_CG640_E));
 }

Begin
 SW_CG640_E:=FpIoctl(fd,nr_SW_CG640_E,nil)=0;
end;

Function SW_EGAMONOAPA(fd:longint):boolean;
{IO('S',M_EGAMONOAPA));
 }

Begin
 SW_EGAMONOAPA:=FpIoctl(fd,nr_SW_EGAMONOAPA,nil)=0;
end;

Function SW_CG640x350(fd:longint):boolean;
{IO('S',M_CG640x350));
 }

Begin
 SW_CG640x350:=FpIoctl(fd,nr_SW_CG640x350,nil)=0;
end;

Function SW_ENH_MONOAPA2(fd:longint):boolean;
{IO('S',M_ENHMONOAPA2));
 }

Begin
 SW_ENH_MONOAPA2:=FpIoctl(fd,nr_SW_ENH_MONOAPA2,nil)=0;
end;

Function SW_ENH_CG640(fd:longint):boolean;
{IO('S',M_ENH_CG640));
 }

Begin
 SW_ENH_CG640:=FpIoctl(fd,nr_SW_ENH_CG640,nil)=0;
end;

Function SW_ENH_B40x25(fd:longint):boolean;
{IO('S',M_ENH_B40x25));
 }

Begin
 SW_ENH_B40x25:=FpIoctl(fd,nr_SW_ENH_B40x25,nil)=0;
end;

Function SW_ENH_C40x25(fd:longint):boolean;
{IO('S',M_ENH_C40x25));
 }

Begin
 SW_ENH_C40x25:=FpIoctl(fd,nr_SW_ENH_C40x25,nil)=0;
end;

Function SW_ENH_B80x25(fd:longint):boolean;
{IO('S',M_ENH_B80x25));
 }

Begin
 SW_ENH_B80x25:=FpIoctl(fd,nr_SW_ENH_B80x25,nil)=0;
end;

Function SW_ENH_C80x25(fd:longint):boolean;
{IO('S',M_ENH_C80x25));
 }

Begin
 SW_ENH_C80x25:=FpIoctl(fd,nr_SW_ENH_C80x25,nil)=0;
end;

Function SW_ENH_B80x43(fd:longint):boolean;
{IO('S',M_ENH_B80x43));
 }

Begin
 SW_ENH_B80x43:=FpIoctl(fd,nr_SW_ENH_B80x43,nil)=0;
end;

Function SW_ENH_C80x43(fd:longint):boolean;
{IO('S',M_ENH_C80x43));
 }

Begin
 SW_ENH_C80x43:=FpIoctl(fd,nr_SW_ENH_C80x43,nil)=0;
end;

Function SW_MCAMODE(fd:longint):boolean;
{IO('S',M_MCA_MODE));
 }

Begin
 SW_MCAMODE:=FpIoctl(fd,nr_SW_MCAMODE,nil)=0;
end;

Function SW_VGA_C40x25(fd:longint):boolean;
{IO('S',M_VGA_C40x25));
 }

Begin
 SW_VGA_C40x25:=FpIoctl(fd,nr_SW_VGA_C40x25,nil)=0;
end;

Function SW_VGA_C80x25(fd:longint):boolean;
{IO('S',M_VGA_C80x25));
 }

Begin
 SW_VGA_C80x25:=FpIoctl(fd,nr_SW_VGA_C80x25,nil)=0;
end;

Function SW_VGA_C80x30(fd:longint):boolean;
{IO('S',M_VGA_C80x30));
 }

Begin
 SW_VGA_C80x30:=FpIoctl(fd,nr_SW_VGA_C80x30,nil)=0;
end;

Function SW_VGA_C80x50(fd:longint):boolean;
{IO('S',M_VGA_C80x50));
 }

Begin
 SW_VGA_C80x50:=FpIoctl(fd,nr_SW_VGA_C80x50,nil)=0;
end;

Function SW_VGA_C80x60(fd:longint):boolean;
{IO('S',M_VGA_C80x60));
 }

Begin
 SW_VGA_C80x60:=FpIoctl(fd,nr_SW_VGA_C80x60,nil)=0;
end;

Function SW_VGA_M80x25(fd:longint):boolean;
{IO('S',M_VGA_M80x25));
 }

Begin
 SW_VGA_M80x25:=FpIoctl(fd,nr_SW_VGA_M80x25,nil)=0;
end;

Function SW_VGA_M80x30(fd:longint):boolean;
{IO('S',M_VGA_M80x30));
 }

Begin
 SW_VGA_M80x30:=FpIoctl(fd,nr_SW_VGA_M80x30,nil)=0;
end;

Function SW_VGA_M80x50(fd:longint):boolean;
{IO('S',M_VGA_M80x50));
 }

Begin
 SW_VGA_M80x50:=FpIoctl(fd,nr_SW_VGA_M80x50,nil)=0;
end;

Function SW_VGA_M80x60(fd:longint):boolean;
{IO('S',M_VGA_M80x60));
 }

Begin
 SW_VGA_M80x60:=FpIoctl(fd,nr_SW_VGA_M80x60,nil)=0;
end;

Function SW_VGA11(fd:longint):boolean;
{IO('S',M_VGA11));
 }

Begin
 SW_VGA11:=FpIoctl(fd,nr_SW_VGA11,nil)=0;
end;

Function SW_BG640x480(fd:longint):boolean;
{IO('S',M_VGA11));
 }

Begin
 SW_BG640x480:=FpIoctl(fd,nr_SW_BG640x480,nil)=0;
end;

Function SW_VGA12(fd:longint):boolean;
{IO('S',M_VGA12));
 }

Begin
 SW_VGA12:=FpIoctl(fd,nr_SW_VGA12,nil)=0;
end;

Function SW_CG640x480(fd:longint):boolean;
{IO('S',M_VGA12));
 }

Begin
 SW_CG640x480:=FpIoctl(fd,nr_SW_CG640x480,nil)=0;
end;

Function SW_VGA13(fd:longint):boolean;
{IO('S',M_VGA13));
 }

Begin
 SW_VGA13:=FpIoctl(fd,nr_SW_VGA13,nil)=0;
end;

Function SW_VGA_CG320(fd:longint):boolean;
{IO('S',M_VGA13));
 }

Begin
 SW_VGA_CG320:=FpIoctl(fd,nr_SW_VGA_CG320,nil)=0;
end;

Function SW_VGA_CG640(fd:longint):boolean;
{IO('S',M_VGA_CG640));
 }

Begin
 SW_VGA_CG640:=FpIoctl(fd,nr_SW_VGA_CG640,nil)=0;
end;

Function SW_VGA_MODEX(fd:longint):boolean;
{IO('S',M_VGA_MODEX));
 }

Begin
 SW_VGA_MODEX:=FpIoctl(fd,nr_SW_VGA_MODEX,nil)=0;
end;

Function SW_PC98_80x25(fd:longint):boolean;
{IO('S',M_PC98_80x25));
 }

Begin
 SW_PC98_80x25:=FpIoctl(fd,nr_SW_PC98_80x25,nil)=0;
end;

Function SW_PC98_80x30(fd:longint):boolean;
{IO('S',M_PC98_80x30));
 }

Begin
 SW_PC98_80x30:=FpIoctl(fd,nr_SW_PC98_80x30,nil)=0;
end;

Function SW_PC98_EGC640x400(fd:longint):boolean;
{IO('S',M_PC98_EGC640x400));
 }

Begin
 SW_PC98_EGC640x400:=FpIoctl(fd,nr_SW_PC98_EGC640x400,nil)=0;
end;

Function SW_PC98_PEGC640x400(fd:longint):boolean;
{IO('S',M_PC98_PEGC640x400));
 }

Begin
 SW_PC98_PEGC640x400:=FpIoctl(fd,nr_SW_PC98_PEGC640x400,nil)=0;
end;

Function SW_PC98_PEGC640x480(fd:longint):boolean;
{IO('S',M_PC98_PEGC640x480));
 }

Begin
 SW_PC98_PEGC640x480:=FpIoctl(fd,nr_SW_PC98_PEGC640x480,nil)=0;
end;

Function SW_VGA_C90x25(fd:longint):boolean;
{IO('S',M_VGA_C90x25));
 }

Begin
 SW_VGA_C90x25:=FpIoctl(fd,nr_SW_VGA_C90x25,nil)=0;
end;

Function SW_VGA_M90x25(fd:longint):boolean;
{IO('S',M_VGA_M90x25));
 }

Begin
 SW_VGA_M90x25:=FpIoctl(fd,nr_SW_VGA_M90x25,nil)=0;
end;

Function SW_VGA_C90x30(fd:longint):boolean;
{IO('S',M_VGA_C90x30));
 }

Begin
 SW_VGA_C90x30:=FpIoctl(fd,nr_SW_VGA_C90x30,nil)=0;
end;

Function SW_VGA_M90x30(fd:longint):boolean;
{IO('S',M_VGA_M90x30));
 }

Begin
 SW_VGA_M90x30:=FpIoctl(fd,nr_SW_VGA_M90x30,nil)=0;
end;

Function SW_VGA_C90x43(fd:longint):boolean;
{IO('S',M_VGA_C90x43));
 }

Begin
 SW_VGA_C90x43:=FpIoctl(fd,nr_SW_VGA_C90x43,nil)=0;
end;

Function SW_VGA_M90x43(fd:longint):boolean;
{IO('S',M_VGA_M90x43));
 }

Begin
 SW_VGA_M90x43:=FpIoctl(fd,nr_SW_VGA_M90x43,nil)=0;
end;

Function SW_VGA_C90x50(fd:longint):boolean;
{IO('S',M_VGA_C90x50));
 }

Begin
 SW_VGA_C90x50:=FpIoctl(fd,nr_SW_VGA_C90x50,nil)=0;
end;

Function SW_VGA_M90x50(fd:longint):boolean;
{IO('S',M_VGA_M90x50));
 }

Begin
 SW_VGA_M90x50:=FpIoctl(fd,nr_SW_VGA_M90x50,nil)=0;
end;

Function SW_VGA_C90x60(fd:longint):boolean;
{IO('S',M_VGA_C90x60));
 }

Begin
 SW_VGA_C90x60:=FpIoctl(fd,nr_SW_VGA_C90x60,nil)=0;
end;

Function SW_VGA_M90x60(fd:longint):boolean;
{IO('S',M_VGA_M90x60));
 }

Begin
 SW_VGA_M90x60:=FpIoctl(fd,nr_SW_VGA_M90x60,nil)=0;
end;

Function SW_TEXT_80x25(fd:longint):boolean;
{IO('S',M_TEXT_80x25));
 }

Begin
 SW_TEXT_80x25:=FpIoctl(fd,nr_SW_TEXT_80x25,nil)=0;
end;

Function SW_TEXT_80x30(fd:longint):boolean;
{IO('S',M_TEXT_80x30));
 }

Begin
 SW_TEXT_80x30:=FpIoctl(fd,nr_SW_TEXT_80x30,nil)=0;
end;

Function SW_TEXT_80x43(fd:longint):boolean;
{IO('S',M_TEXT_80x43));
 }

Begin
 SW_TEXT_80x43:=FpIoctl(fd,nr_SW_TEXT_80x43,nil)=0;
end;

Function SW_TEXT_80x50(fd:longint):boolean;
{IO('S',M_TEXT_80x50));
 }

Begin
 SW_TEXT_80x50:=FpIoctl(fd,nr_SW_TEXT_80x50,nil)=0;
end;

Function SW_TEXT_80x60(fd:longint):boolean;
{IO('S',M_TEXT_80x60));
 }

Begin
 SW_TEXT_80x60:=FpIoctl(fd,nr_SW_TEXT_80x60,nil)=0;
end;

Function SW_TEXT_132x25(fd:longint):boolean;
{IO('S',M_TEXT_132x25));
 }

Begin
 SW_TEXT_132x25:=FpIoctl(fd,nr_SW_TEXT_132x25,nil)=0;
end;

Function SW_TEXT_132x30(fd:longint):boolean;
{IO('S',M_TEXT_132x30));
 }

Begin
 SW_TEXT_132x30:=FpIoctl(fd,nr_SW_TEXT_132x30,nil)=0;
end;

Function SW_TEXT_132x43(fd:longint):boolean;
{IO('S',M_TEXT_132x43));
 }

Begin
 SW_TEXT_132x43:=FpIoctl(fd,nr_SW_TEXT_132x43,nil)=0;
end;

Function SW_TEXT_132x50(fd:longint):boolean;
{IO('S',M_TEXT_132x50));
 }

Begin
 SW_TEXT_132x50:=FpIoctl(fd,nr_SW_TEXT_132x50,nil)=0;
end;

Function SW_TEXT_132x60(fd:longint):boolean;
{IO('S',M_TEXT_132x60));
 }

Begin
 SW_TEXT_132x60:=FpIoctl(fd,nr_SW_TEXT_132x60,nil)=0;
end;

Function SW_VESA_CG640x400(fd:longint):boolean;
{IO('V',M_VESA_CG640x400 - M_VESA_BASE));
 }

Begin
 SW_VESA_CG640x400:=FpIoctl(fd,nr_SW_VESA_CG640x400,nil)=0;
end;

Function SW_VESA_CG640x480(fd:longint):boolean;
{IO('V',M_VESA_CG640x480 - M_VESA_BASE));
 }

Begin
 SW_VESA_CG640x480:=FpIoctl(fd,nr_SW_VESA_CG640x480,nil)=0;
end;

Function SW_VESA_800x600(fd:longint):boolean;
{IO('V',M_VESA_800x600 - M_VESA_BASE));
 }

Begin
 SW_VESA_800x600:=FpIoctl(fd,nr_SW_VESA_800x600,nil)=0;
end;

Function SW_VESA_CG800x600(fd:longint):boolean;
{IO('V',M_VESA_CG800x600 - M_VESA_BASE));
 }

Begin
 SW_VESA_CG800x600:=FpIoctl(fd,nr_SW_VESA_CG800x600,nil)=0;
end;

Function SW_VESA_1024x768(fd:longint):boolean;
{IO('V',M_VESA_1024x768 - M_VESA_BASE));
 }

Begin
 SW_VESA_1024x768:=FpIoctl(fd,nr_SW_VESA_1024x768,nil)=0;
end;

Function SW_VESA_CG1024x768(fd:longint):boolean;
{IO('V',M_VESA_CG1024x768 - M_VESA_BAS));
 }

Begin
 SW_VESA_CG1024x768:=FpIoctl(fd,nr_SW_VESA_CG1024x768,nil)=0;
end;

Function SW_VESA_1280x1024(fd:longint):boolean;
{IO('V',M_VESA_1280x1024 - M_VESA_BASE));
 }

Begin
 SW_VESA_1280x1024:=FpIoctl(fd,nr_SW_VESA_1280x1024,nil)=0;
end;

Function SW_VESA_CG1280x1024(fd:longint):boolean;
{IO('V',M_VESA_CG1280x1024 - M_VESA_BA));
 }

Begin
 SW_VESA_CG1280x1024:=FpIoctl(fd,nr_SW_VESA_CG1280x1024,nil)=0;
end;

Function SW_VESA_C80x60(fd:longint):boolean;
{IO('V',M_VESA_C80x60 - M_VESA_BASE));
 }

Begin
 SW_VESA_C80x60:=FpIoctl(fd,nr_SW_VESA_C80x60,nil)=0;
end;

Function SW_VESA_C132x25(fd:longint):boolean;
{IO('V',M_VESA_C132x25 - M_VESA_BASE));
 }

Begin
 SW_VESA_C132x25:=FpIoctl(fd,nr_SW_VESA_C132x25,nil)=0;
end;

Function SW_VESA_C132x43(fd:longint):boolean;
{IO('V',M_VESA_C132x43 - M_VESA_BASE));
 }

Begin
 SW_VESA_C132x43:=FpIoctl(fd,nr_SW_VESA_C132x43,nil)=0;
end;

Function SW_VESA_C132x50(fd:longint):boolean;
{IO('V',M_VESA_C132x50 - M_VESA_BASE));
 }

Begin
 SW_VESA_C132x50:=FpIoctl(fd,nr_SW_VESA_C132x50,nil)=0;
end;

Function SW_VESA_C132x60(fd:longint):boolean;
{IO('V',M_VESA_C132x60 - M_VESA_BASE));
 }

Begin
 SW_VESA_C132x60:=FpIoctl(fd,nr_SW_VESA_C132x60,nil)=0;
end;

Function SW_VESA_32K_320(fd:longint):boolean;
{IO('V',M_VESA_32K_320 - M_VESA_BASE));
 }

Begin
 SW_VESA_32K_320:=FpIoctl(fd,nr_SW_VESA_32K_320,nil)=0;
end;

Function SW_VESA_64K_320(fd:longint):boolean;
{IO('V',M_VESA_64K_320 - M_VESA_BASE));
 }

Begin
 SW_VESA_64K_320:=FpIoctl(fd,nr_SW_VESA_64K_320,nil)=0;
end;

Function SW_VESA_FULL_320(fd:longint):boolean;
{IO('V',M_VESA_FULL_320 - M_VESA_BASE));
 }

Begin
 SW_VESA_FULL_320:=FpIoctl(fd,nr_SW_VESA_FULL_320,nil)=0;
end;

Function SW_VESA_32K_640(fd:longint):boolean;
{IO('V',M_VESA_32K_640 - M_VESA_BASE));
 }

Begin
 SW_VESA_32K_640:=FpIoctl(fd,nr_SW_VESA_32K_640,nil)=0;
end;

Function SW_VESA_64K_640(fd:longint):boolean;
{IO('V',M_VESA_64K_640 - M_VESA_BASE));
 }

Begin
 SW_VESA_64K_640:=FpIoctl(fd,nr_SW_VESA_64K_640,nil)=0;
end;

Function SW_VESA_FULL_640(fd:longint):boolean;
{IO('V',M_VESA_FULL_640 - M_VESA_BASE));
 }

Begin
 SW_VESA_FULL_640:=FpIoctl(fd,nr_SW_VESA_FULL_640,nil)=0;
end;

Function SW_VESA_32K_800(fd:longint):boolean;
{IO('V',M_VESA_32K_800 - M_VESA_BASE));
 }

Begin
 SW_VESA_32K_800:=FpIoctl(fd,nr_SW_VESA_32K_800,nil)=0;
end;

Function SW_VESA_64K_800(fd:longint):boolean;
{IO('V',M_VESA_64K_800 - M_VESA_BASE));
 }

Begin
 SW_VESA_64K_800:=FpIoctl(fd,nr_SW_VESA_64K_800,nil)=0;
end;

Function SW_VESA_FULL_800(fd:longint):boolean;
{IO('V',M_VESA_FULL_800 - M_VESA_BASE));
 }

Begin
 SW_VESA_FULL_800:=FpIoctl(fd,nr_SW_VESA_FULL_800,nil)=0;
end;

Function SW_VESA_32K_1024(fd:longint):boolean;
{IO('V',M_VESA_32K_1024 - M_VESA_BASE));
 }

Begin
 SW_VESA_32K_1024:=FpIoctl(fd,nr_SW_VESA_32K_1024,nil)=0;
end;

Function SW_VESA_64K_1024(fd:longint):boolean;
{IO('V',M_VESA_64K_1024 - M_VESA_BASE));
 }

Begin
 SW_VESA_64K_1024:=FpIoctl(fd,nr_SW_VESA_64K_1024,nil)=0;
end;

Function SW_VESA_FULL_1024(fd:longint):boolean;
{IO('V',M_VESA_FULL_1024 - M_VESA_BASE));
 }

Begin
 SW_VESA_FULL_1024:=FpIoctl(fd,nr_SW_VESA_FULL_1024,nil)=0;
end;

Function SW_VESA_32K_1280(fd:longint):boolean;
{IO('V',M_VESA_32K_1280 - M_VESA_BASE));
 }

Begin
 SW_VESA_32K_1280:=FpIoctl(fd,nr_SW_VESA_32K_1280,nil)=0;
end;

Function SW_VESA_64K_1280(fd:longint):boolean;
{IO('V',M_VESA_64K_1280 - M_VESA_BASE));
 }

Begin
 SW_VESA_64K_1280:=FpIoctl(fd,nr_SW_VESA_64K_1280,nil)=0;
end;

Function SW_VESA_FULL_1280(fd:longint):boolean;
{IO('V',M_VESA_FULL_1280 - M_VESA_BASE));
 }

Begin
 SW_VESA_FULL_1280:=FpIoctl(fd,nr_SW_VESA_FULL_1280,nil)=0;
end;

{----------------------------- kbio.h FpIoctl's ---------------------------}

Function KDGKBMODE(fd:longint;var param1 : longint):boolean;
{IOR('K',6,sizeof(int) }

Begin
 KDGKBMODE:=FpIoctl(fd,nr_KDGKBMODE,@param1)=0;
end;

Function KDSKBMODE(fd:longint;param1 : longint):boolean;
{IO('K',7 /* int */));
 }

Begin
 KDSKBMODE:=FpIoctl(fd,nr_KDSKBMODE,pointer(param1))=0;
end;

Function KDMKTONE(fd:longint;param1 : longint):boolean;
{IO('K',8 /* int */));
 }

Begin
 KDMKTONE:=FpIoctl(fd,nr_KDMKTONE,pointer(param1))=0;
end;

{$ifndef definconsole}
Function KDGETMODE(fd:longint;var param1 : longint):boolean;
{IOR('K',9,sizeof(int) }

Begin
 KDGETMODE:=FpIoctl(fd,nr_KDGETMODE,@param1)=0;
end;

Function KDSETMODE(fd:longint;param1 : longint):boolean;
{IO('K',10 /* int */));
 }

Begin
 KDSETMODE:=FpIoctl(fd,nr_KDSETMODE,pointer(param1))=0;
end;

Function KDSBORDER(fd:longint;param1 : longint):boolean;
{IO('K',13 /* int */));
 }

Begin
 KDSBORDER:=FpIoctl(fd,nr_KDSBORDER,pointer(param1))=0;
end;
{$endif}
Function KDGKBSTATE(fd:longint;var param1 : longint):boolean;
{IOR('K',19,sizeof(int) }

Begin
 KDGKBSTATE:=FpIoctl(fd,nr_KDGKBSTATE,@param1)=0;
end;

Function KDSKBSTATE(fd:longint;param1 : longint):boolean;
{IO('K',20 /* int */));
 }

Begin
 KDSKBSTATE:=FpIoctl(fd,nr_KDSKBSTATE,pointer(param1))=0;
end;

Function KDENABIO(fd:longint):boolean;
{IO('K',60));
 }

Begin
 KDENABIO:=FpIoctl(fd,nr_KDENABIO,nil)=0;
end;

Function KDDISABIO(fd:longint):boolean;
{IO('K',61));
 }

Begin
 KDDISABIO:=FpIoctl(fd,nr_KDDISABIO,nil)=0;
end;

Function KIOCSOUND(fd:longint;param1 : longint):boolean;
{IO('K',63 /* int */));
 }

Begin
 KIOCSOUND:=FpIoctl(fd,nr_KIOCSOUND,pointer(param1))=0;
end;

Function KDGKBTYPE(fd:longint;var param1 : longint):boolean;
{IOR('K',64,sizeof(int) }

Begin
 KDGKBTYPE:=FpIoctl(fd,nr_KDGKBTYPE,@param1)=0;
end;

Function KDGETLED(fd:longint;var param1 : longint):boolean;
{IOR('K',65,sizeof(int) }

Begin
 KDGETLED:=FpIoctl(fd,nr_KDGETLED,@param1)=0;
end;

Function KDSETLED(fd:longint;param1 : longint):boolean;
{IO('K',66 /* int */));
 }

Begin
 KDSETLED:=FpIoctl(fd,nr_KDSETLED,pointer(param1))=0;
end;

Function KDSETRAD(fd:longint;param1 : longint):boolean;
{IO('K',67 /* int */));
 }

Begin
 KDSETRAD:=FpIoctl(fd,nr_KDSETRAD,pointer(param1))=0;
end;
{$ifndef definconsole}

Function KDRASTER(fd:longint;var param1 : scr_size_t):boolean;
{IOW('K',100,sizeof(scr_size_t) }

Begin
 KDRASTER:=FpIoctl(fd,nr_KDRASTER,@param1)=0;
end;
{$endif}
Function KDGKBINFO(fd:longint;var param1 : keyboard_info_t):boolean;
{IOR('K',101,sizeof(keyboard_info_t) }

Begin
 KDGKBINFO:=FpIoctl(fd,nr_KDGKBINFO,@param1)=0;
end;

Function KDSETREPEAT(fd:longint;var param1 : keyboard_repeat_t):boolean;
{IOW('K',102,sizeof(keyboard_repeat_t) }

Begin
 KDSETREPEAT:=FpIoctl(fd,nr_KDSETREPEAT,@param1)=0;
end;

Function KDGETREPEAT(fd:longint;var param1 : keyboard_repeat_t):boolean;
{IOR('K',103,sizeof(keyboard_repeat_t) }

Begin
 KDGETREPEAT:=FpIoctl(fd,nr_KDGETREPEAT,@param1)=0;
end;

Function GETFKEY(fd:longint;var param1 : fkeyarg_t):boolean;
{IOWR('k',0,sizeof(fkeyarg_t) }

Begin
 GETFKEY:=FpIoctl(fd,nr_GETFKEY,@param1)=0;
end;

Function SETFKEY(fd:longint;var param1 : fkeyarg_t):boolean;
{IOWR('k',1,sizeof(fkeyarg_t) }

Begin
 SETFKEY:=FpIoctl(fd,nr_SETFKEY,@param1)=0;
end;
{$ifndef definconsole}
Function GIO_SCRNMAP(fd:longint;var param1 : scrmap_t):boolean;
{IOR('k',2,sizeof(scrmap_t) }

Begin
 GIO_SCRNMAP:=FpIoctl(fd,nr_GIO_SCRNMAP,@param1)=0;
end;

Function PIO_SCRNMAP(fd:longint;var param1 : scrmap_t):boolean;
{IOW('k',3,sizeof(scrmap_t) }

Begin
 PIO_SCRNMAP:=FpIoctl(fd,nr_PIO_SCRNMAP,@param1)=0;
end;
{$endif}
Function GIO_KEYMAP(fd:longint;var param1 : keymap_t):boolean;
{IOR('k',6,sizeof(keymap_t) }

Begin
 GIO_KEYMAP:=FpIoctl(fd,nr_GIO_KEYMAP,@param1)=0;
end;

Function PIO_KEYMAP(fd:longint;var param1 : keymap_t):boolean;
{IOW('k',7,sizeof(keymap_t) }

Begin
 PIO_KEYMAP:=FpIoctl(fd,nr_PIO_KEYMAP,@param1)=0;
end;

Function GIO_DEADKEYMAP(fd:longint;var param1 : accentmap_t):boolean;
{IOR('k',8,sizeof(accentmap_t) }

Begin
 GIO_DEADKEYMAP:=FpIoctl(fd,nr_GIO_DEADKEYMAP,@param1)=0;
end;

Function PIO_DEADKEYMAP(fd:longint;var param1 : accentmap_t):boolean;
{IOW('k',9,sizeof(accentmap_t) }

Begin
 PIO_DEADKEYMAP:=FpIoctl(fd,nr_PIO_DEADKEYMAP,@param1)=0;
end;

Function GIO_KEYMAPENT(fd:longint;var param1 : keyarg_t):boolean;
{IOWR('k',10,sizeof(keyarg_t) }

Begin
 GIO_KEYMAPENT:=FpIoctl(fd,nr_GIO_KEYMAPENT,@param1)=0;
end;

Function PIO_KEYMAPENT(fd:longint;var param1 : keyarg_t):boolean;
{IOW('k',11,sizeof(keyarg_t) }

Begin
 PIO_KEYMAPENT:=FpIoctl(fd,nr_PIO_KEYMAPENT,@param1)=0;
end;



{----------------------------- mouse.h FpIoctl's ---------------------------}

Function MOUSE_GETSTATUS(fd:longint;var param1 : mousestatus_t):boolean;
{IOR('M',0,sizeof(mousestatus_t)));
}

Begin
 MOUSE_GETSTATUS:=FpIoctl(fd,nr_MOUSE_GETSTATUS,@param1)=0;
end;

Function MOUSE_GETHWINFO(fd:longint;var param1 : mousehw_t):boolean;
{IOR('M',1,sizeof(mousehw_t)));
}

Begin
 MOUSE_GETHWINFO:=FpIoctl(fd,nr_MOUSE_GETHWINFO,@param1)=0;
end;

Function MOUSE_GETMODE(fd:longint;var param1 : mousemode_t):boolean;
{IOR('M',2,sizeof(mousemode_t)));
}

Begin
 MOUSE_GETMODE:=FpIoctl(fd,nr_MOUSE_GETMODE,@param1)=0;
end;

Function MOUSE_SETMODE(fd:longint;var param1 : mousemode_t):boolean;
{IOW('M',3,sizeof(mousemode_t)));
}

Begin
 MOUSE_SETMODE:=FpIoctl(fd,nr_MOUSE_SETMODE,@param1)=0;
end;

Function MOUSE_GETLEVEL(fd:longint;var param1 : longint):boolean;
{IOR('M',4,sizeof(int)));
}

Begin
 MOUSE_GETLEVEL:=FpIoctl(fd,nr_MOUSE_GETLEVEL,@param1)=0;
end;

Function MOUSE_SETLEVEL(fd:longint;var param1 : longint):boolean;
{IOW('M',5,sizeof(int)));
}

Begin
 MOUSE_SETLEVEL:=FpIoctl(fd,nr_MOUSE_SETLEVEL,@param1)=0;
end;

Function MOUSE_GETVARS(fd:longint;var param1 : mousevar_t):boolean;
{IOR('M',6,sizeof(mousevar_t)));
}

Begin
 MOUSE_GETVARS:=FpIoctl(fd,nr_MOUSE_GETVARS,@param1)=0;
end;

Function MOUSE_SETVARS(fd:longint;var param1 : mousevar_t):boolean;
{IOW('M',7,sizeof(mousevar_t)));
}

Begin
 MOUSE_SETVARS:=FpIoctl(fd,nr_MOUSE_SETVARS,@param1)=0;
end;

Function MOUSE_READSTATE(fd:longint;var param1 : mousedata_t):boolean;
{IOWR('M',8,sizeof(mousedata_t)));
}

Begin
 MOUSE_READSTATE:=FpIoctl(fd,nr_MOUSE_READSTATE,@param1)=0;
end;

Function MOUSE_READDATA(fd:longint;var param1 : mousedata_t):boolean;
{IOWR('M',9,sizeof(mousedata_t)));
}

Begin
 MOUSE_READDATA:=FpIoctl(fd,nr_MOUSE_READDATA,@param1)=0;
end;

Function MOUSE_SETRESOLUTION(fd:longint;var param1 : longint):boolean;
{IOW('M',10,sizeof(int)));
}

Begin
 MOUSE_SETRESOLUTION:=FpIoctl(fd,nr_MOUSE_SETRESOLUTION,@param1)=0;
end;

Function MOUSE_SETSCALING(fd:longint;var param1 : longint):boolean;
{IOW('M',11,sizeof(int)));
}

Begin
 MOUSE_SETSCALING:=FpIoctl(fd,nr_MOUSE_SETSCALING,@param1)=0;
end;

Function MOUSE_SETRATE(fd:longint;var param1 : longint):boolean;
{IOW('M',12,sizeof(int)));
}

Begin
 MOUSE_SETRATE:=FpIoctl(fd,nr_MOUSE_SETRATE,@param1)=0;
end;

Function MOUSE_GETHWID(fd:longint;var param1 : longint):boolean;
{IOR('M',13,sizeof(int)));
}

Begin
 MOUSE_GETHWID:=FpIoctl(fd,nr_MOUSE_GETHWID,@param1)=0;
end;

end.
