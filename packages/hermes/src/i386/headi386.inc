{
    This file is part of the PTCPas framebuffer library
    Copyright (C) 2001-2010 Nikolay Nikolov (nickysn@users.sourceforge.net)

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

{$L x8616lut}
{$L x86_clr}
{$L x86_main}
{$L x86p_16}
{$L x86p_32}
{$L x86p_cpy}
{$L x86p_i8}
{$L x86p_s32}
{$L x86pscpy}

procedure ConvertX86(hci: PHermesConverterInterface); cdecl; external name ExternalAsmPrefix+'ConvertX86';
procedure ConvertX86Stretch(hci: PHermesConverterInterface); cdecl; external name ExternalAsmPrefix+'ConvertX86Stretch';
procedure ClearX86_32(hci: PHermesClearInterface); cdecl; external name ExternalAsmPrefix+'ClearX86_32';
procedure ClearX86_24(hci: PHermesClearInterface); cdecl; external name ExternalAsmPrefix+'ClearX86_24';
procedure ClearX86_16(hci: PHermesClearInterface); cdecl; external name ExternalAsmPrefix+'ClearX86_16';
procedure ClearX86_8(hci: PHermesClearInterface); cdecl; external name ExternalAsmPrefix+'ClearX86_8';

function Hermes_X86_CPU: Integer; cdecl; external name ExternalAsmPrefix+'Hermes_X86_CPU';

procedure ConvertX86p32_32BGR888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_32BGR888';
procedure ConvertX86p32_32RGBA888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_32RGBA888';
procedure ConvertX86p32_32BGRA888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_32BGRA888';
procedure ConvertX86p32_24RGB888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_24RGB888';
procedure ConvertX86p32_24BGR888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_24BGR888';
procedure ConvertX86p32_16RGB565(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_16RGB565';
procedure ConvertX86p32_16BGR565(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_16BGR565';
procedure ConvertX86p32_16RGB555(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_16RGB555';
procedure ConvertX86p32_16BGR555(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_16BGR555';
procedure ConvertX86p32_8RGB332(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_8RGB332';

procedure ConvertX86p32_16RGB565_S(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p32_16RGB565_S';

procedure ConvertX86p16_32RGB888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_32RGB888';
procedure ConvertX86p16_32BGR888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_32BGR888';
procedure ConvertX86p16_32RGBA888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_32RGBA888';
procedure ConvertX86p16_32BGRA888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_32BGRA888';
procedure ConvertX86p16_24RGB888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_24RGB888';
procedure ConvertX86p16_24BGR888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_24BGR888';
procedure ConvertX86p16_16BGR565(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_16BGR565';
procedure ConvertX86p16_16RGB555(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_16RGB555';
procedure ConvertX86p16_16BGR555(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_16BGR555';
procedure ConvertX86p16_8RGB332(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86p16_8RGB332';

procedure CopyX86p_4byte(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'CopyX86p_4byte';
procedure CopyX86p_3byte(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'CopyX86p_3byte';
procedure CopyX86p_2byte(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'CopyX86p_2byte';
procedure CopyX86p_1byte(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'CopyX86p_1byte';

procedure ConvertX86pI8_32(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86pI8_32';
procedure ConvertX86pI8_24(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86pI8_24';
procedure ConvertX86pI8_16(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertX86pI8_16';
