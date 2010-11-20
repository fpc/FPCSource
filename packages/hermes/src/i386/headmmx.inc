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

{$L mmx_clr}
{$L mmx_main}
{$L mmxp2_32}
{$L mmxp_32}

procedure ConvertMMX(hci: PHermesConverterInterface); cdecl; external name ExternalAsmPrefix+'ConvertMMX';

procedure ClearMMX_32(hci: PHermesClearInterface); cdecl; external name ExternalAsmPrefix+'ClearMMX_32';
procedure ClearMMX_24(hci: PHermesClearInterface); cdecl; external name ExternalAsmPrefix+'ClearMMX_24';
procedure ClearMMX_16(hci: PHermesClearInterface); cdecl; external name ExternalAsmPrefix+'ClearMMX_16';
procedure ClearMMX_8(hci: PHermesClearInterface); cdecl; external name ExternalAsmPrefix+'ClearMMX_8';

procedure ConvertMMXpII32_24RGB888(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertMMXpII32_24RGB888';
procedure ConvertMMXpII32_16RGB565(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertMMXpII32_16RGB565';
procedure ConvertMMXpII32_16BGR565(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertMMXpII32_16BGR565';
procedure ConvertMMXpII32_16RGB555(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertMMXpII32_16RGB555';
procedure ConvertMMXpII32_16BGR555(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertMMXpII32_16BGR555';

procedure ConvertMMXp32_16RGB555(CONVERT_PARAMETERS); cdecl; external name ExternalAsmPrefix+'ConvertMMXp32_16RGB555';
