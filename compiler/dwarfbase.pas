{
    Copyright (c) 2003-2019 by Peter Vreman and Florian Klaempfl

    This units contains special support for DWARF debug info

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit dwarfbase;

{$i fpcdefs.inc}

  interface

    const
      { Call frame information }
      DW_CFA_set_loc          = $01;
      DW_CFA_advance_loc1     = $02;
      DW_CFA_advance_loc2     = $03;
      DW_CFA_advance_loc4     = $04;
      DW_CFA_offset_extended  = $05;
      DW_CFA_restore_extended = $06;
      DW_CFA_undefined        = $07;
      DW_CFA_def_cfa          = $0c;
      DW_CFA_def_cfa_register = $0d;
      DW_CFA_def_cfa_offset   = $0e;
      DW_CFA_advance_loc      = $40;
      DW_CFA_offset           = $80;
      { Own additions }
      DW_CFA_start_frame = $f0;
      DW_CFA_end_frame   = $f1;
      { pseudo operation to set the LSDALable, must
        be set before DW_CFA_start_frame is executed }
      DW_Set_LSDALabel   = $f2;

      DW_LNS_copy            = $01;
      DW_LNS_advance_pc      = $02;
      DW_LNS_advance_line    = $03;
      DW_LNS_set_file        = $04;
      DW_LNS_set_column      = $05;
      DW_LNS_negate_stmt     = $06;
      DW_LNS_set_basic_block = $07;
      DW_LNS_const_add_pc    = $08;

      DW_LNS_fixed_advance_pc   = $09;
      DW_LNS_set_prologue_end   = $0a;
      DW_LNS_set_epilogue_begin = $0b;
      DW_LNS_set_isa            = $0c;

      DW_LNE_end_sequence = $01;
      DW_LNE_set_address  = $02;
      DW_LNE_define_file  = $03;
      DW_LNE_lo_user      = $80;
      DW_LNE_hi_user      = $ff;

      DW_EH_PE_absptr	= $00;
      DW_EH_PE_uleb128	= $01;
      DW_EH_PE_udata2	= $02;
      DW_EH_PE_udata4	= $03;
      DW_EH_PE_udata8	= $04;
      DW_EH_PE_sleb128	= $09;
      DW_EH_PE_sdata2	= $0A;
      DW_EH_PE_sdata4	= $0B;
      DW_EH_PE_sdata8	= $0C;

      DW_EH_PE_pcrel	= $10;
      DW_EH_PE_textrel	= $20;
      DW_EH_PE_datarel	= $30;
      DW_EH_PE_funcrel	= $40;
      DW_EH_PE_aligned	= $50;
      DW_EH_PE_indirect = $80;

      DW_EH_PE_omit     = $ff;

  implementation

end.


