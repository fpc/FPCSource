{
    Copyright (c) 2003-2006 by Peter Vreman and Florian Klaempfl

    This units contains support for DWARF debug info generation

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
{
  This units contains support for DWARF debug info generation.

  Currently a lot of code looks like being mergable with dbgstabs. This might
  change however when improved dwarf info is generated, so the stuff shouldn't be
  merged yet. (FK)
}
unit dbgdwarf;

{$i fpcdefs.inc}

interface

    uses
      aasmbase,aasmtai,
      symbase,symtype,symdef,
      DbgBase;

    type
      { Tag names and codes.   }
      tdwarf_tag = (DW_TAG_padding := $00,DW_TAG_array_type := $01,
        DW_TAG_class_type := $02,DW_TAG_entry_point := $03,
        DW_TAG_enumeration_type := $04,DW_TAG_formal_parameter := $05,
        DW_TAG_imported_declaration := $08,DW_TAG_label := $0a,
        DW_TAG_lexical_block := $0b,DW_TAG_member := $0d,
        DW_TAG_pointer_type := $0f,DW_TAG_reference_type := $10,
        DW_TAG_compile_unit := $11,DW_TAG_string_type := $12,
        DW_TAG_structure_type := $13,DW_TAG_subroutine_type := $15,
        DW_TAG_typedef := $16,DW_TAG_union_type := $17,
        DW_TAG_unspecified_parameters := $18,
        DW_TAG_variant := $19,DW_TAG_common_block := $1a,
        DW_TAG_common_inclusion := $1b,DW_TAG_inheritance := $1c,
        DW_TAG_inlined_subroutine := $1d,DW_TAG_module := $1e,
        DW_TAG_ptr_to_member_type := $1f,DW_TAG_set_type := $20,
        DW_TAG_subrange_type := $21,DW_TAG_with_stmt := $22,
        DW_TAG_access_declaration := $23,DW_TAG_base_type := $24,
        DW_TAG_catch_block := $25,DW_TAG_const_type := $26,
        DW_TAG_constant := $27,DW_TAG_enumerator := $28,
        DW_TAG_file_type := $29,DW_TAG_friend := $2a,
        DW_TAG_namelist := $2b,DW_TAG_namelist_item := $2c,
        DW_TAG_packed_type := $2d,DW_TAG_subprogram := $2e,
        DW_TAG_template_type_param := $2f,DW_TAG_template_value_param := $30,
        DW_TAG_thrown_type := $31,DW_TAG_try_block := $32,
        DW_TAG_variant_part := $33,DW_TAG_variable := $34,
        DW_TAG_volatile_type := $35,
        { DWARF 3.   }
        DW_TAG_dwarf_procedure := $36,
        DW_TAG_restrict_type := $37,DW_TAG_interface_type := $38,
        DW_TAG_namespace := $39,DW_TAG_imported_module := $3a,
        DW_TAG_unspecified_type := $3b,DW_TAG_partial_unit := $3c,
        DW_TAG_imported_unit := $3d,

        { SGI/MIPS Extensions.   }
        DW_TAG_MIPS_loop := $4081,

        { HP extensions.  See: ftp://ftp.hp.com/pub/lang/tools/WDB/wdb-4.0.tar.gz .   }
        DW_TAG_HP_array_descriptor := $4090,

        { GNU extensions.   }
        { For FORTRAN 77 and Fortran 90.   }
        DW_TAG_format_label := $4101,
        { For C++.   }
        DW_TAG_function_template := $4102,DW_TAG_class_template := $4103,

        DW_TAG_GNU_BINCL := $4104,DW_TAG_GNU_EINCL := $4105,
        { Extensions for UPC.  See: http://upc.gwu.edu/~upc.   }
        DW_TAG_upc_shared_type := $8765,DW_TAG_upc_strict_type := $8766,
        DW_TAG_upc_relaxed_type := $8767,

        { PGI (STMicroelectronics) extensions.  No documentation available.   }
        DW_TAG_PGI_kanji_type := $A000,
        DW_TAG_PGI_interface_block := $A020);

      { Attribute names and codes.   }
      tdwarf_attribute = (DW_AT_sibling := $01,DW_AT_location := $02,
        DW_AT_name := $03,DW_AT_ordering := $09,
        DW_AT_subscr_data := $0a,DW_AT_byte_size := $0b,
        DW_AT_bit_offset := $0c,DW_AT_bit_size := $0d,
        DW_AT_element_list := $0f,DW_AT_stmt_list := $10,
        DW_AT_low_pc := $11,DW_AT_high_pc := $12,
        DW_AT_language := $13,DW_AT_member := $14,
        DW_AT_discr := $15,DW_AT_discr_value := $16,
        DW_AT_visibility := $17,DW_AT_import := $18,
        DW_AT_string_length := $19,DW_AT_common_reference := $1a,
        DW_AT_comp_dir := $1b,DW_AT_const_value := $1c,
        DW_AT_containing_type := $1d,DW_AT_default_value := $1e,
        DW_AT_inline := $20,DW_AT_is_optional := $21,
        DW_AT_lower_bound := $22,DW_AT_producer := $25,
        DW_AT_prototyped := $27,DW_AT_return_addr := $2a,
        DW_AT_start_scope := $2c,DW_AT_stride_size := $2e,
        DW_AT_upper_bound := $2f,DW_AT_abstract_origin := $31,
        DW_AT_accessibility := $32,DW_AT_address_class := $33,
        DW_AT_artificial := $34,DW_AT_base_types := $35,
        DW_AT_calling_convention := $36,DW_AT_count := $37,
        DW_AT_data_member_location := $38,DW_AT_decl_column := $39,
        DW_AT_decl_file := $3a,DW_AT_decl_line := $3b,
        DW_AT_declaration := $3c,DW_AT_discr_list := $3d,
        DW_AT_encoding := $3e,DW_AT_external := $3f,
        DW_AT_frame_base := $40,DW_AT_friend := $41,
        DW_AT_identifier_case := $42,DW_AT_macro_info := $43,
        DW_AT_namelist_items := $44,DW_AT_priority := $45,
        DW_AT_segment := $46,DW_AT_specification := $47,
        DW_AT_static_link := $48,DW_AT_type := $49,
        DW_AT_use_location := $4a,DW_AT_variable_parameter := $4b,
        DW_AT_virtuality := $4c,DW_AT_vtable_elem_location := $4d,

        { DWARF 3 values.   }
        DW_AT_allocated := $4e,DW_AT_associated := $4f,
        DW_AT_data_location := $50,DW_AT_stride := $51,
        DW_AT_entry_pc := $52,DW_AT_use_UTF8 := $53,
        DW_AT_extension := $54,DW_AT_ranges := $55,
        DW_AT_trampoline := $56,DW_AT_call_column := $57,
        DW_AT_call_file := $58,DW_AT_call_line := $59,

        { SGI/MIPS extensions.   }
        DW_AT_MIPS_fde := $2001,DW_AT_MIPS_loop_begin := $2002,
        DW_AT_MIPS_tail_loop_begin := $2003,DW_AT_MIPS_epilog_begin := $2004,
        DW_AT_MIPS_loop_unroll_factor := $2005,
        DW_AT_MIPS_software_pipeline_depth := $2006,
        DW_AT_MIPS_linkage_name := $2007,DW_AT_MIPS_stride := $2008,
        DW_AT_MIPS_abstract_name := $2009,DW_AT_MIPS_clone_origin := $200a,
        DW_AT_MIPS_has_inlines := $200b,

        { HP extensions.   }
        DW_AT_HP_block_index := $2000,
        DW_AT_HP_unmodifiable := $2001,DW_AT_HP_actuals_stmt_list := $2010,
        DW_AT_HP_proc_per_section := $2011,DW_AT_HP_raw_data_ptr := $2012,
        DW_AT_HP_pass_by_reference := $2013,DW_AT_HP_opt_level := $2014,
        DW_AT_HP_prof_version_id := $2015,DW_AT_HP_opt_flags := $2016,
        DW_AT_HP_cold_region_low_pc := $2017,DW_AT_HP_cold_region_high_pc := $2018,
        DW_AT_HP_all_variables_modifiable := $2019,
        DW_AT_HP_linkage_name := $201a,DW_AT_HP_prof_flags := $201b,

        { GNU extensions.   }
        DW_AT_sf_names := $2101,DW_AT_src_info := $2102,
        DW_AT_mac_info := $2103,DW_AT_src_coords := $2104,
        DW_AT_body_begin := $2105,DW_AT_body_end := $2106,
        DW_AT_GNU_vector := $2107,

        { VMS extensions.  }
        DW_AT_VMS_rtnbeg_pd_address := $2201,

        { UPC extension.   }
        DW_AT_upc_threads_scaled := $3210,

        { PGI (STMicroelectronics) extensions.   }
        DW_AT_PGI_lbase := $3a00,
        DW_AT_PGI_soffset := $3a01,DW_AT_PGI_lstride := $3a02
      );

      { Form names and codes.   }
      Tdwarf_form = (DW_FORM_addr := $01,DW_FORM_block2 := $03,
        DW_FORM_block4 := $04,DW_FORM_data2 := $05,
        DW_FORM_data4 := $06,DW_FORM_data8 := $07,
        DW_FORM_string := $08,DW_FORM_block := $09,
        DW_FORM_block1 := $0a,DW_FORM_data1 := $0b,
        DW_FORM_flag := $0c,DW_FORM_sdata := $0d,
        DW_FORM_strp := $0e,DW_FORM_udata := $0f,
        DW_FORM_ref_addr := $10,DW_FORM_ref1 := $11,
        DW_FORM_ref2 := $12,DW_FORM_ref4 := $13,
        DW_FORM_ref8 := $14,DW_FORM_ref_udata := $15,
        DW_FORM_indirect := $16);

      TDebugInfoDwarf = class(TDebugInfo)
      private
        currfileidx : longint;
        currabbrevnumber : longint;

        writing_def_dwarf : boolean;

        function append_entry(tag : tdwarf_tag;has_children : boolean;data : array of const) : longint;
        procedure append_labelentry(attr : tdwarf_attribute;sym : tasmsymbol);
        procedure append_labelentry_ref(attr : tdwarf_attribute;sym : tasmsymbol);
        procedure finish_entry;
        procedure finish_children;

        procedure append_procdef(list:taasmoutput;pd:tprocdef);
        procedure append_dwarftag(list:taasmoutput;def:tdef);
        procedure insertsym(list:taasmoutput;sym:tsym);
        procedure write_symtable_syms(list:taasmoutput;st:tsymtable);
        function def_dwarf_lab(def:tdef) : tasmsymbol;
      public
        procedure insertdef(list:taasmoutput;def:tdef);override;

        procedure insertmoduleinfo;override;
        procedure inserttypeinfo;override;
        procedure referencesections(list:taasmoutput);override;
        procedure insertlineinfo(list:taasmoutput);override;
        procedure write_symtable_defs(list:taasmoutput;st:tsymtable);override;
      end;

implementation

    uses
      version,
      cutils,
      globtype,
      globals,
      verbose,
      systems,
      cpubase,
      finput,
      fmodule,
      symconst,symsym
      ;

    const
      DW_TAG_lo_user = $4080;
      DW_TAG_hi_user = $ffff;

      { Flag that tells whether entry has a child or not.   }
      DW_children_no = 0;
      DW_children_yes = 1;

    const
      { Implementation-defined range start.   }
      DW_AT_lo_user = $2000;
      { Implementation-defined range end.   }
      DW_AT_hi_user = $3ff0;

    type
      { Source language names and codes.   }
      tdwarf_source_language = (DW_LANG_C89 := $0001,DW_LANG_C := $0002,DW_LANG_Ada83 := $0003,
        DW_LANG_C_plus_plus := $0004,DW_LANG_Cobol74 := $0005,
        DW_LANG_Cobol85 := $0006,DW_LANG_Fortran77 := $0007,
        DW_LANG_Fortran90 := $0008,DW_LANG_Pascal83 := $0009,
        DW_LANG_Modula2 := $000a,DW_LANG_Java := $000b,

        { DWARF 3.   }
        DW_LANG_C99 := $000c,DW_LANG_Ada95 := $000d,
        DW_LANG_Fortran95 := $000e,

        { MIPS.   }
        DW_LANG_Mips_Assembler := $8001,

        { UPC.   }
        DW_LANG_Upc := $8765
      );

    const
      { Implementation-defined range start.   }
      DW_LANG_lo_user = $8000;

      { Implementation-defined range start.   }
      DW_LANG_hi_user = $ffff;

    type
      { Names and codes for macro information.   }
      tdwarf_macinfo_record_type = (DW_MACINFO_define := 1,DW_MACINFO_undef := 2,
        DW_MACINFO_start_file := 3,DW_MACINFO_end_file := 4,
        DW_MACINFO_vendor_ext := 255);


    type
      { Type encodings.   }
      Tdwarf_type = (DW_ATE_void := $0,DW_ATE_address := $1,
        DW_ATE_boolean := $2,DW_ATE_complex_float := $3,
        DW_ATE_float := $4,DW_ATE_signed := $5,
        DW_ATE_signed_char := $6,DW_ATE_unsigned := $7,
        DW_ATE_unsigned_char := $8,DW_ATE_imaginary_float := $9,

        { HP extensions.   }
        DW_ATE_HP_float80 := $80,DW_ATE_HP_complex_float80 := $81,
        DW_ATE_HP_float128 := $82,DW_ATE_HP_complex_float128 := $83,
        DW_ATE_HP_floathpintel := $84,DW_ATE_HP_imaginary_float80 := $85,
        DW_ATE_HP_imaginary_float128 := $86
        );


    const
      DW_ATE_lo_user = $80;
      DW_ATE_hi_user = $ff;


    type
      Tdwarf_array_dim_ordering = (DW_ORD_row_major := 0,DW_ORD_col_major := 1
        );

      { Access attribute.   }
      Tdwarf_access_attribute = (DW_ACCESS_public := 1,DW_ACCESS_protected := 2,
        DW_ACCESS_private := 3);

      { Visibility.   }
      Tdwarf_visibility_attribute = (DW_VIS_local := 1,DW_VIS_exported := 2,
        DW_VIS_qualified := 3);

      { Virtuality.   }
      Tdwarf_virtuality_attribute = (DW_VIRTUALITY_none := 0,DW_VIRTUALITY_virtual := 1,
        DW_VIRTUALITY_pure_virtual := 2);

      { Case sensitivity.   }
      Tdwarf_id_case = (DW_ID_case_sensitive := 0,DW_ID_up_case := 1,
        DW_ID_down_case := 2,DW_ID_case_insensitive := 3
        );

      { Calling convention.   }
      Tdwarf_calling_convention = (DW_CC_normal := $1,DW_CC_program := $2,
        DW_CC_nocall := $3,DW_CC_GNU_renesas_sh := $40
        );

      { Location atom names and codes.   }
      Tdwarf_location_atom = (DW_OP_addr := $03,DW_OP_deref := $06,DW_OP_const1u := $08,
        DW_OP_const1s := $09,DW_OP_const2u := $0a,
        DW_OP_const2s := $0b,DW_OP_const4u := $0c,
        DW_OP_const4s := $0d,DW_OP_const8u := $0e,
        DW_OP_const8s := $0f,DW_OP_constu := $10,
        DW_OP_consts := $11,DW_OP_dup := $12,DW_OP_drop := $13,
        DW_OP_over := $14,DW_OP_pick := $15,DW_OP_swap := $16,
        DW_OP_rot := $17,DW_OP_xderef := $18,DW_OP_abs := $19,
        DW_OP_and := $1a,DW_OP_div := $1b,DW_OP_minus := $1c,
        DW_OP_mod := $1d,DW_OP_mul := $1e,DW_OP_neg := $1f,
        DW_OP_not := $20,DW_OP_or := $21,DW_OP_plus := $22,
        DW_OP_plus_uconst := $23,DW_OP_shl := $24,
        DW_OP_shr := $25,DW_OP_shra := $26,DW_OP_xor := $27,
        DW_OP_bra := $28,DW_OP_eq := $29,DW_OP_ge := $2a,
        DW_OP_gt := $2b,DW_OP_le := $2c,DW_OP_lt := $2d,
        DW_OP_ne := $2e,DW_OP_skip := $2f,DW_OP_lit0 := $30,
        DW_OP_lit1 := $31,DW_OP_lit2 := $32,DW_OP_lit3 := $33,
        DW_OP_lit4 := $34,DW_OP_lit5 := $35,DW_OP_lit6 := $36,
        DW_OP_lit7 := $37,DW_OP_lit8 := $38,DW_OP_lit9 := $39,
        DW_OP_lit10 := $3a,DW_OP_lit11 := $3b,
        DW_OP_lit12 := $3c,DW_OP_lit13 := $3d,
        DW_OP_lit14 := $3e,DW_OP_lit15 := $3f,
        DW_OP_lit16 := $40,DW_OP_lit17 := $41,
        DW_OP_lit18 := $42,DW_OP_lit19 := $43,
        DW_OP_lit20 := $44,DW_OP_lit21 := $45,
        DW_OP_lit22 := $46,DW_OP_lit23 := $47,
        DW_OP_lit24 := $48,DW_OP_lit25 := $49,
        DW_OP_lit26 := $4a,DW_OP_lit27 := $4b,
        DW_OP_lit28 := $4c,DW_OP_lit29 := $4d,
        DW_OP_lit30 := $4e,DW_OP_lit31 := $4f,
        DW_OP_reg0 := $50,DW_OP_reg1 := $51,DW_OP_reg2 := $52,
        DW_OP_reg3 := $53,DW_OP_reg4 := $54,DW_OP_reg5 := $55,
        DW_OP_reg6 := $56,DW_OP_reg7 := $57,DW_OP_reg8 := $58,
        DW_OP_reg9 := $59,DW_OP_reg10 := $5a,DW_OP_reg11 := $5b,
        DW_OP_reg12 := $5c,DW_OP_reg13 := $5d,
        DW_OP_reg14 := $5e,DW_OP_reg15 := $5f,
        DW_OP_reg16 := $60,DW_OP_reg17 := $61,
        DW_OP_reg18 := $62,DW_OP_reg19 := $63,
        DW_OP_reg20 := $64,DW_OP_reg21 := $65,
        DW_OP_reg22 := $66,DW_OP_reg23 := $67,
        DW_OP_reg24 := $68,DW_OP_reg25 := $69,
        DW_OP_reg26 := $6a,DW_OP_reg27 := $6b,
        DW_OP_reg28 := $6c,DW_OP_reg29 := $6d,
        DW_OP_reg30 := $6e,DW_OP_reg31 := $6f,
        DW_OP_breg0 := $70,DW_OP_breg1 := $71,
        DW_OP_breg2 := $72,DW_OP_breg3 := $73,
        DW_OP_breg4 := $74,DW_OP_breg5 := $75,
        DW_OP_breg6 := $76,DW_OP_breg7 := $77,
        DW_OP_breg8 := $78,DW_OP_breg9 := $79,
        DW_OP_breg10 := $7a,DW_OP_breg11 := $7b,
        DW_OP_breg12 := $7c,DW_OP_breg13 := $7d,
        DW_OP_breg14 := $7e,DW_OP_breg15 := $7f,
        DW_OP_breg16 := $80,DW_OP_breg17 := $81,
        DW_OP_breg18 := $82,DW_OP_breg19 := $83,
        DW_OP_breg20 := $84,DW_OP_breg21 := $85,
        DW_OP_breg22 := $86,DW_OP_breg23 := $87,
        DW_OP_breg24 := $88,DW_OP_breg25 := $89,
        DW_OP_breg26 := $8a,DW_OP_breg27 := $8b,
        DW_OP_breg28 := $8c,DW_OP_breg29 := $8d,
        DW_OP_breg30 := $8e,DW_OP_breg31 := $8f,
        DW_OP_regx := $90,DW_OP_fbreg := $91,DW_OP_bregx := $92,
        DW_OP_piece := $93,DW_OP_deref_size := $94,
        DW_OP_xderef_size := $95,DW_OP_nop := $96,

        { DWARF 3 extensions.   }
        DW_OP_push_object_address := $97,DW_OP_call2 := $98,
        DW_OP_call4 := $99,DW_OP_call_ref := $9a,

        { GNU extensions.   }
        DW_OP_GNU_push_tls_address := $e0,

        { HP extensions.   }
        DW_OP_HP_unknown := $e0,
        DW_OP_HP_is_value := $e1,DW_OP_HP_fltconst4 := $e2,
        DW_OP_HP_fltconst8 := $e3,DW_OP_HP_mod_range := $e4,
        DW_OP_HP_unmod_range := $e5,DW_OP_HP_tls := $e6
        );


    const
      { Implementation-defined range start.   }
      DW_OP_lo_user = $e0;
      { Implementation-defined range end.   }
      DW_OP_hi_user = $ff;


    function TDebugInfoDwarf.def_dwarf_lab(def:tdef) : tasmsymbol;
      begin
        { procdefs only need a number, mark them as already written
          so they won't be written implicitly }
        if (def.deftype=procdef) then
          def.dbg_state:=dbg_state_written;
        { dwarf must already be written, or we must be busy writing it }
        if writing_def_dwarf and
           not(def.dbg_state in [dbg_state_writing,dbg_state_written]) then
          internalerror(200601241);
        { Keep track of used dwarf entries, this info is only usefull for dwarf entries
          referenced by the symbols. Definitions will always include all
          required stabs }
        if def.dbg_state=dbg_state_unused then
          def.dbg_state:=dbg_state_used;
        { Need a new label? }
        if def.dwarf_lab=nil then
          begin
            objectlibrary.getdatalabel(def.dwarf_lab);
          end;
        result:=def.dwarf_lab;
      end;


    { writing the data through a few simply procedures allows to create easily extra information
      for debugging of debug info }
    function TDebugInfoDwarf.append_entry(tag : tdwarf_tag;has_children : boolean;data : array of const) : longint;
      var
        i : longint;
      begin
        inc(currabbrevnumber);
        
        asmlist[al_dwarf_abbrev].concat(tai_comment.Create(strpnew('Abbrev '+tostr(currabbrevnumber))));

        { abbrev number }
        asmlist[al_dwarf_info].concat(tai_const.create_uleb128bit(currabbrevnumber));
        asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(currabbrevnumber));
        { tag }
        asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(tag)));

        { children? }
        asmlist[al_dwarf_abbrev].concat(tai_const.create_8bit(ord(has_children)));

        i:=0;
        while i<=high(data) do
          begin
            { attribute }
            if data[i].VType=vtInteger then
              begin
                asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(data[i].VInteger));
              end
            else
              internalerror(200601261);
            inc(i);

            { form }
            if data[i].VType=vtInteger then
              begin
                asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(data[i].VInteger));
              end
            else
              internalerror(200601262);
            inc(i);

            { info itself }
            case tdwarf_form(data[i-1].VInteger) of
              DW_FORM_string:
                case data[i].VType of
                  vtChar:
                    asmlist[al_dwarf_info].concat(tai_string.create(data[i].VChar));
                  vtString:
                    asmlist[al_dwarf_info].concat(tai_string.create(data[i].VString^));
                  vtAnsistring:
                    asmlist[al_dwarf_info].concat(tai_string.create(Ansistring(data[i].VAnsiString^)));
                  else
                    begin
                      writeln(data[i].VType);
                      internalerror(200601264);
                    end;
                end;


              DW_FORM_flag:
                asmlist[al_dwarf_info].concat(tai_const.create_8bit(byte(data[i].VBoolean)));
              DW_FORM_data1:
                asmlist[al_dwarf_info].concat(tai_const.create_8bit(data[i].VInteger));
              DW_FORM_data2:
                asmlist[al_dwarf_info].concat(tai_const.create_16bit(data[i].VInteger));
              DW_FORM_data4:
                asmlist[al_dwarf_info].concat(tai_const.create_32bit(data[i].VInteger));
              DW_FORM_data8:
                asmlist[al_dwarf_info].concat(tai_const.create_64bit(data[i].VInteger));

              { block gets only the size, the rest is appended manually by the caller }
              DW_FORM_block1:
                asmlist[al_dwarf_info].concat(tai_const.create_8bit(data[i].VInteger));
              else
                internalerror(200601263);
            end;
            inc(i);
          end;
      end;


    procedure TDebugInfoDwarf.append_labelentry(attr : tdwarf_attribute;sym : tasmsymbol);
      begin
        asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(attr)));
        asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_addr)));
        asmlist[al_dwarf_info].concat(tai_const.create_sym(sym));
      end;


    procedure TDebugInfoDwarf.append_labelentry_ref(attr : tdwarf_attribute;sym : tasmsymbol);
      begin
        asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(attr)));
{$ifdef cpu64bit}
        asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_ref8)));
{$else cpu64bit}
        asmlist[al_dwarf_abbrev].concat(tai_const.create_uleb128bit(ord(DW_FORM_ref4)));
{$endif cpu64bit}
        asmlist[al_dwarf_info].concat(tai_const.create_sym(sym));
      end;


    procedure TDebugInfoDwarf.finish_entry;
      begin
        asmlist[al_dwarf_abbrev].concat(tai_const.create_8bit(0));
        asmlist[al_dwarf_abbrev].concat(tai_const.create_8bit(0));
      end;


    procedure TDebugInfoDwarf.finish_children;
      begin
        asmlist[al_dwarf_abbrev].concat(tai_const.create_8bit(0));
      end;


    procedure TDebugInfoDwarf.append_dwarftag(list:taasmoutput;def:tdef);

      procedure append_dwarftag_orddef(list:taasmoutput;def:torddef);
        begin
          case def.typ of
            s32bit :
              begin
                append_entry(DW_TAG_base_type,false,[
                  DW_AT_name,DW_FORM_string,'Longint'#0,
                  DW_AT_encoding,DW_FORM_data1,DW_ATE_signed,
                  DW_AT_byte_size,DW_FORM_data1,4
                  ]);
                finish_entry;
              end;
            uvoid :
              begin
                append_entry(DW_TAG_unspecified_type,false,[
                  DW_AT_name,DW_FORM_string,'Void'#0]);
                finish_entry;
              end;
            uchar :
              begin
                append_entry(DW_TAG_base_type,false,[
                  DW_AT_name,DW_FORM_string,'Char'#0,
                  DW_AT_encoding,DW_FORM_data1,DW_ATE_unsigned_char,
                  DW_AT_byte_size,DW_FORM_data1,1
                  ]);
                finish_entry;
              end;
            uwidechar :
              ;
            bool8bit :
              ;
            bool16bit :
              ;
            bool32bit :
              ;
            u64bit :
              ;
            s64bit :
              ;
            {u32bit : result:=def_stab_number(s32inttype.def)+';0;-1;'); }
            else
              ;
          end;
        end;

      begin
        list.concat(tai_symbol.create(def_dwarf_lab(def),0));
        case def.deftype of
        {
          stringdef :
            result:=stringdef_stabstr(tstringdef(def));
          enumdef :
            result:=enumdef_stabstr(tenumdef(def));
        }
          orddef :
            append_dwarftag_orddef(list,torddef(def));
        {
          floatdef :
            result:=floatdef_stabstr(tfloatdef(def));
          filedef :
            result:=filedef_stabstr(tfiledef(def));
          recorddef :
            result:=recorddef_stabstr(trecorddef(def));
          variantdef :
            result:=def_stabstr_evaluate(def,'formal${numberstring};',[]);
          pointerdef :
            result:=strpnew('*'+def_stab_number(tpointerdef(def).pointertype.def));
          classrefdef :
            result:=strpnew(def_stab_number(pvmttype.def));
          setdef :
            result:=def_stabstr_evaluate(def,'@s$1;S$2',[tostr(def.size*8),def_stab_number(tsetdef(def).elementtype.def)]);
          formaldef :
            result:=def_stabstr_evaluate(def,'formal${numberstring};',[]);
          arraydef :
            result:=def_stabstr_evaluate(def,'ar$1;$2;$3;$4',[def_stab_number(tarraydef(def).rangetype.def),
               tostr(tarraydef(def).lowrange),tostr(tarraydef(def).highrange),def_stab_number(tarraydef(def).elementtype.def)]);
          procdef :
            result:=procdef_stabstr(tprocdef(def));
          procvardef :
            result:=strpnew('*f'+def_stab_number(tprocvardef(def).rettype.def));
          objectdef :
            result:=objectdef_stabstr(tobjectdef(def));
          undefineddef :
            result:=def_stabstr_evaluate(def,'formal${numberstring};',[]);
        }
        else
          internalerror(200601281);
        end;
      end;


    procedure TDebugInfoDwarf.insertdef(list:taasmoutput;def:tdef);
      var
        anc : tobjectdef;
        oldtypesym : tsym;
      begin
        if (def.dbg_state in [dbg_state_writing,dbg_state_written]) then
          exit;
        { never write generic template defs }
        if df_generic in def.defoptions then
          begin
            def.dbg_state:=dbg_state_written;
            exit;
          end;
        { to avoid infinite loops }
        def.dbg_state := dbg_state_writing;
        { write dependencies first }
        case def.deftype of
          stringdef :
            begin
              if tstringdef(def).string_typ=st_widestring then
                insertdef(list,cwidechartype.def)
              else
                begin
                  insertdef(list,cchartype.def);
                  insertdef(list,u8inttype.def);
                end;
            end;
          floatdef :
            insertdef(list,s32inttype.def);
          filedef :
            begin
              insertdef(list,s32inttype.def);
{$ifdef cpu64bit}
              insertdef(list,s64inttype.def);
{$endif cpu64bit}
              insertdef(list,u8inttype.def);
              insertdef(list,cchartype.def);
            end;
          classrefdef :
            insertdef(list,pvmttype.def);
          pointerdef :
            insertdef(list,tpointerdef(def).pointertype.def);
          setdef :
            insertdef(list,tsetdef(def).elementtype.def);
          procvardef,
          procdef :
            insertdef(list,tprocdef(def).rettype.def);
          arraydef :
            begin
              insertdef(list,tarraydef(def).rangetype.def);
              insertdef(list,tarraydef(def).elementtype.def);
            end;
          recorddef :
            trecorddef(def).symtable.foreach(@field_write_defs,list);
          objectdef :
            begin
              insertdef(list,vmtarraytype.def);
              { first the parents }
              anc:=tobjectdef(def);
              while assigned(anc.childof) do
                begin
                  anc:=anc.childof;
                  insertdef(list,anc);
                end;
              tobjectdef(def).symtable.foreach(@field_write_defs,list);
              tobjectdef(def).symtable.foreach(@method_write_defs,list);
            end;
        end;

        case def.deftype of
          objectdef :
            begin
              { classes require special code to write the record and the invisible pointer }
              if is_class(def) then
                begin
                  { Write the record class itself }
                  tobjectdef(def).writing_class_record_dbginfo:=true;
                  append_dwarftag(list,def);
                  tobjectdef(def).writing_class_record_dbginfo:=false;
                  { Write the invisible pointer class }
                  oldtypesym:=def.typesym;
                  def.typesym:=nil;
                  append_dwarftag(list,def);
                  def.typesym:=oldtypesym;
                end
              else
                append_dwarftag(list,def);
              { VMT symbol }
              if (oo_has_vmt in tobjectdef(def).objectoptions) and
                 assigned(def.owner) and
                 assigned(def.owner.name) then
                {
                list.concat(Tai_stab.create(stab_stabs,strpnew('"vmt_'+def.owner.name^+tobjectdef(def).name+':S'+
                       def_stab_number(vmttype.def)+'",'+tostr(N_STSYM)+',0,0,'+tobjectdef(def).vmt_mangledname)));
                }
                ;
            end;
          procdef :
            begin
              { procdefs are handled separatly }
            end;
          else
            append_dwarftag(list,def);
        end;

        def.dbg_state := dbg_state_written;
      end;


    procedure TDebugInfoDwarf.write_symtable_defs(list:taasmoutput;st:tsymtable);

       procedure dowritedwarf(list:taasmoutput;st:tsymtable);
         var
           p : tdef;
         begin
           p:=tdef(st.defindex.first);
           while assigned(p) do
             begin
               if (p.dbg_state=dbg_state_used) then
                 insertdef(list,p);
               p:=tdef(p.indexnext);
             end;
         end;

      var
        old_writing_def_dwarf : boolean;
      begin
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - Begin Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - Begin unit '+st.name^+' has index '+tostr(st.moduleid))));
        end;
        old_writing_def_dwarf:=writing_def_dwarf;
        writing_def_dwarf:=true;
        dowritedwarf(list,st);
        writing_def_dwarf:=old_writing_def_dwarf;
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - End Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - End unit '+st.name^+' has index '+tostr(st.moduleid))));
        end;
      end;


    procedure TDebugInfoDwarf.append_procdef(list:taasmoutput;pd:tprocdef);
    {
      var
        templist : taasmoutput;
        stabsendlabel : tasmlabel;
        mangled_length : longint;
        p : pchar;
        hs : string;
    }
      begin
        {
        if assigned(pd.procstarttai) then
          begin
            templist:=taasmoutput.create;
            { para types }
            write_def_stabstr(templist,pd);
            if assigned(pd.parast) then
              write_symtable_syms(templist,pd.parast);
            { local type defs and vars should not be written
              inside the main proc stab }
            if assigned(pd.localst) and
               (pd.localst.symtabletype=localsymtable) then
              write_symtable_syms(templist,pd.localst);
            asmlist[al_procedures].insertlistbefore(pd.procstarttai,templist);
            { end of procedure }
            objectlibrary.getlabel(stabsendlabel,alt_dbgtype);
            templist.concat(tai_label.create(stabsendlabel));
            if assigned(pd.funcretsym) and
               (tabstractnormalvarsym(pd.funcretsym).refs>0) then
              begin
                if tabstractnormalvarsym(pd.funcretsym).localloc.loc=LOC_REFERENCE then
                  begin
    {$warning Need to add gdb support for ret in param register calling}
                    if paramanager.ret_in_param(pd.rettype.def,pd.proccalloption) then
                      hs:='X*'
                    else
                      hs:='X';
                    templist.concat(Tai_stab.create(stab_stabs,strpnew(
                       '"'+pd.procsym.name+':'+hs+def_stab_number(pd.rettype.def)+'",'+
                       tostr(N_tsym)+',0,0,'+tostr(tabstractnormalvarsym(pd.funcretsym).localloc.reference.offset))));
                    if (m_result in aktmodeswitches) then
                      templist.concat(Tai_stab.create(stab_stabs,strpnew(
                         '"RESULT:'+hs+def_stab_number(pd.rettype.def)+'",'+
                         tostr(N_tsym)+',0,0,'+tostr(tabstractnormalvarsym(pd.funcretsym).localloc.reference.offset))));
                  end;
              end;
            mangled_length:=length(pd.mangledname);
            getmem(p,2*mangled_length+50);
            strpcopy(p,'192,0,0,');
            {$IFDEF POWERPC64}strpcopy(strend(p), '.');{$ENDIF POWERPC64}
            strpcopy(strend(p),pd.mangledname);
            if (tf_use_function_relative_addresses in target_info.flags) then
              begin
                strpcopy(strend(p),'-');
                {$IFDEF POWERPC64}strpcopy(strend(p), '.');{$ENDIF POWERPC64}
                strpcopy(strend(p),pd.mangledname);
              end;
            templist.concat(Tai_stab.Create(stab_stabn,strnew(p)));
            strpcopy(p,'224,0,0,'+stabsendlabel.name);
            if (tf_use_function_relative_addresses in target_info.flags) then
              begin
                strpcopy(strend(p),'-');
                {$IFDEF POWERPC64}strpcopy(strend(p), '.');{$ENDIF POWERPC64}
                strpcopy(strend(p),pd.mangledname);
              end;
            templist.concat(Tai_stab.Create(stab_stabn,strnew(p)));
            freemem(p,2*mangled_length+50);
            asmlist[al_procedures].insertlistbefore(pd.procendtai,templist);
            templist.free;
          end;
      }
      end;


    procedure TDebugInfoDwarf.insertsym(list:taasmoutput;sym:tsym);

        function fieldvarsym_stabstr(sym:tfieldvarsym):Pchar;
          begin
            {
            result:=nil;
            if (sym.owner.symtabletype=objectsymtable) and
               (sp_static in sym.symoptions) then
              result:=sym_stabstr_evaluate(sym,'"${ownername}__${name}:S$1",${N_LCSYM},0,${line},${mangledname}',
                  [def_stab_number(sym.vartype.def)]);
            }
          end;


        procedure append_globalvarsym(sym:tglobalvarsym);
          begin
            { external symbols can't be resolved at link time, so we
              can't generate stabs for them

              not sure if this applies to dwarf as well (FK)
            }
            if vo_is_external in sym.varoptions then
              exit;

            append_entry(DW_TAG_variable,false,[
              DW_AT_name,DW_FORM_string,sym.name+#0,
              {
              DW_AT_decl_file,DW_FORM_data1,0,
              DW_AT_decl_line,DW_FORM_data1,
              }
              DW_AT_external,DW_FORM_flag,true,
              { data continues below }
              DW_AT_location,DW_FORM_block1,1+sizeof(aword)
              ]);
            { append block data }
            asmlist[al_dwarf_info].concat(tai_const.create_8bit(3));
            asmlist[al_dwarf_info].concat(tai_const.createname(sym.mangledname,AT_DATA,0));

            append_labelentry_ref(DW_AT_type,def_dwarf_lab(sym.vartype.def));

            // ,,
            {
            case sym.localloc.loc of
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_MMREGISTER,
              LOC_CMMREGISTER,
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER :
                begin
                  regidx:=findreg_by_number(sym.localloc.register);
                  { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
                  { this is the register order for GDB}
                  if regidx<>0 then
                    result:=sym_stabstr_evaluate(sym,'"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(regstabs_table[regidx])]);
                end;
              else
                begin
                  if (vo_is_thread_var in sym.varoptions) then
                    threadvaroffset:='+'+tostr(sizeof(aint))
                  else
                    threadvaroffset:='';
                  { Here we used S instead of
                    because with G GDB doesn't look at the address field
                    but searches the same name or with a leading underscore
                    but these names don't exist in pascal !}
                  st:='S'+st;
                  result:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_LCSYM},0,${line},${mangledname}$2',[st,threadvaroffset]);
                end;
            end;
            }
            finish_entry;
          end;


        function localvarsym_stabstr(sym:tlocalvarsym):Pchar;
          var
            st : string;
            regidx : Tregisterindex;
          begin
            {
            result:=nil;
            { There is no space allocated for not referenced locals }
            if (sym.owner.symtabletype=localsymtable) and (sym.refs=0) then
              exit;

            st:=def_stab_number(sym.vartype.def);
            case sym.localloc.loc of
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_MMREGISTER,
              LOC_CMMREGISTER,
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER :
                begin
                  regidx:=findreg_by_number(sym.localloc.register);
                  { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
                  { this is the register order for GDB}
                  if regidx<>0 then
                    result:=sym_stabstr_evaluate(sym,'"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(regstabs_table[regidx])]);
                end;
              LOC_REFERENCE :
                { offset to ebp => will not work if the framepointer is esp
                  so some optimizing will make things harder to debug }
                result:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_TSYM},0,${line},$2',[st,tostr(sym.localloc.reference.offset)])
              else
                internalerror(2003091814);
            end;
            }
          end;

        function paravarsym_stabstr(sym:tparavarsym):Pchar;
          var
            st : string;
            regidx : Tregisterindex;
            c : char;
          begin
            {
            result:=nil;
            { set loc to LOC_REFERENCE to get somewhat usable debugging info for -Or }
            { while stabs aren't adapted for regvars yet                             }
            if (vo_is_self in sym.varoptions) then
              begin
                case sym.localloc.loc of
                  LOC_REGISTER,
                  LOC_CREGISTER:
                    regidx:=findreg_by_number(sym.localloc.register);
                  LOC_REFERENCE: ;
                  else
                    internalerror(2003091815);
                end;
                if (po_classmethod in tabstractprocdef(sym.owner.defowner).procoptions) or
                   (po_staticmethod in tabstractprocdef(sym.owner.defowner).procoptions) then
                  begin
                    if (sym.localloc.loc=LOC_REFERENCE) then
                      result:=sym_stabstr_evaluate(sym,'"pvmt:p$1",${N_TSYM},0,0,$2',
                        [def_stab_number(pvmttype.def),tostr(sym.localloc.reference.offset)]);
      (*            else
                      result:=sym_stabstr_evaluate(sym,'"pvmt:r$1",${N_RSYM},0,0,$2',
                        [def_stab_number(pvmttype.def),tostr(regstabs_table[regidx])]) *)
                    end
                else
                  begin
                    if not(is_class(tprocdef(sym.owner.defowner)._class)) then
                      c:='v'
                    else
                      c:='p';
                    if (sym.localloc.loc=LOC_REFERENCE) then
                      result:=sym_stabstr_evaluate(sym,'"$$t:$1",${N_TSYM},0,0,$2',
                            [c+def_stab_number(tprocdef(sym.owner.defowner)._class),tostr(sym.localloc.reference.offset)]);
      (*            else
                      result:=sym_stabstr_evaluate(sym,'"$$t:r$1",${N_RSYM},0,0,$2',
                            [c+def_stab_number(tprocdef(sym.owner.defowner)._class),tostr(regstabs_table[regidx])]); *)
                  end;
              end
            else
              begin
                st:=def_stab_number(sym.vartype.def);

                if paramanager.push_addr_param(sym.varspez,sym.vartype.def,tprocdef(sym.owner.defowner).proccalloption) and
                   not(vo_has_local_copy in sym.varoptions) and
                   not is_open_string(sym.vartype.def) then
                  st := 'v'+st { should be 'i' but 'i' doesn't work }
                else
                  st := 'p'+st;
                case sym.localloc.loc of
                  LOC_REGISTER,
                  LOC_CREGISTER,
                  LOC_MMREGISTER,
                  LOC_CMMREGISTER,
                  LOC_FPUREGISTER,
                  LOC_CFPUREGISTER :
                    begin
                      regidx:=findreg_by_number(sym.localloc.register);
                      { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
                      { this is the register order for GDB}
                      if regidx<>0 then
                        result:=sym_stabstr_evaluate(sym,'"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(longint(regstabs_table[regidx]))]);
                    end;
                  LOC_REFERENCE :
                    { offset to ebp => will not work if the framepointer is esp
                      so some optimizing will make things harder to debug }
                    result:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_TSYM},0,${line},$2',[st,tostr(sym.localloc.reference.offset)])
                  else
                    internalerror(2003091814);
                end;
              end;
            }
          end;

        function constsym_stabstr(sym:tconstsym):Pchar;
          var
            st : string;
          begin
            {
            case sym.consttyp of
              conststring:
                begin
                  if sym.value.len<200 then
                    st:='s'''+backspace_quote(octal_quote(strpas(pchar(sym.value.valueptr)),[#0..#9,#11,#12,#14..#31,'''']),['"','\',#10,#13])+''''
                  else
                    st:='<constant string too long>';
                end;
              constord:
                st:='i'+tostr(sym.value.valueord);
              constpointer:
                st:='i'+tostr(sym.value.valueordptr);
              constreal:
                begin
                  system.str(pbestreal(sym.value.valueptr)^,st);
                  st := 'r'+st;
                end;
              else
                begin
                  { if we don't know just put zero !! }
                  st:='i0';
                end;
            end;
            { valgrind does not support constants }
            if cs_gdb_valgrind in aktglobalswitches then
              result:=nil
            else
              result:=sym_stabstr_evaluate(sym,'"${name}:c=$1;",${N_FUNCTION},0,${line},0',[st]);
            }
          end;

        function typesym_stabstr(sym:ttypesym) : pchar;
          var
            stabchar : string[2];
          begin
            {
            result:=nil;
            if not assigned(sym.restype.def) then
              internalerror(200509262);
            if sym.restype.def.deftype in tagtypes then
              stabchar:='Tt'
            else
              stabchar:='t';
            result:=sym_stabstr_evaluate(sym,'"${name}:$1$2",${N_LSYM},0,${line},0',[stabchar,def_stab_number(sym.restype.def)]);
            }
          end;


        procedure append_procsym(sym:tprocsym);
          var
            i : longint;
          begin
            for i:=1 to sym.procdef_count do
              append_procdef(list,sym.procdef[i]);
          end;

      begin
        case sym.typ of
          globalvarsym :
            append_globalvarsym(tglobalvarsym(sym));
          unitsym:
            { for now, we ignore unit symbols }
            ;
          procsym :
            append_procsym(tprocsym(sym));
          {
          labelsym :
            stabstr:=sym_stabstr_evaluate(sym,'"${name}",${N_LSYM},0,${line},0',[]);
          fieldvarsym :
            stabstr:=fieldvarsym_stabstr(tfieldvarsym(sym));
          localvarsym :
            stabstr:=localvarsym_stabstr(tlocalvarsym(sym));
          paravarsym :
            stabstr:=paravarsym_stabstr(tparavarsym(sym));
          typedconstsym :
            stabstr:=sym_stabstr_evaluate(sym,'"${name}:S$1",${N_STSYM},0,${line},${mangledname}',
                [def_stab_number(ttypedconstsym(sym).typedconsttype.def)]);
          constsym :
            stabstr:=constsym_stabstr(tconstsym(sym));
          typesym :
            stabstr:=typesym_stabstr(ttypesym(sym));
          }
          else
            internalerror(200601242);
        end;
        {
        if stabstr<>nil then
          list.concat(Tai_stab.create(stab_stabs,stabstr));
        { For object types write also the symtable entries }
        if (sym.typ=typesym) and (ttypesym(sym).restype.def.deftype=objectdef) then
          write_symtable_syms(list,tobjectdef(ttypesym(sym).restype.def).symtable);
        sym.isstabwritten:=true;
        }
      end;


    procedure TDebugInfoDwarf.write_symtable_syms(list:taasmoutput;st:tsymtable);
      var
        p : tsym;
      begin
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - Begin Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - Begin unit '+st.name^+' has index '+tostr(st.moduleid))));
        end;
        p:=tsym(st.symindex.first);
        while assigned(p) do
          begin
            if (not p.isstabwritten) then
              insertsym(list,p);
            p:=tsym(p.indexnext);
          end;
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - End Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - End unit '+st.name^+' has index '+tostr(st.moduleid))));
        end;
      end;


    procedure TDebugInfoDwarf.insertmoduleinfo;
      var
        templist : taasmoutput;
      begin
        { insert .Ltext0 label }
        templist:=taasmoutput.create;
        new_section(templist,sec_code,'',0);
        templist.concat(tai_symbol.createname('.Ltext0',AT_DATA,0));
        asmlist[al_start].insertlist(templist);
        templist.free;

        { insert .Letext0 label }
        templist:=taasmoutput.create;
        new_section(templist,sec_code,'',0);
        templist.concat(tai_symbol.createname('.Letext0',AT_DATA,0));
        asmlist[al_end].insertlist(templist);
        templist.free;

        { insert .Ldebug_abbrev0 label }
        templist:=taasmoutput.create;
        new_section(templist,sec_debug_abbrev,'',0);
        templist.concat(tai_symbol.createname('.Ldebug_abbrev0',AT_DATA,0));
        asmlist[al_start].insertlist(templist);
        templist.free;
      end;


    procedure TDebugInfoDwarf.inserttypeinfo;
      var
        storefilepos  : tfileposinfo;
        lenstartlabel : tasmlabel;
      begin
        storefilepos:=aktfilepos;
        aktfilepos:=current_module.mainfilepos;

        currabbrevnumber:=0;
        writing_def_dwarf:=false;

        { write start labels }
        asmlist[al_dwarf_info].concat(tai_section.create(sec_debug_info,'',0));
        asmlist[al_dwarf_info].concat(tai_symbol.createname('.Ldebug_info0',AT_DATA,0));

        { start abbrev section }
        new_section(asmlist[al_dwarf_abbrev],sec_debug_abbrev,'',0);

        { debug info header }
        objectlibrary.getlabel(lenstartlabel,alt_dbgfile);
        { size }
        asmlist[al_dwarf_info].concat(tai_const.create_rel_sym(aitconst_ptr,
          lenstartlabel,tasmsymbol.create('.Ledebug_info0',AB_COMMON,AT_DATA)));

        asmlist[al_dwarf_info].concat(tai_label.create(lenstartlabel));
        { version }
        asmlist[al_dwarf_info].concat(tai_const.create_16bit(2));
        { abbrev table }
        asmlist[al_dwarf_info].concat(tai_const.createname('.Ldebug_abbrev0',AT_DATA,0));
        { address size }
        asmlist[al_dwarf_info].concat(tai_const.create_8bit(sizeof(aint)));

        append_entry(DW_TAG_compile_unit,true,[
          // reference DW_AT_stmt_list,,,
          DW_AT_name,DW_FORM_string,'test.pp'#0,
          DW_AT_producer,DW_FORM_string,'Free Pascal '+full_version_string+' '+date_string+#0,
          DW_AT_comp_dir,DW_FORM_string,''#0,
          DW_AT_language,DW_FORM_data1,DW_LANG_Pascal83,
          DW_AT_identifier_case,DW_FORM_data1,DW_ID_case_insensitive]);
        append_labelentry(DW_AT_low_pc,objectlibrary.newasmsymbol('.Ltext0',AB_LOCAL,AT_DATA));
        append_labelentry(DW_AT_high_pc,objectlibrary.newasmsymbol('.Letext0',AB_LOCAL,AT_DATA));
        finish_entry;

        { first write all global/local symbols. This will flag all required tdefs  }
        if assigned(current_module.globalsymtable) then
          write_symtable_syms(asmlist[al_dwarf_info],current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_syms(asmlist[al_dwarf_info],current_module.localsymtable);

        { reset unit type info flag }
        reset_unit_type_info;

        { write used types from the used units }
        write_used_unit_type_info(asmlist[al_dwarf_info],current_module);

        { last write the types from this unit }
        if assigned(current_module.globalsymtable) then
          write_symtable_defs(asmlist[al_dwarf_info],current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_defs(asmlist[al_dwarf_info],current_module.localsymtable);

        { close compilation unit entry }
        finish_children;

        { end of debug info table }
        asmlist[al_dwarf_info].concat(tai_const.create_8bit(0));
        asmlist[al_dwarf_info].concat(tai_symbol.createname('.Ledebug_info0',AT_DATA,0));

        aktfilepos:=storefilepos;
      end;


    procedure TDebugInfoDwarf.referencesections(list:taasmoutput);
      begin
      end;


    procedure TDebugInfoDwarf.insertlineinfo(list:taasmoutput);
      var
        currfileinfo,
        lastfileinfo : tfileposinfo;
        currfuncname : pstring;
        currsectype  : tasmsectiontype;
        hlabel       : tasmlabel;
        hp : tai;
        infile : tinputfile;
      begin
        FillChar(lastfileinfo,sizeof(lastfileinfo),0);
        currfuncname:=nil;
        currsectype:=sec_code;
        hp:=Tai(list.first);
        while assigned(hp) do
          begin
            case hp.typ of
              ait_section :
                currsectype:=tai_section(hp).sectype;
              ait_function_name :
                currfuncname:=tai_function_name(hp).funcname;
              ait_force_line :
                lastfileinfo.line:=-1;
            end;

            if (currsectype=sec_code) and
               (hp.typ=ait_instruction) then
              begin
                currfileinfo:=tailineinfo(hp).fileinfo;
                { file changed ? (must be before line info) }
                if (currfileinfo.fileindex<>0) and
                   (lastfileinfo.fileindex<>currfileinfo.fileindex) then
                  begin
                    infile:=current_module.sourcefiles.get_file(currfileinfo.fileindex);
                    if assigned(infile) then
                      begin
                        inc(currfileidx);
                        if (infile.path^<>'') then
                          list.insertbefore(tai_file.create(
                            BsToSlash(FixPath(infile.path^,false)+FixFileName(infile.name^)),currfileidx
                          ),hp)
                        else
                          list.insertbefore(tai_file.create(
                            FixFileName(infile.name^),currfileidx),hp);
                        { force new line info }
                        lastfileinfo.line:=-1;
                      end;
                  end;

                { line changed ? }
                if (lastfileinfo.line<>currfileinfo.line) and (currfileinfo.line<>0) then
                  list.insertbefore(tai_loc.create(
                    currfileidx,currfileinfo.line,currfileinfo.column),hp);
                lastfileinfo:=currfileinfo;
              end;

            hp:=tai(hp.next);
          end;
      end;


    const
      dbg_dwarf_info : tdbginfo =
         (
           id     : dbg_dwarf;
           idtxt  : 'DWARF';
         );

initialization
  RegisterDebugInfo(dbg_dwarf_info,TDebugInfoDwarf);
end.
