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

unit dbgdwarfconst;

{$i fpcdefs.inc}

interface

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
    DW_TAG_condition := $3f,
    DW_TAG_shared_type := $40,

    { DWARF 4 }
    DW_TAG_type_unit := $41,
    DW_TAG_rvalue_reference_type := $42,
    DW_TAG_template_alias := $43,


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
    DW_TAG_PGI_interface_block := $A020
  );

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

{$push}
{$notes off}
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
    DW_AT_data_location := $50,DW_AT_byte_stride := $51,
    DW_AT_entry_pc := $52,DW_AT_use_UTF8 := $53,
    DW_AT_extension := $54,DW_AT_ranges := $55,
    DW_AT_trampoline := $56,DW_AT_call_column := $57,
    DW_AT_call_file := $58,DW_AT_call_line := $59,
    DW_AT_description := $5a,     { string }
    DW_AT_binary_scale := $5b,    { constant }
    DW_AT_decimal_scale := $5c,   { constant }
    DW_AT_small := $5d,           { reference }
    DW_AT_decimal_sign := $5e,    { constant }
    DW_AT_digit_count := $5f,     { constant }
    DW_AT_picture_string := $60,  { string }
    DW_AT_mutable := $61,         { flag }
    DW_AT_threads_scaled := $62,  { flag }
    DW_AT_explicit := $63,        { flag }
    DW_AT_object_pointer := $64,  { reference }
    DW_AT_endianity := $65,       { constant }
    DW_AT_elemental := $66,       { flag }
    DW_AT_pure := $67,            { flag }
    DW_AT_recursive := $68,       { flag }

    { DWARF 4 values }
    DW_AT_signature := $69,       { reference }
    DW_AT_main_subprogram := $6a, { flag }
    DW_AT_data_bit_offset := $6b, { constant }
    DW_AT_const_expr := $6c,      { flag }
    DW_AT_enum_class := $6d,      { flag }
    DW_AT_linkage_name := $6e,    { string }


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

    { WATCOM extensions. }
    DW_AT_WATCOM_memory_model := $2082,
    DW_AT_WATCOM_references_start := $2083,
    DW_AT_WATCOM_parm_entry := $2084,

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
    DW_AT_PGI_soffset := $3a01,DW_AT_PGI_lstride := $3a02,

    { Apple extensions }
    DW_AT_APPLE_optimized := $3fe1,
    DW_AT_APPLE_flags := $3fe2,
    DW_AT_APPLE_major_runtime_vers := $3fe5,
    DW_AT_APPLE_runtime_class := $3fe6
  );
{$pop}

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
    DW_FORM_indirect := $16,

    { DWARF 4 }
    DW_FORM_sec_offset := $17,   { lineptr, loclistptr, macptr, rangelistptr }
    DW_FORM_exprloc := $18,      { exprloc }
    DW_FORM_flag_present := $19, { flag }
    DW_FORM_ref_sig8 := $20      { reference }
    );

  { values of DW_AT_address_class }
  Tdwarf_addr = (
    DW_ADDR_none := 0,
    DW_ADDR_near16 := 1,
    DW_ADDR_far16 := 2,
    DW_ADDR_huge16 := 3,
    DW_ADDR_near32 := 4,
    DW_ADDR_far32 := 5
  );

  { values of DW_AT_WATCOM_memory_model }
  Tdwarf_watcom_memory_model = (
    DW_WATCOM_MEMORY_MODEL_none := 0,
    DW_WATCOM_MEMORY_MODEL_flat := 1,
    DW_WATCOM_MEMORY_MODEL_small := 2,
    DW_WATCOM_MEMORY_MODEL_medium := 3,
    DW_WATCOM_MEMORY_MODEL_compact := 4,
    DW_WATCOM_MEMORY_MODEL_large := 5,
    DW_WATCOM_MEMORY_MODEL_huge := 6
  );

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

    { Objective-C }
    DW_LANG_ObjC := $10,

    { MIPS.   }
    DW_LANG_Mips_Assembler := $8001,

    { UPC.   }
    DW_LANG_Upc := $8765
  );

{$push}
{$notes off}
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

    { DWARF 4 extensions.   }
    DW_OP_implicit_value := $9e, DW_OP_stack_value := $9f,

    { GNU extensions.   }
    DW_OP_GNU_push_tls_address := $e0,

    { HP extensions.   }
    DW_OP_HP_unknown := $e0,
    DW_OP_HP_is_value := $e1,DW_OP_HP_fltconst4 := $e2,
    DW_OP_HP_fltconst8 := $e3,DW_OP_HP_mod_range := $e4,
    DW_OP_HP_unmod_range := $e5,DW_OP_HP_tls := $e6,

    { WebAssembly extensions. }
    DW_OP_WASM_location = $ed
    );
{$pop}




implementation

end.

