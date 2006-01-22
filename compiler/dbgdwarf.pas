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
unit dbgdwarf;

{$i fpcdefs.inc}

interface

    uses
      aasmtai,
      symtype,
      DbgBase;

    type
      TDebugInfoDwarf = class(TDebugInfo)
      private
        currfileidx : longint;
        procedure append_dwarftag(list:taasmoutput;def:tdef);
      public
        procedure insertlineinfo(list:taasmoutput);override;
      end;

implementation

    uses
      cutils,
      globals,
      Systems,
      aasmbase,
      finput,
      fmodule,
      symconst,
      symdef
      ;

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

    const
      DW_TAG_lo_user = $4080;
      DW_TAG_hi_user = $ffff;

      { Flag that tells whether entry has a child or not.   }
      DW_children_no = 0;
      DW_children_yes = 1;

     type
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

    procedure TDebugInfoDwarf.append_dwarftag(list:taasmoutput;def:tdef);

      procedure append_dwarftag_orddef(list:taasmoutput;def:torddef);
        begin
          case def.typ of
            uvoid :
              ;
            uchar :
              ;
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
