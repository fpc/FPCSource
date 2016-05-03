{
    Copyright (c) 2016 by Jonas Maebe

    This unit implements Objective-C assembler helper routines.

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

{$i fpcdefs.inc}

unit objcasm;

  interface

  uses
    aasmbase;

{ Workaround for mantis #29906: bug in PPC jump table generation if a jump
  table is created for a case-statement that handles at least the lowest
  and highest possible value of the case expression type }
{$ifndef VER3_0_0}
  function objc_section_name(sec: TObjCAsmSectionType): string;
{$else}
  function objc_section_name(sec: TAsmSectionType): string;
{$endif}

implementation

  uses
    verbose,
    systems;

{$ifndef VER3_0_0}
  function objc_section_name(sec: TObjCAsmSectionType): string;
{$else}
  function objc_section_name(sec: TAsmSectionType): string;
{$endif}
    begin
      result:='';
      if target_info.system in systems_darwin then
        case sec of
          sec_objc_protocol:
            begin
              if not(target_info.system in systems_objc_nfabi) then
                result:='__OBJC, __protocol, regular, no_dead_strip'
              else
                internalerror(2015010605);
            end;
          sec_objc_protocol_ext:
            begin
              result:='__OBJC, __protocol_ext, regular, no_dead_strip';
            end;
          sec_objc_category:
            begin
              if not(target_info.system in systems_objc_nfabi) then
                result:='__OBJC, __category, regular, no_dead_strip'
              else
                internalerror(2015010606);
            end;
          sec_objc_class_ext:
            begin
              result:='__OBJC, __class_ext, regular, no_dead_strip';
            end;
          sec_objc_property:
            begin
              result:='__OBJC, __property, regular, no_dead_strip';
            end;
          sec_objc_image_info:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_imageinfo, regular, no_dead_strip'
              else
                result:='__OBJC, __image_info, regular, no_dead_strip';
            end;
          sec_objc_cstring_object:
            begin
              result:='__OBJC, __cstring_object, regular, no_dead_strip';
            end;
          sec_objc_sel_fixup:
            begin
              result:='__OBJC, __sel_fixup, regular, no_dead_strip';
              exit;
            end;
          sec_objc_message_refs:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_selrefs, literal_pointers, no_dead_strip'
              else
                result:='__OBJC, __message_refs, literal_pointers, no_dead_strip';
            end;
          sec_objc_symbols:
            begin
              if not(target_info.system in systems_objc_nfabi) then
                result:='__OBJC, __symbols, regular, no_dead_strip'
              else
                internalerror(2016010603);
            end;
          sec_objc_instance_vars:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_const'
              else
                result:=' __OBJC, __instance_vars, regular, no_dead_strip';
            end;
          sec_objc_cls_refs:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_clsrefs, regular, no_dead_strip'
              else
                result:='__OBJC, __cls_refs, literal_pointers, no_dead_strip';
            end;
          sec_objc_meth_var_types:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__TEXT, __objc_methtype, cstring_literals'
             else
               result:='__TEXT, __cstring, cstring_literals';
            end;
          sec_objc_meth_var_names:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__TEXT, __objc_methname, cstring_literals'
              else
                result:='__TEXT, __cstring, cstring_literals';
            end;
          sec_objc_class_names:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__TEXT, __objc_classname, cstring_literals'
              else
                result:='__TEXT, __cstring, cstring_literals';
            end;
          sec_objc_inst_meth:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_const'
              else
                result:='__OBJC, __inst_meth, regular, no_dead_strip';
            end;
          sec_objc_cls_meth:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_const'
              else
                result:='__OBJC, __cls_meth, regular, no_dead_strip';
            end;
          sec_objc_cat_inst_meth:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_const'
              else
                result:='__OBJC, __cat_inst_meth, regular, no_dead_strip';
            end;
          sec_objc_cat_cls_meth:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_const'
              else
                result:='__OBJC, __cat_cls_meth, regular, no_dead_strip';
            end;
          sec_objc_meta_class:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_const'
              else
                result:='__OBJC, __meta_class, regular, no_dead_strip';
            end;
          sec_objc_class:
            begin
              if target_info.system in systems_objc_nfabi then
                result:='__DATA, __objc_const'
              else
                result:='__OBJC, __class, regular, no_dead_strip';
            end;
          sec_objc_sup_refs:
            begin
              result:='__DATA, __objc_superrefs, regular, no_dead_strip';
            end;
          sec_objc_classlist:
            begin
              result:='__DATA, __objc_classlist, regular, no_dead_strip';
            end;
          sec_objc_nlclasslist:
            begin
              result:='__DATA, __objc_nlclasslist, regular, no_dead_strip';
            end;
          sec_objc_catlist:
            begin
              result:='__DATA, __objc_catlist, regular, no_dead_strip';
            end;
          sec_objc_nlcatlist:
            begin
              result:='__DATA, __objc_nlcatlist, regular, no_dead_strip';
            end;
          sec_objc_protolist:
            begin
              result:='__DATA, __objc_protolist, coalesced, no_dead_strip';
            end;
          sec_objc_module_info:
            begin
              if not(target_info.system in systems_objc_nfabi) then
                result:=' __OBJC, __module_info, regular, no_dead_strip'
              else
                internalerror(2016010601);
            end;
          sec_objc_data:
            begin
              result:='__DATA, __objc_data';
            end;
          sec_objc_const:
            begin
              result:='__DATA, __objc_const';
            end;
          sec_data_coalesced:
            begin
              result:='__DATA, __datacoal_nt, coalesced';
            end;
          else
            internalerror(2016010101);
          end
      else
        internalerror(2016010102);
    end;

end.

