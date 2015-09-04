{
    Copyright (c) 2002-2014 by Florian Klaempfl

    Generate i8086 assembler for load nodes

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
unit n8086ld;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      symsym,symtype,
      node,ncgld, aasmbase;

    type

      { ti8086loadnode }

      ti8086loadnode = class(tcgloadnode)
        protected
         procedure generate_nested_access(vs: tsym); override;
         procedure generate_absaddr_access(vs: tabsolutevarsym); override;
        public
         procedure pass_generate_code;override;
      end;


implementation

    uses
      globals,aasmdata,defutil,
      symconst,symcpu,
      nld,
      cgbase,cgobj,cgutils,
      cpubase,cpuinfo;

{*****************************************************************************
                            TI8086LOADNODE
*****************************************************************************}

    procedure ti8086loadnode.generate_nested_access(vs: tsym);
      begin
        inherited;

        { the parentfp pointer is always a near pointer (this is turbo pascal
          compatible) regardless of memory model, so we need to set the segment
          manually.

          todo: once the far data memory models are fully implemented, the
          parentfp type should be changed to a near 'ss' pointer in all memory
          models and then this code can be removed. But this can only happen
          after:
          1) all calls to a_loadaddr_ref_reg go through the high level code
             generator
          2) a_loadaddr_ref_reg in the low level code generator stops using
             the presence of a segment in the source reference to determine the
             destination reg size
          3) make_simple_ref is updated to remove unnecessary segment prefixes
          4) hlcg.reference_reset_base is updated to set the segment on near_ss
             pointers }
        if (left.nodetype=loadparentfpn) and
           (current_settings.x86memorymodel in x86_far_data_models) then
          location.reference.segment:=NR_SS;
      end;

    procedure ti8086loadnode.generate_absaddr_access(vs: tabsolutevarsym);
      begin
        if tcpuabsolutevarsym(symtableentry).absseg then
          begin
            location.reference.segment:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
            cg.a_load_const_reg(current_asmdata.CurrAsmList,OS_16,aint(tcpuabsolutevarsym(symtableentry).addrsegment),location.reference.segment);
          end;
        inherited;
      end;

    procedure ti8086loadnode.pass_generate_code;
      var
        gvs: tstaticvarsym;
        segref: treference;
        refsym: TAsmSymbol;
        segreg: TRegister;
        newsize: TCgSize;
      begin
        if current_settings.x86memorymodel=mm_huge then
          begin
            case symtableentry.typ of
              staticvarsym:
                begin
                  gvs:=tstaticvarsym(symtableentry);
                  if (vo_is_dll_var in gvs.varoptions) then
                  { DLL variable }
                    begin
                      inherited pass_generate_code;
                      exit;
                    end
                  { Thread variable }
                  else if (vo_is_thread_var in gvs.varoptions) then
                    begin
                      inherited pass_generate_code;
                      exit;
                    end
                  { Normal (or external) variable }
                  else
                    begin
                      if gvs.Owner.iscurrentunit then
                        begin
                          inherited pass_generate_code;
                          exit;
                        end;

                      { we don't know the size of all arrays }
                      newsize:=def_cgsize(resultdef);
                      { alignment is overridden per case below }
                      location_reset_ref(location,LOC_REFERENCE,newsize,resultdef.alignment);

                      if gvs.localloc.loc=LOC_INVALID then
                        begin
                          if not(vo_is_weak_external in gvs.varoptions) then
                            refsym:=current_asmdata.RefAsmSymbol(gvs.mangledname)
                          else
                            refsym:=current_asmdata.WeakRefAsmSymbol(gvs.mangledname);

                          segreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);

                          reference_reset_symbol(segref,refsym,0,0);
                          segref.refaddr:=addr_seg;
                          cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_16,OS_16,segref,segreg);

                          reference_reset_symbol(location.reference,refsym,0,location.reference.alignment);
                          location.reference.segment:=segreg;
                        end
                      else
                        location:=gvs.localloc;
                    end;

                  { make const a LOC_CREFERENCE }
                  if (gvs.varspez=vs_const) and
                     (location.loc=LOC_REFERENCE) then
                    location.loc:=LOC_CREFERENCE;
                end;
              else
                inherited pass_generate_code;
            end;
          end
        else
          inherited pass_generate_code;
      end;


begin
   cloadnode:=ti8086loadnode;
end.
