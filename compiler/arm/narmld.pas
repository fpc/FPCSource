{
    Copyright (c) 1998-2018 by Florian Klaempfl

    Generate arm assembler for load nodes

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
unit narmld;

{$i fpcdefs.inc}

interface

    uses
      globtype,
      symsym,
      node,ncgld,pass_1,aasmbase;

    type
      tarmloadnode = class(tcgloadnode)
         procedure generate_threadvar_access(gvs : tstaticvarsym); override;
      end;


implementation

    uses
      globals,verbose,
      cgbase,cgobj,cgutils,
      aasmdata,aasmcpu,
      systems,
      symcpu,symdef,
      nld,
      cpubase,
      parabase,
      procinfo;

{*****************************************************************************
                            TARMLOADNODE
*****************************************************************************}

    procedure tarmloadnode.generate_threadvar_access(gvs: tstaticvarsym);
      var
        href: treference;
        hregister : tregister;
        handled: boolean;
        l : TAsmLabel;
      begin
        handled:=false;
        if tf_section_threadvars in target_info.flags then
          begin
            if target_info.system in [system_arm_linux] then
              begin
                if not(pi_uses_threadvar in current_procinfo.flags) then
                  internalerror(2012012101);
                case current_settings.tlsmodel of
                  tlsm_global_dynamic:
                    begin
{$ifdef use_tls_dialect_gnu}
                      current_asmdata.getjumplabel(l);
                      reference_reset_symbol(href,current_asmdata.RefAsmSymbol(gvs.mangledname,AT_TLS),-8,sizeof(AInt),[]);
                      href.refaddr:=addr_tlsgd;
                      href.relsymbol:=l;
                      hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
                      cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,hregister);
                      cg.a_label(current_asmdata.CurrAsmList,l);
                      cg.getcpuregister(current_asmdata.CurrAsmList,NR_R0);
                      cg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_ADDR,hregister,NR_PC,NR_R0);
                      cg.g_call(current_asmdata.CurrAsmList,'__tls_get_addr');
                      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_R0);
                      hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,NR_R0,hregister);
                      reference_reset(location.reference,location.reference.alignment,location.reference.volatility);
                      location.reference.base:=hregister;
{$else use_tls_dialect_gnu}
                      { On arm, we use the gnu2 tls dialect. It has the advantage that it can be relaxed (optimized) by the linker,
                        this is not possible with the gnu tls dialect.

                        gnu2 is proposed and documented in
                          Glauber de Oliveira Costa, Alexandre Oliva: Speeding Up Thread-Local Storage Access in DynamicLibraries in the ARM platform, 2006.
                          Link: https://www.fsfla.org/~lxoliva/writeups/TLS/paper-lk2006.pdf
                      }
                      current_asmdata.getjumplabel(l);
                      reference_reset_symbol(href,current_asmdata.RefAsmSymbol(gvs.mangledname,AT_TLS),0,sizeof(AInt),[]);
                      href.refaddr:=addr_tlsdesc;
                      href.relsymbol:=l;
                      hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
                      cg.getcpuregister(current_asmdata.CurrAsmList,NR_R0);
                      cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,NR_R0);
                      cg.a_label(current_asmdata.CurrAsmList,l);

                      { we have to go the ugly way so we can set addr_tlscall }
                      cg.allocallcpuregisters(current_asmdata.CurrAsmList);
                      cg.a_call_name(current_asmdata.CurrAsmList,gvs.mangledname,false);
                      with taicpu(current_asmdata.CurrAsmList.Last) do
                        begin
                          if opcode<>A_BL then
                            Internalerror(2019092902);
                          oper[0]^.ref^.refaddr:=addr_tlscall;
                        end;
                      cg.deallocallcpuregisters(current_asmdata.CurrAsmList);

                      cg.getcpuregister(current_asmdata.CurrAsmList,NR_R0);
                      cg.ungetcpuregister(current_asmdata.CurrAsmList,NR_R0);
                      hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,NR_R0,hregister);
                      reference_reset(location.reference,location.reference.alignment,location.reference.volatility);
                      location.reference.base:=current_procinfo.tlsoffset;
                      include(current_procinfo.flags,pi_needs_tls);
                      location.reference.index:=hregister;
{$endif use_tls_dialect_gnu}
                      handled:=true;
                    end;
                  tlsm_initial_exec:
                    begin
                      current_asmdata.getjumplabel(l);
                      reference_reset_symbol(href,current_asmdata.RefAsmSymbol(gvs.mangledname,AT_TLS),-8,sizeof(AInt),[]);
                      href.refaddr:=addr_tpoff;
                      href.relsymbol:=l;
                      hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
                      cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,hregister);
                      cg.a_label(current_asmdata.CurrAsmList,l);
                      reference_reset(href,0,[]);
                      href.base:=NR_PC;
                      href.index:=hregister;
                      hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
                      cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,href,hregister);
                      location.reference.base:=current_procinfo.tlsoffset;
                      include(current_procinfo.flags,pi_needs_tls);
                      location.reference.index:=hregister;
                      handled:=true;
                    end;
                  tlsm_local_exec:
                    begin
                      reference_reset_symbol(href,current_asmdata.RefAsmSymbol(gvs.mangledname,AT_TLS),0,sizeof(AInt),[]);
                      href.refaddr:=addr_tpoff;
                      hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
                      cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,hregister);
                      reference_reset(href,0,[]);
                      location.reference.base:=current_procinfo.tlsoffset;
                      include(current_procinfo.flags,pi_needs_tls);
                      location.reference.index:=hregister;
                      handled:=true;
                    end;
                  else
                    Internalerror(2019092802);
                end;
              end;
          end;

        if not handled then
          inherited;
      end;


begin
   cloadnode:=tarmloadnode;
end.
