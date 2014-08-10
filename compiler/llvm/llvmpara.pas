{
    Copyright (c) 2013 by Jonas Maebe

    Includes the llvm parameter manager

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
unit llvmpara;

{$i fpcdefs.inc}

  interface

    uses
      globtype,aasmdata,
      symconst,symtype,symdef,symsym,
      parabase,
      cpupara;

    type
      { LLVM stands for "low level code generator", and regarding parameter
        handling it is indeed very low level. We are responsible for decomposing
        aggregate parameters into multiple simple parameters in case they have
        to be passed in special registers (such as floating point or SSE), and
        also for indicating whether e.g. 8 bit parameters need to be sign or
        zero exntended. This corresponds to pretty much what we do when creating
        parameter locations, so we reuse the original parameter manager and then
        process its output.

        The future will tell whether we can do this without
        architecture-specific code, or whether we will have to integrate parts
        into the various tcpuparamanager classes }
      tllvmparamanager = class(tcpuparamanager)
        function param_use_paraloc(const cgpara: tcgpara): boolean; override;
        procedure createtempparaloc(list: TAsmList; calloption: tproccalloption; parasym: tparavarsym; can_use_final_stack_loc: boolean; var cgpara: TCGPara); override;
        function create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint; override;
        function get_funcretloc(p: tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara; override;
       private
        procedure set_llvm_paraloc_name(p: tabstractprocdef; hp: tparavarsym; var para: tcgpara);
        procedure add_llvm_callee_paraloc_names(p: tabstractprocdef);
      end;


  implementation

    uses
      verbose,
      aasmbase,
      llvmsym,
      paramgr,defutil,llvmdef,
      cgbase,cgutils,tgobj,hlcgobj;

  { tllvmparamanager }

  function tllvmparamanager.param_use_paraloc(const cgpara: tcgpara): boolean;
    begin
      { we can use the paraloc on the callee side if the SSA property is
        guaranteed, i.e., if it is a constant location (and if it's not been
        split up into multiple locations for ABI reasons). We can't deduce that
        from the paraloc though, we need the parasym for that. Potential
        future optimisation, although llvm will probably optimise away the
        temps we create anyway }
      result:=false;
    end;


  procedure tllvmparamanager.createtempparaloc(list: TAsmList; calloption: tproccalloption; parasym: tparavarsym; can_use_final_stack_loc: boolean; var cgpara: TCGPara);
    var
      paraloc,
      nextloc: pcgparalocation;
    begin
      inherited;
      paraloc:=cgpara.location;
      { No need to set paraloc^.llvmloc.*, these are not used/needed for temp
        paralocs }
      while assigned(paraloc) do
        begin
          { varargs parameters do not have a parasym.owner, but they're always
            by value }
          if (assigned(parasym.owner) and
              paramanager.push_addr_param(parasym.varspez,parasym.vardef,tabstractprocdef(parasym.owner.defowner).proccalloption)) or
             not llvmbyvalparaloc(paraloc) then
            begin
              case paraloc^.loc of
                LOC_REFERENCE:
                  begin
                    case hlcg.def2regtyp(paraloc^.def) of
                      R_INTREGISTER,
                      R_ADDRESSREGISTER:
                        paraloc^.loc:=LOC_REGISTER;
                      R_FPUREGISTER:
                        paraloc^.loc:=LOC_FPUREGISTER;
                      R_MMREGISTER:
                        paraloc^.Loc:=LOC_MMREGISTER;
                      else
                        internalerror(2013012308);
                    end;
                    paraloc^.register:=hlcg.getregisterfordef(list,paraloc^.def);
                    paraloc^.llvmvalueloc:=true;
                  end;
                LOC_REGISTER,
                LOC_FPUREGISTER,
                LOC_MMREGISTER:
                  begin
                    paraloc^.llvmvalueloc:=true;
                  end;
                LOC_VOID:
                  ;
                else
                  internalerror(2014012302);
              end;
            end
          else
            begin
              { turn this paraloc into the "byval" parameter: at the llvm level,
                a pointer to the value that it should place on the stack (or
                passed in registers, in some cases) }
              paraloc^.llvmvalueloc:=false;
              paraloc^.def:=getpointerdef(paraloc^.def);
              paraloc^.size:=def_cgsize(paraloc^.def);
              paraloc^.loc:=LOC_REGISTER;
              paraloc^.register:=hlcg.getaddressregister(list,paraloc^.def);
              { remove all other paralocs }
              nextloc:=paraloc^.next;
              while assigned(nextloc) do
                begin
                  dispose(nextloc);
                  nextloc:=paraloc^.next;
                end;
            end;
          paraloc^.llvmloc.loc:=paraloc^.loc;
          paraloc^.llvmloc.reg:=paraloc^.register;
          paraloc:=paraloc^.next;
        end;
    end;


  function tllvmparamanager.create_paraloc_info(p: tabstractprocdef; side: tcallercallee): longint;
    begin
      result:=inherited create_paraloc_info(p, side);
      { on the calleeside, llvm declares the parameters similar to Pascal or C
        (a list of parameters and their types), but they correspond more
        closely to parameter locations than to parameters -> add names to the
        locations }
      if side=calleeside then
        add_llvm_callee_paraloc_names(p)
    end;


  function tllvmparamanager.get_funcretloc(p: tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
    var
      paraloc: pcgparalocation;
    begin
      result:=inherited;
      paraloc:=result.location;
      repeat
        paraloc^.llvmvalueloc:=true;
        paraloc:=paraloc^.next;
      until not assigned(paraloc);
    end;


  { hp non-nil: parasym to check
    hp nil: function result
  }
  procedure tllvmparamanager.set_llvm_paraloc_name(p: tabstractprocdef; hp: tparavarsym; var para: tcgpara);
    var
      paraloc: PCGParaLocation;
      paralocnr: longint;
    begin
      paraloc:=hp.paraloc[calleeside].location;
      paralocnr:=0;
      repeat
        paraloc^.llvmloc.loc:=LOC_REFERENCE;
        paraloc^.llvmloc.sym:=current_asmdata.DefineAsmSymbol(llvmparaname(hp,paralocnr),AB_TEMP,AT_DATA);
        { byval: a pointer to a type that should actually be passed by
            value (e.g. a record that should be passed on the stack) }
        paraloc^.llvmvalueloc:=
          paramanager.push_addr_param(hp.varspez,hp.vardef,p.proccalloption) or
          not llvmbyvalparaloc(paraloc);
        paraloc:=paraloc^.next;
        inc(paralocnr);
      until not assigned(paraloc);
    end;


  procedure tllvmparamanager.add_llvm_callee_paraloc_names(p: tabstractprocdef);
    var
      paranr: longint;
      hp: tparavarsym;
    begin
      for paranr:=0 to p.paras.count-1 do
        begin
          hp:=tparavarsym(p.paras[paranr]);
          set_llvm_paraloc_name(p,hp,hp.paraloc[calleeside]);
        end;
    end;

begin
  { replace the native parameter manager. Maybe this has to be moved to a
    procedure like the creations of the code generators, but possibly not since
    we still call the original paramanager }
  paramanager.free;
  paramanager:=tllvmparamanager.create;
end.

