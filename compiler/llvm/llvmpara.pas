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
      aasmbase,
      llvmsym,
      paramgr,
      cgbase,hlcgobj;


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
    begin
      inherited;
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

  { hp non-nil: parasym to check
    hp nil: function result
  }
  procedure tllvmparamanager.set_llvm_paraloc_name(p: tabstractprocdef; hp: tparavarsym; var para: tcgpara);

    { the default for llvm is to pass aggregates in integer registers or
      on the stack (as the ABI prescribes). Records that require special
      handling, e.g. (partly) passing in fpu registers, have to be handled
      explicitly. This function returns whether an aggregate is handled
      specially }
    function has_non_default_paraloc: boolean;
      var
        loc: PCGParaLocation;
      begin
        loc:=para.Location;
        result:=true;
        while assigned(loc) do
          begin
            if not(loc^.loc in [LOC_REGISTER,LOC_REFERENCE]) then
              exit;
          end;
        result:=false;
      end;

    var
      paraloc: PCGParaLocation;
      signextstr: TSymStr;
      usedef: tdef;
      paralocnr: longint;
    begin
      { byval: a pointer to a type that should actually be passed by
          value (e.g. a record that should be passed on the stack) }
       if (assigned(hp) and
           (hp.vardef.typ in [arraydef,recorddef,objectdef]) and
           not paramanager.push_addr_param(hp.varspez,hp.vardef,p.proccalloption) and
           not has_non_default_paraloc) or
          (not assigned(hp) and
           ret_in_param(para.def,p)) then
        begin
          { the first location is the name of the "byval" parameter }
          paraloc:=para.location;
          if assigned(hp) then
             begin
              paraloc^.llvmloc:=current_asmdata.DefineAsmSymbol(llvmparaname(hp,0),AB_LOCAL,AT_DATA);
              paraloc^.llvmvalueloc:=false;
             end
          else
            begin
              paraloc^.llvmloc:=current_asmdata.DefineAsmSymbol('%f.result',AB_LOCAL,AT_DATA);
              paraloc^.llvmvalueloc:=true;
            end;
          { the other locations, if any, are not represented individually; llvm
            takes care of them behind the scenes }
          while assigned(paraloc^.next) do
            begin
              paraloc:=paraloc^.next;
              paraloc^.llvmloc:=nil;
            end;
          exit;
        end;
      paraloc:=hp.paraloc[calleeside].location;
      paralocnr:=0;
      repeat
        paraloc^.llvmloc:=current_asmdata.DefineAsmSymbol(llvmparaname(hp,paralocnr),AB_LOCAL,AT_DATA);
        paraloc^.llvmvalueloc:=true;
        paraloc:=paraloc^.next;
        inc(paralocnr);
      until not assigned(paraloc);
    end;


  function tllvmparamanager.get_funcretloc(p: tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
    begin
      result:=inherited;
      { TODO: initialise result.llvmloc }
    end;

    procedure tllvmparamanager.add_llvm_callee_paraloc_names(p: tabstractprocdef);
      var
        paranr: longint;
        para: tcgpara;
        hp: tparavarsym;
        first: boolean;
      begin
        first:=true;
        for paranr:=0 to p.paras.count-1 do
          begin
            hp:=tparavarsym(p.paras[paranr]);
            set_llvm_paraloc_name(p,hp,hp.paraloc[calleeside]);
          end;
      end;

begin
  { replace the native code generator. Maybe this has to be moved to a procedure
    like the creations of the code generators, but possibly not since we still
    call the original paramanager }
  paramanager.free;
  paramanager:=tllvmparamanager.create;
end.

