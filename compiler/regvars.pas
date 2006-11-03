{
    Copyright (c) 1998-2002 by Florian Klaempfl and Jonas Maebe

    This unit handles register variable allocation

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
unit regvars;

{$i fpcdefs.inc}

interface

    uses
       aasmbase,aasmtai,aasmdata,aasmcpu,
       node,
       symsym,
       cpubase, cgbase, tgobj;

{$ifdef OLDREGVARS}
    procedure assign_regvars(p: tnode);
    procedure load_regvars(asml: TAsmList; p: tnode);
    procedure cleanup_regvars(asml: TAsmList);
    procedure store_regvar(asml: TAsmList; reg: tregister);
    procedure load_regvar(asml: TAsmList; vsym: tvarsym);
    procedure load_regvar_reg(asml: TAsmList; reg: tregister);
    procedure load_all_regvars(asml: TAsmList);
    procedure free_regvars(list: TAsmList);
{    procedure translate_regvars(list: TAsmList); }
{$endif OLDREGVARS}

{$ifdef i386}
(*
    procedure sync_regvars_other(list1, list2: TAsmList; const regvarsloaded1,
      regvarsloaded2: regvarother_booleanarray);
    procedure sync_regvars_int(list1, list2: TAsmList; const regvarsloaded1,
      regvarsloaded2: Tsuperregisterset);
*)
{$endif i386}

implementation

    uses
      globtype,systems,comphook,
      cutils,cclasses,verbose,globals,
      psub,
      symconst,symbase,symtype,symdef,paramgr,defutil,
      cpuinfo,cgobj,procinfo;

{$ifdef OLDREGVARS}
    procedure searchregvars(p : tnamedindexitem;arg:pointer);
      var
         i,j,k : longint;
         parasym : boolean;
      begin
         parasym:=pboolean(arg)^;
         if (tsym(p).typ=varsym) and ((tvarsym(p).varregable <> vr_none) or
             ((tvarsym(p).varspez in [vs_var,vs_const,vs_out]) and
              paramanager.push_addr_param(tvarsym(p).varspez,tvarsym(p).vardef,current_procinfo.procdef.proccalloption))) and
            not tvarsym(p).vardef.needs_inittable then
           begin
              j:=tvarsym(p).refs;
              { walk through all momentary register variables }
              for i:=1 to maxvarregs do
                begin
                  with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
                   if ((regvars[i]=nil) or (j>regvars_refs[i])) and (j>0) then
                     begin
                        for k:=maxvarregs-1 downto i do
                          begin
                             regvars[k+1]:=regvars[k];
                             regvars_para[k+1]:=regvars_para[k];
                             regvars_refs[k+1]:=regvars_refs[k];
                          end;
                        { calc the new refs
                        tvarsym(p).refs:=j; }
                        regvars[i]:=tsym(p);
                        regvars_para[i]:=parasym;
                        regvars_refs[i]:=j;
                        break;
                     end;
                end;
           end;
      end;


    procedure searchfpuregvars(p : tnamedindexitem;arg:pointer);
      var
         i,j,k : longint;
         parasym : boolean;
      begin
         parasym:=pboolean(arg)^;
         if (tsym(p).typ=varsym) and (tvarsym(p).varregable <> vr_none) then
           begin
              j:=tvarsym(p).refs;
              { parameter get a less value }
              { walk through all momentary register variables }
              for i:=1 to maxfpuvarregs do
                begin
                  with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
                   if ((fpuregvars[i]=nil) or (j>fpuregvars_refs[i])) and (j>0) then
                     begin
                        for k:=maxfpuvarregs-1 downto i do
                          begin
                             fpuregvars[k+1]:=fpuregvars[k];
                             fpuregvars_para[k+1]:=fpuregvars_para[k];
                             fpuregvars_refs[k+1]:=fpuregvars_refs[k];
                          end;
                        { calc the new refs
                        tvarsym(p).refs:=j; }
                        fpuregvars[i]:=tsym(p);
                        fpuregvars_para[i]:=parasym;
                        fpuregvars_refs[i]:=j;
                        break;
                     end;
                end;
           end;
      end;

    procedure assign_regvars(p: tnode);
          { register variables }
    var
{$ifndef i386}
      hp: tparaitem;
{$endif i386}
      regvarinfo: pregvarinfo;
      i: longint;
      parasym : boolean;
      siz : tcgsize;
    begin
      { max. optimizations     }
      { only if no asm is used }
      { and no try statement   }
      if (cs_opt_regvar in current_settings.optimizerswitches) and
        { we have to store regvars back to memory in this case (the nested }
        { procedures can access the variables of the parent)               }
        (tcgprocinfo(current_procinfo).nestedprocs.count = 0) and
         not(pi_has_assembler_block in current_procinfo.flags) and
         not(pi_uses_exceptions in current_procinfo.flags) then
        begin
          new(regvarinfo);
          fillchar(regvarinfo^,sizeof(regvarinfo^),0);
          current_procinfo.procdef.regvarinfo := regvarinfo;
          if (p.registersint<maxvarregs) then
            begin
              parasym:=false;
              symtablestack.foreach_static(searchregvars,@parasym);
              { copy parameter into a register ? }
              parasym:=true;
              symtablestack.next.foreach_static(searchregvars,@parasym);
              { hold needed registers free }
              for i:=maxvarregs downto maxvarregs-p.registersint+1 do
                begin
                  regvarinfo^.regvars[i]:=nil;
                  regvarinfo^.regvars_para[i] := false;
                end;
              { now assign register }
              for i:=1 to maxvarregs-p.registersint do
                begin
                  if assigned(regvarinfo^.regvars[i]) and
                    { currently we assume we can use registers for all }
                    { regvars if procedure does no call                }
                     (not(pi_do_call in current_procinfo.flags) or
                    { otherwise, demand some (arbitrary) minimum usage }
                      (tvarsym(regvarinfo^.regvars[i]).refs > 100)) then
                    begin
                      { register is no longer available for }
                      { expressions                          }
                      { search the register which is the most }
                      { unused                                }

                      { call by reference/const ? }
                      if paramanager.push_addr_param(tvarsym(regvarinfo^.regvars[i]).varspez,tvarsym(regvarinfo^.regvars[i]).vardef,current_procinfo.procdef.proccalloption) then
                        siz:=OS_32
                      else
                       if (tvarsym(regvarinfo^.regvars[i]).vardef.typ in [orddef,enumdef]) and
                          (tvarsym(regvarinfo^.regvars[i]).vardef.size=1) then
                        siz:=OS_8
                      else
                       if (tvarsym(regvarinfo^.regvars[i]).vardef.typ in [orddef,enumdef]) and
                          (tvarsym(regvarinfo^.regvars[i]).vardef.size=2) then
                        siz:=OS_16
                      else
                        siz:=OS_32;

                      { allocate a register for this regvar }
                      tvarsym(regvarinfo^.regvars[i]).localloc.register:=cg.getintregister(current_asmdata.CurrAsmList,siz);
                      tvarsym(regvarinfo^.regvars[i]).localloc.loc:=LOC_REGISTER;
                      { and make sure it can't be freed }
{                      rg.makeregvarint(getsupreg(regvarinfo^.regvars[i].localloc.register));}
                    end
                  else
                    begin
                      regvarinfo^.regvars[i] := nil;
                      regvarinfo^.regvars_para[i] := false;
                    end;
                end;
            end;
            if ((p.registersfpu+1)<maxfpuvarregs) then
              begin
                parasym:=false;
                symtablestack.foreach_static(searchfpuregvars,@parasym);
{$ifndef i386}
                { this code should be never enabled because     }
                { 1. the caller loads parameters into registers }
                { 2. (later) the CSE loads a parameter into a   }
                {    register, if necessary                     }
                {                                        (FK)   }
                { copy parameter into a register ? }
                parasym:=true;
                symtablestack.next.foreach_static(searchregvars,@parasym);
{$endif i386}
                { hold needed registers free }

                { in non leaf procedures we must be very careful }
                { with assigning registers                       }
{$ifdef i386}
                if current_settings.maxfpuregisters=-1 then
                  begin
                   if (pi_do_call in current_procinfo.flags) then
                     begin
                      for i:=maxfpuvarregs downto 2 do
                        regvarinfo^.fpuregvars[i]:=nil;
                     end
                   else
{$endif i386}
                     begin
                      for i:=maxfpuvarregs downto maxfpuvarregs-p.registersfpu do
                        regvarinfo^.fpuregvars[i]:=nil;
                     end;
{$ifdef i386}
                  end
                else
                  begin
                    for i:=current_settings.maxfpuregisters+1 to maxfpuvarregs do
                      regvarinfo^.fpuregvars[i]:=nil;
                  end;
{$endif i386}
                { now assign register }
                for i:=1 to maxfpuvarregs do
                  begin
                   if assigned(regvarinfo^.fpuregvars[i]) then
                     begin
{$ifdef i386}
                       { reserve place on the FPU stack }
                       {$error fixme x86 fpuregvars}
{                       regvarinfo^.fpuregvars[i].localloc.register:=trgcpu(rg).correct_fpuregister(NR_ST0,i);}
{$else i386}
{$ifdef x86_64}
{$endif x86_64}
                       begin
                         tvarsym(regvarinfo^.fpuregvars[i]).localloc.register:=cg.getfpuregister(current_asmdata.CurrAsmList,OS_F64);
                         tvarsym(regvarinfo^.fpuregvars[i]).localloc.loc:=LOC_FPUREGISTER;
{                         rg.makeregvarother(regvarinfo^.fpuregvars[i].localloc.register);}
                       end;
{$endif i386}
                     end;
                  end;
              end;
        end;
     end;



    procedure store_regvar(asml: TAsmList; reg: tregister);
    var
      i: longint;
      cgsize : tcgsize;
      r : tregister;
      hr: treference;
      regvarinfo: pregvarinfo;
      vsym: tvarsym;
      regidx : tregisterindex;
      supreg : tsuperregister;
    begin
{$ifdef i386}
      regvarinfo := pregvarinfo(current_procinfo.procdef.regvarinfo);
      if not assigned(regvarinfo) then
        exit;
      if getregtype(reg)=R_INTREGISTER then
        begin
          supreg:=getsupreg(reg);
          for i := 1 to maxvarregs do
            if assigned(regvarinfo^.regvars[i]) and
               (getsupreg(tvarsym(regvarinfo^.regvars[i]).localloc.register)=supreg) then
              begin
                {$warning fixme regvar_loaded_int}
(*                if supreg in rg.regvar_loaded_int then
                  begin
                    vsym := tvarsym(regvarinfo^.regvars[i]);
                    { we only have to store the regvar back to memory if it's }
                    { possible that it's been modified  (JM)                  }
                    if not(vsym.varspez in [vs_const,vs_var,vs_out]) then
                      begin
{$warning FIXME Check vsym.localloc for regvars}
//                        reference_reset_base(hr,current_procinfo.framepointer,vsym.adjusted_address);
                        cgsize:=def_cgsize(vsym.vardef);
                        cg.a_load_reg_ref(asml,cgsize,cgsize,vsym.localloc.register,hr);
                      end;
                    asml.concat(tai_regalloc.dealloc(vsym.localloc.register));
                    exclude(rg.regvar_loaded_int,supreg);
                  end;
*)
                break;
              end;
        end
      else
        begin
          for i := 1 to maxvarregs do
            if assigned(regvarinfo^.regvars[i]) then
              begin
                {$warning fixme regvars}
(*
                r:=rg.makeregsize(regvarinfo^.regvars[i].localloc.register,OS_INT);
                if (r = reg) then
                  begin
                    regidx:=findreg_by_number(r);
                    if rg.regvar_loaded_other[regidx] then
                      begin
                        vsym := tvarsym(regvarinfo^.regvars[i]);
                        { we only have to store the regvar back to memory if it's }
                        { possible that it's been modified  (JM)                  }
                        if not(vsym.varspez in [vs_const,vs_var,vs_out]) then
                          begin
{$warning FIXME Check vsym.localloc for regvars}
//                            reference_reset_base(hr,current_procinfo.framepointer,vsym.adjusted_address);
                            cgsize:=def_cgsize(vsym.vardef);
                            cg.a_load_reg_ref(asml,cgsize,cgsize,vsym.localloc.register,hr);
                          end;
                        asml.concat(tai_regalloc.dealloc(vsym.localloc.register));
                        rg.regvar_loaded_other[regidx] := false;
                      end;
                    break;
                  end;
*)
              end;
        end;
{$endif i386}
    end;

    procedure load_regvar(asml: TAsmList; vsym: tvarsym);
    var
      hr: treference;
      opsize: tcgsize;
      r,
      reg : tregister;
      regidx : tregisterindex;
    begin
{$ifndef i386}
      exit;
{$endif i386}
      reg:=vsym.localloc.register;
      {$warning fixme regvars}
(*
      if getregtype(reg)=R_INTREGISTER then
        begin

          if not(getsupreg(reg) in rg.regvar_loaded_int) then
            begin
              asml.concat(tai_regalloc.alloc(reg));
{$warning FIXME Check vsym.localloc for regvars}
//              reference_reset_base(hr,current_procinfo.framepointer,vsym.adjusted_address);
              if paramanager.push_addr_param(vsym.varspez,vsym.vardef,current_procinfo.procdef.proccalloption) then
                opsize := OS_ADDR
              else
                opsize := def_cgsize(vsym.vardef);
              cg.a_load_ref_reg(asml,opsize,opsize,hr,reg);
              include(rg.regvar_loaded_int,getsupreg(reg));
            end;
        end
      else
        begin
          r:=rg.makeregsize(reg,OS_INT);
          regidx:=findreg_by_number(r);
          if not rg.regvar_loaded_other[regidx] then
            begin
              asml.concat(tai_regalloc.alloc(reg));
{$warning FIXME Check vsym.localloc for regvars}
//              reference_reset_base(hr,current_procinfo.framepointer,vsym.adjusted_address);
              if paramanager.push_addr_param(vsym.varspez,vsym.vardef,current_procinfo.procdef.proccalloption) then
                opsize := OS_ADDR
              else
                opsize := def_cgsize(vsym.vardef);
              cg.a_load_ref_reg(asml,opsize,opsize,hr,reg);
              rg.regvar_loaded_other[regidx] := true;
            end;
        end;
*)
    end;

    procedure load_regvar_reg(asml: TAsmList; reg: tregister);
    var
      i: longint;
      regvarinfo: pregvarinfo;
      reg_spare : tregister;
      supreg : tsuperregister;
    begin
{
      regvarinfo := pregvarinfo(current_procinfo.procdef.regvarinfo);
      if not assigned(regvarinfo) then
        exit;
      if getregtype(reg)=R_INTREGISTER then
        begin
          supreg:=getsupreg(reg);
          for i := 1 to maxvarregs do
            if assigned(regvarinfo^.regvars[i]) and
               (getsupreg(regvarinfo^.regvars[i].localloc.register) = supreg) then
              load_regvar(asml,tvarsym(regvarinfo^.regvars[i]))
        end
      else
        begin
          reg_spare := cg.makeregsize(reg,OS_INT);
          for i := 1 to maxvarregs do
            if assigned(regvarinfo^.regvars[i]) and
               (cg.makeregsize(regvarinfo^.regvars[i].localloc.register,OS_INT) = reg_spare) then
              load_regvar(asml,tvarsym(regvarinfo^.regvars[i]))
        end;
}
    end;

    procedure load_all_regvars(asml: TAsmList);
{
    var
      i: longint;
      regvarinfo: pregvarinfo;
}
    begin
{
      regvarinfo := pregvarinfo(current_procinfo.procdef.regvarinfo);
      if not assigned(regvarinfo) then
        exit;
      for i := 1 to maxvarregs do
        if assigned(regvarinfo^.regvars[i]) then
          load_regvar(asml,tvarsym(regvarinfo^.regvars[i]))
}
    end;


    procedure load_regvars(asml: TAsmList; p: tnode);
    var
      i: longint;
      regvarinfo: pregvarinfo;
    begin
      if (cs_opt_regvar in current_settings.optimizerswitches) and
         not(pi_has_assembler_block in current_procinfo.flags) and
         not(pi_uses_exceptions in current_procinfo.flags) then
        begin
          regvarinfo := pregvarinfo(current_procinfo.procdef.regvarinfo);
          { can happen when inlining assembler procedures (JM) }
          if not assigned(regvarinfo) then
            exit;
          for i:=1 to maxfpuvarregs do
            begin
              if assigned(regvarinfo^.fpuregvars[i]) then
                begin
{$ifdef i386}
                  { reserve place on the FPU stack }
                  {$warning fixme fpustack}
(*
                  regvarinfo^.fpuregvars[i].localloc.register:=trgcpu(rg).correct_fpuregister(NR_ST0,i-1);
*)
                  asml.concat(Taicpu.op_none(A_FLDZ,S_NO));
{$endif i386}
                end;
            end;
{$ifdef i386}
          if assigned(p) then
            if cs_asm_source in current_settings.globalswitches then
              asml.insert(tai_comment.Create(strpnew(tostr(p.registersfpu)+
              ' registers on FPU stack used by temp. expressions')));
{$endif i386}
{
          for i:=1 to maxfpuvarregs do
            begin
               if assigned(regvarinfo^.fpuregvars[i]) then
                 begin
                    if cs_asm_source in current_settings.globalswitches then
                      asml.insert(tai_comment.Create(strpnew(regvarinfo^.fpuregvars[i].name+
                        ' with weight '+tostr(regvarinfo^.fpuregvars[i].refs)+' assigned to register '+
                        std_regname(regvarinfo^.fpuregvars[i].localloc.register))));
                    if (status.verbosity and v_debug)=v_debug then
                      Message3(cg_d_register_weight,std_regname(regvarinfo^.fpuregvars[i].localloc.register),
                        tostr(regvarinfo^.fpuregvars[i].refs),regvarinfo^.fpuregvars[i].name);
                 end;
            end;
          if cs_asm_source in current_settings.globalswitches then
            asml.insert(tai_comment.Create(strpnew('Register variable assignment:')));
}
        end;
    end;

{$ifdef i386}
(*
    procedure sync_regvars_other(list1, list2: TAsmList; const regvarsloaded1,
      regvarsloaded2: regvarother_booleanarray);
    var
      counter: tregisterindex;
    begin
      for counter := low(rg.regvar_loaded_other) to high(rg.regvar_loaded_other) do
        begin
           rg.regvar_loaded_other[counter] := regvarsloaded1[counter] and
             regvarsloaded2[counter];
           if regvarsloaded1[counter] xor regvarsloaded2[counter] then
             if regvarsloaded1[counter] then
               load_regvar_reg(list2,counter)
             else
               load_regvar_reg(list1,counter);
        end;
    end;


    procedure sync_regvars_int(list1, list2: TAsmList; const regvarsloaded1,
      regvarsloaded2: Tsuperregisterset);
    var
      i : longint;
      r : tregister;
    begin
      for i:=1 to maxvarregs do
        begin
          r:=newreg(R_INTREGISTER,varregs[i],R_SUBWHOLE);
          if (varregs[i] in regvarsloaded1) and
             not(varregs[i] in regvarsloaded2) then
            load_regvar_reg(list2,r)
          else
            if (varregs[i] in regvarsloaded2) and
               not(varregs[i] in regvarsloaded1) then
              load_regvar_reg(list1,r);
        end;
    end;
*)
{$endif i386}


    procedure cleanup_regvars(asml: TAsmList);
    var
      i: longint;
      reg : tregister;
      regidx : tregisterindex;
    begin
      { can happen when inlining assembler procedures (JM) }
      if not assigned(current_procinfo.procdef.regvarinfo) then
        exit;
      if (cs_opt_regvar in current_settings.optimizerswitches) and
         not(pi_has_assembler_block in current_procinfo.flags) and
         not(pi_uses_exceptions in current_procinfo.flags) then
        with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
          begin
{$ifdef i386}
            for i:=1 to maxfpuvarregs do
              if assigned(fpuregvars[i]) then
                { ... and clean it up }
                asml.concat(Taicpu.op_reg(A_FSTP,S_NO,NR_ST0));
{$endif i386}
(*
            for i := 1 to maxvarregs do
             begin
               if assigned(regvars[i]) then
                begin
                  reg:=regvars[i].localloc.register;
                  if getregtype(reg)=R_INTREGISTER then
                    begin
                    end
                  else
                    begin
                      reg:=cg.makeregsize(reg,OS_INT);
                      regidx:=findreg_by_number(reg);
                      {$warning fixme regvar dealloc}
{
                      if (rg.regvar_loaded_other[regidx]) then
                       asml.concat(tai_regalloc.dealloc(reg));
}
                    end;
                end;
             end;
*)
          end;
    end;

{

    Note: this one can't really be "fixed": register colouring happens after
      stabs generation. It could still be useful to generate the "var X is
      assigned to register Y with weight ZZZ" messages though

    procedure translate_regvars(list: TAsmList);
      var
        i: longint;
        r: tregister;
      begin
        if not assigned(current_procinfo.procdef.regvarinfo) then
          exit;
        with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
          begin
            for i := 1 to maxvarregs do
              if assigned(regvars[i]) then
                begin
                  cg.rg[R_INTREGISTER].translate_register(tvarsym(regvars[i]).localloc.register);
                  r:=tvarsym(regvars[i]).localloc.register;
                  if cs_asm_source in current_settings.globalswitches then
                   list.insert(tai_comment.Create(strpnew(tvarsym(regvars[i]).name+
                    ' with weight '+tostr(tvarsym(regvars[i]).refs)+' assigned to register '+
                    std_regname(r))));
                  Message3(cg_d_register_weight,std_regname(r),
                    tostr(tvarsym(regvars[i]).refs),tvarsym(regvars[i]).name);
                end;
            for i := 1 to maxfpuvarregs do
              if assigned(fpuregvars[i]) then
                begin
                  cg.rg[R_FPUREGISTER].translate_register(tvarsym(regvars[i]).localloc.register);
                  r:=tvarsym(fpuregvars[i]).localloc.register;
                  if cs_asm_source in current_settings.globalswitches then
                   list.insert(tai_comment.Create(strpnew(tvarsym(fpuregvars[i]).name+
                    ' with weight '+tostr(tvarsym(fpuregvars[i]).refs)+' assigned to register '+
                    std_regname(r))));
                  Message3(cg_d_register_weight,std_regname(r),
                    tostr(tvarsym(fpuregvars[i]).refs),tvarsym(fpuregvars[i]).name);
                end;
          end;
      end;
}

    procedure free_regvars(list: TAsmList);
      var
        i: longint;
        reg: tregister;
        size: tcgsize;
      begin
        if not assigned(current_procinfo.procdef.regvarinfo) then
          exit;
        with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
          begin
            for i := 1 to maxvarregs do
              if assigned(regvars[i]) then
                begin
                  reg:=cg.makeregsize(list,tvarsym(regvars[i]).localloc.register,OS_INT);
                  cg.a_load_reg_reg(list,OS_INT,OS_INT,reg,reg);
                end;
            for i := 1 to maxfpuvarregs do
              if assigned(fpuregvars[i]) then
                begin
                  reg:=tvarsym(fpuregvars[i]).localloc.register;
                  size:=reg_cgsize(reg);
                  cg.a_loadfpu_reg_reg(list,size,reg,reg);
                end;
          end;
      end;


{$endif OLDREGVARS}


end.
