{
    $Id$
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
       aasmbase,aasmtai,aasmcpu,
       node,
       symsym,
       cpubase, cginfo, tgobj, rgobj;

    procedure assign_regvars(p: tnode);
    procedure load_regvars(asml: TAAsmoutput; p: tnode);
    procedure cleanup_regvars(asml: TAAsmoutput);
    procedure store_regvar(asml: TAAsmoutput; reg: tregister);
    procedure load_regvar(asml: TAAsmoutput; vsym: tvarsym);
    procedure load_regvar_reg(asml: TAAsmoutput; reg: tregister);
    procedure load_all_regvars(asml: TAAsmoutput);
   procedure free_regvars(list: taasmoutput);
   procedure translate_regvars(list: taasmoutput; const table:Ttranstable);

{$ifdef i386}
    procedure sync_regvars_other(list1, list2: taasmoutput; const regvarsloaded1,
      regvarsloaded2: regvarother_booleanarray);
    procedure sync_regvars_int(list1, list2: taasmoutput; const regvarsloaded1,
      regvarsloaded2: Tsuperregisterset);
{$endif i386}

implementation

    uses
      globtype,systems,comphook,
      cutils,cclasses,verbose,globals,
      psub,
      symconst,symbase,symtype,symdef,paramgr,defutil,
      cpuinfo,cgbase,cgobj,rgcpu;


    procedure searchregvars(p : tnamedindexitem;arg:pointer);
      var
         i,j,k : longint;
         parasym : boolean;
      begin
         parasym:=pboolean(arg)^;
         if (tsym(p).typ=varsym) and (vo_regable in tvarsym(p).varoptions) then
           begin
              j:=tvarsym(p).refs;
              { parameter get a less value }
              if parasym then
                begin
                   if cs_littlesize in aktglobalswitches  then
                     dec(j,1)
                   else
                     dec(j,100);
                end;
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
                        regvars[i]:=tvarsym(p);
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
         if (tsym(p).typ=varsym) and (vo_fpuregable in tvarsym(p).varoptions) then
           begin
              j:=tvarsym(p).refs;
              { parameter get a less value }
              if parasym then
                begin
                   if cs_littlesize in aktglobalswitches  then
                     dec(j,1)
                   else
                     dec(j,100);
                end;
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
                        fpuregvars[i]:=tvarsym(p);
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
      if (cs_regvars in aktglobalswitches) and
{$ifndef i386}
         { we have to store regvars back to memory in this case! }
         (tcgprocinfo(current_procinfo).nestedprocs.count = 0) and
{$endif i386}
         not(pi_uses_asm in current_procinfo.flags) and
         not(pi_uses_exceptions in current_procinfo.flags) then
        begin
          new(regvarinfo);
          fillchar(regvarinfo^,sizeof(regvarinfo^),0);
          current_procinfo.procdef.regvarinfo := regvarinfo;
          if (p.registers32<maxvarregs) then
            begin
              parasym:=false;
              symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}searchregvars,@parasym);
              { copy parameter into a register ? }
              parasym:=true;
              symtablestack.next.foreach_static({$ifdef FPCPROCVAR}@{$endif}searchregvars,@parasym);
              { hold needed registers free }
              for i:=maxvarregs downto maxvarregs-p.registers32+1-maxintscratchregs do
                begin
                  regvarinfo^.regvars[i]:=nil;
                  regvarinfo^.regvars_para[i] := false;
                end;
              { now assign register }
              for i:=1 to maxvarregs-p.registers32-maxintscratchregs do
                begin
                  if assigned(regvarinfo^.regvars[i]) and
                    { currently we assume we can use volatile registers for all }
                    { regvars if procedure does no call                         }
                     (not(pi_do_call in current_procinfo.flags) or
                    { otherwise, demand some (arbitrary) minimum usage }
                      (regvarinfo^.regvars[i].refs > 100)) then
                    begin
                      { register is no longer available for }
                      { expressions                          }
                      { search the register which is the most }
                      { unused                                }

                      { call by reference/const ? }
                      if paramanager.push_addr_param(regvarinfo^.regvars[i].varspez,regvarinfo^.regvars[i].vartype.def,current_procinfo.procdef.proccalloption) then
                        siz:=OS_32
                      else
                       if (regvarinfo^.regvars[i].vartype.def.deftype in [orddef,enumdef]) and
                          (regvarinfo^.regvars[i].vartype.def.size=1) then
                        siz:=OS_8
                      else
                       if (regvarinfo^.regvars[i].vartype.def.deftype in [orddef,enumdef]) and
                          (regvarinfo^.regvars[i].vartype.def.size=2) then
                        siz:=OS_16
                      else
                        siz:=OS_32;

                      { allocate a register for this regvar }
                      regvarinfo^.regvars[i].localloc.register:=rg.getregisterint(exprasmlist,siz);
                      { and make sure it can't be freed }
                      rg.makeregvarint(getsupreg(regvarinfo^.regvars[i].localloc.register));
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
                symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}searchfpuregvars,@parasym);
{$ifdef dummy}
                { this code should be never enabled because     }
                { 1. the caller loads parameters into registers }
                { 2. (later) the CSE loads a parameter into a   }
                {    register, if necessary                     }
                {                                        (FK)   }
                { copy parameter into a register ? }
                parasym:=true;
                symtablestack.next.foreach_static({$ifdef FPCPROCVAR}@{$endif}searchregvars);
{$endif dummy}
                { hold needed registers free }

                { in non leaf procedures we must be very careful }
                { with assigning registers                       }
                if aktmaxfpuregisters=-1 then
                  begin
                   if (pi_do_call in current_procinfo.flags) then
                     begin
                      for i:=maxfpuvarregs downto 2 do
                        regvarinfo^.fpuregvars[i]:=nil;
                     end
                   else
                     begin
                      for i:=maxfpuvarregs downto maxfpuvarregs-p.registersfpu do
                        regvarinfo^.fpuregvars[i]:=nil;
                     end;
                  end
                else
                  begin
                    for i:=aktmaxfpuregisters+1 to maxfpuvarregs do
                      regvarinfo^.fpuregvars[i]:=nil;
                  end;
                { now assign register }
                for i:=1 to maxfpuvarregs do
                  begin
                   if assigned(regvarinfo^.fpuregvars[i]) then
                     begin
{$ifdef i386}
                       { reserve place on the FPU stack }
                       regvarinfo^.fpuregvars[i].localloc.register:=trgcpu(rg).correct_fpuregister(NR_ST0,i);
{$else i386}
                       regvarinfo^.fpuregvars[i].localloc.register:=fpuvarregs[i];
                       rg.makeregvarother(regvarinfo^.fpuregvars[i].localloc.register);
{$endif i386}
                     end;
                  end;
              end;
        end;
     end;



    procedure store_regvar(asml: TAAsmoutput; reg: tregister);
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
               (getsupreg(regvarinfo^.regvars[i].localloc.register)=supreg) then
              begin
                if supreg in rg.regvar_loaded_int then
                  begin
                    vsym := tvarsym(regvarinfo^.regvars[i]);
                    { we only have to store the regvar back to memory if it's }
                    { possible that it's been modified  (JM)                  }
                    if not(vsym.varspez in [vs_const,vs_var,vs_out]) then
                      begin
{$warning FIXME Check vsym.localloc for regvars}
//                        reference_reset_base(hr,current_procinfo.framepointer,vsym.adjusted_address);
                        cgsize:=def_cgsize(vsym.vartype.def);
                        cg.a_load_reg_ref(asml,cgsize,cgsize,vsym.localloc.register,hr);
                      end;
                    asml.concat(tai_regalloc.dealloc(vsym.localloc.register));
                    exclude(rg.regvar_loaded_int,supreg);
                  end;
                break;
              end;
        end
      else
        begin
          for i := 1 to maxvarregs do
            if assigned(regvarinfo^.regvars[i]) then
              begin
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
                            cgsize:=def_cgsize(vsym.vartype.def);
                            cg.a_load_reg_ref(asml,cgsize,cgsize,vsym.localloc.register,hr);
                          end;
                        asml.concat(tai_regalloc.dealloc(vsym.localloc.register));
                        rg.regvar_loaded_other[regidx] := false;
                      end;
                    break;
                  end;
              end;
        end;
{$endif i386}
    end;

    procedure load_regvar(asml: TAAsmoutput; vsym: tvarsym);
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
      if getregtype(reg)=R_INTREGISTER then
        begin
          if not(getsupreg(reg) in rg.regvar_loaded_int) then
            begin
              asml.concat(tai_regalloc.alloc(reg));
{$warning FIXME Check vsym.localloc for regvars}
//              reference_reset_base(hr,current_procinfo.framepointer,vsym.adjusted_address);
              if paramanager.push_addr_param(vsym.varspez,vsym.vartype.def,current_procinfo.procdef.proccalloption) then
                opsize := OS_ADDR
              else
                opsize := def_cgsize(vsym.vartype.def);
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
              if paramanager.push_addr_param(vsym.varspez,vsym.vartype.def,current_procinfo.procdef.proccalloption) then
                opsize := OS_ADDR
              else
                opsize := def_cgsize(vsym.vartype.def);
              cg.a_load_ref_reg(asml,opsize,opsize,hr,reg);
              rg.regvar_loaded_other[regidx] := true;
            end;
        end;
    end;

    procedure load_regvar_reg(asml: TAAsmoutput; reg: tregister);
    var
      i: longint;
      regvarinfo: pregvarinfo;
      reg_spare : tregister;
      supreg : tsuperregister;
    begin
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
          reg_spare := rg.makeregsize(reg,OS_INT);
          for i := 1 to maxvarregs do
            if assigned(regvarinfo^.regvars[i]) and
               (rg.makeregsize(regvarinfo^.regvars[i].localloc.register,OS_INT) = reg_spare) then
              load_regvar(asml,tvarsym(regvarinfo^.regvars[i]))
        end;
    end;

    procedure load_all_regvars(asml: TAAsmoutput);
    var
      i: longint;
      regvarinfo: pregvarinfo;
    begin
      regvarinfo := pregvarinfo(current_procinfo.procdef.regvarinfo);
      if not assigned(regvarinfo) then
        exit;
      for i := 1 to maxvarregs do
        if assigned(regvarinfo^.regvars[i]) then
          load_regvar(asml,tvarsym(regvarinfo^.regvars[i]))
    end;


    procedure load_regvars(asml: TAAsmoutput; p: tnode);
    var
      i: longint;
      regvarinfo: pregvarinfo;
    begin
      if (cs_regvars in aktglobalswitches) and
         not(pi_uses_asm in current_procinfo.flags) and
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
                  regvarinfo^.fpuregvars[i].localloc.register:=trgcpu(rg).correct_fpuregister(NR_ST0,i-1);
                  asml.concat(Taicpu.op_none(A_FLDZ,S_NO));
{$endif i386}
                end;
            end;
{$ifdef i386}
          if assigned(p) then
            if cs_asm_source in aktglobalswitches then
              asml.insert(tai_comment.Create(strpnew(tostr(p.registersfpu)+
              ' registers on FPU stack used by temp. expressions')));
{$endif i386}
          for i:=1 to maxfpuvarregs do
            begin
               if assigned(regvarinfo^.fpuregvars[i]) then
                 begin
                    if cs_asm_source in aktglobalswitches then
                      asml.insert(tai_comment.Create(strpnew(regvarinfo^.fpuregvars[i].name+
                        ' with weight '+tostr(regvarinfo^.fpuregvars[i].refs)+' assigned to register '+
                        std_regname(regvarinfo^.fpuregvars[i].localloc.register))));
                    if (status.verbosity and v_debug)=v_debug then
                      Message3(cg_d_register_weight,std_regname(regvarinfo^.fpuregvars[i].localloc.register),
                        tostr(regvarinfo^.fpuregvars[i].refs),regvarinfo^.fpuregvars[i].name);
                 end;
            end;
          if cs_asm_source in aktglobalswitches then
            asml.insert(tai_comment.Create(strpnew('Register variable assignment:')));
        end;
    end;

{$ifdef i386}
    procedure sync_regvars_other(list1, list2: taasmoutput; const regvarsloaded1,
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


    procedure sync_regvars_int(list1, list2: taasmoutput; const regvarsloaded1,
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
{$endif i386}


    procedure cleanup_regvars(asml: TAAsmoutput);
    var
      i: longint;
      reg : tregister;
      regidx : tregisterindex;
    begin
      { can happen when inlining assembler procedures (JM) }
      if not assigned(current_procinfo.procdef.regvarinfo) then
        exit;
      if (cs_regvars in aktglobalswitches) and
         not(pi_uses_asm in current_procinfo.flags) and
         not(pi_uses_exceptions in current_procinfo.flags) then
        with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
          begin
{$ifdef i386}
            for i:=1 to maxfpuvarregs do
              if assigned(fpuregvars[i]) then
                { ... and clean it up }
                asml.concat(Taicpu.op_reg(A_FSTP,S_NO,NR_ST0));
{$endif i386}
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
                      reg:=rg.makeregsize(reg,OS_INT);
                      regidx:=findreg_by_number(reg);
                      if (rg.regvar_loaded_other[regidx]) then
                       asml.concat(tai_regalloc.dealloc(reg));
                    end;
                end;
             end;
          end;
    end;


    procedure free_regvars(list: taasmoutput);
      var
        i: longint;
      begin
        if not assigned(current_procinfo.procdef.regvarinfo) then
          exit;
        with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
          for i := 1 to maxvarregs do
            if assigned(regvars[i]) { and
              (regvars[i] <> tvarsym(current_procinfo.procdef.funcretsym))} then
              begin
                { make sure the unget isn't just a nop }
                exclude(rg.is_reg_var_int,getsupreg(regvars[i].localloc.register));
                rg.ungetregisterint(list,regvars[i].localloc.register);
              end;
      end;


    procedure translate_regvars(list: taasmoutput; const table:Ttranstable);
      var
        i: longint;
        r: tregister;
      begin
        if not assigned(current_procinfo.procdef.regvarinfo) then
          exit;
        with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
          for i := 1 to maxvarregs do
            if assigned(regvars[i]) { and
              (regvars[i] <> tvarsym(current_procinfo.procdef.funcretsym))} then
              begin
                setsupreg(regvars[i].localloc.register,getsupreg(table[getsupreg(regvars[i].localloc.register)]));
                r:=regvars[i].localloc.register;
                if cs_asm_source in aktglobalswitches then
                 list.insert(tai_comment.Create(strpnew(regvars[i].name+
                  ' with weight '+tostr(regvars[i].refs)+' assigned to register '+
                  std_regname(r))));
                Message3(cg_d_register_weight,std_regname(r),
                  tostr(regvars[i].refs),regvars[i].name);
              end;
      end;

end.

{
  $Log$
  Revision 1.66  2003-09-23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.65  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.64  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.63  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.62.2.2  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.62.2.1  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.62  2003/08/17 20:47:47  daniel
    * Notranslation changed into -sr functionality

  Revision 1.61  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.60  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.59  2003/08/09 18:56:54  daniel
    * cs_regalloc renamed to cs_regvars to avoid confusion with register
      allocator
    * Some preventive changes to i386 spillinh code

  Revision 1.58  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.57  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.56  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.55  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.54  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.53  2003/05/31 20:33:57  jonas
    * temp fix/hack for nested procedures (disable regvars in all procedures
      that have nested procedures)
    * leave register parameters in their own register (instead of storing
      them to memory or assigning them to another register) if the current
      procedure doesn't call any other procedures

  Revision 1.52  2003/05/30 18:55:21  jonas
    * fixed several regvar related bugs for non-i386. make cycle with -Or now
      works for ppc

  Revision 1.51  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.50  2003/05/16 14:33:31  peter
    * regvar fixes

  Revision 1.49  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.48  2003/05/12 17:22:00  jonas
    * fixed (last?) remaining -tvarsym(X).address to
      tg.direction*tvarsym(X).address...

  Revision 1.47  2003/04/27 11:21:34  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.46  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.45  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.44  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.43  2002/11/25 17:43:24  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.42  2002/11/18 17:31:59  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.41  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.40  2002/08/18 20:06:25  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.39  2002/08/17 09:23:41  florian
    * first part of procinfo rewrite

  Revision 1.38  2002/08/06 20:55:22  florian
    * first part of ppc calling conventions fix

  Revision 1.37  2002/07/20 11:57:57  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.36  2002/07/11 14:41:30  florian
    * start of the new generic parameter handling

  Revision 1.35  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.34  2002/06/24 12:43:00  jonas
    * fixed errors found with new -CR code from Peter when cycling with -O2p3r

  Revision 1.33  2002/05/18 13:34:17  peter
    * readded missing revisions

  Revision 1.32  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.30  2002/05/12 16:53:10  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.29  2002/04/21 15:23:34  carl
  + changeregsize -> makeregsize

  Revision 1.28  2002/04/19 15:46:03  peter
    * mangledname rewrite, tprocdef.mangledname is now created dynamicly
      in most cases and not written to the ppu
    * add mangeledname_prefix() routine to generate the prefix of
      manglednames depending on the current procedure, object and module
    * removed static procprefix since the mangledname is now build only
      on demand from tprocdef.mangledname

  Revision 1.27  2002/04/15 19:44:19  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.26  2002/04/15 19:04:04  carl
  + reg2str -> std_reg2str()

  Revision 1.25  2002/04/06 18:13:01  jonas
    * several powerpc-related additions and fixes

  Revision 1.24  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.23  2002/03/31 20:26:36  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}
