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
    procedure store_regvar_int(asml:Taasmoutput;reg:Tsuperregister);
    procedure store_regvar(asml: TAAsmoutput; reg: tregister);
    procedure load_regvar(asml: TAAsmoutput; vsym: tvarsym);
    procedure load_regvar_reg(asml: TAAsmoutput; reg: tregister);
    procedure load_all_regvars(asml: TAAsmoutput);

    procedure sync_regvars(list1, list2: taasmoutput; const regvarsloaded1,
      regvarsloaded2: regvar_booleanarray);

implementation

    uses
      globtype,systems,comphook,
      cutils,cclasses,verbose,globals,
      symconst,symbase,symtype,symdef,paramgr,defutil,
      cgbase,cgobj,cgcpu,rgcpu;


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
                  with pregvarinfo(aktprocdef.regvarinfo)^ do
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
                  with pregvarinfo(aktprocdef.regvarinfo)^ do
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
      regvarinfo: pregvarinfo;
      i: longint;
      parasym : boolean;
      r : Tregister;
    begin
      { max. optimizations     }
      { only if no asm is used }
      { and no try statement   }
      if (cs_regalloc in aktglobalswitches) and
         ((procinfo.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
        begin
          new(regvarinfo);
          fillchar(regvarinfo^,sizeof(regvarinfo^),0);
          aktprocdef.regvarinfo := regvarinfo;
          if (p.registers32<4) then
            begin
              parasym:=false;
              symtablestack.foreach_static({$ifdef FPCPROCVAR}@{$endif}searchregvars,@parasym);
              { copy parameter into a register ? }
              parasym:=true;
              symtablestack.next.foreach_static({$ifdef FPCPROCVAR}@{$endif}searchregvars,@parasym);
              { hold needed registers free }
              for i:=maxvarregs downto maxvarregs-p.registers32+1 do
                begin
                  regvarinfo^.regvars[i]:=nil;
                  regvarinfo^.regvars_para[i] := false;
                end;
              { now assign register }
              for i:=1 to maxvarregs-p.registers32 do
                begin
                  if assigned(regvarinfo^.regvars[i]) and
                    (rg.reg_pushes[varregs[i]] < regvarinfo^.regvars[i].refs) then
                    begin
                      { register is no longer available for }
                      { expressions                          }
                      { search the register which is the most }
                      { unused                                }
                      r.enum:=varregs[i];
                      if r.enum=R_INTREGISTER then
                        rg.makeregvarint(r.number)
                      else
                        rg.makeregvarother(r);

                      { possibly no 32 bit register are needed }
                      { call by reference/const ? }
                      if (regvarinfo^.regvars[i].varspez in [vs_var,vs_out]) or
                         ((regvarinfo^.regvars[i].varspez=vs_const) and
                           paramanager.push_addr_param(regvarinfo^.regvars[i].vartype.def,aktprocdef.proccalloption)) then
                        begin
                           r.enum:=varregs[i];
                           regvarinfo^.regvars[i].reg:=r;
                        end
                      else
                       if (regvarinfo^.regvars[i].vartype.def.deftype in [orddef,enumdef]) and
                          (regvarinfo^.regvars[i].vartype.def.size=1) then
                        begin
                          r.enum:=varregs[i];
                          regvarinfo^.regvars[i].reg:=rg.makeregsize(r,OS_8);
                        end
                      else
                       if (regvarinfo^.regvars[i].vartype.def.deftype in [orddef,enumdef]) and
                          (regvarinfo^.regvars[i].vartype.def.size=2) then
                         begin
                           r.enum:=varregs[i];
                           regvarinfo^.regvars[i].reg:=rg.makeregsize(r,OS_16);
                         end
                      else
                        begin
                          r.enum:=varregs[i];
                          regvarinfo^.regvars[i].reg:=r;
                        end;
                      { procedure uses this register }
                      include(rg.usedinproc,varregs[i]);
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
                   if (procinfo.flags and pi_do_call)<>0 then
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
                       r.enum:=R_ST0;
                       regvarinfo^.fpuregvars[i].reg:=trgcpu(rg).correct_fpuregister(r,i);
{$else i386}
                       rg.makeregvarother(regvarinfo^.fpuregvars[i].reg);
{$endif i386}
                     end;
                  end;
              end;
        end;
     end;


    procedure store_regvar_int(asml:Taasmoutput;reg:Tsuperregister);
    
    begin
      internalerror(200301104);
    end;

    procedure store_regvar(asml: TAAsmoutput; reg: tregister);
    var
      i: longint;
      hr: treference;
      regvarinfo: pregvarinfo;
      vsym: tvarsym;
    begin
      if reg.enum>lastreg then
        internalerror(200301081);
      regvarinfo := pregvarinfo(aktprocdef.regvarinfo);
      if not assigned(regvarinfo) then
        exit;
      for i := 1 to maxvarregs do
        if assigned(regvarinfo^.regvars[i]) and
           (rg.makeregsize(regvarinfo^.regvars[i].reg,OS_INT).enum = reg.enum) then
          begin
            if rg.regvar_loaded[rg.makeregsize(reg,OS_INT).enum] then
              begin
                vsym := tvarsym(regvarinfo^.regvars[i]);
                { we only have to store the regvar back to memory if it's }
                { possible that it's been modified  (JM)                  }
                if not(vsym.varspez in [vs_const,vs_var,vs_out]) then
                  begin
                    reference_reset(hr);
                    if vsym.owner.symtabletype in [inlinelocalsymtable,localsymtable] then
                      hr.offset:=-vsym.address+vsym.owner.address_fixup
                    else
                      hr.offset:=vsym.address+vsym.owner.address_fixup;
                    hr.base:=procinfo.framepointer;
                    cg.a_load_reg_ref(asml,def_cgsize(vsym.vartype.def),vsym.reg,hr);
                  end;
                asml.concat(tai_regalloc.dealloc(rg.makeregsize(reg,OS_INT)));
                rg.regvar_loaded[rg.makeregsize(reg,OS_INT).enum] := false;
              end;
            break;
          end;
    end;

    procedure load_regvar(asml: TAAsmoutput; vsym: tvarsym);
    var
      hr: treference;
      opsize: tcgsize;
      reg : tregister;
    begin
      reg:=rg.makeregsize(vsym.reg,OS_INT);
      if reg.enum>lastreg then
        internalerror(200301081);
      if not rg.regvar_loaded[reg.enum] then
        begin
          asml.concat(tai_regalloc.alloc(reg));
          reference_reset(hr);
          if vsym.owner.symtabletype in [inlinelocalsymtable,localsymtable] then
            hr.offset:=-vsym.address+vsym.owner.address_fixup
          else
            hr.offset:=vsym.address+vsym.owner.address_fixup;
          hr.base:=procinfo.framepointer;
          if (vsym.varspez in [vs_var,vs_out]) or
             ((vsym.varspez=vs_const) and
               paramanager.push_addr_param(vsym.vartype.def,aktprocdef.proccalloption)) then
            opsize := OS_ADDR
          else
            opsize := def_cgsize(vsym.vartype.def);
          cg.a_load_ref_reg(asml,opsize,hr,reg);
          rg.regvar_loaded[reg.enum] := true;
        end;
    end;

    procedure load_regvar_reg(asml: TAAsmoutput; reg: tregister);
    var
      i: longint;
      regvarinfo: pregvarinfo;
      reg_spare : tregister;
    begin
      regvarinfo := pregvarinfo(aktprocdef.regvarinfo);
      if not assigned(regvarinfo) then
        exit;
      reg_spare := rg.makeregsize(reg,OS_INT);
      if reg_spare.enum>lastreg then
        internalerror(2003010801);
      for i := 1 to maxvarregs do
        if assigned(regvarinfo^.regvars[i]) and
           (rg.makeregsize(regvarinfo^.regvars[i].reg,OS_INT).enum = reg_spare.enum) then
          load_regvar(asml,tvarsym(regvarinfo^.regvars[i]))
    end;

    procedure load_all_regvars(asml: TAAsmoutput);
    var
      i: longint;
      regvarinfo: pregvarinfo;
    begin
      regvarinfo := pregvarinfo(aktprocdef.regvarinfo);
      if not assigned(regvarinfo) then
        exit;
      for i := 1 to maxvarregs do
        if assigned(regvarinfo^.regvars[i]) {and
           (makereg32(regvarinfo^.regvars[i].reg) in [R_EAX,R_EBX,R_ECX,R_EDX])} then
          load_regvar(asml,tvarsym(regvarinfo^.regvars[i]))
    end;


    procedure load_regvars(asml: TAAsmoutput; p: tnode);
    var
      i: longint;
      regvarinfo: pregvarinfo;
      r:Tregister;
    begin
      if (cs_regalloc in aktglobalswitches) and
         ((procinfo.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
        begin
          regvarinfo := pregvarinfo(aktprocdef.regvarinfo);
          { can happen when inlining assembler procedures (JM) }
          if not assigned(regvarinfo) then
            exit;
          for i:=1 to maxvarregs do
            begin
             if assigned(regvarinfo^.regvars[i]) then
               begin
                if cs_asm_source in aktglobalswitches then
                 asml.insert(tai_comment.Create(strpnew(regvarinfo^.regvars[i].name+
                  ' with weight '+tostr(regvarinfo^.regvars[i].refs)+' assigned to register '+
                  std_reg2str[regvarinfo^.regvars[i].reg.enum])));
                if (status.verbosity and v_debug)=v_debug then
                 Message3(cg_d_register_weight,std_reg2str[regvarinfo^.regvars[i].reg.enum],
                  tostr(regvarinfo^.regvars[i].refs),regvarinfo^.regvars[i].name);
               end;
            end;
          for i:=1 to maxfpuvarregs do
            begin
              if assigned(regvarinfo^.fpuregvars[i]) then
                begin
{$ifdef i386}
                  r.enum:=R_ST0;
                  { reserve place on the FPU stack }
                  regvarinfo^.fpuregvars[i].reg:=trgcpu(rg).correct_fpuregister(r,i-1);
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
                        std_reg2str[regvarinfo^.fpuregvars[i].reg.enum])));
                    if (status.verbosity and v_debug)=v_debug then
                      Message3(cg_d_register_weight,std_reg2str[regvarinfo^.fpuregvars[i].reg.enum],
                        tostr(regvarinfo^.fpuregvars[i].refs),regvarinfo^.fpuregvars[i].name);
                 end;
            end;
          if cs_asm_source in aktglobalswitches then
            asml.insert(tai_comment.Create(strpnew('Register variable assignment:')));
        end;
    end;


    procedure sync_regvars(list1, list2: taasmoutput; const regvarsloaded1,
      regvarsloaded2: regvar_booleanarray);
    var
      counter: tregister;
    begin
      for counter.enum := low(rg.regvar_loaded) to high(rg.regvar_loaded) do
        begin
           rg.regvar_loaded[counter.enum] := regvarsloaded1[counter.enum] and
             regvarsloaded2[counter.enum];
           if regvarsloaded1[counter.enum] xor regvarsloaded2[counter.enum] then
             if regvarsloaded1[counter.enum] then
               load_regvar_reg(list2,counter)
             else
               load_regvar_reg(list1,counter);
        end;
    end;


    procedure cleanup_regvars(asml: TAAsmoutput);
    var
      i: longint;
      r,reg : tregister;
    begin
      { can happen when inlining assembler procedures (JM) }
      if not assigned(aktprocdef.regvarinfo) then
        exit;
      if (cs_regalloc in aktglobalswitches) and
         ((procinfo.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
        with pregvarinfo(aktprocdef.regvarinfo)^ do
          begin
{$ifdef i386}
            r.enum:=R_ST0;
            for i:=1 to maxfpuvarregs do
              if assigned(fpuregvars[i]) then
                { ... and clean it up }
                asml.concat(Taicpu.op_reg(A_FSTP,S_NO,r));
{$endif i386}
            for i := 1 to maxvarregs do
             begin
               if assigned(regvars[i]) then
                begin
                  reg:=rg.makeregsize(regvars[i].reg,OS_INT);
                  if reg.enum>lastreg then
                    internalerror(200201081);
                  if (rg.regvar_loaded[reg.enum]) then
                   asml.concat(tai_regalloc.dealloc(reg));
                end;
             end;
          end;
    end;

end.

{
  $Log$
  Revision 1.45  2003-02-19 22:00:14  daniel
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
