{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl and Jonas Maebe

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

{$i defines.inc}

interface

uses
  aasm,
  tree;

procedure assign_regvars(var p: ptree);
procedure load_regvars(asml: paasmoutput; p: ptree);
procedure cleanup_regvars(asml: paasmoutput);

implementation

   uses
     globtype,systems,comphook,
     cutils,cobjects,verbose,globals,
     symconst,symtable,types,
     hcodegen,temp_gen,cpubase,cpuasm
{$ifndef newcg}
   {$ifndef CG11}
     ,tcflw
   {$endif}
{$endif newcg}
{$ifdef GDB}
     ,gdb
{$endif}
{$ifdef i386}
     ,tgeni386,cgai386

{$endif}
{$ifdef m68k}
     ,tgen68k,cga68k
{$endif}
     ;

     type
       pregvarinfo = ^tregvarinfo;
       tregvarinfo = record
          regvars : array[1..maxvarregs] of pvarsym;
          regvars_para : array[1..maxvarregs] of boolean;
          regvars_refs : array[1..maxvarregs] of longint;

          fpuregvars : array[1..maxfpuvarregs] of pvarsym;
          fpuregvars_para : array[1..maxfpuvarregs] of boolean;
          fpuregvars_refs : array[1..maxfpuvarregs] of longint;
       end;


    var
      parasym : boolean;

    procedure searchregvars(p : pnamedindexobject);
      var
         i,j,k : longint;
      begin
         if (psym(p)^.typ=varsym) and (vo_regable in pvarsym(p)^.varoptions) then
           begin
              j:=pvarsym(p)^.refs;
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
                  with pregvarinfo(aktprocsym^.definition^.regvarinfo)^ do
                   if ((regvars[i]=nil) or (j>regvars_refs[i])) and (j>0) then
                     begin
                        for k:=maxvarregs-1 downto i do
                          begin
                             regvars[k+1]:=regvars[k];
                             regvars_para[k+1]:=regvars_para[k];
                             regvars_refs[k+1]:=regvars_refs[k];
                          end;
                        { calc the new refs
                        pvarsym(p)^.refs:=j; }
                        regvars[i]:=pvarsym(p);
                        regvars_para[i]:=parasym;
                        regvars_refs[i]:=j;
                        break;
                     end;
                end;
           end;
      end;


    procedure searchfpuregvars(p : pnamedindexobject);
      var
         i,j,k : longint;
      begin
         if (psym(p)^.typ=varsym) and (vo_fpuregable in pvarsym(p)^.varoptions) then
           begin
              j:=pvarsym(p)^.refs;
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
                  with pregvarinfo(aktprocsym^.definition^.regvarinfo)^ do
                   if ((fpuregvars[i]=nil) or (j>fpuregvars_refs[i])) and (j>0) then
                     begin
                        for k:=maxfpuvarregs-1 downto i do
                          begin
                             fpuregvars[k+1]:=fpuregvars[k];
                             fpuregvars_para[k+1]:=fpuregvars_para[k];
                             fpuregvars_refs[k+1]:=fpuregvars_refs[k];
                          end;
                        { calc the new refs
                        pvarsym(p)^.refs:=j; }
                        fpuregvars[i]:=pvarsym(p);
                        fpuregvars_para[i]:=parasym;
                        fpuregvars_refs[i]:=j;
                        break;
                     end;
                end;
           end;
      end;

{$ifdef i386}
    function reg32(reg: tregister): tregister;
      begin
        case regsize(reg) of
          S_B: reg32 := reg8toreg32(reg);
          S_W: reg32 := reg16toreg32(reg);
          S_L: reg32 := reg;
        end;
      end;
{$else i386}
    function reg32(reg: tregister): tregister;
      begin
        reg32 := reg;
      end;
{$endif i386}

    procedure assign_regvars(var p: ptree);
          { register variables }
    var
      regvarinfo: pregvarinfo;
      i: longint;
    begin
      { max. optimizations     }
      { only if no asm is used }
      { and no try statement   }
      if (cs_regalloc in aktglobalswitches) and
         ((procinfo^.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
        begin
          new(regvarinfo);
          fillchar(regvarinfo^,sizeof(regvarinfo^),0);
          aktprocsym^.definition^.regvarinfo := regvarinfo;
          if (p^.registers32<4) then
            begin
              parasym:=false;
              symtablestack^.foreach({$ifdef FPCPROCVAR}@{$endif}searchregvars);
              { copy parameter into a register ? }
              parasym:=true;
              symtablestack^.next^.foreach({$ifdef FPCPROCVAR}@{$endif}searchregvars);
              { hold needed registers free }
              for i:=maxvarregs downto maxvarregs-p^.registers32+1 do
                begin
                  regvarinfo^.regvars[i]:=nil;
                  regvarinfo^.regvars_para[i] := false;
                end;
              { now assign register }
              for i:=1 to maxvarregs-p^.registers32 do
                begin
                  if assigned(regvarinfo^.regvars[i]) and
                    (reg_pushes[varregs[i]] < regvarinfo^.regvars[i]^.refs) then
                    begin
                      { register is no longer available for }
                      { expressions                          }
                      { search the register which is the most }
                      { unused                                        }
                      usableregs:=usableregs-[varregs[i]];
                      is_reg_var[varregs[i]]:=true;
                      dec(c_usableregs);

                      { possibly no 32 bit register are needed }
                      { call by reference/const ? }
                      if (regvarinfo^.regvars[i]^.varspez=vs_var) or
                         ((regvarinfo^.regvars[i]^.varspez=vs_const) and
                           push_addr_param(regvarinfo^.regvars[i]^.vartype.def)) then
                        begin
                           regvarinfo^.regvars[i]^.reg:=varregs[i];
                        end
                      else
                       if (regvarinfo^.regvars[i]^.vartype.def^.deftype in [orddef,enumdef]) and
                          (porddef(regvarinfo^.regvars[i]^.vartype.def)^.size=1) then
                        begin
{$ifdef i386}
                          regvarinfo^.regvars[i]^.reg:=reg32toreg8(varregs[i]);
{$endif}
                        end
                      else
                       if (regvarinfo^.regvars[i]^.vartype.def^.deftype in [orddef,enumdef]) and
                          (porddef(regvarinfo^.regvars[i]^.vartype.def)^.size=2) then
                         begin
{$ifdef i386}
                           regvarinfo^.regvars[i]^.reg:=reg32toreg16(varregs[i]);
{$endif}
                         end
                      else
                        begin
                          regvarinfo^.regvars[i]^.reg:=varregs[i];
                        end;
                      if regvarinfo^.regvars_para[i] then
                        unused:=unused - [regvarinfo^.regvars[i]^.reg];
                      { procedure uses this register }
{$ifdef i386}
                      usedinproc:=usedinproc or ($80 shr byte(varregs[i]));
{$endif i386}
{$ifdef m68k}
                      usedinproc:=usedinproc or ($800 shr word(varregs[i]));
{$endif m68k}
                    end
                  else
                    begin
                      regvarinfo^.regvars[i] := nil;
                      regvarinfo^.regvars_para[i] := false;
                    end;
                end;
            end;
            if ((p^.registersfpu+1)<maxfpuvarregs) then
              begin
                parasym:=false;
                symtablestack^.foreach({$ifdef FPCPROCVAR}@{$endif}searchfpuregvars);
{$ifdef dummy}
                { copy parameter into a register ? }
                parasym:=true;
                symtablestack^.next^.foreach({$ifdef FPCPROCVAR}@{$endif}searchregvars);
{$endif dummy}
                { hold needed registers free }

                { in non leaf procedures we must be very careful }
                { with assigning registers             }
                if aktmaxfpuregisters=-1 then
                  begin
                   if (procinfo^.flags and pi_do_call)<>0 then
                     begin
                      for i:=maxfpuvarregs downto 2 do
                      regvarinfo^.fpuregvars[i]:=nil;
                     end
                   else
                     begin
                      for i:=maxfpuvarregs downto maxfpuvarregs-p^.registersfpu do
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
                       regvarinfo^.fpuregvars[i]^.reg:=correct_fpuregister(R_ST0,i-1);
{$endif i386}
{$ifdef m68k}
                       regvarinfo^.fpuregvars[i]^.reg:=fpuvarregs[i];
{$endif m68k}
                     end;
                  end;
              end;
        end;
    end;


    procedure load_regvars(asml: paasmoutput; p: ptree);
    var
      i: longint;
      hr      : preference;
      regvarinfo: pregvarinfo;
{$ifdef i386}
      opsize: topsize;
      opcode: tasmop;
      signed: boolean;
{$endif i386}
    begin
      if (cs_regalloc in aktglobalswitches) and
         ((procinfo^.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
        begin
          regvarinfo := pregvarinfo(aktprocsym^.definition^.regvarinfo);
          { can happen when inlining assembler procedures (JM) }
          if not assigned(regvarinfo) then
            exit;
          for i:=1 to maxvarregs do
            begin
              { parameter must be load }
              if regvarinfo^.regvars_para[i] then
                begin
{$ifdef i386}
                  asml^.concat(new(pairegalloc,alloc(reg32(regvarinfo^.regvars[i]^.reg))));
{$endif i386}
                  { procinfo is there actual,    }
                  { because we can't never be in a }
                  { nested procedure        }
                  { when loading parameter to reg  }
                  new(hr);
                  reset_reference(hr^);
                  hr^.offset:=pvarsym(regvarinfo^.regvars[i])^.address+procinfo^.para_offset;
                  hr^.base:=procinfo^.framepointer;
{$ifdef i386}
                { zero the regvars because the upper 48bits must be clear }
                { for 8bits vars when using them with btrl (JM)           }
                  signed :=
                    (pvarsym(regvarinfo^.regvars[i])^.vartype.def^.deftype =
                      orddef) and
                    is_signed(pvarsym(regvarinfo^.regvars[i])^.vartype.def);
                  case regsize(regvarinfo^.regvars[i]^.reg) of
                    S_L:
                      begin
                        opsize := S_L;
                        opcode := A_MOV;
                      end;
                    S_W:
                      begin
                        opsize := S_WL;
                        if signed then
                          opcode := A_MOVSX
                        else opcode := A_MOVZX;
                      end;
                    S_B:
                      begin
                        opsize := S_BL;
                        if signed then
                          opcode := A_MOVSX
                        else opcode := A_MOVZX;
                      end;
                  end;
                  asml^.concat(new(paicpu,op_ref_reg(opcode,opsize,
                    hr,reg32(regvarinfo^.regvars[i]^.reg))));
{$endif i386}
{$ifdef m68k}
                  asml^.concat(new(paicpu,op_ref_reg(A_MOVE,regsize(regvarinfo^.regvars[i]^.reg),
                    hr,regvarinfo^.regvars[i]^.reg)));
{$endif m68k}
                end
            end;
          for i:=1 to maxvarregs do
            begin
             if assigned(regvarinfo^.regvars[i]) then
               begin
{$ifdef i386}
                if not(regvarinfo^.regvars_para[i]) then
                  begin
                    asml^.concat(new(pairegalloc,alloc(reg32(regvarinfo^.regvars[i]^.reg))));
                    { zero the regvars because the upper 48bits must be clear }
                    { for 8bits vars when using them with btrl (JM)           }
                    if (regsize(regvarinfo^.regvars[i]^.reg) in [S_B,S_W]) then
                      asml^.concat(new(paicpu,op_reg_reg(A_XOR,S_L,
                        reg32(regvarinfo^.regvars[i]^.reg),
                        reg32(regvarinfo^.regvars[i]^.reg))));
                  end;
{$endif i386}
                if cs_asm_source in aktglobalswitches then
                asml^.insert(new(pai_asm_comment,init(strpnew(regvarinfo^.regvars[i]^.name+
                  ' with weight '+tostr(regvarinfo^.regvars[i]^.refs)+' assigned to register '+
                  reg2str(regvarinfo^.regvars[i]^.reg)))));
                if (status.verbosity and v_debug)=v_debug then
                Message3(cg_d_register_weight,reg2str(regvarinfo^.regvars[i]^.reg),
                  tostr(regvarinfo^.regvars[i]^.refs),regvarinfo^.regvars[i]^.name);
               end;
            end;
          for i:=1 to maxfpuvarregs do
            begin
              if assigned(regvarinfo^.fpuregvars[i]) then
                begin
{$ifdef i386}
                  { reserve place on the FPU stack }
                  regvarinfo^.fpuregvars[i]^.reg:=correct_fpuregister(R_ST0,i-1);
                  asml^.concat(new(paicpu,op_none(A_FLDZ,S_NO)));
{$endif i386}
{$ifdef dummy}
                  { parameter must be load }
                  if regvarinfo^.fpuregvars_para[i] then
                    begin
                      { procinfo is there actual,    }
                      { because we can't never be in a }
                      { nested procedure        }
                      { when loading parameter to reg  }
                      new(hr);
                      reset_reference(hr^);
                      hr^.offset:=pvarsym(regvarinfo^.regvars[i])^.address+procinfo^.para_offset;
                      hr^.base:=procinfo^.framepointer;
{$ifdef i386}
                      asml^.concat(new(paicpu,op_ref_reg(A_MOV,regsize(regvarinfo^.regvars[i]^.reg),
                        hr,regvarinfo^.regvars[i]^.reg)));
{$endif i386}
{$ifdef m68k}
                      asml^.concat(new(paicpu,op_ref_reg(A_MOVE,regsize(regvarinfo^.regvars[i]^.reg),
                        hr,regvarinfo^.regvars[i]^.reg)));
{$endif m68k}
                    end;
{$endif dummy}
                end;
            end;
          if assigned(p) then
            if cs_asm_source in aktglobalswitches then
              asml^.insert(new(pai_asm_comment,init(strpnew(tostr(p^.registersfpu)+
              ' registers on FPU stack used by temp. expressions'))));
          for i:=1 to maxfpuvarregs do
            begin
               if assigned(regvarinfo^.fpuregvars[i]) then
                 begin
                    if cs_asm_source in aktglobalswitches then
                      asml^.insert(new(pai_asm_comment,init(strpnew(regvarinfo^.fpuregvars[i]^.name+
                        ' with weight '+tostr(regvarinfo^.fpuregvars[i]^.refs)+' assigned to register '+
                        reg2str(regvarinfo^.fpuregvars[i]^.reg)))));
                    if (status.verbosity and v_debug)=v_debug then
                      Message3(cg_d_register_weight,reg2str(regvarinfo^.fpuregvars[i]^.reg),
                        tostr(regvarinfo^.fpuregvars[i]^.refs),regvarinfo^.fpuregvars[i]^.name);
                 end;
            end;
          if cs_asm_source in aktglobalswitches then
            asml^.insert(new(pai_asm_comment,init(strpnew('Register variable assignment:'))));
        end;
    end;


    procedure cleanup_regvars(asml: paasmoutput);
    var
      i: longint;
    begin
    {$ifdef i386}
      { can happen when inlining assembler procedures (JM) }
      if not assigned(aktprocsym^.definition^.regvarinfo) then
        exit;
      if (cs_regalloc in aktglobalswitches) and
         ((procinfo^.flags and (pi_uses_asm or pi_uses_exceptions))=0) then
        with pregvarinfo(aktprocsym^.definition^.regvarinfo)^ do
          begin
            for i:=1 to maxfpuvarregs do
              if assigned(fpuregvars[i]) then
                { ... and clean it up }
                asml^.concat(new(paicpu,op_reg(A_FSTP,S_NO,R_ST0)));
            for i := 1 to maxvarregs do
              if assigned(regvars[i]) then
                asml^.concat(new(pairegalloc,dealloc(reg32(regvars[i]^.reg))));
          end;
    {$endif i386}
    end;

end.

{
  $Log$
  Revision 1.8  2000-09-30 16:08:45  peter
    * more cg11 updates

  Revision 1.7  2000/09/30 13:08:16  jonas
    * regvars are now zeroed at the start of their life if they contain an 8
      or 16bit var/parameter, because the full 32bits are used if they are
      necessary for a btrl instruction

  Revision 1.6  2000/09/24 15:06:27  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:52  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/17 11:07:51  jonas
    * fixed crash when inlining assembler procedures with -Or

  Revision 1.3  2000/08/04 05:52:00  jonas
    * correct version (I also had a regvars.pp locally, which was used
      instead of the regvars.pas on CVS, so I didn't notice the errors :( )

  Revision 1.2  2000/08/03 14:36:47  jonas
    * fixed inserting of allocated register for regvars (only those for
      parameters were done, and sometimes even the wrong ones)

  Revision 1.1  2000/08/03 13:17:25  jonas
    + allow regvars to be used inside inlined procs, which required  the
      following changes:
        + load regvars in genentrycode/free them in genexitcode (cgai386)
        * moved all regvar related code to new regvars unit
        + added pregvarinfo type to hcodegen
        + added regvarinfo field to tprocinfo (symdef/symdefh)
        * deallocate the regvars of the caller in secondprocinline before
          inlining the called procedure and reallocate them afterwards

}