{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Helper routines for the i386 code generator

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

unit cga;

{$i defines.inc}

interface

    uses
       cpubase,cpuasm,
       symconst,symtype,symdef,aasm;

{$define TESTGETTEMP to store const that
 are written into temps for later release PM }

    function def_opsize(p1:tdef):topsize;
    function def2def_opsize(p1,p2:tdef):topsize;
    function def_getreg(p1:tdef):tregister;
    function makereg8(r:tregister):tregister;
    function makereg16(r:tregister):tregister;
    function makereg32(r:tregister):tregister;


    procedure locflags2reg(var l:tlocation;opsize:topsize);
    procedure locjump2reg(var l:tlocation;opsize:topsize; otl, ofl: tasmlabel);


    procedure emitlab(var l : tasmlabel);
    procedure emitjmp(c : tasmcond;var l : tasmlabel);
    procedure emit_flag2reg(flag:tresflags;hregister:tregister);

    procedure emit_none(i : tasmop;s : topsize);

    procedure emit_const(i : tasmop;s : topsize;c : longint);
    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
    procedure emit_ref(i : tasmop;s : topsize;ref : preference);

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;ref : preference);
    procedure emit_ref_reg(i : tasmop;s : topsize;ref : preference;reg : tregister);
    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : preference);
    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);


    procedure emit_sym(i : tasmop;s : topsize;op : tasmsymbol);
    procedure emit_sym_ofs(i : tasmop;s : topsize;op : tasmsymbol;ofs : longint);
    procedure emit_sym_ofs_reg(i : tasmop;s : topsize;op : tasmsymbol;ofs:longint;reg : tregister);
    procedure emit_sym_ofs_ref(i : tasmop;s : topsize;op : tasmsymbol;ofs:longint;ref : preference);

    procedure emitcall(const routine:string);

    procedure emit_mov_loc_ref(const t:tlocation;const ref:treference;siz:topsize;freetemp:boolean);
    procedure emit_mov_loc_reg(const t:tlocation;reg:tregister);
    procedure emit_mov_ref_reg64(r : treference;rl,rh : tregister);
    procedure emit_lea_loc_ref(const t:tlocation;const ref:treference;freetemp:boolean);
    procedure emit_lea_loc_reg(const t:tlocation;reg:tregister;freetemp:boolean);
    procedure emit_push_loc(const t:tlocation);
    procedure emit_push_mem_size(const t: treference; size: longint);

    { pushes qword location to the stack }
    procedure emit_pushq_loc(const t : tlocation);
    procedure release_qword_loc(const t : tlocation);

    { remove non regvar registers in loc from regs (in the format }
    { pushusedregisters uses)                                     }
    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: byte);
    { releases the registers of a location }
    procedure release_loc(const t : tlocation);

    procedure emit_pushw_loc(const t:tlocation);
    procedure emit_push_lea_loc(const t:tlocation;freetemp:boolean);
    procedure emit_to_mem(var t:tlocation;def:tdef);
    procedure emit_to_reg16(var hr:tregister);
    procedure emit_to_reg32(var hr:tregister);
    procedure emit_mov_reg_loc(reg: TRegister; const t:tlocation);
    procedure emit_movq_reg_loc(reghigh,reglow: TRegister;t:tlocation);

    procedure copyshortstring(const dref,sref : treference;len : byte;
                        loadref, del_sref: boolean);

    procedure finalize(t : tdef;const ref : treference;is_already_ref : boolean);
    procedure incrstringref(t : tdef;const ref : treference);
    procedure decrstringref(t : tdef;const ref : treference);

    procedure push_int(l : longint);
    procedure emit_push_mem(const ref : treference);
    procedure emitpushreferenceaddr(const ref : treference);

    procedure incrcomintfref(t: tdef; const ref: treference);
    procedure decrcomintfref(t: tdef; const ref: treference);

    procedure floatload(t : tfloattype;const ref : treference);
    procedure floatstore(t : tfloattype;const ref : treference);
    procedure floatloadops(t : tfloattype;var op : tasmop;var s : topsize);
    procedure floatstoreops(t : tfloattype;var op : tasmop;var s : topsize);

    procedure maybe_loadself;
    procedure emitloadord2reg(const location:Tlocation;orddef:torddef;destreg:Tregister;delloc:boolean);
    procedure concatcopy(source,dest : treference;size : longint;delsource : boolean;loadref:boolean);

    procedure genentrycode(alist : TAAsmoutput;make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean);
    procedure genexitcode(alist : TAAsmoutput;parasize:longint;
                          nostackframe,inlined:boolean);

    { if a unit doesn't have a explicit init/final code,  }
    { we've to generate one, if the units has ansistrings }
    { in the interface or implementation                  }
    procedure genimplicitunitfinal(alist : TAAsmoutput);
    procedure genimplicitunitinit(alist : TAAsmoutput);
{$ifdef test_dest_loc}

const
  { used to avoid temporary assignments }
  dest_loc_known : boolean = false;
  in_dest_loc    : boolean = false;
  dest_loc_tree  : ptree = nil;

var
  dest_loc : tlocation;

procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

{$endif test_dest_loc}


implementation

    uses
{$ifdef delphi}
       sysutils,
{$else}
       strings,
{$endif}
       cutils,cclasses,
       globtype,systems,globals,verbose,
       fmodule,
       symbase,symsym,symtable,types,
       tainst,tgcpu,temp_gen,cgbase,regvars
{$ifdef GDB}
       ,gdb
{$endif}
       ;

{$ifndef NOTARGETWIN32}
  const
     winstackpagesize = 4096;
{$endif}

{*****************************************************************************
                                Helpers
*****************************************************************************}

    function def_opsize(p1:tdef):topsize;
      begin
        case p1.size of
         1 : def_opsize:=S_B;
         2 : def_opsize:=S_W;
         4 : def_opsize:=S_L;
         { I don't know if we need it (FK) }
         8 : def_opsize:=S_L;
        else
         internalerror(130820001);
        end;
      end;


    function def2def_opsize(p1,p2:tdef):topsize;
      var
        o1 : topsize;
      begin
        case p1.size of
         1 : o1:=S_B;
         2 : o1:=S_W;
         4 : o1:=S_L;
         { I don't know if we need it (FK) }
         8 : o1:=S_L;
        else
         internalerror(130820002);
        end;
        if assigned(p2) then
         begin
           case p2.size of
            1 : o1:=S_B;
            2 : begin
                  if o1=S_B then
                   o1:=S_BW
                  else
                   o1:=S_W;
                end;
            4,8:
              begin
                 case o1 of
                    S_B : o1:=S_BL;
                    S_W : o1:=S_WL;
                 end;
              end;
           end;
         end;
        def2def_opsize:=o1;
      end;


    function def_getreg(p1:tdef):tregister;
      begin
        case p1.size of
         1 : def_getreg:=reg32toreg8(getregisterint);
         2 : def_getreg:=reg32toreg16(getregisterint);
         4 : def_getreg:=getregisterint;
        else
         internalerror(130820003);
        end;
      end;


    function makereg8(r:tregister):tregister;
      begin
        case r of
          R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
            makereg8:=reg32toreg8(r);
          R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
            makereg8:=reg16toreg8(r);
          R_AL,R_BL,R_CL,R_DL :
            makereg8:=r;
        end;
      end;


    function makereg16(r:tregister):tregister;
      begin
        case r of
          R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
            makereg16:=reg32toreg16(r);
          R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
            makereg16:=r;
          R_AL,R_BL,R_CL,R_DL :
            makereg16:=reg8toreg16(r);
        end;
      end;


    function makereg32(r:tregister):tregister;
      begin
        case r of
          R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
            makereg32:=r;
          R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
            makereg32:=reg16toreg32(r);
          R_AL,R_BL,R_CL,R_DL :
            makereg32:=reg8toreg32(r);
        end;
      end;


    procedure locflags2reg(var l:tlocation;opsize:topsize);
      var
        hregister : tregister;
      begin
        if (l.loc=LOC_FLAGS) then
         begin
           hregister:=getregisterint;
           case opsize of
            S_W : hregister:=reg32toreg16(hregister);
            S_B : hregister:=reg32toreg8(hregister);
           end;
           emit_flag2reg(l.resflags,hregister);
           l.loc:=LOC_REGISTER;
           l.register:=hregister;
         end
        else internalerror(270720001);
      end;


    procedure locjump2reg(var l:tlocation;opsize:topsize; otl, ofl: tasmlabel);
      var
        hregister : tregister;
        hl : tasmlabel;
      begin
         if l.loc = LOC_JUMP then
           begin
             hregister:=getregisterint;
             case opsize of
               S_W : hregister:=reg32toreg16(hregister);
               S_B : hregister:=reg32toreg8(hregister);
             end;
             l.loc:=LOC_REGISTER;
             l.register:=hregister;
             emitlab(truelabel);
             truelabel:=otl;
             emit_const_reg(A_MOV,opsize,1,hregister);
             getlabel(hl);
             emitjmp(C_None,hl);
             emitlab(falselabel);
             falselabel:=ofl;
             emit_reg_reg(A_XOR,S_L,makereg32(hregister),
             makereg32(hregister));
             emitlab(hl);
           end
        else internalerror(270720002);
      end;


{*****************************************************************************
                              Emit Assembler
*****************************************************************************}

    procedure emitlab(var l : tasmlabel);
      begin
         if not l.is_set then
          exprasmList.concat(Tai_label.Create(l))
         else
          internalerror(7453984);
      end;

    procedure emitjmp(c : tasmcond;var l : tasmlabel);
      var
        ai : taicpu;
      begin
        if c=C_None then
          ai := Taicpu.Op_sym(A_JMP,S_NO,l)
        else
          begin
            ai:=Taicpu.Op_sym(A_Jcc,S_NO,l);
            ai.SetCondition(c);
          end;
        ai.is_jmp:=true;
        exprasmList.concat(ai);
      end;


    procedure emit_flag2reg(flag:tresflags;hregister:tregister);
      var
        ai : taicpu;
        hreg : tregister;
      begin
         hreg:=makereg8(hregister);
         ai:=Taicpu.Op_reg(A_Setcc,S_B,hreg);
         ai.SetCondition(flags_to_cond(flag));
         exprasmList.concat(ai);
         if hreg<>hregister then
          begin
            if hregister in regset16bit then
             emit_to_reg16(hreg)
            else
             emit_to_reg32(hreg);
          end;
      end;


    procedure emit_none(i : tasmop;s : topsize);
      begin
         exprasmList.concat(Taicpu.Op_none(i,s));
      end;

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_reg(i,s,reg));
      end;

    procedure emit_ref(i : tasmop;s : topsize;ref : preference);
      begin
         exprasmList.concat(Taicpu.Op_ref(i,s,ref));
      end;

    procedure emit_const(i : tasmop;s : topsize;c : longint);
      begin
         exprasmList.concat(Taicpu.Op_const(i,s,c));
      end;

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_const_reg(i,s,c,reg));
      end;

    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;ref : preference);
      begin
         exprasmList.concat(Taicpu.Op_const_ref(i,s,c,ref));
      end;

    procedure emit_ref_reg(i : tasmop;s : topsize;ref : preference;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_ref_reg(i,s,ref,reg));
      end;

    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : preference);
      begin
         exprasmList.concat(Taicpu.Op_reg_ref(i,s,reg,ref));
      end;

    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
      begin
         if (reg1<>reg2) or (i<>A_MOV) then
           exprasmList.concat(Taicpu.Op_reg_reg(i,s,reg1,reg2));
      end;

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
      begin
         exprasmList.concat(Taicpu.Op_const_reg_reg(i,s,c,reg1,reg2));
      end;

    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
      begin
         exprasmList.concat(Taicpu.Op_reg_reg_reg(i,s,reg1,reg2,reg3));
      end;

    procedure emit_sym(i : tasmop;s : topsize;op : tasmsymbol);
      begin
        exprasmList.concat(Taicpu.Op_sym(i,s,op));
      end;

    procedure emit_sym_ofs(i : tasmop;s : topsize;op : tasmsymbol;ofs : longint);
      begin
        exprasmList.concat(Taicpu.Op_sym_ofs(i,s,op,ofs));
      end;

    procedure emit_sym_ofs_reg(i : tasmop;s : topsize;op : tasmsymbol;ofs:longint;reg : tregister);
      begin
        exprasmList.concat(Taicpu.Op_sym_ofs_reg(i,s,op,ofs,reg));
      end;

    procedure emit_sym_ofs_ref(i : tasmop;s : topsize;op : tasmsymbol;ofs:longint;ref : preference);
      begin
        exprasmList.concat(Taicpu.Op_sym_ofs_ref(i,s,op,ofs,ref));
      end;

    procedure emitcall(const routine:string);
      begin
        exprasmList.concat(Taicpu.Op_sym(A_CALL,S_NO,newasmsymbol(routine)));
      end;

    { only usefull in startup code }
    procedure emitinsertcall(const routine:string);
      begin
        exprasmList.insert(Taicpu.Op_sym(A_CALL,S_NO,newasmsymbol(routine)));
      end;


    procedure emit_mov_loc_ref(const t:tlocation;const ref:treference;siz:topsize;freetemp:boolean);
      var
        hreg : tregister;
        pushedeax : boolean;

      begin
        pushedeax:=false;
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,siz,
                             t.register,newreference(ref)));
                           ungetregister32(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             emit_const_ref(A_MOV,siz,
                               t.reference.offset,newreference(ref))
                           else
                             begin
                               case siz of
                                 S_B : begin
                                          { we can't do a getregister in the code generator }
                                          { without problems!!!                             }
                                          if usablereg32>0 then
                                            hreg:=reg32toreg8(getregisterint)
                                          else
                                            begin
                                               emit_reg(A_PUSH,S_L,R_EAX);
                                               pushedeax:=true;
                                               hreg:=R_AL;
                                            end;
                                       end;
                                 S_W : hreg:=R_DI;
                                 S_L : hreg:=R_EDI;
                               end;
                               if hreg in [R_DI,R_EDI] then
                                 getexplicitregister32(R_EDI);
                               emit_ref_reg(A_MOV,siz,
                                 newreference(t.reference),hreg);
                               del_reference(t.reference);
                               exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,siz,
                                 hreg,newreference(ref)));
                               if siz=S_B then
                                 begin
                                    if pushedeax then
                                      emit_reg(A_POP,S_L,R_EAX)
                                    else
                                      ungetregister(hreg);
                                 end;
                               if hreg in [R_DI,R_EDI] then
                                 ungetregister32(R_EDI);
                               { we can release the registers }
                               { but only AFTER the MOV! Important for the optimizer!
                                 (JM)}
                               del_reference(ref);
                             end;
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_mov_loc_reg(const t:tlocation;reg:tregister);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           emit_reg_reg(A_MOV,S_L,t.register,reg);
                           ungetregister32(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             emit_const_reg(A_MOV,S_L,
                               t.reference.offset,reg)
                           else
                             begin
                               emit_ref_reg(A_MOV,S_L,
                                 newreference(t.reference),reg);
                             end;
                         end;
        else
         internalerror(330);
        end;
      end;

    procedure emit_mov_reg_loc(reg: TRegister; const t:tlocation);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           emit_reg_reg(A_MOV,RegSize(Reg),
                             reg,t.register);
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(334)
                           else
                             begin
                               exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,RegSize(Reg),
                                 Reg,newreference(t.reference)));
                             end;
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_lea_loc_reg(const t:tlocation;reg:tregister;freetemp:boolean);
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),reg);
                             end;
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;


    procedure emit_movq_reg_loc(reghigh,reglow: TRegister;t:tlocation);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           emit_reg_reg(A_MOV,S_L,
                             reglow,t.registerlow);
                           emit_reg_reg(A_MOV,S_L,
                             reghigh,t.registerhigh);
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(334)
                           else
                             begin
                               exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,
                                 Reglow,newreference(t.reference)));
                               inc(t.reference.offset,4);
                               exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,
                                 Reghigh,newreference(t.reference)));
                             end;
                         end;
        else
         internalerror(330);
        end;
      end;


   procedure emit_pushq_loc(const t : tlocation);

      var
         hr : preference;

      begin
         case t.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,
                   t.registerhigh));
                 exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,
                   t.registerlow));
              end;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 hr:=newreference(t.reference);
                 inc(hr^.offset,4);
                 exprasmList.concat(Taicpu.Op_ref(A_PUSH,S_L,
                   hr));
                 exprasmList.concat(Taicpu.Op_ref(A_PUSH,S_L,
                   newreference(t.reference)));
                 ungetiftemp(t.reference);
              end;
            else internalerror(331);
         end;
      end;

    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: byte);
    begin
      case t.loc of
        LOC_REGISTER:
          begin
            { can't be a regvar, since it would be LOC_CREGISTER then }
            regs := regs and not($80 shr byte(t.register));
            if t.registerhigh <> R_NO then
              regs := regs and not($80 shr byte(t.registerhigh));
          end;
        LOC_MEM,LOC_REFERENCE:
          begin
            if not(cs_regalloc in aktglobalswitches) or
               (t.reference.base in usableregs) then
              regs := regs and
                not($80 shr byte(t.reference.base));
            if not(cs_regalloc in aktglobalswitches) or
               (t.reference.index in usableregs) then
              regs := regs and
                not($80 shr byte(t.reference.index));
          end;
      end;
    end;


    procedure release_loc(const t : tlocation);

      begin
         case t.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 ungetregister32(t.register);
              end;
            LOC_MEM,
            LOC_REFERENCE:
              del_reference(t.reference);
            else internalerror(332);
         end;
      end;

    procedure release_qword_loc(const t : tlocation);
      begin
         case t.loc of
            LOC_REGISTER,
            LOC_CREGISTER:
              begin
                 ungetregister32(t.registerhigh);
                 ungetregister32(t.registerlow);
              end;
            LOC_MEM,
            LOC_REFERENCE:
              del_reference(t.reference);
            else internalerror(331);
         end;
      end;


    procedure emit_push_loc(const t:tlocation);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,makereg32(t.register)));
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             exprasmList.concat(Taicpu.Op_const(A_PUSH,S_L,t.reference.offset))
                           else
                             exprasmList.concat(Taicpu.Op_ref(A_PUSH,S_L,newreference(t.reference)));
                           del_reference(t.reference);
                           ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_pushw_loc(const t:tlocation);
      var
        opsize : topsize;
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           if aktalignment.paraalign=4 then
                             exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,makereg32(t.register)))
                           else
                             exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_W,makereg16(t.register)));
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if aktalignment.paraalign=4 then
                            opsize:=S_L
                           else
                            opsize:=S_W;
                           if t.reference.is_immediate then
                             exprasmList.concat(Taicpu.Op_const(A_PUSH,opsize,t.reference.offset))
                           else
                             exprasmList.concat(Taicpu.Op_ref(A_PUSH,opsize,newreference(t.reference)));
                           del_reference(t.reference);
                           ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_lea_loc_ref(const t:tlocation;const ref:treference;freetemp:boolean);
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
                               getexplicitregister32(R_EDI);
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),R_EDI);
                               exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,
                                 R_EDI,newreference(ref)));
                               ungetregister32(R_EDI);
                             end;
                            { release the registers }
                            del_reference(t.reference);
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;


    procedure emit_push_lea_loc(const t:tlocation;freetemp:boolean);
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
                               getexplicitregister32(R_EDI);
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),R_EDI);
                               exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
                               ungetregister32(R_EDI);
                             end;
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;

    procedure emit_push_mem_size(const t: treference; size: longint);

      var
        s: topsize;

      begin
        if t.is_immediate then
          begin
            if (size=4) or
               (aktalignment.paraalign=4) then
              exprasmList.concat(Taicpu.Op_const(A_PUSH,S_L,t.offset))
            else
              exprasmList.concat(Taicpu.Op_const(A_PUSH,S_W,t.offset));
          end
        else
          if size < 4 then
            begin
              getexplicitregister32(R_EDI);
              case size of
                1: s := S_BL;
                2: s := S_WL;
                else internalerror(200008071);
              end;
              exprasmList.concat(Taicpu.Op_ref_reg(A_MOVZX,s,
                newreference(t),R_EDI));
              if aktalignment.paraalign=4 then
                exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI))
              else
                exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_W,R_DI));
              ungetregister32(R_EDI);
            end
          else
            if size = 4 then
              emit_push_mem(t)
            else
              internalerror(200008072);
      end;


    procedure emit_to_mem(var t:tlocation;def:tdef);

      var
         r : treference;

      begin
        case t.loc of
               LOC_FPU : begin
                           reset_reference(t.reference);
                           gettempofsizereference(10,t.reference);
                           floatstore(tfloatdef(def).typ,t.reference);
                         end;
               LOC_REGISTER:
                 begin
                    if is_64bitint(def) then
                      begin
                         gettempofsizereference(8,r);
                         emit_reg_ref(A_MOV,S_L,t.registerlow,newreference(r));
                         inc(r.offset,4);
                         emit_reg_ref(A_MOV,S_L,t.registerhigh,newreference(r));
                         dec(r.offset,4);
                         t.reference:=r;
                      end
                    else
                      internalerror(1405001);
                 end;
               LOC_MEM,
         LOC_REFERENCE : ;
         LOC_CFPUREGISTER : begin
                           emit_reg(A_FLD,S_NO,correct_fpuregister(t.register,fpuvaroffset));
                           inc(fpuvaroffset);
                           reset_reference(t.reference);
                           gettempofsizereference(10,t.reference);
                           floatstore(tfloatdef(def).typ,t.reference);
                         end;
         else
         internalerror(333);
        end;
        t.loc:=LOC_MEM;
      end;


    procedure emit_to_reg16(var hr:tregister);
      begin
        { ranges are a little bit bug sensitive ! }
        case hr of
           R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP,R_EBP:
             begin
               hr:=reg32toreg16(hr);
             end;
           R_AL,R_BL,R_CL,R_DL:
             begin
               hr:=reg8toreg16(hr);
               emit_const_reg(A_AND,S_W,$ff,hr);
             end;
           R_AH,R_BH,R_CH,R_DH:
             begin
               hr:=reg8toreg16(hr);
               emit_const_reg(A_AND,S_W,$ff00,hr);
             end;
        end;
      end;


    procedure emit_to_reg32(var hr:tregister);
      begin
        { ranges are a little bit bug sensitive ! }
        case hr of
           R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP,R_BP:
             begin
                hr:=reg16toreg32(hr);
                emit_const_reg(A_AND,S_L,$ffff,hr);
             end;
           R_AL,R_BL,R_CL,R_DL:
             begin
                hr:=reg8toreg32(hr);
                emit_const_reg(A_AND,S_L,$ff,hr);
             end;
           R_AH,R_BH,R_CH,R_DH:
             begin
                hr:=reg8toreg32(hr);
                emit_const_reg(A_AND,S_L,$ff00,hr);
             end;
        end;
      end;

    procedure emit_mov_ref_reg64(r : treference;rl,rh : tregister);

      var
         hr : preference;

      begin
         { if we load a 64 bit reference, we must be careful because }
         { we could overwrite the registers of the reference by      }
         { accident                                                  }
         getexplicitregister32(R_EDI);
         if r.base=rl then
           begin
              emit_reg_reg(A_MOV,S_L,r.base,
                R_EDI);
              r.base:=R_EDI;
           end
         else if r.index=rl then
           begin
              emit_reg_reg(A_MOV,S_L,r.index,
                R_EDI);
              r.index:=R_EDI;
           end;
         emit_ref_reg(A_MOV,S_L,
           newreference(r),rl);
         hr:=newreference(r);
         inc(hr^.offset,4);
         emit_ref_reg(A_MOV,S_L,
           hr,rh);
         ungetregister32(R_EDI);
      end;

{*****************************************************************************
                           Emit String Functions
*****************************************************************************}

    procedure incrcomintfref(t: tdef; const ref: treference);

      var
         pushedregs : tpushed;

      begin
         pushusedregisters(pushedregs,$ff);
         emit_ref(A_PUSH,S_L,newreference(ref));
         saveregvars($ff);
         if is_interfacecom(t) then
           emitcall('FPC_INTF_INCR_REF')
         else
           internalerror(1859);
         popusedregisters(pushedregs);
      end;


    procedure decrcomintfref(t: tdef; const ref: treference);

      var
         pushedregs : tpushed;

      begin
         pushusedregisters(pushedregs,$ff);
         emitpushreferenceaddr(ref);
         saveregvars($ff);
         if is_interfacecom(t) then
           begin
              emitcall('FPC_INTF_DECR_REF');
           end
         else internalerror(1859);
         popusedregisters(pushedregs);
      end;



    procedure copyshortstring(const dref,sref : treference;len : byte;
                loadref, del_sref: boolean);
      begin
         emitpushreferenceaddr(dref);
          { if it's deleted right before it's used, the optimizer can move }
          { the reg deallocations to the right places (JM)                 }
         if del_sref then
           del_reference(sref);
         if loadref then
          emit_push_mem(sref)
         else
          emitpushreferenceaddr(sref);
         push_int(len);
         emitcall('FPC_SHORTSTR_COPY');
         maybe_loadself;
      end;


{$ifdef unused}
    procedure copylongstring(const dref,sref : treference;len : longint;loadref:boolean);
      begin
         emitpushreferenceaddr(dref);
         if loadref then
          emit_push_mem(sref)
         else
          emitpushreferenceaddr(sref);
         push_int(len);
         saveregvars($ff);
         emitcall('FPC_LONGSTR_COPY');
         maybe_loadself;
      end;
{$endif unused}


    procedure incrstringref(t : tdef;const ref : treference);
      var
         pushedregs : tpushed;
      begin
         pushusedregisters(pushedregs,$ff);
         emitpushreferenceaddr(ref);
         saveregvars($ff);
         if is_ansistring(t) then
           begin
              emitcall('FPC_ANSISTR_INCR_REF');
           end
         else if is_widestring(t) then
           begin
              emitcall('FPC_WIDESTR_INCR_REF');
           end
         else internalerror(1859);
         popusedregisters(pushedregs);
      end;


    procedure decrstringref(t : tdef;const ref : treference);

      var
         pushedregs : tpushed;

      begin
         pushusedregisters(pushedregs,$ff);
         emitpushreferenceaddr(ref);
         saveregvars($ff);
         if is_ansistring(t) then
           begin
              emitcall('FPC_ANSISTR_DECR_REF');
           end
         else if is_widestring(t) then
           begin
              emitcall('FPC_WIDESTR_DECR_REF');
           end
         else internalerror(1859);
         popusedregisters(pushedregs);
      end;

{*****************************************************************************
                           Emit Push Functions
*****************************************************************************}

    procedure push_int(l : longint);
      begin
         if (l = 0) and
            not(aktoptprocessor in [Class386, ClassP6]) and
            not(cs_littlesize in aktglobalswitches)
           Then
             begin
               getexplicitregister32(R_EDI);
               emit_reg_reg(A_XOR,S_L,R_EDI,R_EDI);
               exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
               ungetregister32(R_EDI);
             end
           else
             exprasmList.concat(Taicpu.Op_const(A_PUSH,S_L,l));
      end;

    procedure emit_push_mem(const ref : treference);

      begin
         if ref.is_immediate then
           push_int(ref.offset)
         else
           begin
             if not(aktoptprocessor in [Class386, ClassP6]) and
                not(cs_littlesize in aktglobalswitches)
               then
                 begin
                   getexplicitregister32(R_EDI);
                   emit_ref_reg(A_MOV,S_L,newreference(ref),R_EDI);
                   exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
                   ungetregister32(R_EDI);
                 end
               else exprasmList.concat(Taicpu.Op_ref(A_PUSH,S_L,newreference(ref)));
           end;
      end;


    procedure emitpushreferenceaddr(const ref : treference);
      var
        href : treference;
      begin
         { this will fail for references to other segments !!! }
         if ref.is_immediate then
         { is this right ? }
           begin
              { push_int(ref.offset)}
              gettempofsizereference(4,href);
              emit_const_ref(A_MOV,S_L,ref.offset,newreference(href));
              emitpushreferenceaddr(href);
              del_reference(href);
           end
         else
           begin
              if ref.segment<>R_NO then
                CGMessage(cg_e_cant_use_far_pointer_there);
              if (ref.base=R_NO) and (ref.index=R_NO) then
                exprasmList.concat(Taicpu.Op_sym_ofs(A_PUSH,S_L,ref.symbol,ref.offset))
              else if (ref.base=R_NO) and (ref.index<>R_NO) and
                 (ref.offset=0) and (ref.scalefactor=0) and (ref.symbol=nil) then
                exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,ref.index))
              else if (ref.base<>R_NO) and (ref.index=R_NO) and
                 (ref.offset=0) and (ref.symbol=nil) then
                exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,ref.base))
              else
                begin
                   getexplicitregister32(R_EDI);
                   emit_ref_reg(A_LEA,S_L,newreference(ref),R_EDI);
                   exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
                   ungetregister32(R_EDI);
                end;
           end;
        end;


{*****************************************************************************
                           Emit Float Functions
*****************************************************************************}

    procedure floatloadops(t : tfloattype;var op : tasmop;var s : topsize);
      begin
         case t of
            s32real : begin
                         op:=A_FLD;
                         s:=S_FS;
                      end;
            s64real : begin
                         op:=A_FLD;
                         { ???? }
                         s:=S_FL;
                      end;
            s80real : begin
                         op:=A_FLD;
                         s:=S_FX;
                      end;
            s64comp : begin
                         op:=A_FILD;
                         s:=S_IQ;
                      end;
            else internalerror(17);
         end;
      end;


    procedure floatload(t : tfloattype;const ref : treference);
      var
         op : tasmop;
         s : topsize;
      begin
         floatloadops(t,op,s);
         exprasmList.concat(Taicpu.Op_ref(op,s,
           newreference(ref)));
         inc(fpuvaroffset);
      end;


    procedure floatstoreops(t : tfloattype;var op : tasmop;var s : topsize);
      begin
         case t of
            s32real : begin
                         op:=A_FSTP;
                         s:=S_FS;
                      end;
            s64real : begin
                         op:=A_FSTP;
                         s:=S_FL;
                      end;
            s80real : begin
                         op:=A_FSTP;
                          s:=S_FX;
                      end;
            s64comp : begin
                         op:=A_FISTP;
                         s:=S_IQ;
                      end;
         else
           internalerror(17);
         end;
      end;


    procedure floatstore(t : tfloattype;const ref : treference);
      var
         op : tasmop;
         s : topsize;
      begin
         floatstoreops(t,op,s);
         exprasmList.concat(Taicpu.Op_ref(op,s,
           newreference(ref)));
         dec(fpuvaroffset);
      end;


{*****************************************************************************
                           Emit Functions
*****************************************************************************}

    procedure concatcopy(source,dest : treference;size : longint;delsource,loadref : boolean);

      const
         isizes : array[0..3] of topsize=(S_L,S_B,S_W,S_B);
         ishr : array[0..3] of byte=(2,0,1,0);

      var
         ecxpushed : boolean;
         oldsourceoffset,
         helpsize : longint;
         i : byte;
         reg8,reg32 : tregister;
         swap : boolean;

         procedure maybepushecx;
         begin
           if not(R_ECX in unused) then
             begin
               exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,R_ECX));
               ecxpushed:=true;
             end
           else getexplicitregister32(R_ECX);
         end;

      begin
         oldsourceoffset:=source.offset;
         if (not loadref) and
            ((size<=8) or
             (not(cs_littlesize in aktglobalswitches ) and (size<=12))) then
           begin
              helpsize:=size shr 2;
              getexplicitregister32(R_EDI);
              for i:=1 to helpsize do
                begin
                   emit_ref_reg(A_MOV,S_L,newreference(source),R_EDI);
                   If (size = 4) and delsource then
                     del_reference(source);
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,newreference(dest)));
                   inc(source.offset,4);
                   inc(dest.offset,4);
                   dec(size,4);
                end;
              if size>1 then
                begin
                   emit_ref_reg(A_MOV,S_W,newreference(source),R_DI);
                   If (size = 2) and delsource then
                     del_reference(source);
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_W,R_DI,newreference(dest)));
                   inc(source.offset,2);
                   inc(dest.offset,2);
                   dec(size,2);
                end;
              ungetregister32(R_EDI);
              if size>0 then
                begin
                   { and now look for an 8 bit register }
                   swap:=false;
                   if R_EAX in unused then reg8:=reg32toreg8(getexplicitregister32(R_EAX))
                   else if R_EDX in unused then reg8:=reg32toreg8(getexplicitregister32(R_EDX))
                   else if R_EBX in unused then reg8:=reg32toreg8(getexplicitregister32(R_EBX))
                   else if R_ECX in unused then reg8:=reg32toreg8(getexplicitregister32(R_ECX))
                   else
                      begin
                         swap:=true;
                         { we need only to check 3 registers, because }
                         { one is always not index or base          }
                         if (dest.base<>R_EAX) and (dest.index<>R_EAX) then
                           begin
                              reg8:=R_AL;
                              reg32:=R_EAX;
                           end
                         else if (dest.base<>R_EBX) and (dest.index<>R_EBX) then
                           begin
                              reg8:=R_BL;
                              reg32:=R_EBX;
                           end
                         else if (dest.base<>R_ECX) and (dest.index<>R_ECX) then
                           begin
                              reg8:=R_CL;
                              reg32:=R_ECX;
                           end;
                      end;
                   if swap then
                     { was earlier XCHG, of course nonsense }
                     begin
                       getexplicitregister32(R_EDI);
                       emit_reg_reg(A_MOV,S_L,reg32,R_EDI);
                     end;
                   emit_ref_reg(A_MOV,S_B,newreference(source),reg8);
                   If delsource then
                     del_reference(source);
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_B,reg8,newreference(dest)));
                   if swap then
                     begin
                       emit_reg_reg(A_MOV,S_L,R_EDI,reg32);
                       ungetregister32(R_EDI);
                     end
                   else
                     ungetregister(reg8);
                end;
           end
         else
           begin
              getexplicitregister32(R_EDI);
              emit_ref_reg(A_LEA,S_L,newreference(dest),R_EDI);
              exprasmList.concat(Tairegalloc.Alloc(R_ESI));
              if loadref then
                emit_ref_reg(A_MOV,S_L,newreference(source),R_ESI)
              else
                begin
                  emit_ref_reg(A_LEA,S_L,newreference(source),R_ESI);
                  if delsource then
                    del_reference(source);
                end;

              exprasmList.concat(Taicpu.Op_none(A_CLD,S_NO));
              ecxpushed:=false;
              if cs_littlesize in aktglobalswitches  then
                begin
                   maybepushecx;
                   emit_const_reg(A_MOV,S_L,size,R_ECX);
                   exprasmList.concat(Taicpu.Op_none(A_REP,S_NO));
                   exprasmList.concat(Taicpu.Op_none(A_MOVSB,S_NO));
                end
              else
                begin
                   helpsize:=size shr 2;
                   size:=size and 3;
                   if helpsize>1 then
                    begin
                      maybepushecx;
                      emit_const_reg(A_MOV,S_L,helpsize,R_ECX);
                      exprasmList.concat(Taicpu.Op_none(A_REP,S_NO));
                    end;
                   if helpsize>0 then
                    exprasmList.concat(Taicpu.Op_none(A_MOVSD,S_NO));
                   if size>1 then
                     begin
                        dec(size,2);
                        exprasmList.concat(Taicpu.Op_none(A_MOVSW,S_NO));
                     end;
                   if size=1 then
                     exprasmList.concat(Taicpu.Op_none(A_MOVSB,S_NO));
                end;
              ungetregister32(R_EDI);
              exprasmList.concat(Tairegalloc.DeAlloc(R_ESI));
              if ecxpushed then
                exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,R_ECX))
              else
                ungetregister32(R_ECX);

              { loading SELF-reference again }
              maybe_loadself;
           end;
         if delsource then
           begin
             source.offset:=oldsourceoffset;
             ungetiftemp(source);
           end;
      end;


    procedure emitloadord2reg(const location:Tlocation;orddef:torddef;
                              destreg:Tregister;delloc:boolean);

    {A lot smaller and less bug sensitive than the original unfolded loads.}

    var tai:Taicpu;
        r:Preference;

    begin
        tai := nil;
        case location.loc of
            LOC_REGISTER,LOC_CREGISTER:
                begin
                    case orddef.typ of
                        u8bit,uchar,bool8bit:
                            tai:=Taicpu.Op_reg_reg(A_MOVZX,S_BL,location.register,destreg);
                        s8bit:
                            tai:=Taicpu.Op_reg_reg(A_MOVSX,S_BL,location.register,destreg);
                        u16bit,uwidechar,bool16bit:
                            tai:=Taicpu.Op_reg_reg(A_MOVZX,S_WL,location.register,destreg);
                        s16bit:
                            tai:=Taicpu.Op_reg_reg(A_MOVSX,S_WL,location.register,destreg);
                        u32bit,bool32bit,s32bit:
                            if location.register <> destreg then
                              tai:=Taicpu.Op_reg_reg(A_MOV,S_L,location.register,destreg);
                        else
                          internalerror(330);
                    end;
                    if delloc then
                        ungetregister(location.register);
                end;
            LOC_MEM,
            LOC_REFERENCE:
                begin
                    if location.reference.is_immediate then
                     tai:=Taicpu.Op_const_reg(A_MOV,S_L,location.reference.offset,destreg)
                    else
                     begin
                       r:=newreference(location.reference);
                       case orddef.typ of
                         u8bit,uchar,bool8bit:
                            tai:=Taicpu.Op_ref_reg(A_MOVZX,S_BL,r,destreg);
                         s8bit:
                            tai:=Taicpu.Op_ref_reg(A_MOVSX,S_BL,r,destreg);
                         u16bit,uwidechar,bool16bit:
                            tai:=Taicpu.Op_ref_reg(A_MOVZX,S_WL,r,destreg);
                         s16bit:
                            tai:=Taicpu.Op_ref_reg(A_MOVSX,S_WL,r,destreg);
                         u32bit,bool32bit:
                            tai:=Taicpu.Op_ref_reg(A_MOV,S_L,r,destreg);
                         s32bit:
                            tai:=Taicpu.Op_ref_reg(A_MOV,S_L,r,destreg);
                         else
                           internalerror(330);
                       end;
                     end;
                    if delloc then
                        del_reference(location.reference);
                end
            else
                internalerror(6);
        end;
        if assigned(tai) then
          exprasmList.concat(tai);
    end;

    { if necessary ESI is reloaded after a call}
    procedure maybe_loadself;

      var
         hp : preference;
         p : pprocinfo;
         i : longint;

      begin
         if assigned(procinfo^._class) then
           begin
              exprasmList.concat(Tairegalloc.Alloc(R_ESI));
              if lexlevel>normal_function_level then
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo^.framepointer_offset;
                   hp^.base:=procinfo^.framepointer;
                   emit_ref_reg(A_MOV,S_L,hp,R_ESI);
                   p:=procinfo^.parent;
                   for i:=3 to lexlevel-1 do
                     begin
                        new(hp);
                        reset_reference(hp^);
                        hp^.offset:=p^.framepointer_offset;
                        hp^.base:=R_ESI;
                        emit_ref_reg(A_MOV,S_L,hp,R_ESI);
                        p:=p^.parent;
                     end;
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=p^.selfpointer_offset;
                   hp^.base:=R_ESI;
                   emit_ref_reg(A_MOV,S_L,hp,R_ESI);
                end
              else
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo^.selfpointer_offset;
                   hp^.base:=procinfo^.framepointer;
                   emit_ref_reg(A_MOV,S_L,hp,R_ESI);
                end;
           end;
      end;


{*****************************************************************************
                            Entry/Exit Code Functions
*****************************************************************************}

  procedure genprofilecode;
    var
      pl : tasmlabel;
    begin
      if (po_assembler in aktprocdef.procoptions) then
       exit;
      case target_info.target of
         target_i386_win32,
         target_i386_freebsd,
         target_i386_linux:
           begin
              getaddrlabel(pl);
              emitinsertcall(target_info.Cprefix+'mcount');
              usedinproc:=usedinproc or ($80 shr byte(R_EDX));
              exprasmList.insert(Taicpu.Op_sym_ofs_reg(A_MOV,S_L,pl,0,R_EDX));
              exprasmList.insert(Tai_section.Create(sec_code));
              exprasmList.insert(Tai_const.Create_32bit(0));
              exprasmList.insert(Tai_label.Create(pl));
              exprasmList.insert(Tai_align.Create(4));
              exprasmList.insert(Tai_section.Create(sec_data));
           end;

         target_i386_go32v2:
           begin
              emitinsertcall('MCOUNT');
           end;
      end;
    end;


    procedure generate_interrupt_stackframe_entry;
      begin
         { save the registers of an interrupt procedure }
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_EAX));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_EBX));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_ECX));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_EDX));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_ESI));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));

         { .... also the segment registers }
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_W,R_DS));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_W,R_ES));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_W,R_FS));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_W,R_GS));
      end;


    procedure generate_interrupt_stackframe_exit;
      begin
         { restore the registers of an interrupt procedure }
         { this was all with entrycode instead of exitcode !!}
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_L,R_EAX));
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_L,R_EBX));
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_L,R_ECX));
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_L,R_EDX));
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_L,R_ESI));
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_L,R_EDI));

         { .... also the segment registers }
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_W,R_DS));
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_W,R_ES));
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_W,R_FS));
         procinfo^.aktexitcode.concat(Taicpu.Op_reg(A_POP,S_W,R_GS));

        { this restores the flags }
         procinfo^.aktexitcode.concat(Taicpu.Op_none(A_IRET,S_NO));
      end;


  { generates the code for threadvar initialisation }
  procedure initialize_threadvar(p : tnamedindexitem);

    var
       hr : treference;

    begin
       if (tsym(p).typ=varsym) and
          (vo_is_thread_var in tvarsym(p).varoptions) then
         begin
            exprasmList.concat(Taicpu.Op_const(A_PUSH,S_L,tvarsym(p).getsize));
            reset_reference(hr);
            hr.symbol:=newasmsymbol(tvarsym(p).mangledname);
            emitpushreferenceaddr(hr);
            saveregvars($ff);
            emitcall('FPC_INIT_THREADVAR');
         end;
    end;

    { initilizes data of type t                           }
    { if is_already_ref is true then the routines assumes }
    { that r points to the data to initialize             }
    procedure initialize(t : tdef;const ref : treference;is_already_ref : boolean);

      var
         hr : treference;

      begin
         if is_ansistring(t) or
           is_widestring(t) or
           is_interfacecom(t) then
           begin
              emit_const_ref(A_MOV,S_L,0,
                newreference(ref));
           end
         else
           begin
              reset_reference(hr);
              hr.symbol:=tstoreddef(t).get_rtti_label(initrtti);
              emitpushreferenceaddr(hr);
              if is_already_ref then
                exprasmList.concat(Taicpu.Op_ref(A_PUSH,S_L,newreference(ref)))
              else
                emitpushreferenceaddr(ref);
              emitcall('FPC_INITIALIZE');
           end;
      end;

    { finalizes data of type t                            }
    { if is_already_ref is true then the routines assumes }
    { that r points to the data to finalizes              }
    procedure finalize(t : tdef;const ref : treference;is_already_ref : boolean);

      var
         r : treference;

      begin
         if is_ansistring(t) or
            is_widestring(t) then
           begin
              decrstringref(t,ref);
           end
         else if is_interfacecom(t) then
           begin
              decrcomintfref(t,ref);
           end
         else
           begin
              reset_reference(r);
              r.symbol:=tstoreddef(t).get_rtti_label(initrtti);
              emitpushreferenceaddr(r);
              if is_already_ref then
                exprasmList.concat(Taicpu.Op_ref(A_PUSH,S_L,
                  newreference(ref)))
              else
                emitpushreferenceaddr(ref);
              emitcall('FPC_FINALIZE');
           end;
      end;


  { generates the code for initialisation of local data }
  procedure initialize_data(p : tnamedindexitem);

    var
       hr : treference;

    begin
       if (tsym(p).typ=varsym) and
          assigned(tvarsym(p).vartype.def) and
          not(is_class(tvarsym(p).vartype.def)) and
          tvarsym(p).vartype.def.needs_inittable then
         begin
            if assigned(procinfo) then
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            if tsym(p).owner.symtabletype in [localsymtable,inlinelocalsymtable] then
              begin
                 hr.base:=procinfo^.framepointer;
                 hr.offset:=-tvarsym(p).address+tvarsym(p).owner.address_fixup;
              end
            else
              begin
                 hr.symbol:=newasmsymbol(tvarsym(p).mangledname);
              end;
            initialize(tvarsym(p).vartype.def,hr,false);
         end;
    end;

  { generates the code for incrementing the reference count of parameters and
    initialize out parameters }
  procedure init_paras(p : tnamedindexitem);

    var
       hrv : treference;
       hr: treference;

    begin
       if (tsym(p).typ=varsym) and
          not is_class(tvarsym(p).vartype.def) and
          tvarsym(p).vartype.def.needs_inittable then
         begin
           if (tvarsym(p).varspez=vs_value) then
             begin
               procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;

               reset_reference(hrv);
               hrv.base:=procinfo^.framepointer;
               if assigned(tvarsym(p).localvarsym) then
                hrv.offset:=-tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup
               else
                hrv.offset:=tvarsym(p).address+procinfo^.para_offset;

               if is_ansistring(tvarsym(p).vartype.def) or
                  is_widestring(tvarsym(p).vartype.def) then
                 begin
                   incrstringref(tvarsym(p).vartype.def,hrv)
                 end
               else if is_interfacecom(tvarsym(p).vartype.def) then
                 begin
                   incrcomintfref(tvarsym(p).vartype.def,hrv)
                 end
               else
                 begin
                   reset_reference(hr);
                   hr.symbol:=tstoreddef(tvarsym(p).vartype.def).get_rtti_label(initrtti);
                   emitpushreferenceaddr(hr);
                   emitpushreferenceaddr(hrv);
                   emitcall('FPC_ADDREF');
                 end;
             end
           else if (tvarsym(p).varspez=vs_out) then
             begin
               reset_reference(hrv);
               hrv.base:=procinfo^.framepointer;
               hrv.offset:=tvarsym(p).address+procinfo^.para_offset;
               getexplicitregister32(R_EDI);
               exprasmList.concat(Taicpu.Op_ref_reg(A_MOV,S_L,newreference(hrv),R_EDI));
               reset_reference(hr);
               hr.base:=R_EDI;
               initialize(tvarsym(p).vartype.def,hr,false);
             end;
         end;
    end;

  { generates the code for decrementing the reference count of parameters }
  procedure final_paras(p : tnamedindexitem);

    var
       hrv : treference;
       hr: treference;

    begin
       if (tsym(p).typ=varsym) and
          not is_class(tvarsym(p).vartype.def) and
          tvarsym(p).vartype.def.needs_inittable then
         begin
           if (tvarsym(p).varspez=vs_value) then
             begin
               procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;

               reset_reference(hrv);
               hrv.base:=procinfo^.framepointer;
               if assigned(tvarsym(p).localvarsym) then
                hrv.offset:=-tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup
               else
                hrv.offset:=tvarsym(p).address+procinfo^.para_offset;

               if is_ansistring(tvarsym(p).vartype.def) or
                  is_widestring(tvarsym(p).vartype.def) then
                 begin
                   decrstringref(tvarsym(p).vartype.def,hrv)
                 end
               else if is_interfacecom(tvarsym(p).vartype.def) then
                 begin
                   decrcomintfref(tvarsym(p).vartype.def,hrv)
                 end
               else
                 begin
                   reset_reference(hr);
                   hr.symbol:=tstoreddef(tvarsym(p).vartype.def).get_rtti_label(initrtti);
                   emitpushreferenceaddr(hr);
                   emitpushreferenceaddr(hrv);
                   emitcall('FPC_DECREF');
                 end;
             end;
         end;
    end;


  { generates the code for finalisation of local data }
  procedure finalize_data(p : tnamedindexitem);

    var
       hr : treference;

    begin
       if (tsym(p).typ=varsym) and
          assigned(tvarsym(p).vartype.def) and
          not(is_class(tvarsym(p).vartype.def)) and
          tvarsym(p).vartype.def.needs_inittable then
         begin
            if assigned(procinfo) then
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            case tsym(p).owner.symtabletype of
               localsymtable,inlinelocalsymtable:
                 begin
                    hr.base:=procinfo^.framepointer;
                    hr.offset:=-tvarsym(p).address+tvarsym(p).owner.address_fixup;
                 end;
               else
                 hr.symbol:=newasmsymbol(tvarsym(p).mangledname);
            end;
            finalize(tvarsym(p).vartype.def,hr,false);
         end;
    end;


  { generates the code to make local copies of the value parameters }
  procedure copyvalueparas(p : tnamedindexitem);
    var
      href1,href2 : treference;
      r    : preference;
      power,len  : longint;
      opsize : topsize;
{$ifndef NOTARGETWIN32}
      again,ok : tasmlabel;
{$endif}
    begin
       if (tsym(p).typ=varsym) and
          (tvarsym(p).varspez=vs_value) and
          (push_addr_param(tvarsym(p).vartype.def)) then
        begin
          if is_open_array(tvarsym(p).vartype.def) or
             is_array_of_const(tvarsym(p).vartype.def) then
           begin
              { get stack space }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=tvarsym(p).address+4+procinfo^.para_offset;
              getexplicitregister32(R_EDI);
              exprasmList.concat(Taicpu.op_ref_reg(A_MOV,S_L,r,R_EDI));
              exprasmList.concat(Taicpu.op_reg(A_INC,S_L,R_EDI));
              if (tarraydef(tvarsym(p).vartype.def).elesize<>1) then
               begin
                 if ispowerof2(tarraydef(tvarsym(p).vartype.def).elesize, power) then
                   exprasmList.concat(Taicpu.op_const_reg(A_SHL,S_L,power,R_EDI))
                 else
                   exprasmList.concat(Taicpu.op_const_reg(A_IMUL,S_L,
                     tarraydef(tvarsym(p).vartype.def).elesize,R_EDI));
               end;
{$ifndef NOTARGETWIN32}
              { windows guards only a few pages for stack growing, }
              { so we have to access every page first              }
              if target_info.target=target_i386_win32 then
                begin
                   getlabel(again);
                   getlabel(ok);
                   emitlab(again);
                   exprasmList.concat(Taicpu.op_const_reg(A_CMP,S_L,winstackpagesize,R_EDI));
                   emitjmp(C_C,ok);
                   exprasmList.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP));
                   exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
                   exprasmList.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize,R_EDI));
                   emitjmp(C_None,again);

                   emitlab(ok);
                   exprasmList.concat(Taicpu.op_reg_reg(A_SUB,S_L,R_EDI,R_ESP));
                   ungetregister32(R_EDI);
                   { now reload EDI }
                   new(r);
                   reset_reference(r^);
                   r^.base:=procinfo^.framepointer;
                   r^.offset:=tvarsym(p).address+4+procinfo^.para_offset;
                   getexplicitregister32(R_EDI);
                   exprasmList.concat(Taicpu.op_ref_reg(A_MOV,S_L,r,R_EDI));

                   exprasmList.concat(Taicpu.op_reg(A_INC,S_L,R_EDI));

                   if (tarraydef(tvarsym(p).vartype.def).elesize<>1) then
                    begin
                      if ispowerof2(tarraydef(tvarsym(p).vartype.def).elesize, power) then
                        exprasmList.concat(Taicpu.op_const_reg(A_SHL,S_L,power,R_EDI))
                      else
                        exprasmList.concat(Taicpu.op_const_reg(A_IMUL,S_L,
                          tarraydef(tvarsym(p).vartype.def).elesize,R_EDI));
                    end;
                end
              else
{$endif NOTARGETWIN32}
                exprasmList.concat(Taicpu.op_reg_reg(A_SUB,S_L,R_EDI,R_ESP));
              { load destination }
              exprasmList.concat(Taicpu.op_reg_reg(A_MOV,S_L,R_ESP,R_EDI));

              { don't destroy the registers! }
              exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_ECX));
              exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_ESI));

              { load count }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=tvarsym(p).address+4+procinfo^.para_offset;
              exprasmList.concat(Taicpu.op_ref_reg(A_MOV,S_L,r,R_ECX));

              { load source }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=tvarsym(p).address+procinfo^.para_offset;
              exprasmList.concat(Taicpu.op_ref_reg(A_MOV,S_L,r,R_ESI));

              { scheduled .... }
              exprasmList.concat(Taicpu.op_reg(A_INC,S_L,R_ECX));

              { calculate size }
              len:=tarraydef(tvarsym(p).vartype.def).elesize;
              opsize:=S_B;
              if (len and 3)=0 then
               begin
                 opsize:=S_L;
                 len:=len shr 2;
               end
              else
               if (len and 1)=0 then
                begin
                  opsize:=S_W;
                  len:=len shr 1;
                end;

              if ispowerof2(len, power) then
                exprasmList.concat(Taicpu.op_const_reg(A_SHL,S_L,power,R_ECX))
              else
                exprasmList.concat(Taicpu.op_const_reg(A_IMUL,S_L,len,R_ECX));
              exprasmList.concat(Taicpu.op_none(A_REP,S_NO));
              case opsize of
                S_B : exprasmList.concat(Taicpu.Op_none(A_MOVSB,S_NO));
                S_W : exprasmList.concat(Taicpu.Op_none(A_MOVSW,S_NO));
                S_L : exprasmList.concat(Taicpu.Op_none(A_MOVSD,S_NO));
              end;
              ungetregister32(R_EDI);
              exprasmList.concat(Taicpu.op_reg(A_POP,S_L,R_ESI));
              exprasmList.concat(Taicpu.op_reg(A_POP,S_L,R_ECX));

              { patch the new address }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=tvarsym(p).address+procinfo^.para_offset;
              exprasmList.concat(Taicpu.op_reg_ref(A_MOV,S_L,R_ESP,r));
           end
          else
           if is_shortstring(tvarsym(p).vartype.def) then
            begin
              reset_reference(href1);
              href1.base:=procinfo^.framepointer;
              href1.offset:=tvarsym(p).address+procinfo^.para_offset;
              reset_reference(href2);
              href2.base:=procinfo^.framepointer;
              href2.offset:=-tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup;
              copyshortstring(href2,href1,tstringdef(tvarsym(p).vartype.def).len,true,false);
            end
           else
            begin
              reset_reference(href1);
              href1.base:=procinfo^.framepointer;
              href1.offset:=tvarsym(p).address+procinfo^.para_offset;
              reset_reference(href2);
              href2.base:=procinfo^.framepointer;
              href2.offset:=-tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup;
              concatcopy(href1,href2,tvarsym(p).vartype.def.size,true,true);
            end;
        end;
    end;

  procedure inittempvariables;

    var
       hp : ptemprecord;
       r : preference;

    begin
       hp:=templist;
       while assigned(hp) do
         begin
           if hp^.temptype in [tt_ansistring,tt_freeansistring,
             tt_widestring,tt_freewidestring,
             tt_interfacecom] then
             begin
               procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
               new(r);
               reset_reference(r^);
               r^.base:=procinfo^.framepointer;
               r^.offset:=hp^.pos;
               emit_const_ref(A_MOV,S_L,0,r);
             end;
           hp:=hp^.next;
         end;
   end;

  procedure finalizetempvariables;

    var
       hp : ptemprecord;
       hr : treference;
    begin
       hp:=templist;
       while assigned(hp) do
         begin
            if hp^.temptype in [tt_ansistring,tt_freeansistring] then
              begin
                procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
                reset_reference(hr);
                hr.base:=procinfo^.framepointer;
                hr.offset:=hp^.pos;
                emitpushreferenceaddr(hr);
                emitcall('FPC_ANSISTR_DECR_REF');
              end
            else if hp^.temptype in [tt_widestring,tt_freewidestring] then
              begin
                procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
                reset_reference(hr);
                hr.base:=procinfo^.framepointer;
                hr.offset:=hp^.pos;
                emitpushreferenceaddr(hr);
                emitcall('FPC_WIDESTR_DECR_REF');
              end
            else if hp^.temptype=tt_interfacecom then
              begin
                procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
                reset_reference(hr);
                hr.base:=procinfo^.framepointer;
                hr.offset:=hp^.pos;
                emitpushreferenceaddr(hr);
                emitcall('FPC_INTF_DECR_REF');
              end;
            hp:=hp^.next;
         end;
   end;

{$ifdef dummy}
  var
     ls : longint;

  procedure largest_size(p : tnamedindexitem);

    begin
       if (tsym(p).typ=varsym) and
         (tvarsym(p).getvaluesize>ls) then
         ls:=tvarsym(p).getvaluesize;
    end;
{$endif dummy}

  procedure alignstack(alist : TAAsmoutput);

    begin
{$ifdef dummy}
       if (cs_optimize in aktglobalswitches) and
         (aktoptprocessor in [classp5,classp6]) then
         begin
            ls:=0;
            aktprocdef.localst.foreach({$ifndef TP}@{$endif}largest_size);
            if ls>=8 then
              aList.insert(Taicpu.Op_const_reg(A_AND,S_L,-8,R_ESP));
         end;
{$endif dummy}
    end;

  procedure genentrycode(alist : TAAsmoutput;make_global:boolean;
                         stackframe:longint;
                         var parasize:longint;var nostackframe:boolean;
                         inlined : boolean);
  {
    Generates the entry code for a procedure
  }
    var
      hs : string;
{$ifdef GDB}
      stab_function_name : tai_stab_function_name;
{$endif GDB}
      hr : preference;
      p : tsymtable;
      r : treference;
      oldlist,
      oldexprasmlist : TAAsmoutput;
      again : tasmlabel;
      i : longint;
      tempbuf,tempaddr : treference;

    begin
       oldexprasmlist:=exprasmlist;
       exprasmlist:=alist;
       if (not inlined) and (aktprocdef.proctypeoption=potype_proginit) then
           begin
              emitinsertcall('FPC_INITIALIZEUNITS');
              { initialize profiling for win32 }
              if (target_info.target=target_I386_WIN32) and
                 (cs_profile in aktmoduleswitches) then
                emitinsertcall('__monstartup');
              { add threadvars }
              oldlist:=exprasmlist;
              exprasmlist:=TAAsmoutput.Create;
              p:=symtablestack;
              while assigned(p) do
                begin
                   p.foreach_static({$ifndef TP}@{$endif}initialize_threadvar);
                   p:=p.next;
                end;
              oldList.insertlist(exprasmlist);
              exprasmlist.free;
              exprasmlist:=oldlist;
           end;

{$ifdef GDB}
      if (not inlined) and (cs_debuginfo in aktmoduleswitches) then
        exprasmList.insert(Tai_force_line.Create);
{$endif GDB}

      { a constructor needs a help procedure }
      if (aktprocdef.proctypeoption=potype_constructor) then
        begin
          if is_class(procinfo^._class) then
            begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              exprasmList.insert(Taicpu.Op_cond_sym(A_Jcc,C_Z,S_NO,faillabel));
              emitinsertcall('FPC_NEW_CLASS');
            end
          else if is_object(procinfo^._class) then
            begin
              exprasmList.insert(Taicpu.Op_cond_sym(A_Jcc,C_Z,S_NO,faillabel));
              emitinsertcall('FPC_HELP_CONSTRUCTOR');
              getexplicitregister32(R_EDI);
              exprasmList.insert(Taicpu.Op_const_reg(A_MOV,S_L,procinfo^._class.vmt_offset,R_EDI));
            end
          else
            Internalerror(200006161);
        end;

      { don't load ESI, does the caller }
      { we must do it for local function }
      { that can be called from a foreach_static }
      { of another object than self !! PM }

         if assigned(procinfo^._class) and  { !!!!! shouldn't we load ESI always? }
            (lexlevel>normal_function_level) then
           maybe_loadself;

      { When message method contains self as a parameter,
        we must load it into ESI }
      If (po_containsself in aktprocdef.procoptions) then
        begin
           new(hr);
           reset_reference(hr^);
           hr^.offset:=procinfo^.selfpointer_offset;
           hr^.base:=procinfo^.framepointer;
           exprasmList.insert(Taicpu.Op_ref_reg(A_MOV,S_L,hr,R_ESI));
           exprasmList.insert(Tairegalloc.Alloc(R_ESI));
        end;
      { should we save edi,esi,ebx like C ? }
      if (po_savestdregs in aktprocdef.procoptions) then
       begin
         if (aktprocdef.usedregisters and ($80 shr byte(R_EBX)))<>0 then
           exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_EBX));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_ESI));
         exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
       end;

      { for the save all registers we can simply use a pusha,popa which
        push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
      if (po_saveregisters in aktprocdef.procoptions) then
        begin
          exprasmList.insert(Taicpu.Op_none(A_PUSHA,S_L));
        end;

      { omit stack frame ? }
      if (not inlined) then
        if (procinfo^.framepointer=stack_pointer) then
          begin
              CGMessage(cg_d_stackframe_omited);
              nostackframe:=true;
              if (aktprocdef.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocdef.parast.datasize+procinfo^.para_offset-4;
              if stackframe<>0 then
                exprasmList.insert(Taicpu.op_const_reg(A_SUB,S_L,stackframe,R_ESP));
          end
        else
          begin
              alignstack(alist);
              if (aktprocdef.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocdef.parast.datasize+procinfo^.para_offset-8;
              nostackframe:=false;
              if stackframe<>0 then
               begin
{$ifndef NOTARGETWIN32}
                 { windows guards only a few pages for stack growing, }
                 { so we have to access every page first              }
                 if (target_info.target=target_i386_win32) and
                    (stackframe>=winstackpagesize) then
                   begin
                     if stackframe div winstackpagesize<=5 then
                       begin
                          exprasmList.insert(Taicpu.Op_const_reg(A_SUB,S_L,stackframe-4,R_ESP));
                          for i:=1 to stackframe div winstackpagesize do
                            begin
                               hr:=new_reference(R_ESP,stackframe-i*winstackpagesize);
                               exprasmList.concat(Taicpu.op_const_ref(A_MOV,S_L,0,hr));
                            end;
                          exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
                       end
                     else
                       begin
                          getlabel(again);
                          getexplicitregister32(R_EDI);
                          exprasmList.concat(Taicpu.op_const_reg(A_MOV,S_L,stackframe div winstackpagesize,R_EDI));
                          emitlab(again);
                          exprasmList.concat(Taicpu.op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP));
                          exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
                          exprasmList.concat(Taicpu.op_reg(A_DEC,S_L,R_EDI));
                          emitjmp(C_NZ,again);
                          ungetregister32(R_EDI);
                          exprasmList.concat(Taicpu.op_const_reg(A_SUB,S_L,stackframe mod winstackpagesize,R_ESP));
                       end
                   end
                 else
{$endif NOTARGETWIN32}
                   exprasmList.insert(Taicpu.Op_const_reg(A_SUB,S_L,stackframe,R_ESP));
                 if (cs_check_stack in aktlocalswitches) and
                   not(target_info.target in [target_i386_freebsd,target_i386_netbsd,
                                              target_i386_linux,target_i386_win32]) then
                   begin
                      emitinsertcall('FPC_STACKCHECK');
                      exprasmList.insert(Taicpu.Op_const(A_PUSH,S_L,stackframe));
                   end;
                 if cs_profile in aktmoduleswitches then
                   genprofilecode;
                 exprasmList.insert(Taicpu.Op_reg_reg(A_MOV,S_L,R_ESP,R_EBP));
                 exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_EBP));
               end { endif stackframe <> 0 }
              else
               begin
                 if cs_profile in aktmoduleswitches then
                   genprofilecode;
                 exprasmList.insert(Taicpu.Op_reg_reg(A_MOV,S_L,R_ESP,R_EBP));
                 exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_EBP));
               end;
          end;

      if (po_interrupt in aktprocdef.procoptions) then
          generate_interrupt_stackframe_entry;

      { initialize return value }
      if (not is_void(aktprocdef.rettype.def)) and
         (aktprocdef.rettype.def.needs_inittable) then
        begin
           procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
           reset_reference(r);
           r.offset:=procinfo^.return_offset;
           r.base:=procinfo^.framepointer;
           initialize(aktprocdef.rettype.def,r,ret_in_param(aktprocdef.rettype.def));
        end;

      { initialisize local data like ansistrings }
      case aktprocdef.proctypeoption of
         potype_unitinit:
           begin
              { using current_module.globalsymtable is hopefully      }
              { more robust than symtablestack and symtablestack.next }
              tsymtable(current_module.globalsymtable).foreach_static({$ifndef TP}@{$endif}initialize_data);
              tsymtable(current_module.localsymtable).foreach_static({$ifndef TP}@{$endif}initialize_data);
           end;
         { units have seperate code for initilization and finalization }
         potype_unitfinalize: ;
         else
           aktprocdef.localst.foreach_static({$ifndef TP}@{$endif}initialize_data);
      end;

      { initialisizes temp. ansi/wide string data }
      inittempvariables;

      { generate copies of call by value parameters }
      if not(po_assembler in aktprocdef.procoptions) and
         not(aktprocdef.proccalloption in [pocall_cdecl,pocall_cppdecl,pocall_palmossyscall,pocall_system]) then
        aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}copyvalueparas);

      if assigned( aktprocdef.parast) then
        aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}init_paras);

      { do we need an exception frame because of ansi/widestrings/interfaces ? }
      if not inlined and
         ((procinfo^.flags and pi_needs_implicit_finally)<>0) and
      { but it's useless in init/final code of units }
        not(aktprocdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
        begin
            usedinproc:=usedinproc or ($80 shr byte(R_EAX));

            exprasmList.concat(Taicpu.op_const_reg(A_SUB,S_L,36,R_ESP));
            exprasmList.concat(Taicpu.op_reg_reg(A_MOV,S_L,R_ESP,R_EDI));

            reset_reference(tempaddr);
            tempaddr.base:=R_EDI;
            emitpushreferenceaddr(tempaddr);

            reset_reference(tempbuf);
            tempbuf.base:=R_EDI;
            tempbuf.offset:=12;
            emitpushreferenceaddr(tempbuf);

            { Type of stack-frame must be pushed}
            exprasmList.concat(Taicpu.op_const(A_PUSH,S_L,1));
            emitcall('FPC_PUSHEXCEPTADDR');

            exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
            emitcall('FPC_SETJMP');
            exprasmList.concat(Taicpu.op_reg(A_PUSH,S_L,R_EAX));
            exprasmList.concat(Taicpu.op_reg_reg(A_TEST,S_L,R_EAX,R_EAX));
            emitjmp(C_NE,aktexitlabel);
            { probably we've to reload self here }
            maybe_loadself;
        end;

      if not inlined then
       begin
         if (cs_profile in aktmoduleswitches) or
            (aktprocdef.owner.symtabletype=globalsymtable) or
            (assigned(procinfo^._class) and (procinfo^._class.owner.symtabletype=globalsymtable)) then
              make_global:=true;

         hs:=aktprocdef.aliasnames.getfirst;

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and target_info.use_function_relative_addresses then
           stab_function_name := Tai_stab_function_name.Create(strpnew(hs));
{$EndIf GDB}

         while hs<>'' do
          begin
            if make_global then
              exprasmList.insert(Tai_symbol.Createname_global(hs,0))
            else
              exprasmList.insert(Tai_symbol.Createname(hs,0));

{$ifdef GDB}
            if (cs_debuginfo in aktmoduleswitches) and
               target_info.use_function_relative_addresses then
              exprasmList.insert(Tai_stab_function_name.Create(strpnew(hs)));
{$endif GDB}

            hs:=aktprocdef.aliasnames.getfirst;
          end;

         if make_global or ((procinfo^.flags and pi_is_global) <> 0) then
          aktprocsym.is_global := True;

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) then
          begin
            if target_info.use_function_relative_addresses then
             exprasmList.insert(stab_function_name);
            exprasmList.insert(Tai_stabs.Create(aktprocdef.stabstring));
            aktprocsym.isstabwritten:=true;
          end;
{$endif GDB}

         { Align, gprof uses 16 byte granularity }
         if (cs_profile in aktmoduleswitches) then
          exprasmList.insert(Tai_align.Create_op(16,$90))
         else
          exprasmList.insert(Tai_align.Create(aktalignment.procalign));
       end;
       if inlined then
         load_regvars(exprasmlist,nil);
      exprasmlist:=oldexprasmlist;
  end;


  procedure handle_return_value(inlined : boolean;var uses_eax,uses_edx : boolean);
    var
       hr : preference;
       op : Tasmop;
       s : Topsize;
  begin
      if not is_void(aktprocdef.rettype.def) then
          begin
              {if ((procinfo^.flags and pi_operator)<>0) and
                 assigned(otsym) then
                procinfo^.funcret_is_valid:=
                  procinfo^.funcret_is_valid or (otsym.refs>0);}
              if (tfuncretsym(aktprocdef.funcretsym).funcretstate<>vs_assigned) and not inlined { and
                ((procinfo^.flags and pi_uses_asm)=0)} then
               CGMessage(sym_w_function_result_not_set);
              hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset);
              if (aktprocdef.rettype.def.deftype in [orddef,enumdef]) then
                begin
                  uses_eax:=true;
                  exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                  case aktprocdef.rettype.def.size of
                   8:
                     begin
                        emit_ref_reg(A_MOV,S_L,hr,R_EAX);
                        hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset+4);
                        exprasmList.concat(Tairegalloc.Alloc(R_EDX));
                        emit_ref_reg(A_MOV,S_L,hr,R_EDX);
                        uses_edx:=true;
                     end;

                   4:
                     emit_ref_reg(A_MOV,S_L,hr,R_EAX);

                   2:
                     emit_ref_reg(A_MOV,S_W,hr,R_AX);

                   1:
                     emit_ref_reg(A_MOV,S_B,hr,R_AL);
                  end;
                end
              else
                if ret_in_acc(aktprocdef.rettype.def) then
                  begin
                    uses_eax:=true;
                    exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                    emit_ref_reg(A_MOV,S_L,hr,R_EAX);
                  end
              else
                 if (aktprocdef.rettype.def.deftype=floatdef) then
                   begin
                      floatloadops(tfloatdef(aktprocdef.rettype.def).typ,op,s);
                      exprasmList.concat(Taicpu.Op_ref(op,s,hr));
                   end
              else
                dispose(hr);
          end
  end;


  procedure handle_fast_exit_return_value;
    var
       hr : preference;
       op : Tasmop;
       s : Topsize;
    begin
      if not is_void(aktprocdef.rettype.def) then
          begin
              hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset);
              if (aktprocdef.rettype.def.deftype in [orddef,enumdef]) then
                begin
                  case aktprocdef.rettype.def.size of
                   8:
                     begin
                        emit_reg_ref(A_MOV,S_L,R_EAX,hr);
                        hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset+4);
                        emit_reg_ref(A_MOV,S_L,R_EDX,hr);
                     end;

                   4:
                     emit_reg_ref(A_MOV,S_L,R_EAX,hr);

                   2:
                     emit_reg_ref(A_MOV,S_W,R_AX,hr);

                   1:
                     emit_reg_ref(A_MOV,S_B,R_AL,hr);
                  end;
                end
              else
                if ret_in_acc(aktprocdef.rettype.def) then
                  begin
                    emit_reg_ref(A_MOV,S_L,R_EAX,hr);
                  end
              else
                 if (aktprocdef.rettype.def.deftype=floatdef) then
                   begin
                      floatstoreops(tfloatdef(aktprocdef.rettype.def).typ,op,s);
                      exprasmlist.concat(taicpu.op_ref(op,s,hr));
                   end
              else
                dispose(hr);
          end
     end;


  procedure genexitcode(alist : TAAsmoutput;parasize:longint;nostackframe,inlined:boolean);

    var
{$ifdef GDB}
       mangled_length : longint;
       p : pchar;
       st : string[2];
{$endif GDB}
       stabsendlabel,nofinal,okexitlabel,
       noreraiselabel,nodestroycall : tasmlabel;
       hr : treference;
       uses_eax,uses_edx,uses_esi : boolean;
       oldexprasmlist : TAAsmoutput;
       ai : taicpu;
       pd : tprocdef;

  begin
      oldexprasmlist:=exprasmlist;
      exprasmlist:=alist;

      if aktexit2label.is_used and
         ((procinfo^.flags and (pi_needs_implicit_finally or pi_uses_exceptions)) <> 0) then
        begin
          exprasmlist.concat(taicpu.op_sym(A_JMP,S_NO,aktexitlabel));
          exprasmlist.concat(tai_label.create(aktexit2label));
          handle_fast_exit_return_value;
        end;

      if aktexitlabel.is_used then
        exprasmList.concat(Tai_label.Create(aktexitlabel));

      cleanup_regvars(alist);

      { call the destructor help procedure }
      if (aktprocdef.proctypeoption=potype_destructor) and
         assigned(procinfo^._class) then
        begin
          if is_class(procinfo^._class) then
            begin
              emitinsertcall('FPC_DISPOSE_CLASS');
            end
          else if is_object(procinfo^._class) then
            begin
              emitinsertcall('FPC_HELP_DESTRUCTOR');
              getexplicitregister32(R_EDI);
              exprasmList.insert(Taicpu.Op_const_reg(A_MOV,S_L,procinfo^._class.vmt_offset,R_EDI));
              { must the object be finalized ? }
              if procinfo^._class.needs_inittable then
                begin
                   getlabel(nofinal);
                   exprasmList.insert(Tai_label.Create(nofinal));
                   emitinsertcall('FPC_FINALIZE');
                   ungetregister32(R_EDI);
                   exprasmList.insert(Taicpu.Op_reg(A_PUSH,S_L,R_ESI));
                   exprasmList.insert(Taicpu.Op_sym(A_PUSH,S_L,procinfo^._class.get_rtti_label(initrtti)));
                   ai:=Taicpu.Op_sym(A_Jcc,S_NO,nofinal);
                   ai.SetCondition(C_Z);
                   exprasmList.insert(ai);
                   reset_reference(hr);
                   hr.base:=R_EBP;
                   hr.offset:=8;
                   exprasmList.insert(Taicpu.Op_const_ref(A_CMP,S_L,0,newreference(hr)));
                end;
            end
          else
            begin
              Internalerror(200006161);
            end;
        end;

      { finalize temporary data }
      finalizetempvariables;

      { finalize local data like ansistrings}
      case aktprocdef.proctypeoption of
         potype_unitfinalize:
           begin
              { using current_module.globalsymtable is hopefully      }
              { more robust than symtablestack and symtablestack.next }
              tsymtable(current_module.globalsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data);
              tsymtable(current_module.localsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data);
           end;
         { units have seperate code for initialization and finalization }
         potype_unitinit: ;
         else
           aktprocdef.localst.foreach_static({$ifndef TP}@{$endif}finalize_data);
      end;

      { finalize paras data }
      if assigned(aktprocdef.parast) then
        aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}final_paras);

      { do we need to handle exceptions because of ansi/widestrings ? }
      if not inlined and
         ((procinfo^.flags and pi_needs_implicit_finally)<>0) and
      { but it's useless in init/final code of units }
        not(aktprocdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
        begin
           { the exception helper routines modify all registers }
           aktprocdef.usedregisters:=$ff;

           getlabel(noreraiselabel);
           emitcall('FPC_POPADDRSTACK');
           exprasmList.concat(Tairegalloc.Alloc(R_EAX));
           exprasmList.concat(Taicpu.op_reg(A_POP,S_L,R_EAX));
           exprasmList.concat(Taicpu.op_reg_reg(A_TEST,S_L,R_EAX,R_EAX));
           ungetregister32(R_EAX);
           emitjmp(C_E,noreraiselabel);
           if (aktprocdef.proctypeoption=potype_constructor) then
             begin
                if assigned(procinfo^._class) then
                  begin
                     pd:=procinfo^._class.searchdestructor;
                     if assigned(pd) then
                       begin
                          getlabel(nodestroycall);
                          emit_const_ref(A_CMP,S_L,0,new_reference(procinfo^.framepointer,
                            procinfo^.selfpointer_offset));
                          emitjmp(C_E,nodestroycall);
                          if is_class(procinfo^._class) then
                            begin
                               emit_const(A_PUSH,S_L,1);
                               emit_reg(A_PUSH,S_L,R_ESI);
                            end
                          else if is_object(procinfo^._class) then
                            begin
                               emit_reg(A_PUSH,S_L,R_ESI);
                               emit_sym(A_PUSH,S_L,newasmsymbol(procinfo^._class.vmt_mangledname));
                            end
                          else
                            begin
                              Internalerror(200006161);
                            end;
                          if (po_virtualmethod in pd.procoptions) then
                            begin
                               emit_ref_reg(A_MOV,S_L,new_reference(R_ESI,0),R_EDI);
                               emit_ref(A_CALL,S_NO,new_reference(R_EDI,procinfo^._class.vmtmethodoffset(pd.extnumber)));
                            end
                          else
                            emitcall(pd.mangledname);
                          { not necessary because the result is never assigned in the
                            case of an exception (FK)
                          emit_const_reg(A_MOV,S_L,0,R_ESI);
                          emit_const_ref(A_MOV,S_L,0,new_reference(procinfo^.framepointer,8));
                          }
                          emitlab(nodestroycall);
                       end;
                  end
             end
           else
           { must be the return value finalized before reraising the exception? }
           if (not is_void(aktprocdef.rettype.def)) and
             (aktprocdef.rettype.def.needs_inittable) and
             ((aktprocdef.rettype.def.deftype<>objectdef) or
              not is_class(aktprocdef.rettype.def)) then
             begin
                reset_reference(hr);
                hr.offset:=procinfo^.return_offset;
                hr.base:=procinfo^.framepointer;
                finalize(aktprocdef.rettype.def,hr,ret_in_param(aktprocdef.rettype.def));
             end;

           emitcall('FPC_RERAISE');
           emitlab(noreraiselabel);
        end;

      { call __EXIT for main program }
      if (not DLLsource) and (not inlined) and (aktprocdef.proctypeoption=potype_proginit) then
       begin
         emitcall('FPC_DO_EXIT');
       end;

      { handle return value }
      uses_eax:=false;
      uses_edx:=false;
      uses_esi:=false;
      if not(po_assembler in aktprocdef.procoptions) then
          if (aktprocdef.proctypeoption<>potype_constructor) then
            handle_return_value(inlined,uses_eax,uses_edx)
          else
              begin
                  { successful constructor deletes the zero flag }
                  { and returns self in eax                   }
                  { eax must be set to zero if the allocation failed !!! }
                  getlabel(okexitlabel);
                  emitjmp(C_NONE,okexitlabel);
                  emitlab(faillabel);
                  if is_class(procinfo^._class) then
                    begin
                      emit_ref_reg(A_MOV,S_L,new_reference(procinfo^.framepointer,8),R_ESI);
                      emitcall('FPC_HELP_FAIL_CLASS');
                    end
                  else if is_object(procinfo^._class) then
                    begin
                      emit_ref_reg(A_MOV,S_L,new_reference(procinfo^.framepointer,12),R_ESI);
                       getexplicitregister32(R_EDI);
                      emit_const_reg(A_MOV,S_L,procinfo^._class.vmt_offset,R_EDI);
                      emitcall('FPC_HELP_FAIL');
                      ungetregister32(R_EDI);
                    end
                  else
                    Internalerror(200006161);

                  emitlab(okexitlabel);

                  { for classes this is done after the call to }
                  { AfterConstruction                          }
                  if is_object(procinfo^._class) then
                    begin
                       exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                       emit_reg_reg(A_MOV,S_L,R_ESI,R_EAX);
                       uses_eax:=true;
                    end;
                  emit_reg_reg(A_TEST,S_L,R_ESI,R_ESI);
                  uses_esi:=true;
              end;

      if aktexit2label.is_used and not aktexit2label.is_set then
        emitlab(aktexit2label);

      if ((cs_debuginfo in aktmoduleswitches) and not inlined) then
        begin
          getlabel(stabsendlabel);
          emitlab(stabsendlabel);
        end;
      { gives problems for long mangled names }
      {List.concat(Tai_symbol.Create(aktprocdef.mangledname+'_end'));}

      { should we restore edi ? }
      { for all i386 gcc implementations }
      if (po_savestdregs in aktprocdef.procoptions) then
        begin
          if (aktprocdef.usedregisters and ($80 shr byte(R_EBX)))<>0 then
           exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,R_EBX));
          exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,R_ESI));
          exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,R_EDI));
          { here we could reset R_EBX
            but that is risky because it only works
            if genexitcode is called after genentrycode
            so lets skip this for the moment PM
          aktprocdef.usedregisters:=
            aktprocdef.usedregisters or not ($80 shr byte(R_EBX));
          }
        end;

      { for the save all registers we can simply use a pusha,popa which
        push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
      if (po_saveregisters in aktprocdef.procoptions) then
        begin
          if uses_esi then
            exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_ESI,new_reference(R_ESP,4)));
          if uses_edx then
            exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDX,new_reference(R_ESP,20)));
          if uses_eax then
            exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EAX,new_reference(R_ESP,28)));
          exprasmList.concat(Taicpu.Op_none(A_POPA,S_L));
          { We add a NOP because of the 386DX CPU bugs with POPAD }
          exprasmlist.concat(taicpu.op_none(A_NOP,S_L));
        end;
      if not(nostackframe) then
        begin
          if not inlined then
            exprasmList.concat(Taicpu.Op_none(A_LEAVE,S_NO));
        end
      else
        begin
          if (gettempsize<>0) and not inlined then
            exprasmList.insert(Taicpu.op_const_reg(A_ADD,S_L,gettempsize,R_ESP));
        end;

      { parameters are limited to 65535 bytes because }
      { ret allows only imm16                    }
      if (parasize>65535) and not(po_clearstack in aktprocdef.procoptions) then
       CGMessage(cg_e_parasize_too_big);

      { at last, the return is generated }

      if not inlined then
      if (po_interrupt in aktprocdef.procoptions) then
          begin
             if uses_esi then
               exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_ESI,new_reference(R_ESP,16)));
             if uses_edx then
               begin
                 exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDX,new_reference(R_ESP,12)));
               end;
             if uses_eax then
               begin
                 exprasmList.concat(Tairegalloc.Alloc(R_EAX));
                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EAX,new_reference(R_ESP,0)));
               end;
             generate_interrupt_stackframe_exit;
          end
      else
       begin
       {Routines with the poclearstack flag set use only a ret.}
       { also routines with parasize=0     }
         if (po_clearstack in aktprocdef.procoptions) then
           begin
{$ifndef OLD_C_STACK}
             { complex return values are removed from stack in C code PM }
             if ret_in_param(aktprocdef.rettype.def) then
               exprasmList.concat(Taicpu.Op_const(A_RET,S_NO,4))
             else
{$endif not OLD_C_STACK}
               exprasmList.concat(Taicpu.Op_none(A_RET,S_NO));
           end
         else if (parasize=0) then
          exprasmList.concat(Taicpu.Op_none(A_RET,S_NO))
         else
          exprasmList.concat(Taicpu.Op_const(A_RET,S_NO,parasize));
       end;

      if not inlined then
        exprasmList.concat(Tai_symbol_end.Createname(aktprocdef.mangledname));

{$ifdef GDB}
      if (cs_debuginfo in aktmoduleswitches) and not inlined  then
          begin
              aktprocdef.concatstabto(exprasmlist);
              if assigned(procinfo^._class) then
                if (not assigned(procinfo^.parent) or
                   not assigned(procinfo^.parent^._class)) then
                  begin
                    if (po_classmethod in aktprocdef.procoptions) or
                       ((po_virtualmethod in aktprocdef.procoptions) and
                        (potype_constructor=aktprocdef.proctypeoption)) or
                       (po_staticmethod in aktprocdef.procoptions) then
                      begin
                        exprasmList.concat(Tai_stabs.Create(strpnew(
                         '"pvmt:p'+tstoreddef(pvmttype.def).numberstring+'",'+
                         tostr(N_tsym)+',0,0,'+tostr(procinfo^.selfpointer_offset))));
                      end
                    else
                      begin
                        if not(is_class(procinfo^._class)) then
                          st:='v'
                        else
                          st:='p';
                        exprasmList.concat(Tai_stabs.Create(strpnew(
                         '"$t:'+st+procinfo^._class.numberstring+'",'+
                         tostr(N_tsym)+',0,0,'+tostr(procinfo^.selfpointer_offset))));
                      end;
                  end
                else
                  begin
                    if not is_class(procinfo^._class) then
                      st:='*'
                    else
                      st:='';
                    exprasmList.concat(Tai_stabs.Create(strpnew(
                     '"$t:r'+st+procinfo^._class.numberstring+'",'+
                     tostr(N_RSYM)+',0,0,'+tostr(GDB_i386index[R_ESI]))));
                  end;

              { define calling EBP as pseudo local var PM }
              { this enables test if the function is a local one !! }
              if  assigned(procinfo^.parent) and (lexlevel>normal_function_level) then
                exprasmList.concat(Tai_stabs.Create(strpnew(
                 '"parent_ebp:'+tstoreddef(voidpointertype.def).numberstring+'",'+
                 tostr(N_LSYM)+',0,0,'+tostr(procinfo^.framepointer_offset))));

              if (not is_void(aktprocdef.rettype.def)) then
                begin
                  if ret_in_param(aktprocdef.rettype.def) then
                    exprasmList.concat(Tai_stabs.Create(strpnew(
                     '"'+aktprocsym.name+':X*'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                     tostr(N_tsym)+',0,0,'+tostr(procinfo^.return_offset))))
                  else
                    exprasmList.concat(Tai_stabs.Create(strpnew(
                     '"'+aktprocsym.name+':X'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                     tostr(N_tsym)+',0,0,'+tostr(procinfo^.return_offset))));
                  if (m_result in aktmodeswitches) then
                    if ret_in_param(aktprocdef.rettype.def) then
                      exprasmList.concat(Tai_stabs.Create(strpnew(
                       '"RESULT:X*'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                       tostr(N_tsym)+',0,0,'+tostr(procinfo^.return_offset))))
                    else
                      exprasmList.concat(Tai_stabs.Create(strpnew(
                       '"RESULT:X'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                       tostr(N_tsym)+',0,0,'+tostr(procinfo^.return_offset))));
                end;
              mangled_length:=length(aktprocdef.mangledname);
              getmem(p,2*mangled_length+50);
              strpcopy(p,'192,0,0,');
              strpcopy(strend(p),aktprocdef.mangledname);
              if (target_info.use_function_relative_addresses) then
                begin
                  strpcopy(strend(p),'-');
                  strpcopy(strend(p),aktprocdef.mangledname);
                end;
              exprasmList.concat(Tai_stabn.Create(strnew(p)));
              {List.concat(Tai_stabn.Create(strpnew('192,0,0,'
               +aktprocdef.mangledname))));
              p[0]:='2';p[1]:='2';p[2]:='4';
              strpcopy(strend(p),'_end');}
              strpcopy(p,'224,0,0,'+stabsendlabel.name);
              if (target_info.use_function_relative_addresses) then
                begin
                  strpcopy(strend(p),'-');
                  strpcopy(strend(p),aktprocdef.mangledname);
                end;
              exprasmList.concatlist(withdebuglist);
              exprasmList.concat(Tai_stabn.Create(strnew(p)));
               { strpnew('224,0,0,'
               +aktprocdef.mangledname+'_end'))));}
              freemem(p,2*mangled_length+50);
          end;
{$endif GDB}
       if inlined then
         cleanup_regvars(exprasmlist);
      exprasmlist:=oldexprasmlist;
  end;

    procedure genimplicitunitfinal(alist : TAAsmoutput);

      begin
         { using current_module.globalsymtable is hopefully      }
         { more robust than symtablestack and symtablestack.next }
         tsymtable(current_module.globalsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data);
         tsymtable(current_module.localsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data);
         exprasmList.insert(Tai_symbol.Createname_global('FINALIZE$$'+current_module.modulename^,0));
         exprasmList.insert(Tai_symbol.Createname_global(target_info.cprefix+current_module.modulename^+'_finalize',0));
{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
           target_info.use_function_relative_addresses then
           exprasmList.insert(Tai_stab_function_name.Create(strpnew('FINALIZE$$'+current_module.modulename^)));
{$endif GDB}
         exprasmList.concat(Taicpu.Op_none(A_RET,S_NO));
         aList.concatlist(exprasmlist);
      end;

    procedure genimplicitunitinit(alist : TAAsmoutput);

      begin
         { using current_module.globalsymtable is hopefully      }
         { more robust than symtablestack and symtablestack.next }
         tsymtable(current_module.globalsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data);
         tsymtable(current_module.localsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data);
         exprasmList.insert(Tai_symbol.Createname_global('INIT$$'+current_module.modulename^,0));
         exprasmList.insert(Tai_symbol.Createname_global(target_info.cprefix+current_module.modulename^+'_init',0));
{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
           target_info.use_function_relative_addresses then
           exprasmList.insert(Tai_stab_function_name.Create(strpnew('INIT$$'+current_module.modulename^)));
{$endif GDB}
         exprasmList.concat(Taicpu.Op_none(A_RET,S_NO));
         aList.concatlist(exprasmlist);
      end;

{$ifdef test_dest_loc}
       procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

         begin
            if (dest_loc.loc=LOC_CREGISTER) or (dest_loc.loc=LOC_REGISTER) then
              begin
                emit_reg_reg(A_MOV,s,reg,dest_loc.register);
                set_location(p^.location,dest_loc);
                in_dest_loc:=true;
              end
            else
            if (dest_loc.loc=LOC_REFERENCE) or (dest_loc.loc=LOC_MEM) then
              begin
                exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,s,reg,newreference(dest_loc.reference)));
                set_location(p^.location,dest_loc);
                in_dest_loc:=true;
              end
            else
              internalerror(20080);
         end;

{$endif test_dest_loc}

end.
{
  $Log$
  Revision 1.14  2002-01-19 14:21:17  peter
    * fixed init/final for value parameters

  Revision 1.13  2001/12/30 17:24:45  jonas
    * range checking is now processor independent (part in cgobj, part in    cg64f32) and should work correctly again (it needed some changes after    the changes of the low and high of tordef's to int64)  * maketojumpbool() is now processor independent (in ncgutil)  * getregister32 is now called getregisterint

  Revision 1.12  2001/12/29 15:28:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.11  2001/11/18 18:59:59  peter
    * changed aktprocsym to aktprocdef for stabs generation

  Revision 1.10  2001/11/06 16:39:02  jonas
    * moved call to "cleanup_regvars" to cga.pas for i386 because it has
      to insert "fstp %st0" instructions after the exit label

  Revision 1.9  2001/11/02 22:58:09  peter
    * procsym definition rewrite

  Revision 1.8  2001/10/25 21:22:41  peter
    * calling convention rewrite

  Revision 1.7  2001/10/20 17:22:57  peter
    * concatcopy could release a wrong reference because the offset was
      increased without restoring the original before the release of
      a temp

  Revision 1.6  2001/10/14 11:49:51  jonas
    * finetuned register allocation info for assignments

  Revision 1.5  2001/09/30 21:28:34  peter
    * int64->boolean fixed

  Revision 1.4  2001/08/30 20:13:57  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.3  2001/08/29 12:01:47  jonas
    + support for int64 LOC_REGISTERS in remove_non_regvars_from_loc

  Revision 1.2  2001/08/26 13:36:52  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.29  2001/08/12 20:23:02  peter
    * netbsd doesn't use stackchecking

  Revision 1.28  2001/08/07 18:47:13  peter
    * merged netbsd start
    * profile for win32

  Revision 1.27  2001/08/06 21:40:49  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.26  2001/07/30 20:59:28  peter
    * m68k updates from v10 merged

  Revision 1.25  2001/07/01 20:16:18  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.24  2001/05/27 14:30:55  florian
    + some widestring stuff added

  Revision 1.23  2001/04/21 13:33:16  peter
    * move winstackpagesize const to cgai386 to remove uses t_win32

  Revision 1.22  2001/04/21 12:05:32  peter
    * add nop after popa (merged)

  Revision 1.21  2001/04/18 22:02:00  peter
    * registration of targets and assemblers

  Revision 1.20  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.19  2001/04/05 21:33:07  peter
    * fast exit fix merged

  Revision 1.18  2001/04/02 21:20:35  peter
    * resulttype rewrite

  Revision 1.17  2001/01/05 17:36:58  florian
  * the info about exception frames is stored now on the stack
  instead on the heap

  Revision 1.16  2000/12/25 00:07:31  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.15  2000/12/05 11:44:32  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.14  2000/11/29 00:30:43  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.13  2000/11/28 00:28:07  pierre
   * stabs fixing

  Revision 1.12  2000/11/22 15:12:06  jonas
    * fixed inline-related problems (partially "merges")

  Revision 1.11  2000/11/17 10:30:24  florian
    * passing interfaces as parameters fixed

  Revision 1.10  2000/11/07 23:40:48  florian
    + AfterConstruction and BeforeDestruction impemented

  Revision 1.9  2000/11/06 23:49:20  florian
    * fixed init_paras call

  Revision 1.8  2000/11/06 23:15:01  peter
    * added copyvaluepara call again

  Revision 1.7  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.6  2000/10/31 22:02:55  peter
    * symtable splitted, no real code changes

  Revision 1.5  2000/10/24 22:23:04  peter
    * emitcall -> emitinsertcall for profiling (merged)

  Revision 1.4  2000/10/24 12:47:45  jonas
    * allocate registers which hold function result

  Revision 1.3  2000/10/24 08:54:25  michael
  + Extra patch from peter

  Revision 1.2  2000/10/24 07:20:03  pierre
   * fix for bug 1193 (merged)

  Revision 1.1  2000/10/15 09:47:42  peter
    * moved to i386/

  Revision 1.19  2000/10/14 10:14:46  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.18  2000/10/10 14:55:28  jonas
    * added missing regallocs for edi in emit_mov_ref_reg64 (merged)

  Revision 1.17  2000/10/01 19:48:23  peter
    * lot of compile updates for cg11

  Revision 1.16  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.15  2000/09/24 15:06:12  peter
    * use defines.inc

  Revision 1.14  2000/09/16 12:22:52  peter
    * freebsd support merged

  Revision 1.13  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.12  2000/08/24 19:07:54  peter
    * don't initialize if localvarsym is set because that varsym will
      already be initialized
    * first initialize local data before copy of value para's (merged)

  Revision 1.11  2000/08/19 20:09:33  peter
    * check size after checking openarray in push_value_para (merged)

  Revision 1.10  2000/08/16 13:06:06  florian
    + support of 64 bit integer constants

  Revision 1.9  2000/08/10 18:42:03  peter
    * fixed for constants in emit_push_mem_size for go32v2 (merged)

  Revision 1.8  2000/08/07 11:29:40  jonas
    + emit_push_mem_size() which pushes a value in memory of a certain size
    * pushsetelement() and pushvaluepara() use this new procedure, because
      otherwise they could sometimes try to push data past the end of the
      heap, causing a crash
     (merged from fixes branch)

  Revision 1.7  2000/08/03 13:17:25  jonas
    + allow regvars to be used inside inlined procs, which required  the
      following changes:
        + load regvars in genentrycode/free them in genexitcode (cgai386)
        * moved all regvar related code to new regvars unit
        + added pregvarinfo type to hcodegen
        + added regvarinfo field to tprocinfo (symdef/symdefh)
        * deallocate the regvars of the caller in secondprocinline before
          inlining the called procedure and reallocate them afterwards

  Revision 1.6  2000/08/02 08:05:04  jonas
    * fixed web bug1087
    * allocate R_ECX explicitely if it's used
    (merged from fixes branch)

  Revision 1.5  2000/07/27 09:25:05  jonas
    * moved locflags2reg() procedure from cg386add to cgai386
    + added locjump2reg() procedure to cgai386
    * fixed internalerror(2002) when the result of a case expression has
      LOC_JUMP
    (all merged from fixes branch)

  Revision 1.4  2000/07/21 15:14:02  jonas
    + added is_addr field for labels, if they are only used for getting the address
       (e.g. for io checks) and corresponding getaddrlabel() procedure

  Revision 1.3  2000/07/13 12:08:25  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:37  michael
  + removed logs

}
