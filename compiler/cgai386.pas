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

 ****************************************************************************}

unit cgai386;

  interface

    uses
       cobjects,tree,
       cpubase,cpuasm,
       symconst,symtable,aasm;

{$define TESTGETTEMP to store const that
 are written into temps for later release PM }

    function def_opsize(p1:pdef):topsize;
    function def2def_opsize(p1,p2:pdef):topsize;
    function def_getreg(p1:pdef):tregister;
    function makereg8(r:tregister):tregister;
    function makereg16(r:tregister):tregister;
    function makereg32(r:tregister):tregister;

    procedure emitlab(var l : pasmlabel);
    procedure emitjmp(c : tasmcond;var l : pasmlabel);
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


    procedure emit_sym(i : tasmop;s : topsize;op : pasmsymbol);
    procedure emit_sym_ofs(i : tasmop;s : topsize;op : pasmsymbol;ofs : longint);
    procedure emit_sym_ofs_reg(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;reg : tregister);
    procedure emit_sym_ofs_ref(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;ref : preference);

    procedure emitcall(const routine:string);

    procedure emit_mov_loc_ref(const t:tlocation;const ref:treference;siz:topsize);
    procedure emit_mov_loc_reg(const t:tlocation;reg:tregister);
    procedure emit_mov_ref_reg64(r : treference;rl,rh : tregister);
    procedure emit_lea_loc_ref(const t:tlocation;const ref:treference;freetemp:boolean);
    procedure emit_lea_loc_reg(const t:tlocation;reg:tregister;freetemp:boolean);
    procedure emit_push_loc(const t:tlocation);

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
    procedure emit_to_mem(var p:ptree);
    procedure emit_to_reg16(var hr:tregister);
    procedure emit_to_reg32(var hr:tregister);
    procedure emit_mov_reg_loc(reg: TRegister; const t:tlocation);
    procedure emit_movq_reg_loc(reghigh,reglow: TRegister;t:tlocation);

    procedure copyshortstring(const dref,sref : treference;len : byte;loadref:boolean);
    procedure loadansistring(p : ptree);

    procedure finalize(t : pdef;const ref : treference;is_already_ref : boolean);
    procedure decrstringref(t : pdef;const ref : treference);

    function maybe_push(needed : byte;p : ptree;isint64 : boolean) : boolean;
    procedure push_int(l : longint);
    procedure emit_push_mem(const ref : treference);
    procedure emitpushreferenceaddr(const ref : treference);
    procedure pushsetelement(p : ptree);
    procedure restore(p : ptree;isint64 : boolean);
    procedure push_value_para(p:ptree;inlined:boolean;para_offset:longint;alignment : longint);

{$ifdef TEMPS_NOT_PUSH}
    { does the same as restore/, but uses temp. space instead of pushing }
    function maybe_push(needed : byte;p : ptree;isint64 : boolean) : boolean;
    procedure restorefromtemp(p : ptree;isint64 : boolean);
{$endif TEMPS_NOT_PUSH}

    procedure floatload(t : tfloattype;const ref : treference);
    procedure floatstore(t : tfloattype;const ref : treference);
    procedure floatloadops(t : tfloattype;var op : tasmop;var s : topsize);
    procedure floatstoreops(t : tfloattype;var op : tasmop;var s : topsize);

    procedure maybe_loadesi;
    procedure maketojumpbool(p : ptree);
    procedure emitloadord2reg(const location:Tlocation;orddef:Porddef;destreg:Tregister;delloc:boolean);
    procedure emitoverflowcheck(p:ptree);
    procedure emitrangecheck(p:ptree;todef:pdef);
    procedure concatcopy(source,dest : treference;size : longint;delsource : boolean;loadref:boolean);
    procedure firstcomplex(p : ptree);

    procedure genentrycode(alist : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean);
    procedure genexitcode(alist : paasmoutput;parasize:longint;
                          nostackframe,inlined:boolean);

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
       strings,globtype,systems,globals,verbose,files,types,pbase,
       tgeni386,temp_gen,hcodegen,ppu
{$ifdef GDB}
       ,gdb
{$endif}
{$ifndef NOTARGETWIN32}
       ,t_win32
{$endif}
       ;


{*****************************************************************************
                                Helpers
*****************************************************************************}

    function def_opsize(p1:pdef):topsize;
      begin
        case p1^.size of
         1 : def_opsize:=S_B;
         2 : def_opsize:=S_W;
         4 : def_opsize:=S_L;
        else
         internalerror(78);
        end;
      end;


    function def2def_opsize(p1,p2:pdef):topsize;
      var
        o1 : topsize;
      begin
        case p1^.size of
         1 : o1:=S_B;
         2 : o1:=S_W;
         4 : o1:=S_L;
         { I don't know if we need it (FK) }
         8 : o1:=S_L;
        else
         internalerror(78);
        end;
        if assigned(p2) then
         begin
           case p2^.size of
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


    function def_getreg(p1:pdef):tregister;
      begin
        case p1^.size of
         1 : def_getreg:=reg32toreg8(getregister32);
         2 : def_getreg:=reg32toreg16(getregister32);
         4 : def_getreg:=getregister32;
        else
         internalerror(78);
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


{*****************************************************************************
                              Emit Assembler
*****************************************************************************}

    procedure emitlab(var l : pasmlabel);
      begin
         if not l^.is_set then
          exprasmlist^.concat(new(pai_label,init(l)))
         else
          internalerror(7453984);
      end;

{$ifdef nojmpfix}
    procedure emitjmp(c : tasmcond;var l : pasmlabel);
      var
        ai : Paicpu;
      begin
        if c=C_None then
          exprasmlist^.concat(new(paicpu,op_sym(A_JMP,S_NO,l)))
        else
          begin
            ai:=new(paicpu,op_sym(A_Jcc,S_NO,l));
            ai^.SetCondition(c);
            ai^.is_jmp:=true;
            exprasmlist^.concat(ai);
          end;
      end;
{$else nojmpfix}
    procedure emitjmp(c : tasmcond;var l : pasmlabel);
      var
        ai : Paicpu;
      begin
        if c=C_None then
          ai := new(paicpu,op_sym(A_JMP,S_NO,l))
        else
          begin
            ai:=new(paicpu,op_sym(A_Jcc,S_NO,l));
            ai^.SetCondition(c);
          end;
        ai^.is_jmp:=true;
        exprasmlist^.concat(ai);
      end;
{$endif nojmpfix}

    procedure emit_flag2reg(flag:tresflags;hregister:tregister);
      var
        ai : paicpu;
        hreg : tregister;
      begin
         hreg:=makereg8(hregister);
         ai:=new(paicpu,op_reg(A_Setcc,S_B,hreg));
         ai^.SetCondition(flag_2_cond[flag]);
         exprasmlist^.concat(ai);
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
         exprasmlist^.concat(new(paicpu,op_none(i,s)));
      end;

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_reg(i,s,reg)));
      end;

    procedure emit_ref(i : tasmop;s : topsize;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_ref(i,s,ref)));
      end;

    procedure emit_const(i : tasmop;s : topsize;c : longint);
      begin
         exprasmlist^.concat(new(paicpu,op_const(i,s,c)));
      end;

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_const_reg(i,s,c,reg)));
      end;

    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_const_ref(i,s,c,ref)));
      end;

    procedure emit_ref_reg(i : tasmop;s : topsize;ref : preference;reg : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_ref_reg(i,s,ref,reg)));
      end;

    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;ref : preference);
      begin
         exprasmlist^.concat(new(paicpu,op_reg_ref(i,s,reg,ref)));
      end;

    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
      begin
         if (reg1<>reg2) or (i<>A_MOV) then
           exprasmlist^.concat(new(paicpu,op_reg_reg(i,s,reg1,reg2)));
      end;

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_const_reg_reg(i,s,c,reg1,reg2)));
      end;

    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
      begin
         exprasmlist^.concat(new(paicpu,op_reg_reg_reg(i,s,reg1,reg2,reg3)));
      end;

    procedure emit_sym(i : tasmop;s : topsize;op : pasmsymbol);
      begin
        exprasmlist^.concat(new(paicpu,op_sym(i,s,op)));
      end;

    procedure emit_sym_ofs(i : tasmop;s : topsize;op : pasmsymbol;ofs : longint);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs(i,s,op,ofs)));
      end;

    procedure emit_sym_ofs_reg(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;reg : tregister);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(i,s,op,ofs,reg)));
      end;

    procedure emit_sym_ofs_ref(i : tasmop;s : topsize;op : pasmsymbol;ofs:longint;ref : preference);
      begin
        exprasmlist^.concat(new(paicpu,op_sym_ofs_ref(i,s,op,ofs,ref)));
      end;

    procedure emitcall(const routine:string);
      begin
        exprasmlist^.concat(new(paicpu,op_sym(A_CALL,S_NO,newasmsymbol(routine))));
      end;

    { only usefull in startup code }
    procedure emitinsertcall(const routine:string);
      begin
        exprasmlist^.insert(new(paicpu,op_sym(A_CALL,S_NO,newasmsymbol(routine))));
      end;


    procedure emit_mov_loc_ref(const t:tlocation;const ref:treference;siz:topsize);
      var
        hreg : tregister;
        pushedeax : boolean;

      begin
        pushedeax:=false;
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,siz,
                             t.register,newreference(ref))));
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
                                            hreg:=reg32toreg8(getregister32)
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
{$ifndef noAllocEdi}
                               if hreg in [R_DI,R_EDI] then
                                 getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                               emit_ref_reg(A_MOV,siz,
                                 newreference(t.reference),hreg);
                               del_reference(t.reference);
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,siz,
                                 hreg,newreference(ref))));
                               if siz=S_B then
                                 begin
                                    if pushedeax then
                                      emit_reg(A_POP,S_L,R_EAX)
                                    else
                                      ungetregister(hreg);
                                 end;
{$ifndef noAllocEdi}
                               if hreg in [R_DI,R_EDI] then
                                 ungetregister32(R_EDI);
{$endif noAllocEdi}
                               { we can release the registers }
                               { but only AFTER the MOV! Important for the optimizer!
                                 (JM)}
                               del_reference(ref);
                             end;
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
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,RegSize(Reg),
                                 Reg,newreference(t.reference))));
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
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                 Reglow,newreference(t.reference))));
                               inc(t.reference.offset,4);
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                 Reghigh,newreference(t.reference))));
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
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,
                   t.registerhigh)));
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,
                   t.registerlow)));
              end;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 hr:=newreference(t.reference);
                 inc(hr^.offset,4);
                 exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,
                   hr)));
                 exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,
                   newreference(t.reference))));
                 ungetiftemp(t.reference);
              end;
            else internalerror(331);
         end;
      end;

    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: byte);
    begin
      case t.loc of
        LOC_REGISTER:
          { can't be a regvar, since it would be LOC_CREGISTER then }
          regs := regs and not($80 shr byte(t.register));
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
                           exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,makereg32(t.register))));
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,t.reference.offset)))
                           else
                             exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,newreference(t.reference))));
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
                           if target_os.stackalignment=4 then
                             exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,makereg32(t.register))))
                           else
                             exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_W,makereg16(t.register))));
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if target_os.stackalignment=4 then
                            opsize:=S_L
                           else
                            opsize:=S_W;
                           if t.reference.is_immediate then
                             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,opsize,t.reference.offset)))
                           else
                             exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,opsize,newreference(t.reference))));
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
{$ifndef noAllocEdi}
                               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),R_EDI);
                               exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                 R_EDI,newreference(ref))));
{$ifndef noAllocEdi}
                               ungetregister32(R_EDI);
{$endif noAllocEdi}
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
{$ifndef noAllocEdi}
                               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                               emit_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),R_EDI);
                               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$ifndef noAllocEdi}
                               ungetregister32(R_EDI);
{$endif noAllocEdi}
                             end;
                           if freetemp then
                            ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;


    procedure emit_to_mem(var p:ptree);
      begin
        case p^.location.loc of
               LOC_FPU : begin
                           reset_reference(p^.location.reference);
                           gettempofsizereference(10,p^.location.reference);
                           floatstore(pfloatdef(p^.resulttype)^.typ,p^.location.reference);
                           {  This can't be never a l-value! (FK)
                              p^.location.loc:=LOC_REFERENCE; }
                         end;
               LOC_MEM,
         LOC_REFERENCE : ;
         LOC_CFPUREGISTER : begin
                           emit_reg(A_FLD,S_NO,correct_fpuregister(p^.location.register,fpuvaroffset));
                           inc(fpuvaroffset);
                           reset_reference(p^.location.reference);
                           gettempofsizereference(10,p^.location.reference);
                           floatstore(pfloatdef(p^.resulttype)^.typ,p^.location.reference);
                           {  This can't be never a l-value! (FK)
                              p^.location.loc:=LOC_REFERENCE; }
                         end;
         else
         internalerror(333);
        end;
        p^.location.loc:=LOC_MEM;
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
      end;

{*****************************************************************************
                           Emit String Functions
*****************************************************************************}

    procedure copyshortstring(const dref,sref : treference;len : byte;loadref:boolean);
      begin
         emitpushreferenceaddr(dref);
         if loadref then
          emit_push_mem(sref)
         else
          emitpushreferenceaddr(sref);
         push_int(len);
         emitcall('FPC_SHORTSTR_COPY');
         maybe_loadesi;
      end;

    procedure copylongstring(const dref,sref : treference;len : longint;loadref:boolean);
      begin
         emitpushreferenceaddr(dref);
         if loadref then
          emit_push_mem(sref)
         else
          emitpushreferenceaddr(sref);
         push_int(len);
         emitcall('FPC_LONGSTR_COPY');
         maybe_loadesi;
      end;


    procedure decrstringref(t : pdef;const ref : treference);

      var
         pushedregs : tpushed;

      begin
         pushusedregisters(pushedregs,$ff);
         emitpushreferenceaddr(ref);
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

    procedure loadansistring(p : ptree);
    {
      copies an ansistring from p^.right to p^.left, we
      assume, that both sides are ansistring, firstassignement have
      to take care of that, an ansistring can't be a register variable
    }
      var
         pushed : tpushed;
         ungettemp : boolean;
      begin
         { before pushing any parameter, we have to save all used      }
         { registers, but before that we have to release the       }
         { registers of that node to save uneccessary pushed       }
         { so be careful, if you think you can optimize that code (FK) }

         { nevertheless, this has to be changed, because otherwise the }
         { register is released before it's contents are pushed ->     }
         { problems with the optimizer (JM)                         }
         del_reference(p^.left^.location.reference);
         ungettemp:=false;
         case p^.right^.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
{$IfNDef regallocfix}
                 ungetregister32(p^.right^.location.register);
                 pushusedregisters(pushed,$ff);
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.right^.location.register)));
{$Else regallocfix}
                 pushusedregisters(pushed, $ff xor ($80 shr byte(p^.right^.location.register)));
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.right^.location.register)));
                 ungetregister32(p^.right^.location.register);
{$EndIf regallocfix}
              end;
            LOC_REFERENCE,LOC_MEM:
              begin
{$IfNDef regallocfix}
                 del_reference(p^.right^.location.reference);
                 pushusedregisters(pushed,$ff);
                 emit_push_mem(p^.right^.location.reference);
{$Else regallocfix}
                 pushusedregisters(pushed,$ff
                   xor ($80 shr byte(p^.right^.location.reference.base))
                   xor ($80 shr byte(p^.right^.location.reference.index)));
                 emit_push_mem(p^.right^.location.reference);
                 del_reference(p^.right^.location.reference);
{$EndIf regallocfix}
                 ungettemp:=true;
              end;
         end;
         emitpushreferenceaddr(p^.left^.location.reference);
         del_reference(p^.left^.location.reference);
         emitcall('FPC_ANSISTR_ASSIGN');
         maybe_loadesi;
         popusedregisters(pushed);
         if ungettemp then
           ungetiftemp(p^.right^.location.reference);
      end;


{*****************************************************************************
                           Emit Push Functions
*****************************************************************************}

    function maybe_push(needed : byte;p : ptree;isint64 : boolean) : boolean;
      var
         pushed : boolean;
         {hregister : tregister; }
{$ifdef TEMPS_NOT_PUSH}
         href : treference;
{$endif TEMPS_NOT_PUSH}
      begin
         if needed>usablereg32 then
           begin
              if (p^.location.loc=LOC_REGISTER) then
                begin
                   if isint64 then
                     begin
{$ifdef TEMPS_NOT_PUSH}
                        gettempofsizereference(href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmlist^.concat(new(paicpu,op_reg(A_MOV,S_L,p^.location.registerhigh,href)));
                        href.offset:=href.offset-4;
{$else TEMPS_NOT_PUSH}
                        exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.registerhigh)));
{$endif TEMPS_NOT_PUSH}
                        ungetregister32(p^.location.registerhigh);
                     end
{$ifdef TEMPS_NOT_PUSH}
                   else
                     begin
                        gettempofsizereference(href,4);
                        p^.temp_offset:=href.offset;
                     end
{$endif TEMPS_NOT_PUSH}
                     ;
                   pushed:=true;
{$ifdef TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,p^.location.register,href)));
{$else TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.register)));
{$endif TEMPS_NOT_PUSH}
                   ungetregister32(p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p^.location.reference);
{$ifndef noAllocEdi}
                    getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                     emit_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                       R_EDI);
{$ifdef TEMPS_NOT_PUSH}
                     gettempofsizereference(href,4);
                     exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,href)));
                     p^.temp_offset:=href.offset;
{$else TEMPS_NOT_PUSH}
                     exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$endif TEMPS_NOT_PUSH}
{$ifndef noAllocEdi}
                     ungetregister32(R_EDI);
{$endif noAllocEdi}
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;

{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : ptree;isint64 : boolean) : boolean;

      var
         pushed : boolean;
         href : treference;

      begin
         if needed>usablereg32 then
           begin
              if (p^.location.loc=LOC_REGISTER) then
                begin
                   if isint64(p^.resulttype) then
                     begin
                        gettempofsizereference(href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmlist^.concat(new(paicpu,op_reg(A_MOV,S_L,p^.location.registerhigh,href)));
                        href.offset:=href.offset-4;
                        ungetregister32(p^.location.registerhigh);
                     end
                   else
                     begin
                        gettempofsizereference(href,4);
                        p^.temp_offset:=href.offset;
                     end;
                   pushed:=true;
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,p^.location.register,href)));
                   ungetregister32(p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p^.location.reference);
{$ifndef noAllocEdi}
                     getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                     emit_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                       R_EDI);
                     gettempofsizereference(href,4);
                     exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,href)));
{$ifndef noAllocEdi}
                     ungetregister32(R_EDI);
{$endif noAllocEdi}
                     p^.temp_offset:=href.offset;
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;
{$endif TEMPS_NOT_PUSH}


    procedure push_int(l : longint);
      begin
         if (l = 0) and
            not(aktoptprocessor in [Class386, ClassP6]) and
            not(cs_littlesize in aktglobalswitches)
           Then
             begin
{$ifndef noAllocEdi}
               getexplicitregister32(R_EDI);
{$endif noAllocEdi}
               emit_reg_reg(A_XOR,S_L,R_EDI,R_EDI);
               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$ifndef noAllocEdi}
               ungetregister32(R_EDI);
{$endif noAllocEdi}
             end
           else
             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,l)));
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
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   emit_ref_reg(A_MOV,S_L,newreference(ref),R_EDI);
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                 end
               else exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,newreference(ref))));
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
                exprasmlist^.concat(new(paicpu,op_sym_ofs(A_PUSH,S_L,ref.symbol,ref.offset)))
              else if (ref.base=R_NO) and (ref.index<>R_NO) and
                 (ref.offset=0) and (ref.scalefactor=0) and (ref.symbol=nil) then
                exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,ref.index)))
              else if (ref.base<>R_NO) and (ref.index=R_NO) and
                 (ref.offset=0) and (ref.symbol=nil) then
                exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,ref.base)))
              else
                begin
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   emit_ref_reg(A_LEA,S_L,newreference(ref),R_EDI);
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                end;
           end;
        end;


     procedure pushsetelement(p : ptree);
     {
       copies p a set element on the stack
     }
      var
         hr,hr16,hr32 : tregister;
      begin
      { copy the element on the stack, slightly complicated }
        if p^.treetype=ordconstn then
         begin
           if target_os.stackalignment=4 then
             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,p^.value)))
           else
             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_W,p^.value)));
         end
        else
         begin
           case p^.location.loc of
             LOC_REGISTER,
             LOC_CREGISTER :
               begin
                 hr:=p^.location.register;
                 case hr of
                   R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
                     begin
                       hr16:=reg32toreg16(hr);
                       hr32:=hr;
                     end;
                   R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
                     begin
                       hr16:=hr;
                       hr32:=reg16toreg32(hr);
                     end;
                   R_AL,R_BL,R_CL,R_DL :
                     begin
                       hr16:=reg8toreg16(hr);
                       hr32:=reg8toreg32(hr);
                     end;
                 end;
                 if target_os.stackalignment=4 then
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,hr32)))
                 else
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_W,hr16)));
                 ungetregister32(hr32);
               end;
           else
             begin
               if target_os.stackalignment=4 then
                 exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,newreference(p^.location.reference))))
               else
                 exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_W,newreference(p^.location.reference))));
               del_reference(p^.location.reference);
             end;
           end;
         end;
      end;


    procedure restore(p : ptree;isint64 : boolean);
      var
         hregister :  tregister;
{$ifdef TEMPS_NOT_PUSH}
         href : treference;
{$endif TEMPS_NOT_PUSH}
      begin
         hregister:=getregister32;
{$ifdef TEMPS_NOT_PUSH}
         reset_reference(href);
         href.base:=procinfo^.frame_pointer;
         href.offset:=p^.temp_offset;
         emit_ref_reg(A_MOV,S_L,href,hregister);
{$else  TEMPS_NOT_PUSH}
         exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,hregister)));
{$endif TEMPS_NOT_PUSH}
         if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p^.location.register:=hregister;
              if isint64 then
                begin
                   p^.location.registerhigh:=getregister32;
{$ifdef TEMPS_NOT_PUSH}
                   href.offset:=p^.temp_offset+4;
                   emit_ref_reg(A_MOV,S_L,p^.location.registerhigh);
                   { set correctly for release ! }
                   href.offset:=p^.temp_offset;
{$else  TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,p^.location.registerhigh)));
{$endif TEMPS_NOT_PUSH}
                end;
           end
         else
           begin
              reset_reference(p^.location.reference);
              { any reasons why this was moved into the index register ? }
              { normally usage of base register is much better (FK)      }
              p^.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p^.left
                because otherwise secondload fails !!!
              set_location(p^.left^.location,p^.location);}
           end;
{$ifdef TEMPS_NOT_PUSH}
         ungetiftemp(href);
{$endif TEMPS_NOT_PUSH}
      end;

{$ifdef TEMPS_NOT_PUSH}
    procedure restorefromtemp(p : ptree;isint64 : boolean);
      var
         hregister :  tregister;
         href : treference;

      begin
         hregister:=getregister32;
         reset_reference(href);
         href.base:=procinfo^.frame_pointer;
         href.offset:=p^.temp_offset;
         emit_ref_reg(A_MOV,S_L,href,hregister);
         if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p^.location.register:=hregister;
              if isint64 then
                begin
                   p^.location.registerhigh:=getregister32;
                   href.offset:=p^.temp_offset+4;
                   emit_ref_reg(A_MOV,S_L,p^.location.registerhigh);
                   { set correctly for release ! }
                   href.offset:=p^.temp_offset;
                end;
           end
         else
           begin
              reset_reference(p^.location.reference);
              p^.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p^.left
                because otherwise secondload fails PM
              set_location(p^.left^.location,p^.location);}
           end;
         ungetiftemp(href);
      end;
{$endif TEMPS_NOT_PUSH}

      procedure push_value_para(p:ptree;inlined:boolean;para_offset:longint;alignment : longint);
        var
          tempreference : treference;
          r : preference;
          opsize : topsize;
          op : tasmop;
          hreg : tregister;
          size : longint;
          hlabel : pasmlabel;
        begin
          case p^.location.loc of
             LOC_REGISTER,
             LOC_CREGISTER:
               begin
                  case p^.location.register of
                     R_EAX,R_EBX,R_ECX,R_EDX,R_ESI,
                     R_EDI,R_ESP,R_EBP :
                        begin
                          if p^.resulttype^.size=8 then
                            begin
                               inc(pushedparasize,8);
                               if inlined then
                                 begin
                                    r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                    exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                      p^.location.registerlow,r)));
                                    r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                                    exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                      p^.location.registerhigh,r)));
                                 end
                               else
                                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.registerhigh)));
                               ungetregister32(p^.location.registerhigh);
                                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.registerlow)));
                               ungetregister32(p^.location.registerlow);
                            end
                          else
                            begin
                               inc(pushedparasize,4);
                               if inlined then
                                 begin
                                    r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                    exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                      p^.location.register,r)));
                                 end
                               else
                                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p^.location.register)));
                               ungetregister32(p^.location.register);
                            end;
                        end;
                     R_AX,R_BX,R_CX,R_DX,R_SI,R_DI:
                        begin
                          if alignment=4 then
                            begin
                              opsize:=S_L;
                              hreg:=reg16toreg32(p^.location.register);
                              inc(pushedparasize,4);
                            end
                          else
                            begin
                              opsize:=S_W;
                              hreg:=p^.location.register;
                              inc(pushedparasize,2);
                            end;
                          if inlined then
                            begin
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                            end
                          else
                            exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
                          ungetregister32(reg16toreg32(p^.location.register));
                        end;
                     R_AL,R_BL,R_CL,R_DL:
                        begin
                          if alignment=4 then
                            begin
                              opsize:=S_L;
                              hreg:=reg8toreg32(p^.location.register);
                              inc(pushedparasize,4);
                            end
                          else
                            begin
                              opsize:=S_W;
                              hreg:=reg8toreg16(p^.location.register);
                              inc(pushedparasize,2);
                            end;
                          { we must push always 16 bit }
                          if inlined then
                            begin
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                            end
                          else
                            exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
                          ungetregister32(reg8toreg32(p^.location.register));
                        end;
                     else internalerror(1899);
                  end;
               end;
             LOC_FPU:
               begin
                  size:=align(pfloatdef(p^.resulttype)^.size,alignment);
                  inc(pushedparasize,size);
                  if not inlined then
                   emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmlist^.first=exprasmlist^.last) then
                    exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                  r:=new_reference(R_ESP,0);
                  floatstoreops(pfloatdef(p^.resulttype)^.typ,op,opsize);
                  { this is the easiest case for inlined !! }
                  if inlined then
                    begin
                       r^.base:=procinfo^.framepointer;
                       r^.offset:=para_offset-pushedparasize;
                    end;
                  exprasmlist^.concat(new(paicpu,op_ref(op,opsize,r)));
                  dec(fpuvaroffset);
               end;
             LOC_CFPUREGISTER:
               begin
                  exprasmlist^.concat(new(paicpu,op_reg(A_FLD,S_NO,
                    correct_fpuregister(p^.location.register,fpuvaroffset))));
                  size:=align(pfloatdef(p^.resulttype)^.size,alignment);
                  inc(pushedparasize,size);
                  if not inlined then
                   emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmlist^.first=exprasmlist^.last) then
                    exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                  r:=new_reference(R_ESP,0);
                  floatstoreops(pfloatdef(p^.resulttype)^.typ,op,opsize);
                  { this is the easiest case for inlined !! }
                  if inlined then
                    begin
                       r^.base:=procinfo^.framepointer;
                       r^.offset:=para_offset-pushedparasize;
                    end;
                  exprasmlist^.concat(new(paicpu,op_ref(op,opsize,r)));
               end;
             LOC_REFERENCE,LOC_MEM:
               begin
                  tempreference:=p^.location.reference;
                  del_reference(p^.location.reference);
                  case p^.resulttype^.deftype of
                    enumdef,
                    orddef :
                      begin
                        case p^.resulttype^.size of
                         8 : begin
                               inc(pushedparasize,8);
                               if inlined then
                                 begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   inc(tempreference.offset,4);
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                 end
                               else
                                 begin
                                   inc(tempreference.offset,4);
                                   emit_push_mem(tempreference);
                                   dec(tempreference.offset,4);
                                   emit_push_mem(tempreference);
                                 end;
                             end;
                         4 : begin
                               inc(pushedparasize,4);
                               if inlined then
                                 begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                 end
                               else
                                 emit_push_mem(tempreference);
                             end;
                       1,2 : begin
                               if alignment=4 then
                                begin
                                  opsize:=S_L;
                                  hreg:=R_EDI;
                                  inc(pushedparasize,4);
                                end
                               else
                                begin
                                  opsize:=S_W;
                                  hreg:=R_DI;
                                  inc(pushedparasize,2);
                                end;
                               if inlined then
                                begin
{$ifndef noAllocEdi}
                                  getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(tempreference),hreg);
                                  r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                  exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
{$ifndef noAllocEdi}
                                  ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                               else
                                exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,opsize,
                                  newreference(tempreference))));
                             end;
                           else
                             internalerror(234231);
                        end;
                      end;
                    floatdef :
                      begin
                        case pfloatdef(p^.resulttype)^.typ of
                          f32bit,
                          s32real :
                            begin
                               inc(pushedparasize,4);
                               if inlined then
                                 begin
{$ifndef noAllocEdi}
                                    getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                    emit_ref_reg(A_MOV,S_L,
                                      newreference(tempreference),R_EDI);
                                    r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                    exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                    ungetregister32(R_EDI);
{$endif noAllocEdi}
                                 end
                               else
                                 emit_push_mem(tempreference);
                            end;
                          s64real,
                          s64comp :
                            begin
                              inc(pushedparasize,4);
                              inc(tempreference.offset,4);
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                emit_push_mem(tempreference);
                              inc(pushedparasize,4);
                              dec(tempreference.offset,4);
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                emit_push_mem(tempreference);
                            end;
                          s80real :
                            begin
                              inc(pushedparasize,4);
                              if alignment=4 then
                                inc(tempreference.offset,8)
                              else
                                inc(tempreference.offset,6);
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                emit_push_mem(tempreference);
                              dec(tempreference.offset,4);
                              inc(pushedparasize,4);
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                emit_push_mem(tempreference);
                              if alignment=4 then
                                begin
                                  opsize:=S_L;
                                  hreg:=R_EDI;
                                  inc(pushedparasize,4);
                                  dec(tempreference.offset,4);
                                end
                              else
                                begin
                                  opsize:=S_W;
                                  hreg:=R_DI;
                                  inc(pushedparasize,2);
                                  dec(tempreference.offset,2);
                                end;
                              if inlined then
                                begin
{$ifndef noAllocEdi}
                                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                   emit_ref_reg(A_MOV,opsize,
                                     newreference(tempreference),hreg);
                                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
{$ifndef noAllocEdi}
                                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                                end
                              else
                                exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,opsize,
                                  newreference(tempreference))));
                          end;
                        end;
                      end;
                    pointerdef,
                    procvardef,
                    classrefdef:
                      begin
                         inc(pushedparasize,4);
                         if inlined then
                           begin
{$ifndef noAllocEdi}
                              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                              emit_ref_reg(A_MOV,S_L,
                                newreference(tempreference),R_EDI);
                              r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
{$ifndef noAllocEdi}
                              ungetregister32(R_EDI);
{$endif noAllocEdi}
                           end
                         else
                           emit_push_mem(tempreference);
                      end;
                    arraydef,
                    recorddef,
                    stringdef,
                    setdef,
                    objectdef :
                      begin
                         { even some structured types are 32 bit }
                         if is_widestring(p^.resulttype) or
                            is_ansistring(p^.resulttype) or
                            is_smallset(p^.resulttype) or
                            ((p^.resulttype^.deftype in [recorddef,arraydef]) and (p^.resulttype^.size<=4)
                             and ((p^.resulttype^.deftype<>arraydef) or not
                              (parraydef(p^.resulttype)^.IsConstructor or
                               parraydef(p^.resulttype)^.isArrayOfConst or
                               is_open_array(p^.resulttype)))
                            ) or
                            ((p^.resulttype^.deftype=objectdef) and
                             pobjectdef(p^.resulttype)^.is_class) then
                           begin
                              if (p^.resulttype^.size>2) or
                                 (alignment=4) then
                                begin
                                  inc(pushedparasize,4);
                                  if inlined then
                                    begin
                                      r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                      concatcopy(tempreference,r^,4,false,false);
                                    end
                                  else
                                    emit_push_mem(tempreference);
                                end
                              else
                                begin
                                  if p^.resulttype^.size>0 then
                                    begin
                                      inc(pushedparasize,2);
                                      if inlined then
                                        begin
                                          r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                          concatcopy(tempreference,r^,2,false,false);
                                        end
                                      else
                                        exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_W,newreference(tempreference))));
                                    end;
                                end;
                           end
                         { call by value open array ? }
                         else
                           internalerror(8954);
                      end;
                    else
                      CGMessage(cg_e_illegal_expression);
                  end;
               end;
             LOC_JUMP:
               begin
                  getlabel(hlabel);
                  if alignment=4 then
                   begin
                     opsize:=S_L;
                     inc(pushedparasize,4);
                   end
                  else
                   begin
                     opsize:=S_W;
                     inc(pushedparasize,2);
                   end;
                  emitlab(truelabel);
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_const_ref(A_MOV,opsize,1,r);
                    end
                  else
                    exprasmlist^.concat(new(paicpu,op_const(A_PUSH,opsize,1)));
                  emitjmp(C_None,hlabel);
                  emitlab(falselabel);
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_const_ref(A_MOV,opsize,0,r);
                    end
                  else
                    exprasmlist^.concat(new(paicpu,op_const(A_PUSH,opsize,0)));
                  emitlab(hlabel);
               end;
             LOC_FLAGS:
               begin
                  if not(R_EAX in unused) then
                    begin
{$ifndef noAllocEdi}
                      getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                      emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
                    end;
                  emit_flag2reg(p^.location.resflags,R_AL);
                  emit_reg_reg(A_MOVZX,S_BW,R_AL,R_AX);
                  if alignment=4 then
                   begin
                     opsize:=S_L;
                     hreg:=R_EAX;
                     inc(pushedparasize,4);
                   end
                  else
                   begin
                     opsize:=S_W;
                     hreg:=R_AX;
                     inc(pushedparasize,2);
                   end;
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                    end
                  else
                    exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
                  if not(R_EAX in unused) then
                    begin
                      emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
{$ifndef noAllocEdi}
                      ungetregister32(R_EDI);
{$endif noAllocEdi}
                    end;
               end;
{$ifdef SUPPORT_MMX}
             LOC_MMXREGISTER,
             LOC_CMMXREGISTER:
               begin
                  inc(pushedparasize,8); { was missing !!! (PM) }
                  emit_const_reg(
                    A_SUB,S_L,8,R_ESP);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmlist^.first=exprasmlist^.last) then
                    exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVQ,S_NO,
                         p^.location.register,r)));
                    end
                  else
                     begin
                        r:=new_reference(R_ESP,0);
                        exprasmlist^.concat(new(paicpu,op_reg_ref(
                          A_MOVQ,S_NO,p^.location.register,r)));
                     end;
               end;
{$endif SUPPORT_MMX}
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
         exprasmlist^.concat(new(paicpu,op_ref(op,s,
           newreference(ref))));
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
         exprasmlist^.concat(new(paicpu,op_ref(op,s,
           newreference(ref))));
         dec(fpuvaroffset);
      end;


{*****************************************************************************
                           Emit Functions
*****************************************************************************}

    procedure maketojumpbool(p : ptree);
    {
      produces jumps to true respectively false labels using boolean expressions
    }
      var
        opsize : topsize;
        storepos : tfileposinfo;
      begin
         if p^.error then
           exit;
         storepos:=aktfilepos;
         aktfilepos:=p^.fileinfo;
         if is_boolean(p^.resulttype) then
           begin
              if is_constboolnode(p) then
                begin
                   if p^.value<>0 then
                     emitjmp(C_None,truelabel)
                   else
                     emitjmp(C_None,falselabel);
                end
              else
                begin
                   opsize:=def_opsize(p^.resulttype);
                   case p^.location.loc of
                      LOC_CREGISTER,LOC_REGISTER : begin
                                        emit_reg_reg(A_OR,opsize,p^.location.register,
                                          p^.location.register);
                                        ungetregister(p^.location.register);
                                        emitjmp(C_NZ,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_MEM,LOC_REFERENCE : begin
                                        emit_const_ref(
                                          A_CMP,opsize,0,newreference(p^.location.reference));
                                        del_reference(p^.location.reference);
                                        emitjmp(C_NZ,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_FLAGS : begin
                                     emitjmp(flag_2_cond[p^.location.resflags],truelabel);
                                     emitjmp(C_None,falselabel);
                                  end;
                   end;
                end;
           end
         else
           CGMessage(type_e_mismatch);
         aktfilepos:=storepos;
      end;


    { produces if necessary overflowcode }
    procedure emitoverflowcheck(p:ptree);
      var
         hl : pasmlabel;
      begin
         if not(cs_check_overflow in aktlocalswitches) then
          exit;
         getlabel(hl);
         if not ((p^.resulttype^.deftype=pointerdef) or
                ((p^.resulttype^.deftype=orddef) and
                 (porddef(p^.resulttype)^.typ in [u64bit,u16bit,u32bit,u8bit,uchar,
                                                  bool8bit,bool16bit,bool32bit]))) then
           emitjmp(C_NO,hl)
         else
           emitjmp(C_NB,hl);
         emitcall('FPC_OVERFLOW');
         emitlab(hl);
      end;

    { produces range check code, while one of the operands is a 64 bit
      integer }
    procedure emitrangecheck64(p : ptree;todef : pdef);

      begin

         CGMessage(cg_w_64bit_range_check_not_supported);
         {internalerror(28699);}
      end;

     { produces if necessary rangecheckcode }
     procedure emitrangecheck(p:ptree;todef:pdef);
     {
       generate range checking code for the value at location t. The
       type used is the checked against todefs ranges. fromdef (p.resulttype)
       is the original type used at that location, when both defs are
       equal the check is also insert (needed for succ,pref,inc,dec)
     }
      var
        neglabel,
        poslabel : pasmlabel;
        href   : treference;
        rstr   : string;
        hreg   : tregister;
        opsize : topsize;
        op     : tasmop;
        fromdef : pdef;
        lto,hto,
        lfrom,hfrom : longint;
        doublebound,
        is_reg,
        popecx : boolean;
      begin
        { range checking on and range checkable value? }
        if not(cs_check_range in aktlocalswitches) or
           not(todef^.deftype in [orddef,enumdef,arraydef]) then
          exit;
        { only check when assigning to scalar, subranges are different,
          when todef=fromdef then the check is always generated }
        fromdef:=p^.resulttype;
        if is_64bitint(fromdef) or is_64bitint(todef) then
          begin
             emitrangecheck64(p,todef);
             exit;
          end;
        {we also need lto and hto when checking if we need to use doublebound!
        (JM)}
        getrange(todef,lto,hto);
        if todef<>fromdef then
         begin
           getrange(p^.resulttype,lfrom,hfrom);
           { first check for not being u32bit, then if the to is bigger than
             from }
           if (lto<hto) and (lfrom<hfrom) and
              (lto<=lfrom) and (hto>=hfrom) then
            exit;
         end;
        { generate the rangecheck code for the def where we are going to
          store the result }
        doublebound:=false;
        case todef^.deftype of
          orddef :
            begin
              porddef(todef)^.genrangecheck;
              rstr:=porddef(todef)^.getrangecheckstring;
              doublebound:=(porddef(todef)^.typ=u32bit) and (lto>hto);
            end;
          enumdef :
            begin
              penumdef(todef)^.genrangecheck;
              rstr:=penumdef(todef)^.getrangecheckstring;
            end;
          arraydef :
            begin
              parraydef(todef)^.genrangecheck;
              rstr:=parraydef(todef)^.getrangecheckstring;
            end;
        end;
      { get op and opsize }
        opsize:=def2def_opsize(fromdef,u32bitdef);
        if opsize in [S_B,S_W,S_L] then
         op:=A_MOV
        else
         if is_signed(fromdef) then
          op:=A_MOVSX
         else
          op:=A_MOVZX;
        is_reg:=(p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]);
        if is_reg then
          hreg:=p^.location.register;
        if not target_os.use_bound_instruction then
         begin
           { FPC_BOUNDCHECK needs to be called with
              %ecx - value
              %edi - pointer to the ranges }
           popecx:=false;
           if not(is_reg) or
              (p^.location.register<>R_ECX) then
            begin
              if not(R_ECX in unused) then
               begin
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_ECX)));
                 popecx:=true;
               end
                 else exprasmlist^.concat(new(pairegalloc,alloc(R_ECX)));
              if is_reg then
               emit_reg_reg(op,opsize,p^.location.register,R_ECX)
              else
               emit_ref_reg(op,opsize,newreference(p^.location.reference),R_ECX);
            end;
           if doublebound then
            begin
              getlabel(neglabel);
              getlabel(poslabel);
              emit_reg_reg(A_OR,S_L,R_ECX,R_ECX);
              emitjmp(C_L,neglabel);
            end;
           { insert bound instruction only }
{$ifndef noAllocEdi}
           getexplicitregister32(R_EDI);
{$endif noAllocEdi}
           exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,newasmsymbol(rstr),0,R_EDI)));
           emitcall('FPC_BOUNDCHECK');
{$ifndef noAllocEdi}
           ungetregister32(R_EDI);
{$endif noAllocEdi}
           { u32bit needs 2 checks }
           if doublebound then
            begin
              emitjmp(C_None,poslabel);
              emitlab(neglabel);
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,newasmsymbol(rstr),8,R_EDI)));
              emitcall('FPC_BOUNDCHECK');
{$ifndef noAllocEdi}
              ungetregister32(R_EDI);
{$endif noAllocEdi}
              emitlab(poslabel);
            end;
           if popecx then
            exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_ECX)))
           else exprasmlist^.concat(new(pairegalloc,dealloc(R_ECX)));
         end
        else
         begin
           reset_reference(href);
           href.symbol:=newasmsymbol(rstr);
           { load the value in a register }
           if is_reg then
            begin
              { be sure that hreg is a 32 bit reg, if not load it in %edi }
              if p^.location.register in [R_EAX..R_EDI] then
               hreg:=p^.location.register
              else
               begin
{$ifndef noAllocEdi}
                 getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                 emit_reg_reg(op,opsize,p^.location.register,R_EDI);
                 hreg:=R_EDI;
               end;
            end
           else
            begin
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              emit_ref_reg(op,opsize,newreference(p^.location.reference),R_EDI);
              hreg:=R_EDI;
            end;
           if doublebound then
            begin
              getlabel(neglabel);
              getlabel(poslabel);
              emit_reg_reg(A_TEST,S_L,hreg,hreg);
              emitjmp(C_L,neglabel);
            end;
           { insert bound instruction only }
           exprasmlist^.concat(new(paicpu,op_reg_ref(A_BOUND,S_L,hreg,newreference(href))));
           { u32bit needs 2 checks }
           if doublebound then
            begin
              href.offset:=8;
              emitjmp(C_None,poslabel);
              emitlab(neglabel);
              exprasmlist^.concat(new(paicpu,op_reg_ref(A_BOUND,S_L,hreg,newreference(href))));
              emitlab(poslabel);
            end;
{$ifndef noAllocEdi}
           if hreg = R_EDI then
             ungetregister32(R_EDI);
{$endif noAllocEdi}
         end;
      end;


    procedure concatcopy(source,dest : treference;size : longint;delsource,loadref : boolean);

      const
         isizes : array[0..3] of topsize=(S_L,S_B,S_W,S_B);
         ishr : array[0..3] of byte=(2,0,1,0);

      var
         ecxpushed : boolean;
         helpsize : longint;
         i : byte;
         reg8,reg32 : tregister;
         swap : boolean;

         procedure maybepushecx;
         begin
           if not(R_ECX in unused) then
             begin
               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_ECX)));
               ecxpushed:=true;
             end;
         end;

      begin
{$IfNDef regallocfix}
        If delsource then
           del_reference(source);
{$EndIf regallocfix}
         if (not loadref) and
            ((size<=8) or
             (not(cs_littlesize in aktglobalswitches ) and (size<=12))) then
           begin
              helpsize:=size shr 2;
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              for i:=1 to helpsize do
                begin
                   emit_ref_reg(A_MOV,S_L,newreference(source),R_EDI);
{$ifdef regallocfix}
                   If (size = 4) and delsource then
                     del_reference(source);
{$endif regallocfix}
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,newreference(dest))));
                   inc(source.offset,4);
                   inc(dest.offset,4);
                   dec(size,4);
                end;
              if size>1 then
                begin
                   emit_ref_reg(A_MOV,S_W,newreference(source),R_DI);
{$ifdef regallocfix}
                   If (size = 2) and delsource then
                     del_reference(source);
{$endif regallocfix}
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_W,R_DI,newreference(dest))));
                   inc(source.offset,2);
                   inc(dest.offset,2);
                   dec(size,2);
                end;
{$ifndef noAllocEdi}
              ungetregister32(R_EDI);
{$endif noAllocEdi}
              if size>0 then
                begin
                   { and now look for an 8 bit register }
                   swap:=false;
                   if R_EAX in unused then reg8:=R_AL
                   else if R_EBX in unused then reg8:=R_BL
                   else if R_ECX in unused then reg8:=R_CL
                   else if R_EDX in unused then reg8:=R_DL
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
{$ifndef noAllocEdi}
                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                       emit_reg_reg(A_MOV,S_L,reg32,R_EDI);
                     end;
                   emit_ref_reg(A_MOV,S_B,newreference(source),reg8);
{$ifdef regallocfix}
                   If delsource then
                     del_reference(source);
{$endif regallocfix}
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_B,reg8,newreference(dest))));
                   if swap then
                     begin
                       emit_reg_reg(A_MOV,S_L,R_EDI,reg32);
{$ifndef noAllocEdi}
                       ungetregister32(R_EDI);
{$endif noAllocEdi}
                     end;
                end;
           end
         else
           begin
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              emit_ref_reg(A_LEA,S_L,newreference(dest),R_EDI);
{$ifdef regallocfix}
             {is this ok?? (JM)}
              del_reference(dest);
{$endif regallocfix}
{$ifndef noAllocEdi}
              exprasmlist^.concat(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
              if loadref then
                emit_ref_reg(A_MOV,S_L,newreference(source),R_ESI)
              else
                begin
                  emit_ref_reg(A_LEA,S_L,newreference(source),R_ESI);
{$ifdef regallocfix}
                  if delsource then
                    del_reference(source);
{$endif regallocfix}
                end;

              exprasmlist^.concat(new(paicpu,op_none(A_CLD,S_NO)));
              ecxpushed:=false;
              if cs_littlesize in aktglobalswitches  then
                begin
                   maybepushecx;
                   emit_const_reg(A_MOV,S_L,size,R_ECX);
                   exprasmlist^.concat(new(paicpu,op_none(A_REP,S_NO)));
                   exprasmlist^.concat(new(paicpu,op_none(A_MOVSB,S_NO)));
                end
              else
                begin
                   helpsize:=size shr 2;
                   size:=size and 3;
                   if helpsize>1 then
                    begin
                      maybepushecx;
                      emit_const_reg(A_MOV,S_L,helpsize,R_ECX);
                      exprasmlist^.concat(new(paicpu,op_none(A_REP,S_NO)));
                    end;
                   if helpsize>0 then
                    exprasmlist^.concat(new(paicpu,op_none(A_MOVSD,S_NO)));
                   if size>1 then
                     begin
                        dec(size,2);
                        exprasmlist^.concat(new(paicpu,op_none(A_MOVSW,S_NO)));
                     end;
                   if size=1 then
                     exprasmlist^.concat(new(paicpu,op_none(A_MOVSB,S_NO)));
                end;
{$ifndef noAllocEdi}
              ungetregister32(R_EDI);
              exprasmlist^.concat(new(pairegalloc,dealloc(R_ESI)));
{$endif noAllocEdi}
              if ecxpushed then
                begin
                  exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_ECX)));
                end;

              { loading SELF-reference again }
              maybe_loadesi;
           end;
         if delsource then
           ungetiftemp(source);
      end;


    procedure emitloadord2reg(const location:Tlocation;orddef:Porddef;
                              destreg:Tregister;delloc:boolean);

    {A lot smaller and less bug sensitive than the original unfolded loads.}

    var tai:Paicpu;
        r:Preference;

    begin
        case location.loc of
            LOC_REGISTER,LOC_CREGISTER:
                begin
                    case orddef^.typ of
                        u8bit:
                            tai:=new(paicpu,op_reg_reg(A_MOVZX,S_BL,location.register,destreg));
                        s8bit:
                            tai:=new(paicpu,op_reg_reg(A_MOVSX,S_BL,location.register,destreg));
                        u16bit:
                            tai:=new(paicpu,op_reg_reg(A_MOVZX,S_WL,location.register,destreg));
                        s16bit:
                            tai:=new(paicpu,op_reg_reg(A_MOVSX,S_WL,location.register,destreg));
                        u32bit:
                            tai:=new(paicpu,op_reg_reg(A_MOV,S_L,location.register,destreg));
                        s32bit:
                            tai:=new(paicpu,op_reg_reg(A_MOV,S_L,location.register,destreg));
                    end;
                    if delloc then
                        ungetregister(location.register);
                end;
            LOC_MEM,
            LOC_REFERENCE:
                begin
                    if location.reference.is_immediate then
                     tai:=new(paicpu,op_const_reg(A_MOV,S_L,location.reference.offset,destreg))
                    else
                     begin
                       r:=newreference(location.reference);
                       case orddef^.typ of
                         u8bit:
                            tai:=new(paicpu,op_ref_reg(A_MOVZX,S_BL,r,destreg));
                         s8bit:
                            tai:=new(paicpu,op_ref_reg(A_MOVSX,S_BL,r,destreg));
                         u16bit:
                            tai:=new(paicpu,op_ref_reg(A_MOVZX,S_WL,r,destreg));
                         s16bit:
                            tai:=new(paicpu,op_ref_reg(A_MOVSX,S_WL,r,destreg));
                         u32bit:
                            tai:=new(paicpu,op_ref_reg(A_MOV,S_L,r,destreg));
                         s32bit:
                            tai:=new(paicpu,op_ref_reg(A_MOV,S_L,r,destreg));
                       end;
                     end;
                    if delloc then
                        del_reference(location.reference);
                end
            else
                internalerror(6);
        end;
        exprasmlist^.concat(tai);
    end;

    { if necessary ESI is reloaded after a call}
    procedure maybe_loadesi;

      var
         hp : preference;
         p : pprocinfo;
         i : longint;

      begin
         if assigned(procinfo^._class) then
           begin
{$ifndef noAllocEdi}
              exprasmlist^.concat(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
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


    procedure firstcomplex(p : ptree);
      var
         hp : ptree;
      begin
         { always calculate boolean AND and OR from left to right }
         if (p^.treetype in [orn,andn]) and
            (p^.left^.resulttype^.deftype=orddef) and
            (porddef(p^.left^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit]) then
           p^.swaped:=false
         else
           if (p^.left^.registers32<p^.right^.registers32) and
           { the following check is appropriate, because all }
           { 4 registers are rarely used and it is thereby   }
           { achieved that the extra code is being dropped   }
           { by exchanging not commutative operators     }
              (p^.right^.registers32<=4) then
            begin
              hp:=p^.left;
              p^.left:=p^.right;
              p^.right:=hp;
              p^.swaped:=true;
            end
         else
           p^.swaped:=false;
      end;


{*****************************************************************************
                            Entry/Exit Code Functions
*****************************************************************************}

  procedure genprofilecode;
    var
      pl : pasmlabel;
    begin
      if (po_assembler in aktprocsym^.definition^.procoptions) then
       exit;
      case target_info.target of
         target_i386_linux:
           begin
              getlabel(pl);
              emitcall('mcount');
              exprasmlist^.insert(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,pl,0,R_EDX)));
              exprasmlist^.insert(new(pai_section,init(sec_code)));
              exprasmlist^.insert(new(pai_const,init_32bit(0)));
              exprasmlist^.insert(new(pai_label,init(pl)));
              exprasmlist^.insert(new(pai_align,init(4)));
              exprasmlist^.insert(new(pai_section,init(sec_data)));
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
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EAX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EBX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_ECX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EDX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_ESI)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));

         { .... also the segment registers }
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_W,R_DS)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_W,R_ES)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_W,R_FS)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_W,R_GS)));
      end;


    procedure generate_interrupt_stackframe_exit;
      begin
         { restore the registers of an interrupt procedure }
         { this was all with entrycode instead of exitcode !!}
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_EAX)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_EBX)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_ECX)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_EDX)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_ESI)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_L,R_EDI)));

         { .... also the segment registers }
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_W,R_DS)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_W,R_ES)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_W,R_FS)));
         procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_POP,S_W,R_GS)));

        { this restores the flags }
         procinfo^.aktexitcode^.concat(new(paicpu,op_none(A_IRET,S_NO)));
      end;


  { generates the code for threadvar initialisation }
  procedure initialize_threadvar(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          (vo_is_thread_var in pvarsym(p)^.varoptions) then
         begin
            exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,pvarsym(p)^.getsize)));
            reset_reference(hr);
            hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
            emitpushreferenceaddr(hr);
            emitcall('FPC_INIT_THREADVAR');
         end;
    end;

    { initilizes data of type t                           }
    { if is_already_ref is true then the routines assumes }
    { that r points to the data to initialize             }
    procedure initialize(t : pdef;const ref : treference;is_already_ref : boolean);

      var
         hr : treference;

      begin
         if is_ansistring(t) or
           is_widestring(t) then
           begin
              emit_const_ref(A_MOV,S_L,0,
                newreference(ref));
           end
         else
           begin
              reset_reference(hr);
              hr.symbol:=t^.get_inittable_label;
              emitpushreferenceaddr(hr);
              if is_already_ref then
                exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,
                  newreference(ref))))
              else
                emitpushreferenceaddr(ref);
              emitcall('FPC_INITIALIZE');
           end;
      end;

    { finalizes data of type t                            }
    { if is_already_ref is true then the routines assumes }
    { that r points to the data to finalizes              }
    procedure finalize(t : pdef;const ref : treference;is_already_ref : boolean);

      var
         r : treference;

      begin
         if is_ansistring(t) or
           is_widestring(t) then
           begin
              decrstringref(t,ref);
           end
         else
           begin
              reset_reference(r);
              r.symbol:=t^.get_inittable_label;
              emitpushreferenceaddr(r);
              if is_already_ref then
                exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_L,
                  newreference(ref))))
              else
                emitpushreferenceaddr(ref);
              emitcall('FPC_FINALIZE');
           end;
      end;


  { generates the code for initialisation of local data }
  procedure initialize_data(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          assigned(pvarsym(p)^.vartype.def) and
          not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
          pvarsym(p)^.vartype.def^.needs_inittable then
         begin
            procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            if psym(p)^.owner^.symtabletype in [localsymtable,inlinelocalsymtable] then
              begin
                 hr.base:=procinfo^.framepointer;
                 hr.offset:=-pvarsym(p)^.address+pvarsym(p)^.owner^.address_fixup;
              end
            else
              begin
                 hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
              end;
            initialize(pvarsym(p)^.vartype.def,hr,false);
         end;
    end;

  { generates the code for incrementing the reference count of parameters }
  procedure incr_data(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
          pvarsym(p)^.vartype.def^.needs_inittable and
          ((pvarsym(p)^.varspez=vs_value) {or
           (pvarsym(p)^.varspez=vs_const) and
           not(dont_copy_const_param(pvarsym(p)^.definition))}) then
         begin
            procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            hr.symbol:=pvarsym(p)^.vartype.def^.get_inittable_label;
            emitpushreferenceaddr(hr);
            reset_reference(hr);
            hr.base:=procinfo^.framepointer;
            hr.offset:=pvarsym(p)^.address+procinfo^.para_offset;

            emitpushreferenceaddr(hr);
            reset_reference(hr);

            emitcall('FPC_ADDREF');
         end;
    end;

  { generates the code for finalisation of local data }
  procedure finalize_data(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          assigned(pvarsym(p)^.vartype.def) and
          not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
          pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
          pvarsym(p)^.vartype.def^.needs_inittable then
         begin
            { not all kind of parameters need to be finalized  }
            if (psym(p)^.owner^.symtabletype=parasymtable) and
              ((pvarsym(p)^.varspez=vs_var)  or
               (pvarsym(p)^.varspez=vs_const) { and
               (dont_copy_const_param(pvarsym(p)^.definition)) } ) then
              exit;
            procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            case psym(p)^.owner^.symtabletype of
               localsymtable,inlinelocalsymtable:
                 begin
                    hr.base:=procinfo^.framepointer;
                    hr.offset:=-pvarsym(p)^.address+pvarsym(p)^.owner^.address_fixup;
                 end;
               parasymtable,inlineparasymtable:
                 begin
                    hr.base:=procinfo^.framepointer;
                    hr.offset:=pvarsym(p)^.address+procinfo^.para_offset;
                 end;
               else
                 hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
            end;
            finalize(pvarsym(p)^.vartype.def,hr,false);
         end;
    end;


  { generates the code to make local copies of the value parameters }
  procedure copyvalueparas(p : pnamedindexobject);{$ifndef fpc}far;{$endif}
    var
      href1,href2 : treference;
      r    : preference;
      len  : longint;
      opsize : topsize;
      again,ok : pasmlabel;
    begin
       if (psym(p)^.typ=varsym) and
          (pvarsym(p)^.varspez=vs_value) and
          (push_addr_param(pvarsym(p)^.vartype.def)) then
        begin
          if is_open_array(pvarsym(p)^.vartype.def) or
             is_array_of_const(pvarsym(p)^.vartype.def) then
           begin
              { get stack space }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=pvarsym(p)^.address+4+procinfo^.para_offset;
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              exprasmlist^.concat(new(paicpu,
                op_ref_reg(A_MOV,S_L,r,R_EDI)));

              exprasmlist^.concat(new(paicpu,
                op_reg(A_INC,S_L,R_EDI)));

              exprasmlist^.concat(new(paicpu,
                op_const_reg(A_IMUL,S_L,
                parraydef(pvarsym(p)^.vartype.def)^.elementtype.def^.size,R_EDI)));
{$ifndef NOTARGETWIN32}
              { windows guards only a few pages for stack growing, }
              { so we have to access every page first              }
              if target_os.id=os_i386_win32 then
                begin
                   getlabel(again);
                   getlabel(ok);
                   emitlab(again);
                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_CMP,S_L,winstackpagesize,R_EDI)));
                   emitjmp(C_C,ok);
                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP)));
                   exprasmlist^.concat(new(paicpu,
                     op_reg(A_PUSH,S_L,R_EAX)));
                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_SUB,S_L,winstackpagesize,R_EDI)));
                   emitjmp(C_None,again);

                   emitlab(ok);
                   exprasmlist^.concat(new(paicpu,
                     op_reg_reg(A_SUB,S_L,R_EDI,R_ESP)));
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                   { now reload EDI }
                   new(r);
                   reset_reference(r^);
                   r^.base:=procinfo^.framepointer;
                   r^.offset:=pvarsym(p)^.address+4+procinfo^.para_offset;
{$ifndef noAllocEdi}
                   getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                   exprasmlist^.concat(new(paicpu,
                     op_ref_reg(A_MOV,S_L,r,R_EDI)));

                   exprasmlist^.concat(new(paicpu,
                     op_reg(A_INC,S_L,R_EDI)));

                   exprasmlist^.concat(new(paicpu,
                     op_const_reg(A_IMUL,S_L,
                     parraydef(pvarsym(p)^.vartype.def)^.elementtype.def^.size,R_EDI)));
                end
              else
{$endif NOTARGETWIN32}
                begin
                   exprasmlist^.concat(new(paicpu,
                     op_reg_reg(A_SUB,S_L,R_EDI,R_ESP)));
                   { load destination }
                   exprasmlist^.concat(new(paicpu,
                     op_reg_reg(A_MOV,S_L,R_ESP,R_EDI)));
                end;

              { don't destroy the registers! }
              exprasmlist^.concat(new(paicpu,
                op_reg(A_PUSH,S_L,R_ECX)));
              exprasmlist^.concat(new(paicpu,
                op_reg(A_PUSH,S_L,R_ESI)));

              { load count }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=pvarsym(p)^.address+4+procinfo^.para_offset;
              exprasmlist^.concat(new(paicpu,
                op_ref_reg(A_MOV,S_L,r,R_ECX)));

              { load source }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              exprasmlist^.concat(new(paicpu,
                op_ref_reg(A_MOV,S_L,r,R_ESI)));

              { scheduled .... }
              exprasmlist^.concat(new(paicpu,
                op_reg(A_INC,S_L,R_ECX)));

              { calculate size }
              len:=parraydef(pvarsym(p)^.vartype.def)^.elementtype.def^.size;
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

              exprasmlist^.concat(new(paicpu,
                op_const_reg(A_IMUL,S_L,len,R_ECX)));
              exprasmlist^.concat(new(paicpu,
                op_none(A_REP,S_NO)));
              case opsize of
                S_B : exprasmlist^.concat(new(paicpu,op_none(A_MOVSB,S_NO)));
                S_W : exprasmlist^.concat(new(paicpu,op_none(A_MOVSW,S_NO)));
                S_L : exprasmlist^.concat(new(paicpu,op_none(A_MOVSD,S_NO)));
              end;
{$ifndef noAllocEdi}
              ungetregister32(R_EDI);
{$endif noAllocEdi}
              exprasmlist^.concat(new(paicpu,
                op_reg(A_POP,S_L,R_ESI)));
              exprasmlist^.concat(new(paicpu,
                op_reg(A_POP,S_L,R_ECX)));

              { patch the new address }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo^.framepointer;
              r^.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              exprasmlist^.concat(new(paicpu,
                op_reg_ref(A_MOV,S_L,R_ESP,r)));
           end
          else
           if is_shortstring(pvarsym(p)^.vartype.def) then
            begin
              reset_reference(href1);
              href1.base:=procinfo^.framepointer;
              href1.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              reset_reference(href2);
              href2.base:=procinfo^.framepointer;
              href2.offset:=-pvarsym(p)^.localvarsym^.address+pvarsym(p)^.localvarsym^.owner^.address_fixup;
              copyshortstring(href2,href1,pstringdef(pvarsym(p)^.vartype.def)^.len,true);
            end
           else
            begin
              reset_reference(href1);
              href1.base:=procinfo^.framepointer;
              href1.offset:=pvarsym(p)^.address+procinfo^.para_offset;
              reset_reference(href2);
              href2.base:=procinfo^.framepointer;
              href2.offset:=-pvarsym(p)^.localvarsym^.address+pvarsym(p)^.localvarsym^.owner^.address_fixup;
              concatcopy(href1,href2,pvarsym(p)^.vartype.def^.size,true,true);
            end;
        end;
    end;

  procedure inittempansistrings;

    var
       hp : ptemprecord;
       r : preference;

    begin
       hp:=templist;
       while assigned(hp) do
         begin
           if hp^.temptype in [tt_ansistring,tt_freeansistring] then
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

  procedure finalizetempansistrings;

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
              end;
            hp:=hp^.next;
         end;
   end;

  var
     ls : longint;

  procedure largest_size(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    begin
       if (psym(p)^.typ=varsym) and
         (pvarsym(p)^.getsize>ls) then
         ls:=pvarsym(p)^.getsize;
    end;

  procedure alignstack(alist : paasmoutput);

    begin
{$ifdef dummy}
       if (cs_optimize in aktglobalswitches) and
         (aktoptprocessor in [classp5,classp6]) then
         begin
            ls:=0;
            aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}largest_size);
            if ls>=8 then
              alist^.insert(new(paicpu,op_const_reg(A_AND,S_L,-8,R_ESP)));
         end;
{$endif dummy}
    end;

  procedure genentrycode(alist : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                         stackframe:longint;
                         var parasize:longint;var nostackframe:boolean;
                         inlined : boolean);
  {
    Generates the entry code for a procedure
  }
    var
      hs : string;
{$ifdef GDB}
      stab_function_name : Pai_stab_function_name;
{$endif GDB}
      hr : preference;
      p : psymtable;
      r : treference;
      oldlist,
      oldexprasmlist : paasmoutput;
      again : pasmlabel;
      i : longint;

    begin
       oldexprasmlist:=exprasmlist;
       exprasmlist:=alist;
       if (not inlined) and (aktprocsym^.definition^.proctypeoption=potype_proginit) then
           begin
              emitinsertcall('FPC_INITIALIZEUNITS');
              if target_info.target=target_I386_WIN32 then
                begin
                   new(hr);
                   reset_reference(hr^);
                   hr^.symbol:=newasmsymbol(
                   'U_SYSWIN32_ISCONSOLE');
                   if apptype=at_cui then
                     exprasmlist^.insert(new(paicpu,op_const_ref(A_MOV,S_B,
                       1,hr)))
                   else
                     exprasmlist^.insert(new(paicpu,op_const_ref(A_MOV,S_B,
                       0,hr)));
                end;

              oldlist:=exprasmlist;
              exprasmlist:=new(paasmoutput,init);
              p:=symtablestack;
              while assigned(p) do
                begin
                   p^.foreach({$ifndef TP}@{$endif}initialize_threadvar);
                   p:=p^.next;
                end;
              oldlist^.insertlist(exprasmlist);
              dispose(exprasmlist,done);
              exprasmlist:=oldlist;
           end;

{$ifdef GDB}
      if (not inlined) and (cs_debuginfo in aktmoduleswitches) then
        exprasmlist^.insert(new(pai_force_line,init));
{$endif GDB}

      { a constructor needs a help procedure }
      if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
        begin
          if procinfo^._class^.is_class then
            begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              exprasmlist^.insert(new(paicpu,op_cond_sym(A_Jcc,C_Z,S_NO,faillabel)));
              emitinsertcall('FPC_NEW_CLASS');
            end
          else
            begin
              exprasmlist^.insert(new(paicpu,op_cond_sym(A_Jcc,C_Z,S_NO,faillabel)));
              emitinsertcall('FPC_HELP_CONSTRUCTOR');
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              exprasmlist^.insert(new(paicpu,op_const_reg(A_MOV,S_L,procinfo^._class^.vmt_offset,R_EDI)));
            end;
        end;

      { don't load ESI, does the caller }

      { When message method contains self as a parameter,
        we must load it into ESI }
      If (po_containsself in aktprocsym^.definition^.procoptions) then
        begin
           new(hr);
           reset_reference(hr^);
           hr^.offset:=procinfo^.selfpointer_offset;
           hr^.base:=procinfo^.framepointer;
{$ifndef noAllocEdi}
           exprasmlist^.concat(new(pairegalloc,alloc(R_ESI)));
{$endif noAllocEdi}
           exprasmlist^.insert(new(paicpu,op_ref_reg(A_MOV,S_L,hr,R_ESI)));
        end;
      { should we save edi,esi,ebx like C ? }
      if (po_savestdregs in aktprocsym^.definition^.procoptions) then
       begin
         if (aktprocsym^.definition^.usedregisters and ($80 shr byte(R_EBX)))<>0 then
           exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EBX)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_ESI)));
         exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
       end;

      { for the save all registers we can simply use a pusha,popa which
        push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
      if (po_saveregisters in aktprocsym^.definition^.procoptions) then
        begin
          exprasmlist^.insert(new(paicpu,op_none(A_PUSHA,S_L)));
        end;

      { omit stack frame ? }
      if not inlined then
      if procinfo^.framepointer=stack_pointer then
          begin
              CGMessage(cg_d_stackframe_omited);
              nostackframe:=true;
              if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.para_offset-4;
              if stackframe<>0 then
                exprasmlist^.insert(new(paicpu,
                  op_const_reg(A_SUB,S_L,gettempsize,R_ESP)));
          end
      else
          begin
              alignstack(alist);
              if (aktprocsym^.definition^.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.para_offset-8;
              nostackframe:=false;
              if stackframe<>0 then
                  begin
{$ifdef unused}
                      if (cs_littlesize in aktglobalswitches) and (stackframe<=65535) then
                          begin
                              if (cs_check_stack in aktlocalswitches) and
                                 not(target_info.target in [target_i386_linux,target_i386_win32]) then
                                begin
                                  emitinsertcall('FPC_STACKCHECK');
                                  exprasmlist^.insert(new(paicpu,op_const(A_PUSH,S_L,stackframe)));
                                end;
                              if cs_profile in aktmoduleswitches then
                                genprofilecode;

                            { %edi is already saved when pocdecl is used
                              if (target_info.target=target_linux) and
                               ((aktprocsym^.definition^.options and poexports)<>0) then
                                  exprasmlist^.insert(new(Paicpu,op_reg(A_PUSH,S_L,R_EDI))); }

                              exprasmlist^.insert(new(paicpu,op_const_const(A_ENTER,S_NO,stackframe,0)))
                          end
                      else
{$endif unused}
                          begin
                            { windows guards only a few pages for stack growing, }
                            { so we have to access every page first              }
                            if (target_os.id=os_i386_win32) and
                              (stackframe>=winstackpagesize) then
                              begin
                                  if stackframe div winstackpagesize<=5 then
                                    begin
                                       exprasmlist^.insert(new(paicpu,op_const_reg(A_SUB,S_L,stackframe-4,R_ESP)));
                                       for i:=1 to stackframe div winstackpagesize do
                                         begin
                                            hr:=new_reference(R_ESP,stackframe-i*winstackpagesize);
                                            exprasmlist^.concat(new(paicpu,
                                              op_const_ref(A_MOV,S_L,0,hr)));
                                         end;
                                       exprasmlist^.concat(new(paicpu,
                                         op_reg(A_PUSH,S_L,R_EAX)));
                                    end
                                  else
                                    begin
                                       getlabel(again);
{$ifndef noAllocEdi}
                                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                                       exprasmlist^.concat(new(paicpu,
                                         op_const_reg(A_MOV,S_L,stackframe div winstackpagesize,R_EDI)));
                                       emitlab(again);
                                       exprasmlist^.concat(new(paicpu,
                                         op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP)));
                                       exprasmlist^.concat(new(paicpu,
                                         op_reg(A_PUSH,S_L,R_EAX)));
                                       exprasmlist^.concat(new(paicpu,
                                         op_reg(A_DEC,S_L,R_EDI)));
                                       emitjmp(C_NZ,again);
{$ifndef noAllocEdi}
                                       ungetregister32(R_EDI);
{$endif noAllocEdi}
                                       exprasmlist^.concat(new(paicpu,
                                         op_const_reg(A_SUB,S_L,stackframe mod winstackpagesize,R_ESP)));
                                    end
                              end
                            else
                              exprasmlist^.insert(new(paicpu,op_const_reg(A_SUB,S_L,stackframe,R_ESP)));
                            if (cs_check_stack in aktlocalswitches) and
                              not(target_info.target in [target_i386_linux,target_i386_win32]) then
                              begin
                                 emitinsertcall('FPC_STACKCHECK');
                                 exprasmlist^.insert(new(paicpu,op_const(A_PUSH,S_L,stackframe)));
                              end;
                            if cs_profile in aktmoduleswitches then
                              genprofilecode;
                            exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOV,S_L,R_ESP,R_EBP)));
                            exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EBP)));
                          end;
                  end { endif stackframe <> 0 }
              else
                 begin
                   if cs_profile in aktmoduleswitches then
                     genprofilecode;
                   exprasmlist^.insert(new(paicpu,op_reg_reg(A_MOV,S_L,R_ESP,R_EBP)));
                   exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_EBP)));
                 end;
          end;

      if (po_interrupt in aktprocsym^.definition^.procoptions) then
          generate_interrupt_stackframe_entry;

      { initialize return value }
      if (procinfo^.returntype.def<>pdef(voiddef)) and
        (procinfo^.returntype.def^.needs_inittable) and
        ((procinfo^.returntype.def^.deftype<>objectdef) or
        not(pobjectdef(procinfo^.returntype.def)^.is_class)) then
        begin
           procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
           reset_reference(r);
           r.offset:=procinfo^.return_offset;
           r.base:=procinfo^.framepointer;
           initialize(procinfo^.returntype.def,r,ret_in_param(procinfo^.returntype.def));
        end;

      { generate copies of call by value parameters }
      if not(po_assembler in aktprocsym^.definition^.procoptions) then
        aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}copyvalueparas);

      { initialisizes local data }
      aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}initialize_data);
      { add a reference to all call by value/const parameters }
      aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}incr_data);

      { initilisizes temp. ansi/wide string data }
      inittempansistrings;

      { do we need an exception frame because of ansi/widestrings ? }
      if (procinfo^.flags and pi_needs_implicit_finally)<>0 then
        begin
            usedinproc:=usedinproc or ($80 shr byte(R_EAX));

            { Type of stack-frame must be pushed}
            exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,1)));
            emitcall('FPC_PUSHEXCEPTADDR');
            exprasmlist^.concat(new(paicpu,
              op_reg(A_PUSH,S_L,R_EAX)));
            emitcall('FPC_SETJMP');
            exprasmlist^.concat(new(paicpu,
              op_reg(A_PUSH,S_L,R_EAX)));
            exprasmlist^.concat(new(paicpu,
              op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
            emitjmp(C_NE,aktexitlabel);
            { probably we've to reload self here }
            maybe_loadesi;
        end;

      if not inlined then
       begin
         if (cs_profile in aktmoduleswitches) or
            (aktprocsym^.definition^.owner^.symtabletype=globalsymtable) or
            (assigned(procinfo^._class) and (procinfo^._class^.owner^.symtabletype=globalsymtable)) then
              make_global:=true;

         hs:=proc_names.get;

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and target_os.use_function_relative_addresses then
           stab_function_name := new(pai_stab_function_name,init(strpnew(hs)));
{$EndIf GDB}

         while hs<>'' do
          begin
            if make_global then
              exprasmlist^.insert(new(pai_symbol,initname_global(hs,0)))
            else
              exprasmlist^.insert(new(pai_symbol,initname(hs,0)));

{$ifdef GDB}
            if (cs_debuginfo in aktmoduleswitches) and
               target_os.use_function_relative_addresses then
              exprasmlist^.insert(new(pai_stab_function_name,init(strpnew(hs))));
{$endif GDB}

            hs:=proc_names.get;
          end;

         if make_global or ((procinfo^.flags and pi_is_global) <> 0) then
          aktprocsym^.is_global := True;

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) then
          begin
            if target_os.use_function_relative_addresses then
             exprasmlist^.insert(stab_function_name);
            exprasmlist^.insert(new(pai_stabs,init(aktprocsym^.stabstring)));
            aktprocsym^.isstabwritten:=true;
          end;
{$endif GDB}

       { Align, gprof uses 16 byte granularity }
         if (cs_profile in aktmoduleswitches) then
          exprasmlist^.insert(new(pai_align,init_op(16,$90)))
         else
          if not(cs_littlesize in aktglobalswitches) then
           exprasmlist^.insert(new(pai_align,init(16)));
       end;
      exprasmlist:=oldexprasmlist;
  end;


  procedure handle_return_value(inlined : boolean);
    var
       hr : preference;
       op : Tasmop;
       s : Topsize;
  begin
      if procinfo^.returntype.def<>pdef(voiddef) then
          begin
              {if ((procinfo^.flags and pi_operator)<>0) and
                 assigned(opsym) then
                procinfo^.funcret_is_valid:=
                  procinfo^.funcret_is_valid or (opsym^.refs>0);}
              if (procinfo^.funcret_state<>vs_assigned) and not inlined { and
                ((procinfo^.flags and pi_uses_asm)=0)} then
               CGMessage(sym_w_function_result_not_set);
              hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset);
              if (procinfo^.returntype.def^.deftype in [orddef,enumdef]) then
                begin
                  case procinfo^.returntype.def^.size of
                   8:
                     begin
                        emit_ref_reg(A_MOV,S_L,hr,R_EAX);
                        hr:=new_reference(procinfo^.framepointer,procinfo^.return_offset+4);
                        emit_ref_reg(A_MOV,S_L,hr,R_EDX);
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
                if ret_in_acc(procinfo^.returntype.def) then
                  emit_ref_reg(A_MOV,S_L,hr,R_EAX)
              else
                 if (procinfo^.returntype.def^.deftype=floatdef) then
                   begin
                      floatloadops(pfloatdef(procinfo^.returntype.def)^.typ,op,s);
                      exprasmlist^.concat(new(paicpu,op_ref(op,s,hr)))
                   end
              else
                dispose(hr);
          end
  end;


  procedure genexitcode(alist : paasmoutput;parasize:longint;nostackframe,inlined:boolean);

    var
{$ifdef GDB}
       mangled_length : longint;
       p : pchar;
{$endif GDB}
       nofinal,okexitlabel,noreraiselabel,nodestroycall : pasmlabel;
       hr : treference;
       oldexprasmlist : paasmoutput;
       ai : paicpu;
       pd : pprocdef;

  begin
      oldexprasmlist:=exprasmlist;
      exprasmlist:=alist;

      if aktexitlabel^.is_used then
        exprasmlist^.insert(new(pai_label,init(aktexitlabel)));

      { call the destructor help procedure }
      if (aktprocsym^.definition^.proctypeoption=potype_destructor) and
         assigned(procinfo^._class) then
        begin
          if procinfo^._class^.is_class then
            begin
              emitinsertcall('FPC_DISPOSE_CLASS');
            end
          else
            begin
              emitinsertcall('FPC_HELP_DESTRUCTOR');
{$ifndef noAllocEdi}
              getexplicitregister32(R_EDI);
{$endif noAllocEdi}
              exprasmlist^.insert(new(paicpu,op_const_reg(A_MOV,S_L,procinfo^._class^.vmt_offset,R_EDI)));
              { must the object be finalized ? }
              if procinfo^._class^.needs_inittable then
                begin
                   getlabel(nofinal);
                   exprasmlist^.insert(new(pai_label,init(nofinal)));
                   emitinsertcall('FPC_FINALIZE');
{$ifndef noAllocEdi}
                   ungetregister32(R_EDI);
{$endif noAllocEdi}
                   exprasmlist^.insert(new(paicpu,op_reg(A_PUSH,S_L,R_ESI)));
                   exprasmlist^.insert(new(paicpu,op_sym(A_PUSH,S_L,procinfo^._class^.get_inittable_label)));
                   ai:=new(paicpu,op_sym(A_Jcc,S_NO,nofinal));
                   ai^.SetCondition(C_Z);
                   exprasmlist^.insert(ai);
                   reset_reference(hr);
                   hr.base:=R_EBP;
                   hr.offset:=8;
                   exprasmlist^.insert(new(paicpu,op_const_ref(A_CMP,S_L,0,newreference(hr))));
                end;
            end;
        end;

      { finalize temporary data }
      finalizetempansistrings;

      { finalize local data }
      aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}finalize_data);

      { finalize paras data }
      if assigned(aktprocsym^.definition^.parast) then
        aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}finalize_data);

      { do we need to handle exceptions because of ansi/widestrings ? }
      if (procinfo^.flags and pi_needs_implicit_finally)<>0 then
        begin
           { the exception helper routines modify all registers }
           aktprocsym^.definition^.usedregisters:=$ff;

           getlabel(noreraiselabel);
           emitcall('FPC_POPADDRSTACK');
           exprasmlist^.concat(new(paicpu,
             op_reg(A_POP,S_L,R_EAX)));
           exprasmlist^.concat(new(paicpu,
             op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
           emitjmp(C_E,noreraiselabel);
           if (aktprocsym^.definition^.proctypeoption=potype_constructor) then
             begin
                if assigned(procinfo^._class) then
                  begin
                     pd:=procinfo^._class^.searchdestructor;
                     if assigned(pd) then
                       begin
                          getlabel(nodestroycall);
                          emit_const_ref(A_CMP,S_L,0,new_reference(procinfo^.framepointer,
                            procinfo^.selfpointer_offset));
                          emitjmp(C_E,nodestroycall);
                          if procinfo^._class^.is_class then
                            begin
                               emit_const(A_PUSH,S_L,1);
                               emit_reg(A_PUSH,S_L,R_ESI);
                            end
                          else
                            begin
                               emit_reg(A_PUSH,S_L,R_ESI);
                               emit_sym(A_PUSH,S_L,newasmsymbol(procinfo^._class^.vmt_mangledname));
                            end;
                          if (po_virtualmethod in pd^.procoptions) then
                            begin
                               emit_ref_reg(A_MOV,S_L,new_reference(R_ESI,0),R_EDI);
                               emit_ref(A_CALL,S_NO,new_reference(R_EDI,procinfo^._class^.vmtmethodoffset(pd^.extnumber)));
                            end
                          else
                            emitcall(pd^.mangledname);
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
           if (procinfo^.returntype.def<>pdef(voiddef)) and
             (procinfo^.returntype.def^.needs_inittable) and
             ((procinfo^.returntype.def^.deftype<>objectdef) or
             not(pobjectdef(procinfo^.returntype.def)^.is_class)) then
             begin
                reset_reference(hr);
                hr.offset:=procinfo^.return_offset;
                hr.base:=procinfo^.framepointer;
                finalize(procinfo^.returntype.def,hr,ret_in_param(procinfo^.returntype.def));
             end;

           emitcall('FPC_RERAISE');
           emitlab(noreraiselabel);
        end;

      { call __EXIT for main program }
      if (not DLLsource) and (not inlined) and (aktprocsym^.definition^.proctypeoption=potype_proginit) then
       begin
         emitcall('FPC_DO_EXIT');
       end;

      { handle return value }
      if not(po_assembler in aktprocsym^.definition^.procoptions) then
          if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
            handle_return_value(inlined)
          else
              begin
                  { successful constructor deletes the zero flag }
                  { and returns self in eax                   }
                  { eax must be set to zero if the allocation failed !!! }
                  getlabel(okexitlabel);
                  emitjmp(C_NONE,okexitlabel);
                  emitlab(faillabel);
                  if procinfo^._class^.is_class then
                    begin
                      emit_ref_reg(A_MOV,S_L,new_reference(procinfo^.framepointer,8),R_ESI);
                      emitcall('FPC_HELP_FAIL_CLASS');
                    end
                  else
                    begin
                      emit_ref_reg(A_MOV,S_L,new_reference(procinfo^.framepointer,12),R_ESI);
{$ifndef noAllocEdi}
                       getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                      emit_const_reg(A_MOV,S_L,procinfo^._class^.vmt_offset,R_EDI);
                      emitcall('FPC_HELP_FAIL');
{$ifndef noAllocEdi}
                      ungetregister32(R_EDI);
{$endif noAllocEdi}
                    end;
                  emitlab(okexitlabel);

                  emit_reg_reg(A_MOV,S_L,R_ESI,R_EAX);
                  emit_reg_reg(A_TEST,S_L,R_ESI,R_ESI);
              end;

      { stabs uses the label also ! }
      if aktexit2label^.is_used or
         ((cs_debuginfo in aktmoduleswitches) and not inlined) then
        emitlab(aktexit2label);
      { gives problems for long mangled names }
      {list^.concat(new(pai_symbol,init(aktprocsym^.definition^.mangledname+'_end')));}

      { should we restore edi ? }
      { for all i386 gcc implementations }
      if (po_savestdregs in aktprocsym^.definition^.procoptions) then
        begin
          if (aktprocsym^.definition^.usedregisters and ($80 shr byte(R_EBX)))<>0 then
           exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_EBX)));
          exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_ESI)));
          exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_EDI)));
          { here we could reset R_EBX
            but that is risky because it only works
            if genexitcode is called after genentrycode
            so lets skip this for the moment PM
          aktprocsym^.definition^.usedregisters:=
            aktprocsym^.definition^.usedregisters or not ($80 shr byte(R_EBX));
          }
        end;

      { for the save all registers we can simply use a pusha,popa which
        push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
      if (po_saveregisters in aktprocsym^.definition^.procoptions) then
        begin
          exprasmlist^.concat(new(paicpu,op_none(A_POPA,S_L)));
        end;
      if not(nostackframe) then
        begin
          if not inlined then
            exprasmlist^.concat(new(paicpu,op_none(A_LEAVE,S_NO)));
        end
      else
        begin
          if (gettempsize<>0) and not inlined then
            exprasmlist^.insert(new(paicpu,
              op_const_reg(A_ADD,S_L,gettempsize,R_ESP)));
        end;

      { parameters are limited to 65535 bytes because }
      { ret allows only imm16                    }
      if (parasize>65535) and not(pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
       CGMessage(cg_e_parasize_too_big);

      { at last, the return is generated }

      if not inlined then
      if (po_interrupt in aktprocsym^.definition^.procoptions) then
          generate_interrupt_stackframe_exit
      else
       begin
       {Routines with the poclearstack flag set use only a ret.}
       { also routines with parasize=0     }
         if (pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
           begin
{$ifndef OLD_C_STACK}
             { complex return values are removed from stack in C code PM }
             if ret_in_param(aktprocsym^.definition^.rettype.def) then
               exprasmlist^.concat(new(paicpu,op_const(A_RET,S_NO,4)))
             else
{$endif not OLD_C_STACK}
               exprasmlist^.concat(new(paicpu,op_none(A_RET,S_NO)));
           end
         else if (parasize=0) then
          exprasmlist^.concat(new(paicpu,op_none(A_RET,S_NO)))
         else
          exprasmlist^.concat(new(paicpu,op_const(A_RET,S_NO,parasize)));
       end;

      if not inlined then
        exprasmlist^.concat(new(pai_symbol_end,initname(aktprocsym^.definition^.mangledname)));

{$ifdef GDB}
      if (cs_debuginfo in aktmoduleswitches) and not inlined  then
          begin
              aktprocsym^.concatstabto(exprasmlist);
              if assigned(procinfo^._class) then
                if (not assigned(procinfo^.parent) or
                   not assigned(procinfo^.parent^._class)) then
                  exprasmlist^.concat(new(pai_stabs,init(strpnew(
                   '"$t:v'+procinfo^._class^.numberstring+'",'+
                   tostr(N_PSYM)+',0,0,'+tostr(procinfo^.selfpointer_offset)))))
                else
                  exprasmlist^.concat(new(pai_stabs,init(strpnew(
                   '"$t:r*'+procinfo^._class^.numberstring+'",'+
                   tostr(N_RSYM)+',0,0,'+tostr(GDB_i386index[R_ESI])))));

              { define calling EBP as pseudo local var PM }
              { this enables test if the function is a local one !! }
              if  assigned(procinfo^.parent) and (lexlevel>normal_function_level) then
                exprasmlist^.concat(new(pai_stabs,init(strpnew(
                 '"parent_ebp:'+voidpointerdef^.numberstring+'",'+
                 tostr(N_LSYM)+',0,0,'+tostr(procinfo^.framepointer_offset)))));

              if (pdef(aktprocsym^.definition^.rettype.def) <> pdef(voiddef)) then
                begin
                  if ret_in_param(aktprocsym^.definition^.rettype.def) then
                    exprasmlist^.concat(new(pai_stabs,init(strpnew(
                     '"'+aktprocsym^.name+':X*'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                     tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))))
                  else
                    exprasmlist^.concat(new(pai_stabs,init(strpnew(
                     '"'+aktprocsym^.name+':X'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                     tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))));
                  if (m_result in aktmodeswitches) then
                    if ret_in_param(aktprocsym^.definition^.rettype.def) then
                      exprasmlist^.concat(new(pai_stabs,init(strpnew(
                       '"RESULT:X*'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                       tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))))
                    else
                      exprasmlist^.concat(new(pai_stabs,init(strpnew(
                       '"RESULT:X'+aktprocsym^.definition^.rettype.def^.numberstring+'",'+
                       tostr(N_PSYM)+',0,0,'+tostr(procinfo^.return_offset)))));
                end;
              mangled_length:=length(aktprocsym^.definition^.mangledname);
              getmem(p,2*mangled_length+50);
              strpcopy(p,'192,0,0,');
              strpcopy(strend(p),aktprocsym^.definition^.mangledname);
              if (target_os.use_function_relative_addresses) then
                begin
                  strpcopy(strend(p),'-');
                  strpcopy(strend(p),aktprocsym^.definition^.mangledname);
                end;
              exprasmlist^.concat(new(pai_stabn,init(strnew(p))));
              {list^.concat(new(pai_stabn,init(strpnew('192,0,0,'
               +aktprocsym^.definition^.mangledname))));
              p[0]:='2';p[1]:='2';p[2]:='4';
              strpcopy(strend(p),'_end');}
              strpcopy(p,'224,0,0,'+aktexit2label^.name);
              if (target_os.use_function_relative_addresses) then
                begin
                  strpcopy(strend(p),'-');
                  strpcopy(strend(p),aktprocsym^.definition^.mangledname);
                end;
              exprasmlist^.concatlist(withdebuglist);
              exprasmlist^.concat(new(pai_stabn,init(
                strnew(p))));
               { strpnew('224,0,0,'
               +aktprocsym^.definition^.mangledname+'_end'))));}
              freemem(p,2*mangled_length+50);
          end;
{$endif GDB}
      exprasmlist:=oldexprasmlist;
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
                exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,s,reg,newreference(dest_loc.reference))));
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
  Revision 1.84  2000-03-01 00:03:12  pierre
    * fixes for locals in inlined procedures
      fix for bug797
    + stabs generation for inlined paras and locals

  Revision 1.83  2000/02/18 21:25:48  florian
    * fixed a bug in int64/qword handling was a quite ugly one

  Revision 1.82  2000/02/18 20:53:14  pierre
    * fixes a stabs problem for functions
    + includes a stabs local var for with statements
      the name is with in lowercase followed by an index
      for nested with.
    + Withdebuglist added because the stabs declarations of local
      var are postponed to end of function.

  Revision 1.81  2000/02/10 23:44:43  florian
    * big update for exception handling code generation: possible mem holes
      fixed, break/continue/exit should work always now as expected

  Revision 1.80  2000/02/09 17:36:10  jonas
    * added missing regalloc for ecx in range check code

  Revision 1.79  2000/02/09 13:22:50  peter
    * log truncated

  Revision 1.78  2000/02/04 21:00:31  florian
    * some (small) problems with register saving fixed

  Revision 1.77  2000/02/04 20:00:21  florian
    * an exception in a construcor calls now the destructor (this applies only
      to classes)

  Revision 1.76  2000/02/04 14:29:57  pierre
   + add pseudo local var parent_ebp for local procs

  Revision 1.75  2000/01/25 08:46:03  pierre
   * Range check for int64 produces a warning only

  Revision 1.74  2000/01/24 12:17:22  florian
    * some improvemenst to cmov support
    * disabled excpetion frame generation in cosntructors temporarily

  Revision 1.73  2000/01/23 21:29:14  florian
    * CMOV support in optimizer (in define USECMOV)
    + start of support of exceptions in constructors

  Revision 1.72  2000/01/23 11:11:36  michael
  + Fixes from Jonas.

  Revision 1.71  2000/01/22 16:02:37  jonas
    * fixed more regalloc bugs (for set adding and unsigned
      multiplication)

  Revision 1.70  2000/01/16 22:17:11  peter
    * renamed call_offset to para_offset

  Revision 1.69  2000/01/12 10:38:17  peter
    * smartlinking fixes for binary writer
    * release alignreg code and moved instruction writing align to cpuasm,
      but it doesn't use the specified register yet

  Revision 1.68  2000/01/09 12:35:02  jonas
    * changed edi allocation to use getexplicitregister32/ungetregister
      (adapted tgeni386 a bit for this) and enabled it by default
    * fixed very big and stupid bug of mine in cg386mat that broke the
      include() code (and make cycle :( ) if you compiled without
      -dnewoptimizations

  Revision 1.67  2000/01/09 01:44:21  jonas
    + (de)allocation info for EDI to fix reported bug on mailinglist.
      Also some (de)allocation info for ESI added. Between -dallocEDI
      because at this time of the night bugs could easily slip in ;)

  Revision 1.66  2000/01/07 01:14:22  peter
    * updated copyright to 2000

  Revision 1.65  1999/12/22 01:01:47  peter
    - removed freelabel()
    * added undefined label detection in internal assembler, this prevents
      a lot of ld crashes and wrong .o files
    * .o files aren't written anymore if errors have occured
    * inlining of assembler labels is now correct

  Revision 1.64  1999/12/20 21:42:35  pierre
    + dllversion global variable
    * FPC_USE_CPREFIX code removed, not necessary anymore
      as we use .edata direct writing by default now.

  Revision 1.63  1999/12/01 22:45:54  peter
    * fixed wrong assembler with in-node

  Revision 1.62  1999/11/30 10:40:43  peter
    + ttype, tsymlist

  Revision 1.61  1999/11/20 01:22:18  pierre
    + cond FPC_USE_CPREFIX (needs also some RTL changes)
      this allows to use unit global vars as DLL exports
      (the underline prefix seems needed by dlltool)

  Revision 1.60  1999/11/17 17:04:58  pierre
   * Notes/hints changes

  Revision 1.59  1999/11/15 14:04:00  pierre
   * self pointer stabs for local function was wrong
}