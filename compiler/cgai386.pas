{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

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
       i386base,i386asm,
{$ifdef dummy}
       end { to get correct syntax highlighting }
{$endif dummy}
       aasm,symtable,win_targ;

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
    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
    procedure emitcall(const routine:string);
    procedure emit_mov_loc_ref(const t:tlocation;const ref:treference);
    procedure emit_mov_loc_reg(const t:tlocation;reg:tregister);
    procedure emit_push_loc(const t:tlocation);

    { pushes qword location to the stack }
    procedure emit_pushq_loc(const t : tlocation);
    procedure release_qword_loc(const t : tlocation);

    { releases the registers of a location }
    procedure release_loc(const t : tlocation);

    procedure emit_pushw_loc(const t:tlocation);
    procedure emit_lea_loc_ref(const t:tlocation;const ref:treference);
    procedure emit_push_lea_loc(const t:tlocation);
    procedure emit_to_reference(var p:ptree);
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
    { does the same as restore/maybe_push, but uses temp. space instead of pushing }
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
        ai : Pai386;
      begin
        if c=C_None then
          exprasmlist^.concat(new(pai386,op_sym(A_JMP,S_NO,l)))
        else
          begin
            ai:=new(pai386,op_sym(A_Jcc,S_NO,l));
            ai^.SetCondition(c);
            ai^.is_jmp:=true;
            exprasmlist^.concat(ai);
          end;
      end;
{$else nojmpfix}
    procedure emitjmp(c : tasmcond;var l : pasmlabel);
      var
        ai : Pai386;
      begin
        if c=C_None then
          ai := new(pai386,op_sym(A_JMP,S_NO,l))
        else
          begin
            ai:=new(pai386,op_sym(A_Jcc,S_NO,l));
            ai^.SetCondition(c);
          end;
        ai^.is_jmp:=true;
        exprasmlist^.concat(ai);
      end;
{$endif nojmpfix}

    procedure emit_flag2reg(flag:tresflags;hregister:tregister);
      var
        ai : pai386;
        hreg : tregister;
      begin
         hreg:=makereg8(hregister);
         ai:=new(pai386,op_reg(A_Setcc,S_B,hreg));
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


    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
      begin
         if (reg1<>reg2) or (i<>A_MOV) then
           exprasmlist^.concat(new(pai386,op_reg_reg(i,s,reg1,reg2)));
      end;


    procedure emitcall(const routine:string);
      begin
        exprasmlist^.concat(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol(routine))));
      end;


    procedure emit_mov_loc_ref(const t:tlocation;const ref:treference);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                             t.register,newreference(ref))));
                           ungetregister32(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,
                               t.reference.offset,newreference(ref))))
                           else
                             begin
                               exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                 newreference(t.reference),R_EDI)));
                               exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                 R_EDI,newreference(ref))));
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
                           exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                             t.register,reg)));
                           ungetregister32(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,
                               t.reference.offset,reg)))
                           else
                             begin
                               exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                 newreference(t.reference),reg)));
                             end;
                           ungetiftemp(t.reference);
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
                           exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,RegSize(Reg),
                             reg,t.register)));
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(334)
                           else
                             begin
                               exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,RegSize(Reg),
                                 Reg,newreference(t.reference))));
                             end;
                         end;
        else
         internalerror(330);
        end;
      end;

    procedure emit_movq_reg_loc(reghigh,reglow: TRegister;t:tlocation);
      begin
        case t.loc of
          LOC_REGISTER,
         LOC_CREGISTER : begin
                           exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                             reglow,t.registerlow)));
                           exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                             reghigh,t.registerhigh)));
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(334)
                           else
                             begin
                               exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                 Reglow,newreference(t.reference))));
                               inc(t.reference.offset,4);
                               exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
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
                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,
                   t.registerhigh)));
                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,
                   t.registerlow)));
              end;
            LOC_MEM,
            LOC_REFERENCE:
              begin
                 hr:=newreference(t.reference);
                 inc(hr^.offset,4);
                 exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,
                   hr)));
                 exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,
                   newreference(t.reference))));
                 ungetiftemp(t.reference);
              end;
            else internalerror(331);
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
                           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,makereg32(t.register))));
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,t.reference.offset)))
                           else
                             exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,newreference(t.reference))));
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
                             exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,makereg32(t.register))))
                           else
                             exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,makereg16(t.register))));
                           ungetregister(t.register); { the register is not needed anymore }
                         end;
               LOC_MEM,
         LOC_REFERENCE : begin
                           if target_os.stackalignment=4 then
                            opsize:=S_L
                           else
                            opsize:=S_W;
                           if t.reference.is_immediate then
                             exprasmlist^.concat(new(pai386,op_const(A_PUSH,opsize,t.reference.offset)))
                           else
                             exprasmlist^.concat(new(pai386,op_ref(A_PUSH,opsize,newreference(t.reference))));
                           del_reference(t.reference);
                           ungetiftemp(t.reference);
                         end;
        else
         internalerror(330);
        end;
      end;


    procedure emit_lea_loc_ref(const t:tlocation;const ref:treference);
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
                               exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),R_EDI)));
                               exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                 R_EDI,newreference(ref))));
                             end;
                           ungetiftemp(t.reference);
                         end;
        else
         internalerror(332);
        end;
      end;


    procedure emit_push_lea_loc(const t:tlocation);
      begin
        case t.loc of
               LOC_MEM,
         LOC_REFERENCE : begin
                           if t.reference.is_immediate then
                             internalerror(331)
                           else
                             begin
                               exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                 newreference(t.reference),R_EDI)));
                               exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));
                             end;
                           {   Wrong !!
                           ungetiftemp(t.reference);}
                         end;
        else
         internalerror(332);
        end;
      end;


    procedure emit_to_reference(var p:ptree);
      begin
        case p^.location.loc of
               LOC_FPU : begin
                           reset_reference(p^.location.reference);
                           gettempofsizereference(10,p^.location.reference);
                           floatstore(pfloatdef(p^.resulttype)^.typ,p^.location.reference);
                           p^.location.loc:=LOC_REFERENCE;
                         end;
               LOC_MEM,
         LOC_REFERENCE : ;
        else
         internalerror(333);
        end;
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
               exprasmlist^.concat(new(pai386,op_const_reg(A_AND,S_W,$ff,hr)));
             end;
           R_AH,R_BH,R_CH,R_DH:
             begin
               hr:=reg8toreg16(hr);
               exprasmlist^.concat(new(pai386,op_const_reg(A_AND,S_W,$ff00,hr)));
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
                exprasmlist^.concat(new(pai386,op_const_reg(A_AND,S_L,$ffff,hr)));
             end;
           R_AL,R_BL,R_CL,R_DL:
             begin
                hr:=reg8toreg32(hr);
                exprasmlist^.concat(new(pai386,op_const_reg(A_AND,S_L,$ff,hr)));
             end;
           R_AH,R_BH,R_CH,R_DH:
             begin
                hr:=reg8toreg32(hr);
                exprasmlist^.concat(new(pai386,op_const_reg(A_AND,S_L,$ff00,hr)));
             end;
        end;
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
              exprasmlist^.concat(new(pai386,
                op_sym(A_CALL,S_NO,newasmsymbol('FPC_ANSISTR_DECR_REF'))));
           end
         else if is_widestring(t) then
           begin
              exprasmlist^.concat(new(pai386,
                op_sym(A_CALL,S_NO,newasmsymbol('FPC_WIDESTR_DECR_REF'))));
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
                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.right^.location.register)));
{$Else regallocfix}
                 pushusedregisters(pushed, $ff xor ($80 shr byte(p^.right^.location.register)));
                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.right^.location.register)));
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
              if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                begin
                   if isint64 then
                     begin
{$ifdef TEMPS_NOT_PUSH}
                        gettempofsizereference(href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmlist^.concat(new(pai386,op_reg(A_MOV,S_L,p^.location.registerhigh,href)));
                        href.offset:=href.offset-4;
{$else TEMPS_NOT_PUSH}
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.location.registerhigh)));
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
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,p^.location.register,href)));
{$else TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.location.register)));
{$endif TEMPS_NOT_PUSH}
                   ungetregister32(p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p^.location.reference);
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                       R_EDI)));
{$ifdef TEMPS_NOT_PUSH}
                     gettempofsizereference(href,4);
                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,href)));
                     p^.temp_offset:=href.offset;
{$else TEMPS_NOT_PUSH}
                     exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));
{$endif TEMPS_NOT_PUSH}
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
              if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                begin
                   if isint64(p^.resulttype) then
                     begin
                        gettempofsizereference(href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmlist^.concat(new(pai386,op_reg(A_MOV,S_L,p^.location.registerhigh,href)));
                        href.offset:=href.offset-4;
                        ungetregister32(p^.location.registerhigh);
                     end
                   else
                     begin
                        gettempofsizereference(href,4);
                        p^.temp_offset:=href.offset;
                     end;
                   pushed:=true;
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,p^.location.register,href)));
                   ungetregister32(p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p^.location.reference);
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                       R_EDI)));
                     gettempofsizereference(href,4);
                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,href)));
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
               exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_EDI,R_EDI)));
               exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));
             end
           else
             exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,l)));
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
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(ref),R_EDI)));
                   exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));
                 end
               else exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,newreference(ref))));
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
              exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,ref.offset,newreference(href))));
              emitpushreferenceaddr(href);
              del_reference(href);
           end
         else
           begin
              if ref.segment<>R_NO then
                CGMessage(cg_e_cant_use_far_pointer_there);
              if (ref.base=R_NO) and (ref.index=R_NO) then
                exprasmlist^.concat(new(pai386,op_sym_ofs(A_PUSH,S_L,ref.symbol,ref.offset)))
              else if (ref.base=R_NO) and (ref.index<>R_NO) and
                 (ref.offset=0) and (ref.scalefactor=0) and (ref.symbol=nil) then
                exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,ref.index)))
              else if (ref.base<>R_NO) and (ref.index=R_NO) and
                 (ref.offset=0) and (ref.symbol=nil) then
                exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,ref.base)))
              else
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(ref),R_EDI)));
                   exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));
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
             exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,p^.value)))
           else
             exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_W,p^.value)));
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
                   exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,hr32)))
                 else
                   exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,hr16)));
                 ungetregister32(hr32);
               end;
           else
             begin
               if target_os.stackalignment=4 then
                 exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,newreference(p^.location.reference))))
               else
                 exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_W,newreference(p^.location.reference))));
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
         href.base:=procinfo.frame_pointer;
         href.offset:=p^.temp_offset;
         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,href,hregister)));
{$else  TEMPS_NOT_PUSH}
         exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,hregister)));
{$endif TEMPS_NOT_PUSH}
         if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p^.location.register:=hregister;
              if isint64 then
                begin
                   p^.location.registerhigh:=getregister32;
{$ifdef TEMPS_NOT_PUSH}
                   href.offset:=p^.temp_offset+4;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,p^.location.registerhigh)));
                   { set correctly for release ! }
                   href.offset:=p^.temp_offset;
{$else  TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,p^.location.registerhigh)));
{$endif TEMPS_NOT_PUSH}
                end;
           end
         else
           begin
              reset_reference(p^.location.reference);
              p^.location.reference.index:=hregister;
              set_location(p^.left^.location,p^.location);
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
         href.base:=procinfo.frame_pointer;
         href.offset:=p^.temp_offset;
         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,href,hregister)));
         if (p^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p^.location.register:=hregister;
              if isint64 then
                begin
                   p^.location.registerhigh:=getregister32;
                   href.offset:=p^.temp_offset+4;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,p^.location.registerhigh)));
                   { set correctly for release ! }
                   href.offset:=p^.temp_offset;
                end;
           end
         else
           begin
              reset_reference(p^.location.reference);
              p^.location.reference.index:=hregister;
              set_location(p^.left^.location,p^.location);
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
                          inc(pushedparasize,4);
                          if inlined then
                            begin
                               r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                               exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                 p^.location.register,r)));
                            end
                          else
                            exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.location.register)));
                          ungetregister32(p^.location.register);
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
                              r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,opsize,hreg,r)));
                            end
                          else
                            exprasmlist^.concat(new(pai386,op_reg(A_PUSH,opsize,hreg)));
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
                              r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,opsize,hreg,r)));
                            end
                          else
                            exprasmlist^.concat(new(pai386,op_reg(A_PUSH,opsize,hreg)));
                          ungetregister32(reg8toreg32(p^.location.register));
                        end;
                  end;
               end;
             LOC_FPU:
               begin
                  size:=align(pfloatdef(p^.resulttype)^.size,alignment);
                  inc(pushedparasize,size);
                  if not inlined then
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,S_L,size,R_ESP)));
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
                       r^.base:=procinfo.framepointer;
                       r^.offset:=para_offset-pushedparasize;
                    end;
                  exprasmlist^.concat(new(pai386,op_ref(op,opsize,r)));
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
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI)));
                                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                   inc(tempreference.offset,4);
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI)));
                                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize+4);
                                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI)));
                                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,
                                    newreference(tempreference),hreg)));
                                  r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                  exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,opsize,hreg,r)));
                                end
                               else
                                exprasmlist^.concat(new(pai386,op_ref(A_PUSH,opsize,
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
                                    exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                      newreference(tempreference),R_EDI)));
                                    r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                    exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI)));
                                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                end
                              else
                                emit_push_mem(tempreference);
                              inc(pushedparasize,4);
                              dec(tempreference.offset,4);
                              if inlined then
                                begin
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI)));
                                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI)));
                                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                end
                              else
                                emit_push_mem(tempreference);
                              dec(tempreference.offset,4);
                              inc(pushedparasize,4);
                              if inlined then
                                begin
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(tempreference),R_EDI)));
                                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,
                                     newreference(tempreference),hreg)));
                                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,opsize,hreg,r)));
                                end
                              else
                                exprasmlist^.concat(new(pai386,op_ref(A_PUSH,opsize,
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
                              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                newreference(tempreference),R_EDI)));
                              r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                              exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                             pobjectdef(p^.resulttype)^.isclass) then
                           begin
                              inc(pushedparasize,4);
                              if inlined then
                                begin
                                  r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                  concatcopy(tempreference,r^,4,false,false);
                                end
                              else
                                emit_push_mem(tempreference);
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
                       r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,opsize,1,r)));
                    end
                  else
                    exprasmlist^.concat(new(pai386,op_const(A_PUSH,opsize,1)));
                  emitjmp(C_None,hlabel);
                  emitlab(falselabel);
                  if inlined then
                    begin
                       r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,opsize,0,r)));
                    end
                  else
                    exprasmlist^.concat(new(pai386,op_const(A_PUSH,opsize,0)));
                  emitlab(hlabel);
               end;
             LOC_FLAGS:
               begin
                  if not(R_EAX in unused) then
                    exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_EAX,R_EDI)));
                  emit_flag2reg(p^.location.resflags,R_AL);
                  exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BW,R_AL,R_AX)));
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
                       r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,opsize,hreg,r)));
                    end
                  else
                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,opsize,hreg)));
                  if not(R_EAX in unused) then
                    exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_EDI,R_EAX)));
               end;
{$ifdef SUPPORT_MMX}
             LOC_MMXREGISTER,
             LOC_CMMXREGISTER:
               begin
                  inc(pushedparasize,8); { was missing !!! (PM) }
                  exprasmlist^.concat(new(pai386,op_const_reg(
                    A_SUB,S_L,8,R_ESP)));
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmlist^.first=exprasmlist^.last) then
                    exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                  if inlined then
                    begin
                       r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(pai386,op_reg_ref(A_MOVQ,S_NO,
                         p^.location.register,r)));
                    end
                  else
                     begin
                        r:=new_reference(R_ESP,0);
                        exprasmlist^.concat(new(pai386,op_reg_ref(
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
         exprasmlist^.concat(new(pai386,op_ref(op,s,
           newreference(ref))));
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
         exprasmlist^.concat(new(pai386,op_ref(op,s,
           newreference(ref))));
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
                                        exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,opsize,p^.location.register,
                                          p^.location.register)));
                                        ungetregister(p^.location.register);
                                        emitjmp(C_NZ,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_MEM,LOC_REFERENCE : begin
                                        exprasmlist^.concat(new(pai386,op_const_ref(
                                          A_CMP,opsize,0,newreference(p^.location.reference))));
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
         internalerror(28699);
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
                 exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));
                 popecx:=true;
               end;
              if is_reg then
               exprasmlist^.concat(new(pai386,op_reg_reg(op,opsize,p^.location.register,R_ECX)))
              else
               exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,newreference(p^.location.reference),R_ECX)));
            end;
           if doublebound then
            begin
              getlabel(neglabel);
              getlabel(poslabel);
              exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,R_ECX,R_ECX)));
              emitjmp(C_L,neglabel);
            end;
           { insert bound instruction only }
           exprasmlist^.concat(new(pai386,op_sym_ofs_reg(A_MOV,S_L,newasmsymbol(rstr),0,R_EDI)));
           emitcall('FPC_BOUNDCHECK');
           { u32bit needs 2 checks }
           if doublebound then
            begin
              emitjmp(C_None,poslabel);
              emitlab(neglabel);
              exprasmlist^.concat(new(pai386,op_sym_ofs_reg(A_MOV,S_L,newasmsymbol(rstr),8,R_EDI)));
              emitcall('FPC_BOUNDCHECK');
              emitlab(poslabel);
            end;
           if popecx then
            exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));
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
                 exprasmlist^.concat(new(pai386,op_reg_reg(op,opsize,p^.location.register,R_EDI)));
                 hreg:=R_EDI;
               end;
            end
           else
            begin
              exprasmlist^.concat(new(pai386,op_ref_reg(op,opsize,newreference(p^.location.reference),R_EDI)));
              hreg:=R_EDI;
            end;
           if doublebound then
            begin
              getlabel(neglabel);
              getlabel(poslabel);
              exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,hreg,hreg)));
              emitjmp(C_L,neglabel);
            end;
           { insert bound instruction only }
           exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hreg,newreference(href))));
           { u32bit needs 2 checks }
           if doublebound then
            begin
              href.offset:=8;
              emitjmp(C_None,poslabel);
              emitlab(neglabel);
              exprasmlist^.concat(new(pai386,op_reg_ref(A_BOUND,S_L,hreg,newreference(href))));
              emitlab(poslabel);
            end;
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
               exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));
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
              for i:=1 to helpsize do
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(source),R_EDI)));
{$ifdef regallocfix}
                   If (size = 4) and delsource then
                     del_reference(source);
{$endif regallocfix}
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,newreference(dest))));
                   inc(source.offset,4);
                   inc(dest.offset,4);
                   dec(size,4);
                end;
              if size>1 then
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,newreference(source),R_DI)));
{$ifdef regallocfix}
                   If (size = 2) and delsource then
                     del_reference(source);
{$endif regallocfix}
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,R_DI,newreference(dest))));
                   inc(source.offset,2);
                   inc(dest.offset,2);
                   dec(size,2);
                end;
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
                     emit_reg_reg(A_MOV,S_L,reg32,R_EDI);
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_B,newreference(source),reg8)));
{$ifdef regallocfix}
                   If delsource then
                     del_reference(source);
{$endif regallocfix}
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_B,reg8,newreference(dest))));
                   if swap then
                     emit_reg_reg(A_MOV,S_L,R_EDI,reg32);
                end;
           end
         else
           begin
              exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(dest),R_EDI)));
{$ifdef regallocfix}
             {is this ok?? (JM)}
              del_reference(dest);
{$endif regallocfix}
              if loadref then
                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(source),R_ESI)))
              else
                begin
                  exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(source),R_ESI)));
{$ifdef regallocfix}
                  if delsource then
                    del_reference(source);
{$endif regallocfix}
                end;

              exprasmlist^.concat(new(pai386,op_none(A_CLD,S_NO)));
              ecxpushed:=false;
              if cs_littlesize in aktglobalswitches  then
                begin
                   maybepushecx;
                   exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,size,R_ECX)));
                   exprasmlist^.concat(new(pai386,op_none(A_REP,S_NO)));
                   exprasmlist^.concat(new(pai386,op_none(A_MOVSB,S_NO)));
                end
              else
                begin
                   helpsize:=size shr 2;
                   size:=size and 3;
                   if helpsize>1 then
                    begin
                      maybepushecx;
                      exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,helpsize,R_ECX)));
                      exprasmlist^.concat(new(pai386,op_none(A_REP,S_NO)));
                    end;
                   if helpsize>0 then
                    exprasmlist^.concat(new(pai386,op_none(A_MOVSD,S_NO)));
                   if size>1 then
                     begin
                        dec(size,2);
                        exprasmlist^.concat(new(pai386,op_none(A_MOVSW,S_NO)));
                     end;
                   if size=1 then
                     exprasmlist^.concat(new(pai386,op_none(A_MOVSB,S_NO)));
                end;
              if ecxpushed then
                exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));

              { loading SELF-reference again }
              maybe_loadesi;
           end;
         if delsource then
           ungetiftemp(source);
      end;


    procedure emitloadord2reg(const location:Tlocation;orddef:Porddef;
                              destreg:Tregister;delloc:boolean);

    {A lot smaller and less bug sensitive than the original unfolded loads.}

    var tai:Pai386;
        r:Preference;

    begin
        case location.loc of
            LOC_REGISTER,LOC_CREGISTER:
                begin
                    case orddef^.typ of
                        u8bit:
                            tai:=new(pai386,op_reg_reg(A_MOVZX,S_BL,location.register,destreg));
                        s8bit:
                            tai:=new(pai386,op_reg_reg(A_MOVSX,S_BL,location.register,destreg));
                        u16bit:
                            tai:=new(pai386,op_reg_reg(A_MOVZX,S_WL,location.register,destreg));
                        s16bit:
                            tai:=new(pai386,op_reg_reg(A_MOVSX,S_WL,location.register,destreg));
                        u32bit:
                            tai:=new(pai386,op_reg_reg(A_MOV,S_L,location.register,destreg));
                        s32bit:
                            tai:=new(pai386,op_reg_reg(A_MOV,S_L,location.register,destreg));
                    end;
                    if delloc then
                        ungetregister(location.register);
                end;
            LOC_MEM,
            LOC_REFERENCE:
                begin
                    if location.reference.is_immediate then
                     tai:=new(pai386,op_const_reg(A_MOV,S_L,location.reference.offset,destreg))
                    else
                     begin
                       r:=newreference(location.reference);
                       case orddef^.typ of
                         u8bit:
                            tai:=new(pai386,op_ref_reg(A_MOVZX,S_BL,r,destreg));
                         s8bit:
                            tai:=new(pai386,op_ref_reg(A_MOVSX,S_BL,r,destreg));
                         u16bit:
                            tai:=new(pai386,op_ref_reg(A_MOVZX,S_WL,r,destreg));
                         s16bit:
                            tai:=new(pai386,op_ref_reg(A_MOVSX,S_WL,r,destreg));
                         u32bit:
                            tai:=new(pai386,op_ref_reg(A_MOV,S_L,r,destreg));
                         s32bit:
                            tai:=new(pai386,op_ref_reg(A_MOV,S_L,r,destreg));
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
         if assigned(procinfo._class) then
           begin
              if lexlevel>normal_function_level then
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo.framepointer_offset;
                   hp^.base:=procinfo.framepointer;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,R_ESI)));
                   p:=procinfo.parent;
                   for i:=3 to lexlevel-1 do
                     begin
                        new(hp);
                        reset_reference(hp^);
                        hp^.offset:=p^.framepointer_offset;
                        hp^.base:=R_ESI;
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,R_ESI)));
                        p:=p^.parent;
                     end;
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=p^.ESI_offset;
                   hp^.base:=R_ESI;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,R_ESI)));
                end
              else
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo.ESI_offset;
                   hp^.base:=procinfo.framepointer;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,R_ESI)));
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
      if (aktprocsym^.definition^.options and poassembler)<>0 then
       exit;
      case target_info.target of
         target_i386_linux:
           begin
              getlabel(pl);
              emitcall('mcount');
              exprasmlist^.insert(new(pai386,op_sym_ofs_reg(A_MOV,S_L,pl,0,R_EDX)));
              exprasmlist^.insert(new(pai_section,init(sec_code)));
              exprasmlist^.insert(new(pai_const,init_32bit(0)));
              exprasmlist^.insert(new(pai_label,init(pl)));
              exprasmlist^.insert(new(pai_align,init(4)));
              exprasmlist^.insert(new(pai_section,init(sec_data)));
           end;

         target_i386_go32v2:
           begin
              exprasmlist^.insert(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol('MCOUNT'))));
           end;
      end;
    end;


    procedure generate_interrupt_stackframe_entry;
      begin
         { save the registers of an interrupt procedure }
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EAX)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EBX)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_ECX)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EDX)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));

         { .... also the segment registers }
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_W,R_DS)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_W,R_ES)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_W,R_FS)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_W,R_GS)));
      end;


    procedure generate_interrupt_stackframe_exit;
      begin
         { restore the registers of an interrupt procedure }
         { this was all with entrycode instead of exitcode !!}
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_EAX)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_EBX)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_ECX)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_EDX)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_ESI)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));

         { .... also the segment registers }
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_W,R_DS)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_W,R_ES)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_W,R_FS)));
         procinfo.aktexitcode^.concat(new(pai386,op_reg(A_POP,S_W,R_GS)));

        { this restores the flags }
         procinfo.aktexitcode^.concat(new(pai386,op_none(A_IRET,S_NO)));
      end;


  { generates the code for threadvar initialisation }
  procedure initialize_threadvar(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
         ((pvarsym(p)^.var_options and vo_is_thread_var)<>0) then
         begin
            exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,pvarsym(p)^.getsize)));
            reset_reference(hr);
            hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
            emitpushreferenceaddr(hr);
            exprasmlist^.concat(new(pai386,
              op_sym(A_CALL,S_NO,newasmsymbol('FPC_INIT_THREADVAR'))));
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
              exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,0,
                newreference(ref))));
           end
         else
           begin
              reset_reference(hr);
              hr.symbol:=t^.get_inittable_label;
              emitpushreferenceaddr(hr);
              if is_already_ref then
                exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,
                  newreference(ref))))
              else
                emitpushreferenceaddr(ref);
              exprasmlist^.concat(new(pai386,
                op_sym(A_CALL,S_NO,newasmsymbol('FPC_INITIALIZE'))));
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
                exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,
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
          assigned(pvarsym(p)^.definition) and
          pvarsym(p)^.definition^.needs_inittable and
          not((pvarsym(p)^.definition^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.definition)^.isclass) then
         begin
            procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            if psym(p)^.owner^.symtabletype=localsymtable then
              begin
                 hr.base:=procinfo.framepointer;
                 hr.offset:=-pvarsym(p)^.address;
              end
            else
              begin
                 hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
              end;
            initialize(pvarsym(p)^.definition,hr,false);
         end;
    end;

  { generates the code for incrementing the reference count of parameters }
  procedure incr_data(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          pvarsym(p)^.definition^.needs_inittable and
          ((pvarsym(p)^.varspez=vs_value) {or
           (pvarsym(p)^.varspez=vs_const) and
           not(dont_copy_const_param(pvarsym(p)^.definition))}) and
          not((pvarsym(p)^.definition^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.definition)^.isclass) then
         begin
            procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            hr.symbol:=pvarsym(p)^.definition^.get_inittable_label;
            emitpushreferenceaddr(hr);
            reset_reference(hr);
            hr.base:=procinfo.framepointer;
            hr.offset:=pvarsym(p)^.address+procinfo.call_offset;

            emitpushreferenceaddr(hr);
            reset_reference(hr);

            exprasmlist^.concat(new(pai386,
              op_sym(A_CALL,S_NO,newasmsymbol('FPC_ADDREF'))));
         end;
    end;

  { generates the code for finalisation of local data }
  procedure finalize_data(p : pnamedindexobject);{$ifndef FPC}far;{$endif}

    var
       hr : treference;

    begin
       if (psym(p)^.typ=varsym) and
          assigned(pvarsym(p)^.definition) and
          pvarsym(p)^.definition^.needs_inittable and
          not((pvarsym(p)^.definition^.deftype=objectdef) and
            pobjectdef(pvarsym(p)^.definition)^.isclass) then
         begin
            { not all kind of parameters need to be finalized  }
            if (psym(p)^.owner^.symtabletype=parasymtable) and
              ((pvarsym(p)^.varspez=vs_var)  or
               (pvarsym(p)^.varspez=vs_const) { and
               (dont_copy_const_param(pvarsym(p)^.definition)) } ) then
              exit;
            procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
            reset_reference(hr);
            case psym(p)^.owner^.symtabletype of
               localsymtable:
                 begin
                    hr.base:=procinfo.framepointer;
                    hr.offset:=-pvarsym(p)^.address;
                 end;
               parasymtable:
                 begin
                    hr.base:=procinfo.framepointer;
                    hr.offset:=pvarsym(p)^.address+procinfo.call_offset;
                 end;
               else
                 hr.symbol:=newasmsymbol(pvarsym(p)^.mangledname);
            end;
            finalize(pvarsym(p)^.definition,hr,false);
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
          (push_addr_param(pvarsym(p)^.definition)) then
        begin
          if is_open_array(pvarsym(p)^.definition) or
             is_array_of_const(pvarsym(p)^.definition) then
           begin
              { get stack space }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo.framepointer;
              r^.offset:=pvarsym(p)^.address+4+procinfo.call_offset;
              exprasmlist^.concat(new(pai386,
                op_ref_reg(A_MOV,S_L,r,R_EDI)));

              exprasmlist^.concat(new(pai386,
                op_reg(A_INC,S_L,R_EDI)));

              exprasmlist^.concat(new(pai386,
                op_const_reg(A_IMUL,S_L,
                parraydef(pvarsym(p)^.definition)^.definition^.size,R_EDI)));
              { windows guards only a few pages for stack growing, }
              { so we have to access every page first              }
              if target_os.id=os_i386_win32 then
                begin
                   getlabel(again);
                   getlabel(ok);
                   emitlab(again);
                   exprasmlist^.concat(new(pai386,
                     op_const_reg(A_CMP,S_L,winstackpagesize,R_EDI)));
                   emitjmp(C_C,ok);
                   exprasmlist^.concat(new(pai386,
                     op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP)));
                   exprasmlist^.concat(new(pai386,
                     op_reg(A_PUSH,S_L,R_EAX)));
                   exprasmlist^.concat(new(pai386,
                     op_const_reg(A_SUB,S_L,winstackpagesize,R_EDI)));
                   emitjmp(C_None,again);

                   emitlab(ok);
                   exprasmlist^.concat(new(pai386,
                     op_reg_reg(A_SUB,S_L,R_EDI,R_ESP)));
                   { now reload EDI }
                   new(r);
                   reset_reference(r^);
                   r^.base:=procinfo.framepointer;
                   r^.offset:=pvarsym(p)^.address+4+procinfo.call_offset;
                   exprasmlist^.concat(new(pai386,
                     op_ref_reg(A_MOV,S_L,r,R_EDI)));

                   exprasmlist^.concat(new(pai386,
                     op_reg(A_INC,S_L,R_EDI)));

                   exprasmlist^.concat(new(pai386,
                     op_const_reg(A_IMUL,S_L,
                     parraydef(pvarsym(p)^.definition)^.definition^.size,R_EDI)));
                end
              else
                begin
                   exprasmlist^.concat(new(pai386,
                     op_reg_reg(A_SUB,S_L,R_EDI,R_ESP)));
                   { load destination }
                   exprasmlist^.concat(new(pai386,
                     op_reg_reg(A_MOV,S_L,R_ESP,R_EDI)));
                end;

              { don't destroy the registers! }
              exprasmlist^.concat(new(pai386,
                op_reg(A_PUSH,S_L,R_ECX)));
              exprasmlist^.concat(new(pai386,
                op_reg(A_PUSH,S_L,R_ESI)));

              { load count }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo.framepointer;
              r^.offset:=pvarsym(p)^.address+4+procinfo.call_offset;
              exprasmlist^.concat(new(pai386,
                op_ref_reg(A_MOV,S_L,r,R_ECX)));

              { load source }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo.framepointer;
              r^.offset:=pvarsym(p)^.address+procinfo.call_offset;
              exprasmlist^.concat(new(pai386,
                op_ref_reg(A_MOV,S_L,r,R_ESI)));

              { scheduled .... }
              exprasmlist^.concat(new(pai386,
                op_reg(A_INC,S_L,R_ECX)));

              { calculate size }
              len:=parraydef(pvarsym(p)^.definition)^.definition^.size;
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

              exprasmlist^.concat(new(pai386,
                op_const_reg(A_IMUL,S_L,len,R_ECX)));
              exprasmlist^.concat(new(pai386,
                op_none(A_REP,S_NO)));
              case opsize of
                S_B : exprasmlist^.concat(new(pai386,op_none(A_MOVSB,S_NO)));
                S_W : exprasmlist^.concat(new(pai386,op_none(A_MOVSW,S_NO)));
                S_L : exprasmlist^.concat(new(pai386,op_none(A_MOVSD,S_NO)));
              end;
              exprasmlist^.concat(new(pai386,
                op_reg(A_POP,S_L,R_ESI)));
              exprasmlist^.concat(new(pai386,
                op_reg(A_POP,S_L,R_ECX)));

              { patch the new address }
              new(r);
              reset_reference(r^);
              r^.base:=procinfo.framepointer;
              r^.offset:=pvarsym(p)^.address+procinfo.call_offset;
              exprasmlist^.concat(new(pai386,
                op_reg_ref(A_MOV,S_L,R_ESP,r)));
           end
          else
           if is_shortstring(pvarsym(p)^.definition) then
            begin
              reset_reference(href1);
              href1.base:=procinfo.framepointer;
              href1.offset:=pvarsym(p)^.address+procinfo.call_offset;
              reset_reference(href2);
              href2.base:=procinfo.framepointer;
              href2.offset:=-pvarsym(p)^.localvarsym^.address;
              copyshortstring(href2,href1,pstringdef(pvarsym(p)^.definition)^.len,true);
            end
           else
            begin
              reset_reference(href1);
              href1.base:=procinfo.framepointer;
              href1.offset:=pvarsym(p)^.address+procinfo.call_offset;
              reset_reference(href2);
              href2.base:=procinfo.framepointer;
              href2.offset:=-pvarsym(p)^.localvarsym^.address;
              concatcopy(href1,href2,pvarsym(p)^.definition^.size,true,true);
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
              procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
              new(r);
              reset_reference(r^);
              r^.base:=procinfo.framepointer;
              r^.offset:=hp^.pos;
              exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,0,r)));
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
                 procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
                 reset_reference(hr);
                 hr.base:=procinfo.framepointer;
                 hr.offset:=hp^.pos;
                 emitpushreferenceaddr(hr);
                 exprasmlist^.concat(new(pai386,
                   op_sym(A_CALL,S_NO,newasmsymbol('FPC_ANSISTR_DECR_REF'))));
              end;
            hp:=hp^.next;
         end;
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
       if (not inlined) and ((aktprocsym^.definition^.options and poproginit)<>0) then
           begin
              exprasmlist^.insert(new(pai386,
                op_sym(A_CALL,S_NO,newasmsymbol('FPC_INITIALIZEUNITS'))));
              if target_info.target=target_I386_WIN32 then
                begin
                   new(hr);
                   reset_reference(hr^);
                   hr^.symbol:=newasmsymbol('U_SYSWIN32_ISCONSOLE');
                   if apptype=at_cui then
                     exprasmlist^.insert(new(pai386,op_const_ref(A_MOV,S_B,
                       1,hr)))
                   else
                     exprasmlist^.insert(new(pai386,op_const_ref(A_MOV,S_B,
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

      { a constructor needs a help procedure }
      if (aktprocsym^.definition^.options and poconstructor)<>0 then
        begin
          if procinfo._class^.isclass then
            begin
              exprasmlist^.insert(new(pai386,op_cond_sym(A_Jcc,C_Z,S_NO,quickexitlabel)));
              exprasmlist^.insert(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol('FPC_NEW_CLASS'))));
            end
          else
            begin
              exprasmlist^.insert(new(pai386,op_cond_sym(A_Jcc,C_Z,S_NO,quickexitlabel)));
              exprasmlist^.insert(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol('FPC_HELP_CONSTRUCTOR'))));
              exprasmlist^.insert(new(pai386,op_const_reg(A_MOV,S_L,procinfo._class^.vmt_offset,R_EDI)));
            end;
        end;

      { don't load ESI, does the caller }

      { When message method contains self as a parameter,
        we must load it into ESI }
      If ((aktprocsym^.definition^.options and pocontainsself)<>0) then
        begin
           new(hr);
           reset_reference(hr^);
           hr^.offset:=procinfo.ESI_offset;
           hr^.base:=procinfo.framepointer;
           exprasmlist^.insert(new(pai386,op_ref_reg(A_MOV,S_L,hr,R_ESI)));
        end;
      { should we save edi ? }
      if ((aktprocsym^.definition^.options and posavestdregs)<>0) then
       begin
         if (aktprocsym^.definition^.usedregisters and ($80 shr byte(R_EBX)))<>0 then
           exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EBX)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
         exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EDI)));
       end;


      { omit stack frame ? }
      if not inlined then
      if procinfo.framepointer=stack_pointer then
          begin
              CGMessage(cg_d_stackframe_omited);
              nostackframe:=true;
              if (aktprocsym^.definition^.options and (pounitinit or poproginit or pounitfinalize)<>0) then
                parasize:=0
              else
                parasize:=aktprocsym^.definition^.parast^.datasize+procinfo.call_offset-4;
          end
      else
          begin
              if (aktprocsym^.definition^.options and (pounitinit or poproginit or pounitfinalize)<>0) then
                parasize:=0
              else
                parasize:=aktprocsym^.definition^.parast^.datasize+procinfo.call_offset-8;
              nostackframe:=false;
              if stackframe<>0 then
                  begin
{$ifdef unused}
                      if (cs_littlesize in aktglobalswitches) and (stackframe<=65535) then
                          begin
                              if (cs_check_stack in aktlocalswitches) and
                                 not(target_info.target in [target_i386_linux,target_i386_win32]) then
                                begin
                                  exprasmlist^.insert(new(pai386,
                                    op_sym(A_CALL,S_NO,newasmsymbol('FPC_STACKCHECK'))));
                                  exprasmlist^.insert(new(pai386,op_const(A_PUSH,S_L,stackframe)));
                                end;
                              if cs_profile in aktmoduleswitches then
                                genprofilecode;

                            { %edi is already saved when pocdecl is used
                              if (target_info.target=target_linux) and
                               ((aktprocsym^.definition^.options and poexports)<>0) then
                                  exprasmlist^.insert(new(Pai386,op_reg(A_PUSH,S_L,R_EDI))); }

                              exprasmlist^.insert(new(pai386,op_const_const(A_ENTER,S_NO,stackframe,0)))
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
                                       exprasmlist^.insert(new(pai386,op_const_reg(A_SUB,S_L,stackframe-4,R_ESP)));
                                       for i:=1 to stackframe div winstackpagesize do
                                         begin
                                            hr:=new_reference(R_ESP,stackframe-i*winstackpagesize);
                                            exprasmlist^.concat(new(pai386,
                                              op_const_ref(A_MOV,S_L,0,hr)));
                                         end;
                                       exprasmlist^.concat(new(pai386,
                                         op_reg(A_PUSH,S_L,R_EAX)));
                                    end
                                  else
                                    begin
                                       getlabel(again);
                                       exprasmlist^.concat(new(pai386,
                                         op_const_reg(A_MOV,S_L,stackframe div winstackpagesize,R_EDI)));
                                       emitlab(again);
                                       exprasmlist^.concat(new(pai386,
                                         op_const_reg(A_SUB,S_L,winstackpagesize-4,R_ESP)));
                                       exprasmlist^.concat(new(pai386,
                                         op_reg(A_PUSH,S_L,R_EAX)));
                                       exprasmlist^.concat(new(pai386,
                                         op_reg(A_DEC,S_L,R_EDI)));
                                       emitjmp(C_NZ,again);
                                       exprasmlist^.concat(new(pai386,
                                         op_const_reg(A_SUB,S_L,stackframe mod winstackpagesize,R_ESP)));
                                    end
                              end
                            else
                              exprasmlist^.insert(new(pai386,op_const_reg(A_SUB,S_L,stackframe,R_ESP)));
                            if (cs_check_stack in aktlocalswitches) and
                              not(target_info.target in [target_i386_linux,target_i386_win32]) then
                              begin
                                 exprasmlist^.insert(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol('FPC_STACKCHECK'))));
                                 exprasmlist^.insert(new(pai386,op_const(A_PUSH,S_L,stackframe)));
                              end;
                            if cs_profile in aktmoduleswitches then
                              genprofilecode;
                            exprasmlist^.insert(new(pai386,op_reg_reg(A_MOV,S_L,R_ESP,R_EBP)));
                            exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EBP)));
                          end;
                  end { endif stackframe <> 0 }
              else
                 begin
                   if cs_profile in aktmoduleswitches then
                     genprofilecode;
                   exprasmlist^.insert(new(pai386,op_reg_reg(A_MOV,S_L,R_ESP,R_EBP)));
                   exprasmlist^.insert(new(pai386,op_reg(A_PUSH,S_L,R_EBP)));
                 end;
          end;

      if (aktprocsym^.definition^.options and pointerrupt)<>0 then
          generate_interrupt_stackframe_entry;

      { initialize return value }
      if (procinfo.retdef<>pdef(voiddef)) and
        (procinfo.retdef^.needs_inittable) and
        ((procinfo.retdef^.deftype<>objectdef) or
        not(pobjectdef(procinfo.retdef)^.isclass)) then
        begin
           procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
           reset_reference(r);
           r.offset:=procinfo.retoffset;
           r.base:=procinfo.framepointer;
           initialize(procinfo.retdef,r,ret_in_param(procinfo.retdef));
        end;

      { generate copies of call by value parameters }
      if (aktprocsym^.definition^.options and poassembler=0) then
        aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}copyvalueparas);

      { initialisizes local data }
      aktprocsym^.definition^.localst^.foreach({$ifndef TP}@{$endif}initialize_data);
      { add a reference to all call by value/const parameters }
      aktprocsym^.definition^.parast^.foreach({$ifndef TP}@{$endif}incr_data);

      { initilisizes temp. ansi/wide string data }
      inittempansistrings;

      { do we need an exception frame because of ansi/widestrings ? }
      if (procinfo.flags and pi_needs_implicit_finally)<>0 then
        begin
            usedinproc:=usedinproc or ($80 shr byte(R_EAX));

            { Type of stack-frame must be pushed}
            exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,1)));
            emitcall('FPC_PUSHEXCEPTADDR');
            exprasmlist^.concat(new(pai386,
              op_reg(A_PUSH,S_L,R_EAX)));
            emitcall('FPC_SETJMP');
            exprasmlist^.concat(new(pai386,
              op_reg(A_PUSH,S_L,R_EAX)));
            exprasmlist^.concat(new(pai386,
              op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
            emitjmp(C_NE,aktexitlabel);
        end;


      if (cs_profile in aktmoduleswitches) or
         (aktprocsym^.definition^.owner^.symtabletype=globalsymtable) or
         (assigned(procinfo._class) and (procinfo._class^.owner^.symtabletype=globalsymtable)) then
           make_global:=true;

      if not inlined then
       begin
         hs:=proc_names.get;

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) then
           exprasmlist^.insert(new(pai_force_line,init));

         if (cs_debuginfo in aktmoduleswitches) and target_os.use_function_relative_addresses then
           stab_function_name := new(pai_stab_function_name,init(strpnew(hs)));
{$EndIf GDB}

         while hs<>'' do
          begin
            if make_global then
              exprasmlist^.insert(new(pai_symbol,initname_global(hs)))
            else
              exprasmlist^.insert(new(pai_symbol,initname(hs)));

{$ifdef GDB}
            if (cs_debuginfo in aktmoduleswitches) and
               target_os.use_function_relative_addresses then
              exprasmlist^.insert(new(pai_stab_function_name,init(strpnew(hs))));
{$endif GDB}

            hs:=proc_names.get;
          end;
       end;

{$ifdef GDB}
      if (not inlined) and (cs_debuginfo in aktmoduleswitches) then
       begin
         if target_os.use_function_relative_addresses then
           exprasmlist^.insert(stab_function_name);
         if make_global or ((procinfo.flags and pi_is_global) <> 0) then
           aktprocsym^.is_global := True;
         exprasmlist^.insert(new(pai_stabs,init(aktprocsym^.stabstring)));
         aktprocsym^.isstabwritten:=true;
       end;
{$endif GDB}

   { Align }
      if (not inlined) then
       begin
       { gprof uses 16 byte granularity !! }
         if (cs_profile in aktmoduleswitches) then
          exprasmlist^.insert(new(pai_align,init_op(16,$90)))
         else
          if not(cs_littlesize in aktglobalswitches) then
           exprasmlist^.insert(new(pai_align,init(4)));
       end;
      exprasmlist:=oldexprasmlist;
  end;


  procedure handle_return_value(inlined : boolean);
    var
       hr : preference;
       op : Tasmop;
       s : Topsize;
  begin
      if procinfo.retdef<>pdef(voiddef) then
          begin
              if ((procinfo.flags and pi_operator)<>0) and
                 assigned(opsym) then
                procinfo.funcret_is_valid:=
                  procinfo.funcret_is_valid or (opsym^.refs>0);
              if not(procinfo.funcret_is_valid) and not inlined { and
                ((procinfo.flags and pi_uses_asm)=0)} then
               CGMessage(sym_w_function_result_not_set);
              hr:=new_reference(procinfo.framepointer,procinfo.retoffset);
              if (procinfo.retdef^.deftype in [orddef,enumdef]) then
                begin
                  case procinfo.retdef^.size of
                   8:
                     begin
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hr,R_EAX)));
                        hr:=new_reference(procinfo.framepointer,procinfo.retoffset+4);
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hr,R_EDX)));
                     end;

                   4:
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hr,R_EAX)));

                   2:
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,hr,R_AX)));

                   1:
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_B,hr,R_AL)));
                  end;
                end
              else
                if ret_in_acc(procinfo.retdef) then
                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hr,R_EAX)))
              else
                 if (procinfo.retdef^.deftype=floatdef) then
                   begin
                      floatloadops(pfloatdef(procinfo.retdef)^.typ,op,s);
                      exprasmlist^.concat(new(pai386,op_ref(op,s,hr)))
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
       noreraiselabel : pasmlabel;
       hr : treference;
       oldexprasmlist : paasmoutput;
  begin
      oldexprasmlist:=exprasmlist;
      exprasmlist:=alist;

      if aktexitlabel^.is_used then
        exprasmlist^.insert(new(pai_label,init(aktexitlabel)));

      { call the destructor help procedure }
      if (aktprocsym^.definition^.options and podestructor)<>0 then
        begin
          if procinfo._class^.isclass then
            begin
              exprasmlist^.insert(new(pai386,op_sym(A_CALL,S_NO,
                newasmsymbol('FPC_DISPOSE_CLASS'))));
            end
          else
            begin
              exprasmlist^.insert(new(pai386,op_sym(A_CALL,S_NO,
                newasmsymbol('FPC_HELP_DESTRUCTOR'))));
              exprasmlist^.insert(new(pai386,op_const_reg(A_MOV,S_L,procinfo._class^.vmt_offset,R_EDI)));
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
      if (procinfo.flags and pi_needs_implicit_finally)<>0 then
        begin
           getlabel(noreraiselabel);
           exprasmlist^.concat(new(pai386,
             op_reg(A_POP,S_L,R_EAX)));
           exprasmlist^.concat(new(pai386,
             op_reg_reg(A_TEST,S_L,R_EAX,R_EAX)));
           emitjmp(C_E,noreraiselabel);
           { must be the return value finalized before reraising the exception? }
           if (procinfo.retdef<>pdef(voiddef)) and
             (procinfo.retdef^.needs_inittable) and
             ((procinfo.retdef^.deftype<>objectdef) or
             not(pobjectdef(procinfo.retdef)^.isclass)) then
             begin
                reset_reference(hr);
                hr.offset:=procinfo.retoffset;
                hr.base:=procinfo.framepointer;
                finalize(procinfo.retdef,hr,ret_in_param(procinfo.retdef));
             end;

           exprasmlist^.concat(new(pai386,
             op_sym(A_CALL,S_NO,newasmsymbol('FPC_RERAISE'))));
           exprasmlist^.concat(new(pai_label,init(noreraiselabel)));
           exprasmlist^.concat(new(pai386,
             op_sym(A_CALL,S_NO,newasmsymbol('FPC_POPADDRSTACK'))));
        end;

      { call __EXIT for main program }
      if (not DLLsource) and (not inlined) and ((aktprocsym^.definition^.options and poproginit)<>0) then
       begin
         exprasmlist^.concat(new(pai386,op_sym(A_CALL,S_NO,newasmsymbol('FPC_DO_EXIT'))));
       end;

      { handle return value }
      if (aktprocsym^.definition^.options and poassembler)=0 then
          if (aktprocsym^.definition^.options and poconstructor)=0 then
            handle_return_value(inlined)
          else
              begin
                  { successful constructor deletes the zero flag }
                  { and returns self in eax                   }
                  exprasmlist^.concat(new(pai_label,init(quickexitlabel)));
                  { eax must be set to zero if the allocation failed !!! }
                  exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_ESI,R_EAX)));
                  exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,R_EAX,R_EAX)));
              end;

      { stabs uses the label also ! }
      if aktexit2label^.is_used or
         ((cs_debuginfo in aktmoduleswitches) and not inlined) then
        exprasmlist^.concat(new(pai_label,init(aktexit2label)));
      { gives problems for long mangled names }
      {list^.concat(new(pai_symbol,init(aktprocsym^.definition^.mangledname+'_end')));}

      { should we restore edi ? }
      { for all i386 gcc implementations }
      if ((aktprocsym^.definition^.options and posavestdregs)<>0) then
        begin
          if (aktprocsym^.definition^.usedregisters and ($80 shr byte(R_EBX)))<>0 then
           exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EBX)));
          exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ESI)));
          exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));
          { here we could reset R_EBX
            but that is risky because it only works
            if genexitcode is called after genentrycode
            so lets skip this for the moment PM
          aktprocsym^.definition^.usedregisters:=
            aktprocsym^.definition^.usedregisters or not ($80 shr byte(R_EBX));
          }
        end;

      if not(nostackframe) and not inlined then
          exprasmlist^.concat(new(pai386,op_none(A_LEAVE,S_NO)));
      { parameters are limited to 65535 bytes because }
      { ret allows only imm16                    }
      if (parasize>65535) and not(aktprocsym^.definition^.options and poclearstack<>0) then
       CGMessage(cg_e_parasize_too_big);

      { at last, the return is generated }

      if not inlined then
      if (aktprocsym^.definition^.options and pointerrupt)<>0 then
          generate_interrupt_stackframe_exit
      else
       begin
       {Routines with the poclearstack flag set use only a ret.}
       { also routines with parasize=0     }
         if (parasize=0) or (aktprocsym^.definition^.options and poclearstack<>0) then
          exprasmlist^.concat(new(pai386,op_none(A_RET,S_NO)))
         else
          exprasmlist^.concat(new(pai386,op_const(A_RET,S_NO,parasize)));
       end;

{$ifdef GDB}
      if (cs_debuginfo in aktmoduleswitches) and not inlined  then
          begin
              aktprocsym^.concatstabto(exprasmlist);
              if assigned(procinfo._class) then
                if (not assigned(procinfo.parent) or
                   not assigned(procinfo.parent^._class)) then
                  exprasmlist^.concat(new(pai_stabs,init(strpnew(
                   '"$t:v'+procinfo._class^.numberstring+'",'+
                   tostr(N_PSYM)+',0,0,'+tostr(procinfo.esi_offset)))))
                else
                  exprasmlist^.concat(new(pai_stabs,init(strpnew(
                   '"$t:r'+procinfo._class^.numberstring+'",'+
                   tostr(N_RSYM)+',0,0,'+tostr(GDB_i386index[R_ESI])))));

              if (pdef(aktprocsym^.definition^.retdef) <> pdef(voiddef)) then
                if ret_in_param(aktprocsym^.definition^.retdef) then
                  exprasmlist^.concat(new(pai_stabs,init(strpnew(
                   '"'+aktprocsym^.name+':X*'+aktprocsym^.definition^.retdef^.numberstring+'",'+
                   tostr(N_PSYM)+',0,0,'+tostr(procinfo.retoffset)))))
                else
                  exprasmlist^.concat(new(pai_stabs,init(strpnew(
                   '"'+aktprocsym^.name+':X'+aktprocsym^.definition^.retdef^.numberstring+'",'+
                   tostr(N_PSYM)+',0,0,'+tostr(procinfo.retoffset)))));

              mangled_length:=length(aktprocsym^.definition^.mangledname);
              getmem(p,mangled_length+50);
              strpcopy(p,'192,0,0,');
              strpcopy(strend(p),aktprocsym^.definition^.mangledname);
              exprasmlist^.concat(new(pai_stabn,init(strnew(p))));
              {list^.concat(new(pai_stabn,init(strpnew('192,0,0,'
               +aktprocsym^.definition^.mangledname))));
              p[0]:='2';p[1]:='2';p[2]:='4';
              strpcopy(strend(p),'_end');}
              freemem(p,mangled_length+50);
              exprasmlist^.concat(new(pai_stabn,init(
                strpnew('224,0,0,'+aktexit2label^.name))));
               { strpnew('224,0,0,'
               +aktprocsym^.definition^.mangledname+'_end'))));}
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
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,s,reg,newreference(dest_loc.reference))));
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
  Revision 1.17  1999-07-22 09:37:38  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.16  1999/07/18 10:41:59  florian
    * fix of my previous commit nevertheless it doesn't work completly

  Revision 1.15  1999/07/18 10:19:44  florian
    * made it compilable with Dlephi 4 again
    + fixed problem with large stack allocations on win32

  Revision 1.14  1999/07/06 21:48:11  florian
    * a lot bug fixes:
       - po_external isn't any longer necessary for procedure compatibility
       - m_tp_procvar is in -Sd now available
       - error messages of procedure variables improved
       - return values with init./finalization fixed
       - data types with init./finalization aren't any longer allowed in variant
         record

  Revision 1.13  1999/07/05 20:25:22  peter
    * merged

  Revision 1.12  1999/07/05 20:13:13  peter
    * removed temp defines

  Revision 1.11  1999/07/05 11:56:56  jonas
    * merged

  Revision 1.5.2.5  1999/07/05 20:03:31  peter
    * removed warning/notes

  Revision 1.5.2.3  1999/07/04 21:50:17  jonas
    * everything between $ifdef jmpfix:
      * when a jxx instruction is disposed, decrease the refcount of the label
        it referenced
      * for jmp instructions to a label, set is_jmp also to true (was only done
        for Jcc instructions)

  Revision 1.9  1999/07/01 15:49:11  florian
    * int64/qword type release
    + lo/hi for int64/qword

  Revision 1.8  1999/06/28 22:29:15  florian
    * qword division fixed
    + code for qword/int64 type casting added:
      range checking isn't implemented yet

  Revision 1.7  1999/06/17 13:19:50  pierre
   * merged from 0_99_12 branch

  Revision 1.5.2.2  1999/06/17 12:38:39  pierre
   * wrong warning for operators removed

  Revision 1.6  1999/06/14 17:47:48  peter
    * merged

  Revision 1.5.2.1  1999/06/14 17:27:08  peter
    * fixed posavestd regs which popped at the wrong place

  Revision 1.5  1999/06/03 16:21:15  pierre
   * fixes a bug due to int64 code in maybe_savetotemp

  Revision 1.4  1999/06/02 22:44:06  pierre
   * previous wrong log corrected

  Revision 1.3  1999/06/02 22:25:29  pierre
  * changed $ifdef FPC @ into $ifndef TP

  Revision 1.2  1999/06/02 10:11:49  florian
    * make cycle fixed i.e. compilation with 0.99.10
    * some fixes for qword
    * start of register calling conventions

  Revision 1.1  1999/06/01 19:33:18  peter
    * reinserted

  Revision 1.158  1999/06/01 14:45:46  peter
    * @procvar is now always needed for FPC

  Revision 1.157  1999/05/27 19:44:20  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.156  1999/05/24 08:55:24  florian
    * non working safecall directiv implemented, I don't know if we
      need it

  Revision 1.155  1999/05/23 19:55:14  florian
    * qword/int64 multiplication fixed
    + qword/int64 subtraction

  Revision 1.154  1999/05/23 18:42:05  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.153  1999/05/21 13:54:55  peter
    * NEWLAB for label as symbol

  Revision 1.152  1999/05/19 22:00:45  florian
    * some new routines for register management:
       maybe_savetotemp,restorefromtemp, saveusedregisters,
       restoreusedregisters

  Revision 1.151  1999/05/19 20:40:11  florian
    * fixed a couple of array related bugs:
      - var a : array[0..1] of char;   p : pchar;  p:=a+123; works now
      - open arrays with an odd size doesn't work: movsb wasn't generated
      - introduced some new array type helper routines (is_special_array) etc.
      - made the array type checking in isconvertable more strict, often
        open array can be used where is wasn't allowed etc...

  Revision 1.150  1999/05/19 15:26:30  florian
    * if a non local variables isn't initialized the compiler doesn't write
      any longer "local var. seems not to be ..."

  Revision 1.149  1999/05/19 13:59:07  jonas
    * no more "enter" generated when -Og is used (caused sometimes crashes under
      Linux, don't know why)

  Revision 1.148  1999/05/18 21:58:30  florian
    * fixed some bugs related to temp. ansistrings and functions results
      which return records/objects/arrays which need init/final.

  Revision 1.147  1999/05/18 14:15:28  peter
    * containsself fixes
    * checktypes()

  Revision 1.146  1999/05/17 22:42:26  florian
    * FPC_ANSISTR_DECR_REF needs a reference!

  Revision 1.145  1999/05/17 21:57:06  florian
    * new temporary ansistring handling

  Revision 1.144  1999/05/15 21:33:18  peter
    * redesigned temp_gen temp allocation so temp allocation for
      ansistring works correct. It also does a best fit instead of first fit

  Revision 1.143  1999/05/13 21:59:22  peter
    * removed oldppu code
    * warning if objpas is loaded from uses
    * first things for new deref writing

  Revision 1.142  1999/05/12 00:19:46  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.141  1999/05/07 00:33:44  pierre
   explicit type conv to pobject checked with cond TESTOBJEXT2

  Revision 1.140  1999/05/06 09:05:17  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.139  1999/05/04 21:44:35  florian
    * changes to compile it with Delphi 4.0

  Revision 1.138  1999/05/02 09:35:36  florian
    + method message handlers which contain an explicit self can't be called
      directly anymore
    + self is now loaded at the start of the an message handler with an explicit
      self
    + $useoverlay fixed: i386 was renamed to i386base

  Revision 1.137  1999/05/01 13:24:16  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.136  1999/04/28 06:01:56  florian
    * changes of Bruessel:
       + message handler can now take an explicit self
       * typinfo fixed: sometimes the type names weren't written
       * the type checking for pointer comparisations and subtraction
         and are now more strict (was also buggy)
       * small bug fix to link.pas to support compiling on another
         drive
       * probable bug in popt386 fixed: call/jmp => push/jmp
         transformation didn't count correctly the jmp references
       + threadvar support
       * warning if ln/sqrt gets an invalid constant argument

  Revision 1.135  1999/04/26 13:31:26  peter
    * release storenumber,double_checksum

  Revision 1.134  1999/04/21 21:53:08  pierre
   * previous log corrected

  Revision 1.133  1999/04/21 16:31:38  pierre
  + TEMPS_NOT_PUSH conditionnal code :
    put needed registers into temp space instead of pushing them

  Revision 1.132  1999/04/21 09:43:30  peter
    * storenumber works
    * fixed some typos in double_checksum
    + incompatible types type1 and type2 message (with storenumber)

  Revision 1.131  1999/04/19 09:45:49  pierre
    +  cdecl or stdcall push all args with longint size
    *  tempansi stuff cleaned up

  Revision 1.130  1999/04/17 13:14:50  peter
    * concat_external added for new init/final

  Revision 1.129  1999/04/16 20:44:35  florian
    * the boolean operators =;<>;xor with LOC_JUMP and LOC_FLAGS
      operands fixed, small things for new ansistring management

  Revision 1.128  1999/04/16 13:42:31  jonas
    * more regalloc fixes (still not complete)

  Revision 1.127  1999/04/16 10:28:23  pierre
    + added posavestdregs used for cdecl AND stdcall functions
      (saves ESI EDI and EBX for i386)

  Revision 1.126  1999/04/16 09:56:06  pierre
   * unused local var commented

  Revision 1.125  1999/04/15 13:08:30  pierre
   * misplaced statement in concatcopy corrected

  Revision 1.124  1999/04/15 12:19:55  peter
    + finalization support

  Revision 1.123  1999/04/09 00:00:52  pierre
   + uses ungetiftempansi

  Revision 1.122  1999/04/08 15:57:47  peter
    + subrange checking for readln()

  Revision 1.121  1999/03/31 13:55:08  peter
    * assembler inlining working for ag386bin

  Revision 1.120  1999/03/26 00:05:27  peter
    * released valintern
    + deffile is now removed when compiling is finished
    * ^( compiles now correct
    + static directive
    * shrd fixed

  Revision 1.119  1999/03/24 23:16:55  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.118  1999/03/16 17:52:49  jonas
    * changes for internal Val code (do a "make cycle OPT=-dvalintern" to test)
    * in cgi386inl: also range checking for subrange types (compile with "-dreadrangecheck")
    * in cgai386: also small fixes to emitrangecheck

  Revision 1.117  1999/03/09 19:29:12  peter
    * ecxpushed was not reset in concatcopy

  Revision 1.116  1999/03/09 11:45:40  pierre
   * small arrays and records (size <=4) are copied directly

  Revision 1.115  1999/03/03 12:15:13  pierre
   * U_SYSWIN32_ISCONSOLE adde to external list

  Revision 1.114  1999/03/02 18:21:33  peter
    + flags support for add and case

  Revision 1.113  1999/03/01 15:46:19  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes

  Revision 1.112  1999/03/01 13:39:44  pierre
   * temp for int_value const parameters

  Revision 1.111  1999/02/25 21:02:32  peter
    * ag386bin updates
    + coff writer

  Revision 1.110  1999/02/22 02:15:17  peter
    * updates for ag386bin

  Revision 1.109  1999/02/16 00:46:09  peter
    * optimized concatcopy with ecx=1 and ecx=0

  Revision 1.108  1999/02/15 13:13:14  pierre
   * fix for bug0216

  Revision 1.107  1999/02/12 10:43:58  florian
    * internal error 10 with ansistrings fixed

  Revision 1.106  1999/02/03 09:50:22  pierre
   * conditionnal code to try to release temp for consts that are not in memory

  Revision 1.105  1999/02/02 11:47:56  peter
    * fixed ansi2short

  Revision 1.104  1999/01/25 09:29:36  florian
    * very rare problem with in-operator fixed, mainly it was a problem of
      emit_to_reg32 (typo in case ranges)

  Revision 1.103  1999/01/21 22:10:42  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.102  1999/01/19 10:19:00  florian
    * bug with mul. of dwords fixed, reported by Alexander Stohr
    * some changes to compile with TP
    + small enhancements for the new code generator

  Revision 1.101  1999/01/15 11:36:48  pierre
   * double temp disallocation on ansistring removed

  Revision 1.100  1998/12/30 13:41:08  peter
    * released valuepara

  Revision 1.99  1998/12/22 13:11:00  florian
    * memory leaks for ansistring type casts fixed

  Revision 1.98  1998/12/19 00:23:46  florian
    * ansistring memory leaks fixed

  Revision 1.97  1998/12/11 16:10:08  florian
    + shifting for 64 bit ints added
    * bug in getexplicitregister32 fixed: usableregs wasn't decremented !!

  Revision 1.96  1998/12/11 00:03:11  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.95  1998/12/10 09:47:19  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.94  1998/12/03 10:17:27  peter
    * target_os.use_bound_instruction boolean

  Revision 1.93  1998/11/30 19:48:56  peter
    * some more rangecheck fixes

  Revision 1.92  1998/11/30 16:34:44  pierre
    * corrected problems with rangecheck
    + added needed code for no rangecheck  in CRC32 functions in ppu unit
    * enumdef lso need its rangenr reset to zero
      when calling reset_global_defs

  Revision 1.91  1998/11/30 09:43:07  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.90  1998/11/29 12:43:45  peter
    * commented the fpc_init_stack_check becuase it is not in the RTL

  Revision 1.89  1998/11/27 14:50:34  peter
    + open strings, $P switch support

  Revision 1.88  1998/11/26 21:33:07  peter
    * rangecheck updates

  Revision 1.87  1998/11/26 13:10:41  peter
    * new int - int conversion -dNEWCNV
    * some function renamings

  Revision 1.86  1998/11/26 09:53:37  florian
    * for classes no init/final. code is necessary, fixed

  Revision 1.85  1998/11/20 15:35:56  florian
    * problems with rtti fixed, hope it works

  Revision 1.84  1998/11/18 17:45:25  peter
    * fixes for VALUEPARA

  Revision 1.83  1998/11/18 15:44:12  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.82  1998/11/17 00:36:41  peter
    * more ansistring fixes

  Revision 1.81  1998/11/16 19:23:33  florian
    * isconsole is now set by win32 applications

  Revision 1.80  1998/11/16 15:35:40  peter
    * rename laod/copystring -> load/copyshortstring
    * fixed int-bool cnv bug
    + char-ansistring conversion

  Revision 1.79  1998/11/16 11:28:56  pierre
    * stackcheck removed for i386_win32
    * exportlist does not crash at least !!
      (was need for tests dir !)z

  Revision 1.78  1998/11/15 16:32:34  florian
    * some stuff of Pavel implement (win32 dll creation)
    * bug with ansistring function results fixed

  Revision 1.77  1998/11/13 15:40:17  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.76  1998/11/12 16:43:33  florian
    * functions with ansi strings as result didn't work, solved

  Revision 1.75  1998/11/12 11:19:44  pierre
   * fix for first line of function break

  Revision 1.74  1998/11/12 09:46:18  pierre
    + break main stops before calls to unit inits
    + break at constructors stops before call to FPC_NEW_CLASS
      or FPC_HELP_CONSTRUCTOR

  Revision 1.73  1998/11/10 10:50:55  pierre
   * temporary fix for long mangled procsym names

  Revision 1.72  1998/11/05 12:02:40  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.71  1998/10/29 15:42:45  florian
    + partial disposing of temp. ansistrings

  Revision 1.70  1998/10/25 23:32:49  peter
    * fixed unsigned mul

  Revision 1.69  1998/10/20 13:11:33  peter
    + def_getreg to get a register with the same size as definition

  Revision 1.68  1998/10/20 08:06:48  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.67  1998/10/16 13:12:50  pierre
    * added vmt_offsets in destructors code also !!!
    * vmt_offset code for m68k

  Revision 1.66  1998/10/16 08:48:40  peter
    * fixed some misplaced $endif GDB

  Revision 1.65  1998/10/15 12:37:40  pierre
    + passes vmt offset to HELP_CONSTRUCTOR for objects

  Revision 1.64  1998/10/13 16:50:13  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.63  1998/10/13 13:10:13  peter
    * new style for m68k/i386 infos and enums

  Revision 1.62  1998/10/08 17:17:17  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.61  1998/10/08 13:48:41  peter
    * fixed memory leaks for do nothing source
    * fixed unit interdependency

  Revision 1.60  1998/10/07 10:37:43  peter
    * fixed stabs

  Revision 1.59  1998/10/06 17:16:45  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.58  1998/10/05 21:33:16  peter
    * fixed 161,165,166,167,168

  Revision 1.57  1998/10/01 09:22:54  peter
    * fixed value openarray
    * ungettemp of arrayconstruct

  Revision 1.56  1998/09/28 16:57:19  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.55  1998/09/28 16:18:15  florian
    * two fixes to get ansi strings work

  Revision 1.54  1998/09/20 17:46:49  florian
    * some things regarding ansistrings fixed

  Revision 1.53  1998/09/20 09:38:44  florian
    * hasharray for defs fixed
    * ansistring code generation corrected (init/final, assignement)

  Revision 1.52  1998/09/17 09:42:31  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.51  1998/09/14 10:44:05  peter
    * all internal RTL functions start with FPC_

  Revision 1.50  1998/09/07 18:46:01  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.49  1998/09/05 22:10:52  florian
    + switch -vb
    * while/repeat loops accept now also word/longbool conditions
    * makebooltojump did an invalid ungetregister32, fixed

  Revision 1.48  1998/09/04 08:41:52  peter
    * updated some error CGMessages

  Revision 1.47  1998/09/03 17:08:41  pierre
    * better lines for stabs
      (no scroll back to if before else part
      no return to case line at jump outside case)
    + source lines also if not in order

  Revision 1.46  1998/09/03 16:03:16  florian
    + rtti generation
    * init table generation changed

  Revision 1.45  1998/09/01 12:48:03  peter
    * use pdef^.size instead of orddef^.typ

  Revision 1.44  1998/09/01 09:07:11  peter
    * m68k fixes, splitted cg68k like cgi386

  Revision 1.43  1998/08/21 08:40:52  pierre
    * EBX,EDI,ESI saved for CDECL on all i386 targets

  Revision 1.42  1998/08/19 16:07:41  jonas
    * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

  Revision 1.41  1998/08/19 00:40:43  peter
    * small crash prevention

  Revision 1.40  1998/08/17 10:10:06  peter
    - removed OLDPPU

  Revision 1.39  1998/08/15 16:51:39  peter
    * save also esi,ebx for cdecl procedures

  Revision 1.38  1998/08/14 18:18:42  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.37  1998/08/11 00:00:30  peter
    * fixed dup log

  Revision 1.36  1998/08/10 14:49:52  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.35  1998/08/05 16:00:11  florian
    * some fixes for ansi strings

  Revision 1.34  1998/07/30 11:18:14  florian
    + first implementation of try ... except on .. do end;
    * limitiation of 65535 bytes parameters for cdecl removed

  Revision 1.33  1998/07/27 21:57:12  florian
    * fix to allow tv like stream registration:
        @tmenu.load doesn't work if load had parameters or if load was only
        declared in an anchestor class of tmenu

  Revision 1.32  1998/07/27 11:23:40  florian
    + procedures with the directive cdecl and with target linux save now
      the register EDI (like GCC procedures).

  Revision 1.31  1998/07/20 18:40:11  florian
    * handling of ansi string constants should now work

  Revision 1.30  1998/07/18 22:54:26  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.29  1998/07/06 13:21:13  michael
  + Fixed Initialization/Finalizarion calls

  Revision 1.28  1998/06/25 08:48:11  florian
    * first version of rtti support

  Revision 1.27  1998/06/24 14:48:32  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.26  1998/06/16 08:56:19  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.25  1998/06/08 13:13:40  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.24  1998/06/07 15:30:23  florian
    + first working rtti
    + data init/final. for local variables

  Revision 1.23  1998/06/05 17:49:53  peter
    * cleanup of cgai386

  Revision 1.22  1998/06/04 09:55:34  pierre
    * demangled name of procsym reworked to become
      independant of the mangling scheme

  Revision 1.21  1998/06/03 22:48:51  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.20  1998/05/30 14:31:03  peter
    + $ASMMODE

  Revision 1.19  1998/05/23 01:21:02  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.18  1998/05/20 09:42:32  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.17  1998/05/11 13:07:53  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.16  1998/05/07 00:17:00  peter
    * smartlinking for sets
    + consts labels are now concated/generated in hcodegen
    * moved some cpu code to cga and some none cpu depended code from cga
      to tree and hcodegen and cleanup of hcodegen
    * assembling .. output reduced for smartlinking ;)

  Revision 1.15  1998/05/06 08:38:35  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.14  1998/05/04 17:54:24  peter
    + smartlinking works (only case jumptable left todo)
    * redesign of systems.pas to support assemblers and linkers
    + Unitname is now also in the PPU-file, increased version to 14

  Revision 1.13  1998/05/01 16:38:43  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.12  1998/05/01 07:43:52  florian
    + basics for rtti implemented
    + switch $m (generate rtti for published sections)

  Revision 1.11  1998/04/29 13:41:17  peter
    + assembler functions are not profiled

  Revision 1.10  1998/04/29 10:33:47  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.9  1998/04/21 10:16:46  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.8  1998/04/13 08:42:50  florian
    * call by reference and call by value open arrays fixed

  Revision 1.7  1998/04/09 23:27:26  peter
    * fixed profiling results

  Revision 1.6  1998/04/09 14:28:03  jonas
    + basic k6 and 6x86 optimizing support (-O7 and -O8)

  Revision 1.5  1998/04/08 16:58:01  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!
}
