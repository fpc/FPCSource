{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Carl Eric Codere

    This unit generates 68000 (or better) assembler from the parse tree

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
unit cga68k;

  interface

    uses
       globtype,cobjects,tree,cpubase,aasm,symtable,symconst;

    procedure emitl(op : tasmop;var l : pasmlabel);
    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);
    procedure emitcall(const routine:string;add_to_externals : boolean);
    procedure emitloadord2reg(location:Tlocation;orddef:Porddef;
                              destreg:Tregister;delloc:boolean);
    procedure emit_to_reg32(var hr:tregister);
    procedure loadsetelement(var p : ptree);
    { produces jumps to true respectively false labels using boolean expressions }
    procedure maketojumpbool(p : ptree);
    procedure emitoverflowcheck(p: ptree);
    procedure push_int(l : longint);
    function maybe_push(needed : byte;p : ptree) : boolean;
    procedure restore(p : ptree);
    procedure emit_push_mem(const ref : treference);
    procedure emitpushreferenceaddr(list : paasmoutput;const ref : treference);
    procedure copystring(const dref,sref : treference;len : byte);
    procedure concatcopy(source,dest : treference;size : longint;delsource : boolean);
    { see implementation }
    procedure maybe_loada5;
    procedure emit_bounds_check(hp: treference; index: tregister);
    procedure loadstring(p:ptree);
    procedure decransiref(const ref : treference);

    procedure floatload(t : tfloattype;const ref : treference; var location:tlocation);
    { return a float op_size from a floatb type  }
    { also does some error checking for problems }
    function getfloatsize(t: tfloattype): topsize;
    procedure floatstore(t : tfloattype;var location:tlocation; const ref:treference);
{    procedure floatloadops(t : tfloattype;var op : tasmop;var s : topsize);
    procedure floatstoreops(t : tfloattype;var op : tasmop;var s : topsize); }

    procedure firstcomplex(p : ptree);

    { generate stackframe for interrupt procedures }
    procedure generate_interrupt_stackframe_entry;
    procedure generate_interrupt_stackframe_exit;
    { generate entry code for a procedure.}
    procedure genentrycode(list : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean);
    { generate the exit code for a procedure. }
    procedure genexitcode(list : paasmoutput;parasize:longint;
                          nostackframe,inlined:boolean);

    procedure removetemps(list : paasmoutput;p : plinkedlist);
    procedure releasedata(p : plinkedlist);

{$ifdef test_dest_loc}
const   { used to avoid temporary assignments }
        dest_loc_known : boolean = false;
        in_dest_loc : boolean = false;
        dest_loc_tree : ptree = nil;

var dest_loc : tlocation;

procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

{$endif test_dest_loc}


  implementation

    uses
       systems,globals,verbose,files,types,pbase,
       tgen68k,hcodegen,temp_gen,ppu
{$ifdef GDB}
       ,gdb
{$endif}
       ;


    {
    procedure genconstadd(size : topsize;l : longint;const str : string);

      begin
         if l=0 then
         else if l=1 then
           exprasmlist^.concat(new(pai68k,op_A_INC,size,str)
         else if l=-1 then
           exprasmlist^.concat(new(pai68k,op_A_INC,size,str)
         else
           exprasmlist^.concat(new(pai68k,op_ADD,size,'$'+tostr(l)+','+str);
      end;
    }
    procedure copystring(const dref,sref : treference;len : byte);

      var
         pushed : tpushed;

      begin
         pushusedregisters(pushed,$ffff);
{         emitpushreferenceaddr(dref);       }
{         emitpushreferenceaddr(sref);       }
{         push_int(len);                     }
         { This speeds up from 116 cycles to 24 cycles on the 68000 }
         { when passing register parameters!                        }
         exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(dref),R_A1)));
         exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(sref),R_A0)));
         exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,len,R_D0)));
         emitcall('FPC_STRCOPY',true);
         maybe_loada5;
         popusedregisters(pushed);
      end;


    procedure decransiref(const ref : treference);
      begin
         emitpushreferenceaddr(exprasmlist,ref);
         emitcall('FPC_ANSISTR_DECR_REF',true);
      end;

    procedure loadstring(p:ptree);
      begin
        case p^.right^.resulttype^.deftype of
         stringdef : begin
                       { load a string ... }
                       { here two possible choices:      }
                       { if it is a char, then simply    }
                       { load 0 length string            }
                       if (p^.right^.treetype=stringconstn) and
                          (str_length(p^.right)=0) then
                        exprasmlist^.concat(new(paicpu,op_const_ref(
                           A_MOVE,S_B,0,newreference(p^.left^.location.reference))))
                       else
                        copystring(p^.left^.location.reference,p^.right^.location.reference,
                           min(pstringdef(p^.right^.resulttype)^.len,pstringdef(p^.left^.resulttype)^.len));
                     end;
            orddef : begin
                       if p^.right^.treetype=ordconstn then
                        begin
                            { offset 0: length of string }
                            { offset 1: character        }
                            exprasmlist^.concat(new(paicpu,op_const_ref(A_MOVE,S_W,1*256+p^.right^.value,
                              newreference(p^.left^.location.reference))))
                        end
                       else
                         begin
                            { not so elegant (goes better with extra register }
                            if (p^.right^.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                              begin
                                 exprasmlist^.concat(new(paicpu,op_reg_reg(
                                    A_MOVE,S_B,p^.right^.location.register,R_D0)));
                                 ungetregister32(p^.right^.location.register);
                              end
                            else
                              begin
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(
                                    A_MOVE,S_B,newreference(p^.right^.location.reference),R_D0)));
                                 del_reference(p^.right^.location.reference);
                              end;
                            { alignment can cause problems }
                            { add length of string to ref }
                            exprasmlist^.concat(new(paicpu,op_const_ref(A_MOVE,S_B,1,
                               newreference(p^.left^.location.reference))));
(*                            if abs(p^.left^.location.reference.offset) >= 1 then
                              Begin *)
                              { temporarily decrease offset }
                                Inc(p^.left^.location.reference.offset);
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_B,R_D0,
                                  newreference(p^.left^.location.reference))));
                                Dec(p^.left^.location.reference.offset);
                                { restore offset }
(*                              end
                            else
                              Begin
                                Comment(V_Debug,'SecondChar2String() internal error.');
                                internalerror(34);
                              end; *)
                         end;
                       end;
        else
         CGMessage(type_e_mismatch);
        end;
      end;





    procedure restore(p : ptree);

      var
         hregister :  tregister;

      begin
         if (p^.location.loc=LOC_REGISTER) or (p^.location.loc=LOC_CREGISTER) then
            hregister:=getregister32
         else
            hregister:=getaddressreg;

         exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_SPPULL,hregister)));
         if (p^.location.loc=LOC_REGISTER) or (p^.location.loc=LOC_CREGISTER) then
           begin
              p^.location.register:=hregister;
           end
         else
           begin
              reset_reference(p^.location.reference);
              p^.location.reference.base:=hregister;
              set_location(p^.left^.location,p^.location);
           end;
      end;

    function maybe_push(needed : byte;p : ptree) : boolean;

      var
         pushed : boolean;
      begin
         if (needed>usablereg32) or (needed > usableaddress) then
           begin
              if (p^.location.loc=LOC_REGISTER) or
                 (p^.location.loc=LOC_CREGISTER) then
                begin
                   pushed:=true;
                   exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,p^.location.register,R_SPPUSH)));
                   ungetregister32(p^.location.register);
                end
               else
                 if ((p^.location.loc=LOC_MEM) or(p^.location.loc=LOC_REFERENCE)) and
                    ((p^.location.reference.base<>R_NO) or
                    (p^.location.reference.index<>R_NO)) then
                  begin
                     del_reference(p^.location.reference);
                     exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                        R_A0)));
                     exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_A0,R_SPPUSH)));
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;


    { emit out of range check for arrays and sets}
    procedure emit_bounds_check(hp: treference; index: tregister);
    { index = index of array to check }
    { memory of range check information for array }
     var
      hl : pasmlabel;
     begin
        if (aktoptprocessor = MC68020) then
          begin
             exprasmlist^.concat(new(paicpu, op_ref_reg(A_CMP2,S_L,newreference(hp),index)));
             getlabel(hl);
             emitl(A_BCC, hl);
             exprasmlist^.concat(new(paicpu, op_const_reg(A_MOVE,S_L,201,R_D0)));
             emitcall('FPC_HALT_ERROR',true);
             emitl(A_LABEL, hl);
          end
        else
          begin
            exprasmlist^.concat(new(paicpu, op_ref_reg(A_LEA,S_L,newreference(hp), R_A1)));
            exprasmlist^.concat(new(paicpu, op_reg_reg(A_MOVE, S_L, index, R_D0)));
            emitcall('FPC_RE_BOUNDS_CHECK',true);
          end;
     end;


    procedure emit_to_reg32(var hr:tregister);
      begin
(*        case hr of
      R_AX..R_DI : begin
                     hr:=reg16toreg32(hr);
                     exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$ffff,hr)));
                   end;
      R_AL..R_DL : begin
                     hr:=reg8toreg32(hr);
                     exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$ff,hr)));
                   end;
      R_AH..R_DH : begin
                     hr:=reg8toreg32(hr);
                     exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,$ff00,hr)));
                   end;
        end; *)
      end;

    function getfloatsize(t: tfloattype): topsize;
    begin
      case t of
      s32real: getfloatsize := S_FS;
      s64real: getfloatsize := S_FL;
      s80real: getfloatsize := S_FX;
{$ifdef extdebug}
    else {else case }
      begin
        Comment(V_Debug,' getfloatsize() trying to get unknown size.');
        internalerror(12);
      end;
{$endif}
     end;
    end;

    procedure emitl(op : tasmop;var l : pasmlabel);

      begin
         if op=A_LABEL then
           exprasmlist^.concat(new(pai_label,init(l)))
         else
           exprasmlist^.concat(new(pai_labeled,init(op,l)))
      end;

    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);

      begin
         if (reg1 <> reg2) or (i <> A_MOVE) then
           exprasmlist^.concat(new(paicpu,op_reg_reg(i,s,reg1,reg2)));
      end;


    procedure emitcall(const routine:string;add_to_externals : boolean);

     begin
        exprasmlist^.concat(new(paicpu,op_csymbol(A_JSR,S_NO,newcsymbol(routine,0))));
        {!!!!!
        if add_to_externals and
           not (cs_compilesystem in aktmoduleswitches) then
          concat_external(routine,EXT_NEAR);
        }
     end;


    procedure maketojumpbool(p : ptree);

      begin
         if p^.error then
           exit;
         if (p^.resulttype^.deftype=orddef) and
            (porddef(p^.resulttype)^.typ=bool8bit) then
           begin
              if is_constboolnode(p) then
                begin
                   if p^.value<>0 then
                     emitl(A_JMP,truelabel)
                   else emitl(A_JMP,falselabel);
                end
              else
                begin
                   case p^.location.loc of
                      LOC_CREGISTER,LOC_REGISTER : begin
                                        exprasmlist^.concat(new(paicpu,op_reg(A_TST,S_B,p^.location.register)));
                                        ungetregister32(p^.location.register);
                                        emitl(A_BNE,truelabel);
                                        emitl(A_JMP,falselabel);
                                     end;
                      LOC_MEM,LOC_REFERENCE : begin
                                        exprasmlist^.concat(new(paicpu,op_ref(
                                          A_TST,S_B,newreference(p^.location.reference))));
                                        del_reference(p^.location.reference);
                                        emitl(A_BNE,truelabel);
                                        emitl(A_JMP,falselabel);
                                     end;
                      LOC_FLAGS : begin
                                     emitl(flag_2_jmp[p^.location.resflags],truelabel);
                                     emitl(A_JMP,falselabel);
                                  end;
                   end;
                end;
           end
         else
          CGMessage(type_e_mismatch);
      end;

    procedure emitoverflowcheck(p: ptree);

      var
         hl : pasmlabel;

      begin
         if cs_check_overflow in aktlocalswitches  then
           begin
              getlabel(hl);
              if not ((p^.resulttype^.deftype=pointerdef) or
                     ((p^.resulttype^.deftype=orddef) and
                (porddef(p^.resulttype)^.typ in [u16bit,u32bit,u8bit,uchar,bool8bit]))) then
                emitl(A_BVC,hl)
              else
                emitl(A_BCC,hl);
              emitcall('FPC_OVERFLOW',true);
              emitl(A_LABEL,hl);
           end;
      end;


    procedure push_int(l : longint);

      begin
         if (l = 0) and (aktoptprocessor = MC68020) then
           begin
          exprasmlist^.concat(new(paicpu,op_reg(A_CLR,S_L,R_D6)));
              exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,
              R_D6, R_SPPUSH)));
           end
         else
         if not(cs_littlesize in aktglobalswitches) and (l >= -128) and (l <= 127) then
           begin
           exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVEQ,S_L,l,R_D6)));
           exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_D6,R_SPPUSH)));
           end
         else
           exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,l,R_SPPUSH)));
      end;

    procedure emit_push_mem(const ref : treference);
    { Push a value on to the stack }
      begin
         if ref.isintvalue then
           push_int(ref.offset)
         else
           exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(ref),R_SPPUSH)));
      end;


    { USES REGISTER R_A1 }
    procedure emitpushreferenceaddr(list : paasmoutput;const ref : treference);
    { Push a pointer to a value on the stack }
      begin
         if ref.isintvalue then
           push_int(ref.offset)
         else
           begin
              if (ref.base=R_NO) and (ref.index=R_NO) then
                list^.concat(new(paicpu,op_ref(A_PEA,S_L,
                    newreference(ref))))
              else if (ref.base=R_NO) and (ref.index<>R_NO) and
                 (ref.offset=0) and (ref.scalefactor=0) and (ref.symbol=nil) then
                list^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,
                    ref.index,R_SPPUSH)))
              else if (ref.base<>R_NO) and (ref.index=R_NO) and
                 (ref.offset=0) and (ref.symbol=nil) then
                list^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,ref.base,R_SPPUSH)))
              else
                begin
                   list^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(ref),R_A1)));
                   list^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_A1,R_SPPUSH)));
                end;
           end;
        end;

    { This routine needs to be further checked to see if it works correctly  }
    { because contrary to the intel version, all large set elements are read }
    { as 32-bit value_str, and then decomposed to find the correct byte.        }

    { CHECKED : Depending on the result size, if reference, a load may be    }
    { required on word, long or byte.                                        }
    procedure loadsetelement(var p : ptree);

      var
         hr : tregister;
         opsize : topsize;

      begin
         { copy the element in the d0.b register, slightly complicated }
         case p^.location.loc of
            LOC_REGISTER,
            LOC_CREGISTER : begin
                              hr:=p^.location.register;
                              emit_reg_reg(A_MOVE,S_L,hr,R_D0);
                              ungetregister32(hr);
                           end;
            else
               begin
                 { This is quite complicated, because of the endian on }
                 { the m68k!                                           }
                 opsize:=S_NO;
                 case integer(p^.resulttype^.size) of
                   1 : opsize:=S_B;
                   2 : opsize:=S_W;
                   4 : opsize:=S_L;
                 else
                   internalerror(19);
                 end;
                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,opsize,
                    newreference(p^.location.reference),R_D0)));
                 exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                    255,R_D0)));
{
                  exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,
                    newreference(p^.location.reference),R_D0)));        }
{                  exprasmlist^.concat(new(paicpu,op_const_reg(A_AND,S_L,
                    $ff,R_D0))); }
                  del_reference(p^.location.reference);
               end;
         end;
      end;


    procedure generate_interrupt_stackframe_entry;
      begin
         { save the registers of an interrupt procedure }

         { .... also the segment registers }
      end;

    procedure generate_interrupt_stackframe_exit;

      begin
         { restore the registers of an interrupt procedure }
      end;


    procedure genentrycode(list : paasmoutput;const proc_names:Tstringcontainer;make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean);
{Generates the entry code for a procedure.}

var hs:string;
    hp:Pused_unit;
    unitinits:taasmoutput;
{$ifdef GDB}
    stab_function_name:Pai_stab_function_name;
{$endif GDB}
begin
    if potype_proginit=aktprocsym^.definition^.proctypeoption then
        begin
            {Init the stack checking.}
            if (cs_check_stack in aktlocalswitches) and
             (target_info.target=target_m68k_linux) then
                begin
                    procinfo^.aktentrycode^.insert(new(paicpu,
                     op_csymbol(A_JSR,S_NO,newcsymbol('FPC_INIT_STACK_CHECK',0))));
                end
            else
            { The main program has already allocated its stack - so we simply compare }
            { with a value of ZERO, and the comparison will directly check!           }
            if (cs_check_stack in aktlocalswitches) then
                begin
                  procinfo^.aktentrycode^.insert(new(paicpu,op_csymbol(A_JSR,S_NO,
                      newcsymbol('FPC_STACKCHECK',0))));
                  procinfo^.aktentrycode^.insert(new(paicpu,op_const_reg(A_MOVE,S_L,
                      0,R_D0)));
                end;


            unitinits.init;

            {Call the unit init procedures.}
            hp:=pused_unit(usedunits.first);
            while assigned(hp) do
                begin
                    { call the unit init code and make it external }
                    if (hp^.u^.flags and uf_init)<>0 then
                        begin
                           unitinits.concat(new(paicpu,op_csymbol(A_JSR,S_NO,newcsymbol('INIT$$'+hp^.u^.modulename^,0))));
                        end;
                   hp:=pused_unit(hp^.next);
                end;
              procinfo^.aktentrycode^.insertlist(@unitinits);
              unitinits.done;
        end;

        { a constructor needs a help procedure }
        if potype_constructor=aktprocsym^.definition^.proctypeoption then
        begin
           if procinfo^._class^.is_class then
             begin
              procinfo^.aktentrycode^.insert(new(pai_labeled,init(A_BEQ,quickexitlabel)));
              procinfo^.aktentrycode^.insert(new(paicpu,op_csymbol(A_JSR,S_NO,
              newcsymbol('FPC_NEW_CLASS',0))));
             end
           else
             begin
              procinfo^.aktentrycode^.insert(new(pai_labeled,init(A_BEQ,quickexitlabel)));
              procinfo^.aktentrycode^.insert(new(paicpu,op_csymbol(A_JSR,S_NO,
              newcsymbol('FPC_HELP_CONSTRUCTOR',0))));
              procinfo^.aktentrycode^.insert(new(paicpu,op_const_reg(A_MOVE,S_L,procinfo^._class^.vmt_offset,R_D0)));
             end;
        end;
    { don't load ESI, does the caller }

{$ifdef GDB}
      if (cs_debuginfo in aktmoduleswitches) then
         list^.insert(new(pai_force_line,init));
{$endif GDB}

    { omit stack frame ? }
    if procinfo^.framepointer=stack_pointer_reg then
        begin
            CGMessage(cg_d_stackframe_omited);
            nostackframe:=true;
            if (aktprocsym^.definition^.proctypeoption=potype_unitinit) or
               (aktprocsym^.definition^.proctypeoption=potype_proginit) or
               (aktprocsym^.definition^.proctypeoption=potype_unitfinalize) then
                parasize:=0
            else
                parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.call_offset;
        end
    else
        begin
            if (aktprocsym^.definition^.proctypeoption=potype_unitinit) or
               (aktprocsym^.definition^.proctypeoption=potype_proginit) or
               (aktprocsym^.definition^.proctypeoption=potype_unitfinalize) then
                parasize:=0
             else
                parasize:=aktprocsym^.definition^.parast^.datasize+procinfo^.call_offset-8;
            nostackframe:=false;
            if stackframe<>0 then
                begin
                    if cs_littlesize in aktglobalswitches  then
                        begin
                            if (cs_check_stack in aktlocalswitches) and
                               (target_info.target<>target_m68k_linux) then
                                begin
                                  { If only not in main program, do we setup stack checking }
                                  if (aktprocsym^.definition^.proctypeoption<>potype_proginit) then
                                   Begin
                                       procinfo^.aktentrycode^.insert(new(paicpu,
                                         op_csymbol(A_JSR,S_NO,newcsymbol('FPC_STACKCHECK',0))));
                                       procinfo^.aktentrycode^.insert(new(paicpu,op_const_reg(A_MOVE,S_L,stackframe,R_D0)));
                                   end;
                                end;
                            { to allocate stack space }
                            { here we allocate space using link signed 16-bit version }
                            { -ve offset to allocate stack space! }
                            if (stackframe > -32767) and (stackframe < 32769) then
                              procinfo^.aktentrycode^.insert(new(paicpu,op_reg_const(A_LINK,S_W,R_A6,-stackframe)))
                            else
                              CGMessage(cg_e_stacklimit_in_local_routine);
                        end
                    else
                        begin
                          { Not to complicate the code generator too much, and since some  }
                          { of the systems only support this format, the stackframe cannot }
                          { exceed 32K in size.                                            }
                          if (stackframe > -32767) and (stackframe < 32769) then
                            begin
                              procinfo^.aktentrycode^.insert(new(paicpu,op_const_reg(A_SUB,S_L,stackframe,R_SP)));
                              { IF only NOT in main program do we check the stack normally }
                              if (cs_check_stack in aktlocalswitches) and
                               (aktprocsym^.definition^.proctypeoption<>potype_proginit) then
                                begin
                                  procinfo^.aktentrycode^.insert(new(paicpu,
                                   op_csymbol(A_JSR,S_NO,newcsymbol('FPC_STACKCHECK',0))));
                                  procinfo^.aktentrycode^.insert(new(paicpu,op_const_reg(A_MOVE,S_L,
                                    stackframe,R_D0)));
                                end;
                               procinfo^.aktentrycode^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,R_SP,R_A6)));
                               procinfo^.aktentrycode^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,R_A6,R_SPPUSH)));
                            end
                          else
                            CGMessage(cg_e_stacklimit_in_local_routine);
                        end;
                end {endif stackframe<>0 }
            else
               begin
                 procinfo^.aktentrycode^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,R_SP,R_A6)));
                 procinfo^.aktentrycode^.insert(new(paicpu,op_reg_reg(A_MOVE,S_L,R_A6,R_SPPUSH)));
               end;
        end;


    if po_interrupt in aktprocsym^.definition^.procoptions then
        generate_interrupt_stackframe_entry;

    {proc_names.insert(aktprocsym^.definition^.mangledname);}

    if (aktprocsym^.definition^.owner^.symtabletype=globalsymtable) or
     ((procinfo^._class<>nil) and (procinfo^._class^.owner^.
     symtabletype=globalsymtable)) then
        make_global:=true;
    hs:=proc_names.get;

{$IfDef GDB}
    if (cs_debuginfo in aktmoduleswitches) and target_os.use_function_relative_addresses then
        stab_function_name := new(pai_stab_function_name,init(strpnew(hs)));
{$EndIf GDB}


    while hs<>'' do
        begin
              if make_global then
                procinfo^.aktentrycode^.insert(new(pai_symbol,initname_global(hs,0)))
              else
                procinfo^.aktentrycode^.insert(new(pai_symbol,initname(hs,0)));
{$ifdef GDB}
            if (cs_debuginfo in aktmoduleswitches) then
             begin
               if target_os.use_function_relative_addresses then
                list^.insert(new(pai_stab_function_name,init(strpnew(hs))));

            { This is not a nice solution to save the name, change it and restore when done }
            { not only not nice but also completely wrong !!! (PM) }
            {   aktprocsym^.setname(hs);
               list^.insert(new(pai_stabs,init(aktprocsym^.stabstring))); }
             end;
{$endif GDB}
              hs:=proc_names.get;
        end;
{$ifdef GDB}

    if (cs_debuginfo in aktmoduleswitches) then
        begin
            if target_os.use_function_relative_addresses then
                procinfo^.aktentrycode^.insert(stab_function_name);
            if make_global or ((procinfo^.flags and pi_is_global) <> 0) then
                aktprocsym^.is_global := True;
            aktprocsym^.isstabwritten:=true;
        end;
{$endif GDB}
    { Alignment required for Motorola }
    procinfo^.aktentrycode^.insert(new(pai_align,init(2)));
end;

{Generate the exit code for a procedure.}
procedure genexitcode(list : paasmoutput;parasize:longint; nostackframe,inlined:boolean);
var hr:Preference;          {This is for function results.}
    op:Tasmop;
    s:Topsize;

begin
    { !!!! insert there automatic destructors }

    procinfo^.aktexitcode^.insert(new(pai_label,init(aktexitlabel)));

    { call the destructor help procedure }
    if potype_destructor=aktprocsym^.definition^.proctypeoption then
     begin
       if procinfo^._class^.is_class then
         begin
           procinfo^.aktexitcode^.insert(new(paicpu,op_csymbol(A_JSR,S_NO,
             newcsymbol('FPC_DISPOSE_CLASS',0))));
         end
       else
         begin
           procinfo^.aktexitcode^.insert(new(paicpu,op_csymbol(A_JSR,S_NO,
             newcsymbol('FPC_HELP_DESTRUCTOR',0))));
           procinfo^.aktexitcode^.insert(new(paicpu,op_const_reg(A_MOVE,S_L,procinfo^._class^.vmt_offset,R_D0)));
         end;
     end;

    { call __EXIT for main program }
    { ????????? }
    if (potype_proginit=aktprocsym^.definition^.proctypeoption) and
      (target_info.target<>target_m68k_PalmOS) then
     begin
       procinfo^.aktexitcode^.concat(new(paicpu,op_csymbol(A_JSR,S_NO,newcsymbol('FPC_DO_EXIT',0))));
     end;

    { handle return value }
    if po_assembler in aktprocsym^.definition^.procoptions then
      if (aktprocsym^.definition^.proctypeoption<>potype_constructor) then
            begin
                if procinfo^.retdef<>pdef(voiddef) then
                    begin
                        if procinfo^.funcret_state<>vs_assigned then
                          CGMessage(sym_w_function_result_not_set);
                        new(hr);
                        reset_reference(hr^);
                        hr^.offset:=procinfo^.retoffset;
                        hr^.base:=procinfo^.framepointer;
                        if (procinfo^.retdef^.deftype in [orddef,enumdef]) then
                            begin
                                case procinfo^.retdef^.size of
                                 4 : procinfo^.aktexitcode^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hr,R_D0)));
                                 2 : procinfo^.aktexitcode^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,hr,R_D0)));
                                 1 : procinfo^.aktexitcode^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,hr,R_D0)));
                                end;
                            end
                        else
                            if (procinfo^.retdef^.deftype in [pointerdef,enumdef,procvardef]) or
                             ((procinfo^.retdef^.deftype=setdef) and
                             (psetdef(procinfo^.retdef)^.settype=smallset)) then
                                procinfo^.aktexitcode^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hr,R_D0)))
                            else
                                if (procinfo^.retdef^.deftype=floatdef) then
                                    begin
                                        if pfloatdef(procinfo^.retdef)^.typ=f32bit then
                                            begin
                                                { Isnt this missing ? }
                                                procinfo^.aktexitcode^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hr,R_D0)));
                                            end
                                        else
                                            begin
                                             { how the return value is handled                          }
                                             { if single value, then return in d0, otherwise return in  }
                                             { TRUE FPU register (does not apply in emulation mode)     }
                                             if (pfloatdef(procinfo^.retdef)^.typ = s32real) then
                                              begin
                                                procinfo^.aktexitcode^.concat(new(paicpu,op_ref_reg(A_MOVE,
                                                  S_L,hr,R_D0)))
                                              end
                                             else
                                              begin
                                               if cs_fp_emulation in aktmoduleswitches then
                                                 procinfo^.aktexitcode^.concat(new(paicpu,op_ref_reg(A_MOVE,
                                                    S_L,hr,R_D0)))
                                               else
                                                 procinfo^.aktexitcode^.concat(new(paicpu,op_ref_reg(A_FMOVE,
                                                 getfloatsize(pfloatdef(procinfo^.retdef)^.typ),hr,R_FP0)));
                                             end;
                                           end;
                                    end
                                else
                                    dispose(hr);
                    end
            end
        else
            begin
                { successful constructor deletes the zero flag }
                { and returns self in accumulator              }
                procinfo^.aktexitcode^.concat(new(pai_label,init(quickexitlabel)));
                { eax must be set to zero if the allocation failed !!! }
                procinfo^.aktexitcode^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,R_A5,R_D0)));
                { faster then OR on mc68000/mc68020 }
                procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_TST,S_L,R_D0)));
            end;
    procinfo^.aktexitcode^.concat(new(pai_label,init(aktexit2label)));
    if not(nostackframe) then
        procinfo^.aktexitcode^.concat(new(paicpu,op_reg(A_UNLK,S_NO,R_A6)));

    { at last, the return is generated }

    if po_interrupt in aktprocsym^.definition^.procoptions then
        generate_interrupt_stackframe_exit
    else
        if (parasize=0) or (pocall_clearstack in aktprocsym^.definition^.proccalloptions) then
            {Routines with the poclearstack flag set use only a ret.}
            { also routines with parasize=0           }
            procinfo^.aktexitcode^.concat(new(paicpu,op_none(A_RTS,S_NO)))
        else
            { return with immediate size possible here }
            { signed!                                  }
            if (aktoptprocessor = MC68020) and (parasize < $7FFF) then
                procinfo^.aktexitcode^.concat(new(paicpu,op_const(
                 A_RTD,S_NO,parasize)))
            { manually restore the stack }
            else
              begin
                    { We must pull the PC Counter from the stack, before  }
                    { restoring the stack pointer, otherwise the PC would }
                    { point to nowhere!                                   }

                    { save the PC counter (pop it from the stack)         }
                    procinfo^.aktexitcode^.concat(new(paicpu,op_reg_reg(
                         A_MOVE,S_L,R_SPPULL,R_A0)));
                    { can we do a quick addition ... }
                    if (parasize > 0) and (parasize < 9) then
                       procinfo^.aktexitcode^.concat(new(paicpu,op_const_reg(
                         A_ADD,S_L,parasize,R_SP)))
                    else { nope ... }
                       procinfo^.aktexitcode^.concat(new(paicpu,op_const_reg(
                         A_ADD,S_L,parasize,R_SP)));
                    { endif }
                    { restore the PC counter (push it on the stack)       }
                    procinfo^.aktexitcode^.concat(new(paicpu,op_reg_reg(
                         A_MOVE,S_L,R_A0,R_SPPUSH)));
                    procinfo^.aktexitcode^.concat(new(paicpu,op_none(
                      A_RTS,S_NO)))
               end;
{$ifdef GDB}
    if cs_debuginfo in aktmoduleswitches  then
        begin
            aktprocsym^.concatstabto(procinfo^.aktexitcode);
            if assigned(procinfo^._class) then
                procinfo^.aktexitcode^.concat(new(pai_stabs,init(strpnew(
                 '"$t:v'+procinfo^._class^.numberstring+'",'+
                 tostr(N_PSYM)+',0,0,'+tostr(procinfo^.selfpointer_offset)))));

            if (porddef(aktprocsym^.definition^.retdef) <> voiddef) then
                procinfo^.aktexitcode^.concat(new(pai_stabs,init(strpnew(
                 '"'+aktprocsym^.name+':X'+aktprocsym^.definition^.retdef^.numberstring+'",'+
                 tostr(N_PSYM)+',0,0,'+tostr(procinfo^.retoffset)))));

            procinfo^.aktexitcode^.concat(new(pai_stabn,init(strpnew('192,0,0,'
             +aktprocsym^.definition^.mangledname))));

            procinfo^.aktexitcode^.concat(new(pai_stabn,init(strpnew('224,0,0,'
             +lab2str(aktexit2label)))));
        end;
{$endif GDB}
end;


    { USES REGISTERS R_A0 AND R_A1 }
    { maximum size of copy is 65535 bytes                                       }
    procedure concatcopy(source,dest : treference;size : longint;delsource : boolean);

      var
         ecxpushed : boolean;
         helpsize : longint;
         i : byte;
         reg8,reg32 : tregister;
         swap : boolean;
         hregister : tregister;
         iregister : tregister;
         jregister : tregister;
         hp1 : treference;
         hp2 : treference;
         hl : pasmlabel;
         hl2: pasmlabel;

      begin
         { this should never occur }
         if size > 65535 then
           internalerror(0);
         hregister := getregister32;
         if delsource then
           del_reference(source);

         { from 12 bytes movs is being used }
         if (size<=8) or (not(cs_littlesize in aktglobalswitches) and (size<=12)) then
           begin
              helpsize:=size div 4;
              { move a dword x times }
              for i:=1 to helpsize do
                begin
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(source),hregister)));
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,hregister,newreference(dest))));
                   inc(source.offset,4);
                   inc(dest.offset,4);
                   dec(size,4);
                end;
              { move a word }
              if size>1 then
                begin
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,newreference(source),hregister)));
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_W,hregister,newreference(dest))));
                   inc(source.offset,2);
                   inc(dest.offset,2);
                   dec(size,2);
                end;
              { move a single byte }
              if size>0 then
                begin
                  exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,newreference(source),hregister)));
                  exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_B,hregister,newreference(dest))));
                end

           end
           else
           begin
              if (usableaddress > 1) then
                begin
                    iregister := getaddressreg;
                    jregister := getaddressreg;
                end
              else
              if (usableaddress = 1) then
                begin
                    iregister := getaddressreg;
                    jregister := R_A1;
                end
              else
                begin
                    iregister := R_A0;
                    jregister := R_A1;
                end;
              { reference for move (An)+,(An)+ }
              reset_reference(hp1);
              hp1.base := iregister;   { source register }
              hp1.direction := dir_inc;
              reset_reference(hp2);
              hp2.base := jregister;
              hp2.direction := dir_inc;
              { iregister = source }
              { jregister = destination }


              exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(source),iregister)));
              exprasmlist^.concat(new(paicpu,op_ref_reg(A_LEA,S_L,newreference(dest),jregister)));

              { double word move only on 68020+ machines }
              { because of possible alignment problems   }
              { use fast loop mode }
              if (aktoptprocessor=MC68020) then
                begin
                   helpsize := size - size mod 4;
                   size := size mod 4;
                   exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,helpsize div 4,hregister)));
                   getlabel(hl2);
                   emitl(A_BRA,hl2);
                   getlabel(hl);
                   emitl(A_LABEL,hl);
                   exprasmlist^.concat(new(paicpu,op_ref_ref(A_MOVE,S_L,newreference(hp1),newreference(hp2))));
                   emitl(A_LABEL,hl2);
                   exprasmlist^.concat(new(pai_labeled, init_reg(A_DBRA,hl,hregister)));
                   if size > 1 then
                     begin
                        dec(size,2);
                        exprasmlist^.concat(new(paicpu,op_ref_ref(A_MOVE,S_W,newreference(hp1), newreference(hp2))));
                     end;
                   if size = 1 then
                    exprasmlist^.concat(new(paicpu,op_ref_ref(A_MOVE,S_B,newreference(hp1), newreference(hp2))));
                end
              else
                begin
                   { Fast 68010 loop mode with no possible alignment problems }
                   helpsize := size;
                   exprasmlist^.concat(new(paicpu,op_const_reg(A_MOVE,S_L,helpsize,hregister)));
                   getlabel(hl2);
                   emitl(A_BRA,hl2);
                   getlabel(hl);
                   emitl(A_LABEL,hl);
                   exprasmlist^.concat(new(paicpu,op_ref_ref(A_MOVE,S_B,newreference(hp1),newreference(hp2))));
                   emitl(A_LABEL,hl2);
                   exprasmlist^.concat(new(pai_labeled, init_reg(A_DBRA,hl,hregister)));
                end;

       { restore the registers that we have just used olny if they are used! }
              if jregister = R_A1 then
                hp2.base := R_NO;
              if iregister = R_A0 then
                hp1.base := R_NO;
              del_reference(hp1);
              del_reference(hp2);
           end;

           { loading SELF-reference again }
           maybe_loada5;

           if delsource then
               ungetiftemp(source);

           ungetregister32(hregister);
    end;


    procedure emitloadord2reg(location:Tlocation;orddef:Porddef;
                              destreg:Tregister;delloc:boolean);

    {A lot smaller and less bug sensitive than the original unfolded loads.}

    var tai:paicpu;
        r:Preference;

    begin
        case location.loc of
            LOC_REGISTER,LOC_CREGISTER:
                begin
                    case orddef^.typ of
                        u8bit: begin
                                 exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_B,location.register,destreg)));
                                 exprasmlist^.concat(new(paicpu,op_const_reg(A_ANDI,S_L,$FF,destreg)));
                               end;
                        s8bit: begin
                                 exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_B,location.register,destreg)));
                                 if (aktoptprocessor <> MC68020) then
                                  begin
                                 { byte to word }
                                     exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_W,destreg)));
                                 { word to long }
                                     exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,destreg)));
                                  end
                                 else { 68020+ and later only }
                                     exprasmlist^.concat(new(paicpu,op_reg(A_EXTB,S_L,destreg)));
                                end;
                        u16bit: begin
                                 exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_W,location.register,destreg)));
                                 exprasmlist^.concat(new(paicpu,op_const_reg(A_ANDI,S_L,$FFFF,destreg)));
                                end;
                        s16bit: begin
                                 exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_W,location.register,destreg)));
                                 { word to long }
                                 exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,destreg)));
                                end;
                        u32bit:
                            exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,location.register,destreg)));
                        s32bit:
                            exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVE,S_L,location.register,destreg)));
                    end;
                    if delloc then
                        ungetregister(location.register);
                end;
            LOC_REFERENCE:
                begin
                    r:=newreference(location.reference);
                    case orddef^.typ of
                        u8bit: begin
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,r,destreg)));
                                 exprasmlist^.concat(new(paicpu,op_const_reg(A_ANDI,S_L,$FF,destreg)));
                               end;
                        s8bit:  begin
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_B,r,destreg)));
                                 if (aktoptprocessor <> MC68020) then
                                  begin
                                 { byte to word }
                                     exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_W,destreg)));
                                 { word to long }
                                     exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,destreg)));
                                  end
                                 else { 68020+ and later only }
                                     exprasmlist^.concat(new(paicpu,op_reg(A_EXTB,S_L,destreg)));
                                end;
                        u16bit: begin
                                 exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,r,destreg)));
                                 exprasmlist^.concat(new(paicpu,op_const_reg(A_ANDI,S_L,$ffff,destreg)));
                                end;
                        s16bit: begin
                                       exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_W,r,destreg)));
                                 { word to long }
                                 exprasmlist^.concat(new(paicpu,op_reg(A_EXT,S_L,destreg)));
                                end;
                        u32bit:
                            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,r,destreg)));
                        s32bit:
                            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,r,destreg)));
                    end;
                    if delloc then
                        del_reference(location.reference);
                end
            else
                internalerror(6);
        end;
    end;


    { if necessary A5 is reloaded after a call}
    procedure maybe_loada5;

      var
         hp : preference;
         p : pprocinfo;
         i : longint;

      begin
         if assigned(procinfo^._class) then
           begin
              if lexlevel>normal_function_level then
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo^.framepointer_offset;
                   hp^.base:=procinfo^.framepointer;
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,R_A5)));
                   p:=procinfo^.parent;
                   for i:=3 to lexlevel-1 do
                     begin
                        new(hp);
                        reset_reference(hp^);
                        hp^.offset:=p^.framepointer_offset;
                        hp^.base:=R_A5;
                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,R_A5)));
                        p:=p^.parent;
                     end;
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=p^.selfpointer_offset;
                   hp^.base:=R_A5;
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,R_A5)));
                end
              else
                begin
                   new(hp);
                   reset_reference(hp^);
                   hp^.offset:=procinfo^.selfpointer_offset;
                   hp^.base:=procinfo^.framepointer;
                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,hp,R_A5)));
                end;
           end;
      end;


  (***********************************************************************)
  (* PROCEDURE FLOATLOAD                                                 *)
  (*  Description: This routine is to be called each time a location     *)
  (*   must be set to LOC_FPU and a value loaded into a FPU register.    *)
  (*                                                                     *)
  (*  Remark: The routine sets up the register field of LOC_FPU correctly*)
  (***********************************************************************)

    procedure floatload(t : tfloattype;const ref : treference; var location:tlocation);

      var
         op : tasmop;
         s : topsize;

      begin
        { no emulation }
        case t of
            s32real : s := S_FS;
            s64real : s := S_FL;
            s80real : s := S_FX;
         else
           begin
             CGMessage(cg_f_unknown_float_type);
           end;
        end; { end case }
        location.loc := LOC_FPU;
        if not ((cs_fp_emulation) in aktmoduleswitches) then
        begin
            location.fpureg := getfloatreg;
            exprasmlist^.concat(new(paicpu,op_ref_reg(A_FMOVE,s,newreference(ref),location.fpureg)))
        end
        else
        { handle emulation }
        begin
          if t = s32real then
          begin
            location.fpureg := getregister32;
            exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOVE,S_L,newreference(ref),location.fpureg)))
          end
          else
             { other floating types are not supported in emulation mode }
            CGMessage(sym_e_type_id_not_defined);
        end;
      end;

{    procedure floatstoreops(t : tfloattype;var op : tasmop;var s : topsize);

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
            s64bit : begin
                         op:=A_FISTP;
                         s:=S_IQ;
                      end;
            else internalerror(17);
         end;
      end; }


    { stores an FPU value to memory }
    { location:tlocation used to free up FPU register }
    { ref: destination of storage                     }
    procedure floatstore(t : tfloattype;var location:tlocation; const ref:treference);

      var
         op : tasmop;
         s : topsize;

      begin
        if location.loc <> LOC_FPU then
         InternalError(34);
        { no emulation }
        case t of
            s32real : s := S_FS;
            s64real : s := S_FL;
            s80real : s := S_FX;
         else
           begin
             CGMessage(cg_f_unknown_float_type);
           end;
        end; { end case }
        if not ((cs_fp_emulation) in aktmoduleswitches) then
        begin
            { This permits the mixing of emulation and non-emulation routines }
            { only possible for REAL = SINGLE value_str                          }
            if not (location.fpureg in [R_FP0..R_FP7]) then
             Begin
               if s = S_FS then
                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,location.fpureg,newreference(ref))))
               else
                 internalerror(255);
             end
            else
               exprasmlist^.concat(new(paicpu,op_reg_ref(A_FMOVE,s,location.fpureg,newreference(ref))));
            ungetregister(location.fpureg);
        end
        else
        { handle emulation }
        begin
          if t = s32real then
          begin
            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,S_L,location.fpureg,newreference(ref))));
            ungetregister32(location.fpureg);
          end
          else
             { other floating types are not supported in emulation mode }
            CGMessage(sym_e_type_id_not_defined);
        end;
        location.fpureg:=R_NO;  { no register in LOC_FPU now }
      end;

    procedure firstcomplex(p : ptree);

      var
         hp : ptree;

      begin
         { always calculate boolean AND and OR from left to right }
         if ((p^.treetype=orn) or (p^.treetype=andn)) and
           (p^.left^.resulttype^.deftype=orddef) and
           (porddef(p^.left^.resulttype)^.typ=bool8bit) then
           p^.swaped:=false
         else if (p^.left^.registers32<p^.right^.registers32)

           { the following check is appropriate, because all }
           { 4 registers are rarely used and it is thereby   }
                      { achieved that the extra code is being dropped   }
           { by exchanging not commutative operators         }
           and (p^.right^.registers32<=4) then
           begin
              hp:=p^.left;
              p^.left:=p^.right;
              p^.right:=hp;
              p^.swaped:=true;
           end
         else p^.swaped:=false;
      end;


{$ifdef test_dest_loc}
        procedure mov_reg_to_dest(p : ptree; s : topsize; reg : tregister);

          begin
             if (dest_loc.loc=LOC_CREGISTER) or (dest_loc.loc=LOC_REGISTER) then
               begin
                 emit_reg_reg(A_MOVE,s,reg,dest_loc.register);
                 set_location(p^.location,dest_loc);
                 in_dest_loc:=true;
               end
             else
             if (dest_loc.loc=LOC_REFERENCE) or (dest_loc.loc=LOC_MEM) then
               begin
                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVE,s,reg,newreference(dest_loc.reference))));
                 set_location(p^.location,dest_loc);
                 in_dest_loc:=true;
               end
             else
               internalerror(20080);
          end;

{$endif test_dest_loc}

    procedure removetemps(list : paasmoutput;p : plinkedlist);

      var
         hp : ptemptodestroy;

      begin
         hp:=ptemptodestroy(p^.first);
         while assigned(hp) do
           begin
              if is_ansistring(hp^.typ) then
                begin
                   emitpushreferenceaddr(list,hp^.address);
                   list^.concat(new(paicpu,
                     op_csymbol(A_JSR,S_NO,newcsymbol('FPC_ANSISTR_DECR_REF',0))));
                end;
              hp:=ptemptodestroy(hp^.next);
           end;
      end;

    procedure releasedata(p : plinkedlist);

      var
         hp : ptemptodestroy;

      begin
         hp:=ptemptodestroy(p^.first);
         while assigned(hp) do
           begin
              ungetiftemp(hp^.address);
              hp:=ptemptodestroy(hp^.next);
           end;
      end;

end.
{
  $Log$
  Revision 1.2  2002-04-20 21:40:48  carl
  * renamed some constants

  Revision 1.1  2000/11/30 20:30:34  peter
    * moved into m68k subdir

  Revision 1.2  2000/07/13 11:32:37  michael
  + removed logs

}
