{
    Copyright (c) 2000-2005 by Florian Klaempfl

    Type checking and register allocation for math nodes

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
unit nmat;

{$i fpcdefs.inc}

interface

    uses
       node;

    type
       tmoddivnode = class(tbinopnode)
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function simplify : tnode;override;
         protected
{$ifndef cpu64bitalu}
          { override the following if you want to implement }
          { parts explicitely in the code generator (JM)    }
          function first_moddiv64bitint: tnode; virtual;
{$endif not cpu64bitalu}
          function firstoptimize: tnode; virtual;
          function first_moddivint: tnode; virtual;
       end;
       tmoddivnodeclass = class of tmoddivnode;

       tshlshrnode = class(tbinopnode)
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function simplify : tnode;override;
{$ifndef cpu64bitalu}
          { override the following if you want to implement }
          { parts explicitely in the code generator (CEC)
            Should return nil, if everything will be handled
            in the code generator
          }
          function first_shlshr64bitint: tnode; virtual;
{$endif not cpu64bitalu}
       end;
       tshlshrnodeclass = class of tshlshrnode;

       tunaryminusnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function simplify : tnode;override;
       end;
       tunaryminusnodeclass = class of tunaryminusnode;

       tnotnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function simplify : tnode;override;
       {$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
       {$endif}
       end;
       tnotnodeclass = class of tnotnode;

    var
       cmoddivnode : tmoddivnodeclass;
       cshlshrnode : tshlshrnodeclass;
       cunaryminusnode : tunaryminusnodeclass;
       cnotnode : tnotnodeclass;

implementation

    uses
      systems,
      verbose,globals,cutils,
      globtype,constexp,
      symconst,symtype,symdef,symtable,
      defutil,
      htypechk,pass_1,
      cgbase,
      ncon,ncnv,ncal,nadd,
      nutils;

{****************************************************************************
                              TMODDIVNODE
 ****************************************************************************}

    function tmoddivnode.simplify:tnode;
      var
        t : tnode;
        rv,lv : tconstexprint;
      begin
        result:=nil;

        if is_constintnode(right) then
          begin
            if tordconstnode(right).value = 1 then
              begin
                case nodetype of
                  modn:
                    result := cordconstnode.create(0,left.resultdef,true);
                  divn:
                    result := left.getcopy;
                end;
                exit;
              end;
            if tordconstnode(right).value = 0 then
              begin
                Message(parser_e_division_by_zero);
                { recover }
                tordconstnode(right).value := 1;
              end;
          end;

        if is_constintnode(right) and is_constintnode(left) then
          begin
            rv:=tordconstnode(right).value;
            lv:=tordconstnode(left).value;

            case nodetype of
              modn:
                t:=genintconstnode(lv mod rv);
              divn:
                t:=genintconstnode(lv div rv);
            end;
            result:=t;
            exit;
         end;
      end;


    function tmoddivnode.pass_typecheck:tnode;
      var
        hp,t : tnode;
        rd,ld : torddef;
      begin
         result:=nil;
         typecheckpass(left);
         typecheckpass(right);
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         set_varstate(right,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         { tp procvar support }
         maybe_call_procvar(left,true);
         maybe_call_procvar(right,true);

         result:=simplify;
         if assigned(result) then
           exit;

         { allow operator overloading }
         t:=self;
         if isbinaryoverloaded(t) then
           begin
              result:=t;
              exit;
           end;

         { we need 2 orddefs always }
         if (left.resultdef.typ<>orddef) then
           inserttypeconv(right,sinttype);
         if (right.resultdef.typ<>orddef) then
           inserttypeconv(right,sinttype);
         if codegenerror then
           exit;

         rd:=torddef(right.resultdef);
         ld:=torddef(left.resultdef);

         { if one operand is a cardinal and the other is a positive constant, convert the }
         { constant to a cardinal as well so we don't have to do a 64bit division (JM)    }
         { Do the same for qwords and positive constants as well, otherwise things like   }
         { "qword mod 10" are evaluated with int64 as result, which is wrong if the       }
         { "qword" was > high(int64) (JM)                                                 }
         { Additionally, do the same for cardinal/qwords and other positive types, but    }
         { always in a way that a smaller type is converted to a bigger type              }
         { (webtbs/tw8870)                                                                }
         if (rd.ordtype in [u32bit,u64bit]) and
            ((is_constintnode(left) and
              (tordconstnode(left).value >= 0)) or
             (not is_signed(ld) and
              (rd.size >= ld.size))) then
           begin
             inserttypeconv(left,right.resultdef);
             ld:=torddef(left.resultdef);
           end;
         if (ld.ordtype in [u32bit,u64bit]) and
            ((is_constintnode(right) and
              (tordconstnode(right).value >= 0)) or
             (not is_signed(rd) and
              (ld.size >= rd.size))) then
          begin
            inserttypeconv(right,left.resultdef);
            rd:=torddef(right.resultdef);
          end;

         { when there is one currency value, everything is done
           using currency }
         if (ld.ordtype=scurrency) or
            (rd.ordtype=scurrency) then
           begin
             if (ld.ordtype<>scurrency) then
              inserttypeconv(left,s64currencytype);
             if (rd.ordtype<>scurrency) then
              inserttypeconv(right,s64currencytype);
             resultdef:=left.resultdef;
           end
         else
{$ifndef cpu64bitaddr}
          { when there is one 64bit value, everything is done
            in 64bit }
          if (is_64bitint(left.resultdef) or
              is_64bitint(right.resultdef)) then
           begin
             if is_signed(rd) or is_signed(ld) then
               begin
                  if (ld.ordtype<>s64bit) then
                    inserttypeconv(left,s64inttype);
                  if (rd.ordtype<>s64bit) then
                    inserttypeconv(right,s64inttype);
               end
             else
               begin
                  if (ld.ordtype<>u64bit) then
                    inserttypeconv(left,u64inttype);
                  if (rd.ordtype<>u64bit) then
                    inserttypeconv(right,u64inttype);
               end;
             resultdef:=left.resultdef;
           end
         else
          { when mixing cardinals and signed numbers, convert everythign to 64bit (JM) }
          if ((rd.ordtype = u32bit) and
              is_signed(ld)) or
             ((ld.ordtype = u32bit) and
              is_signed(rd)) then
           begin
              CGMessage(type_w_mixed_signed_unsigned);
              if (ld.ordtype<>s64bit) then
                inserttypeconv(left,s64inttype);
              if (rd.ordtype<>s64bit) then
                inserttypeconv(right,s64inttype);
              resultdef:=left.resultdef;
           end
         else
{$endif not cpu64bitaddr}
           begin
              { Make everything always default singed int }
              if not(rd.ordtype in [torddef(sinttype).ordtype,torddef(uinttype).ordtype]) then
                inserttypeconv(right,sinttype);
              if not(ld.ordtype in [torddef(sinttype).ordtype,torddef(uinttype).ordtype]) then
                inserttypeconv(left,sinttype);
              resultdef:=right.resultdef;
           end;

         { when the result is currency we need some extra code for
           division. this should not be done when the divn node is
           created internally }
         if (nodetype=divn) and
            not(nf_is_currency in flags) and
            is_currency(resultdef) then
          begin
            hp:=caddnode.create(muln,getcopy,cordconstnode.create(10000,s64currencytype,false));
            include(hp.flags,nf_is_currency);
            result:=hp;
          end;
      end;


    function tmoddivnode.first_moddivint: tnode;
{$ifdef cpuneedsdiv32helper}
      var
        procname: string[31];
      begin
        result := nil;

        { otherwise create a call to a helper }
        if nodetype = divn then
          procname := 'fpc_div_'
        else
          procname := 'fpc_mod_';
        { only qword needs the unsigned code, the
          signed code is also used for currency }
        if is_signed(resultdef) then
          procname := procname + 'longint'
        else
          procname := procname + 'dword';

        result := ccallnode.createintern(procname,ccallparanode.create(left,
          ccallparanode.create(right,nil)));
        left := nil;
        right := nil;
        firstpass(result);
      end;
{$else cpuneedsdiv32helper}
      begin
        result:=nil;
      end;
{$endif cpuneedsdiv32helper}


{$ifndef cpu64bitalu}
    function tmoddivnode.first_moddiv64bitint: tnode;
      var
        procname: string[31];
      begin
        result := nil;

        { when currency is used set the result of the
          parameters to s64bit, so they are not converted }
        if is_currency(resultdef) then
          begin
            left.resultdef:=s64inttype;
            right.resultdef:=s64inttype;
          end;

        { otherwise create a call to a helper }
        if nodetype = divn then
          procname := 'fpc_div_'
        else
          procname := 'fpc_mod_';
        { only qword needs the unsigned code, the
          signed code is also used for currency }
        if is_signed(resultdef) then
          procname := procname + 'int64'
        else
          procname := procname + 'qword';

        result := ccallnode.createintern(procname,ccallparanode.create(left,
          ccallparanode.create(right,nil)));
        left := nil;
        right := nil;
        firstpass(result);
      end;
{$endif not cpu64bitalu}


    function tmoddivnode.firstoptimize: tnode;
      var
        power{,shiftval} : longint;
        newtype: tnodetype;
      begin
        result := nil;
        { divide/mod a number by a constant which is a power of 2? }
        if (cs_opt_peephole in current_settings.optimizerswitches) and
           (right.nodetype = ordconstn) and
{           ((nodetype = divn) or
            not is_signed(resultdef)) and}
           (not is_signed(resultdef)) and
           ispowerof2(tordconstnode(right).value,power) then
          begin
            if nodetype = divn then
              begin
(*
                if is_signed(resultdef) then
                  begin
                    if is_64bitint(left.resultdef) then
                      if not (cs_opt_size in current_settings.optimizerswitches) then
                        shiftval := 63
                      else
                        { the shift code is a lot bigger than the call to }
                        { the divide helper                               }
                        exit
                    else
                      shiftval := 31;
                    { we reuse left twice, so create once a copy of it     }
                    { !!! if left is a call is -> call gets executed twice }
                    left := caddnode.create(addn,left,
                      caddnode.create(andn,
                        cshlshrnode.create(sarn,left.getcopy,
                          cordconstnode.create(shiftval,sinttype,false)),
                        cordconstnode.create(tordconstnode(right).value-1,
                          right.resultdef,false)));
                    newtype := sarn;
                  end
                else
*)
                  newtype := shrn;
                tordconstnode(right).value := power;
                result := cshlshrnode.create(newtype,left,right)
              end
            else
              begin
                dec(tordconstnode(right).value.uvalue);
                result := caddnode.create(andn,left,right);
              end;
            { left and right are reused }
            left := nil;
            right := nil;
            firstpass(result);
            exit;
          end;
      end;


    function tmoddivnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         { Try to optimize mod/div }
         result := firstoptimize;
         if assigned(result) then
           exit;

{$ifndef cpu64bitalu}
         { 64bit }
         if (left.resultdef.typ=orddef) and
            (right.resultdef.typ=orddef) and
            (is_64bitint(left.resultdef) or is_64bitint(right.resultdef)) then
           begin
             result := first_moddiv64bitint;
             if assigned(result) then
               exit;
             expectloc:=LOC_REGISTER;
           end
         else
{$endif not cpu64bitalu}
           begin
             result := first_moddivint;
             if assigned(result) then
               exit;
           end;
         expectloc:=LOC_REGISTER;
      end;



{****************************************************************************
                              TSHLSHRNODE
 ****************************************************************************}

    function tshlshrnode.simplify:tnode;
      var
        t : tnode;
      begin
        result:=nil;
        { constant folding }
        if is_constintnode(left) and is_constintnode(right) then
          begin
             case nodetype of
                shrn:
                  t:=genintconstnode(tordconstnode(left).value shr tordconstnode(right).value);
                shln:
                  t:=genintconstnode(tordconstnode(left).value shl tordconstnode(right).value);
             end;
             result:=t;
             exit;
          end;

      end;


    function tshlshrnode.pass_typecheck:tnode;
      var
         t : tnode;
      begin
         result:=nil;
         typecheckpass(left);
         typecheckpass(right);
         set_varstate(right,vs_read,[vsf_must_be_valid]);
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         { tp procvar support }
         maybe_call_procvar(left,true);
         maybe_call_procvar(right,true);

         result:=simplify;
         if assigned(result) then
           exit;

         { allow operator overloading }
         t:=self;
         if isbinaryoverloaded(t) then
           begin
              result:=t;
              exit;
           end;

         { calculations for ordinals < 32 bit have to be done in
           32 bit for backwards compatibility. That way 'shl 33' is
           the same as 'shl 1'. It's ugly but compatible with delphi/tp/gcc }
         if (not is_64bit(left.resultdef)) and
            (torddef(left.resultdef).ordtype<>u32bit) then
           begin
             { keep singness of orignal type }
             if is_signed(left.resultdef) then
               inserttypeconv(left,s32inttype)
             else
               inserttypeconv(left,u32inttype);
           end;

         inserttypeconv(right,sinttype);

         resultdef:=left.resultdef;
      end;


{$ifndef cpu64bitalu}
    function tshlshrnode.first_shlshr64bitint: tnode;
      var
        procname: string[31];
      begin
        result := nil;
        { otherwise create a call to a helper }
        if nodetype = shln then
          procname := 'fpc_shl_int64'
        else
          procname := 'fpc_shr_int64';
        { this order of parameters works at least for the arm,
          however it should work for any calling conventions (FK) }
        result := ccallnode.createintern(procname,ccallparanode.create(right,
          ccallparanode.create(left,nil)));
        left := nil;
        right := nil;
        firstpass(result);
      end;
{$endif not cpu64bitalu}


    function tshlshrnode.pass_1 : tnode;
      var
         regs : longint;
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

{$ifndef cpu64bitalu}
         { 64 bit ints have their own shift handling }
         if is_64bit(left.resultdef) then
           begin
             result := first_shlshr64bitint;
             if assigned(result) then
               exit;
             regs:=2;
           end
         else
{$endif not cpu64bitalu}
           begin
             regs:=1
           end;

         if (right.nodetype<>ordconstn) then
           inc(regs);
         expectloc:=LOC_REGISTER;
      end;


{****************************************************************************
                            TUNARYMINUSNODE
 ****************************************************************************}

    constructor tunaryminusnode.create(expr : tnode);
      begin
         inherited create(unaryminusn,expr);
      end;


    function tunaryminusnode.simplify:tnode;
      begin
        result:=nil;
        { constant folding }
        if is_constintnode(left) then
          begin
             result:=genintconstnode(-tordconstnode(left).value);
             exit;
          end;
        if is_constrealnode(left) then
          begin
             trealconstnode(left).value_real:=-trealconstnode(left).value_real;
             trealconstnode(left).value_currency:=-trealconstnode(left).value_currency;
             result:=left;
             left:=nil;
             exit;
          end;
      end;


    function tunaryminusnode.pass_typecheck : tnode;
      var
         t : tnode;
      begin
         result:=nil;
         typecheckpass(left);
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         result:=simplify;
         if assigned(result) then
           exit;

         resultdef:=left.resultdef;
         if (left.resultdef.typ=floatdef) or
            is_currency(left.resultdef) then
           begin
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in current_settings.localswitches) and
           is_mmx_able_array(left.resultdef) then
             begin
               { if saturation is on, left.resultdef isn't
                 "mmx able" (FK)
               if (cs_mmx_saturation in current_settings.localswitches^) and
                 (torddef(tarraydef(resultdef).definition).typ in
                 [s32bit,u32bit]) then
                 CGMessage(type_e_mismatch);
               }
             end
{$endif SUPPORT_MMX}
{$ifndef cpu64bitaddr}
         else if is_64bit(left.resultdef) then
           begin
             inserttypeconv(left,s64inttype);
             resultdef:=left.resultdef
           end
{$endif not cpu64bitaddr}
         else if (left.resultdef.typ=orddef) then
           begin
             inserttypeconv(left,sinttype);
             resultdef:=left.resultdef
           end
         else
           begin
             { allow operator overloading }
             t:=self;
             if isunaryoverloaded(t) then
               begin
                  result:=t;
                  exit;
               end;

             CGMessage(type_e_mismatch);
           end;
      end;

    { generic code     }
    { overridden by:   }
    {   i386           }
    function tunaryminusnode.pass_1 : tnode;
      var
        procname: string[31];
        fdef : tdef;
      begin
        result:=nil;
        firstpass(left);
        if codegenerror then
          exit;

        if (cs_fp_emulation in current_settings.moduleswitches) and (left.resultdef.typ=floatdef) then
          begin
            if not(target_info.system in system_wince) then
              begin
                case tfloatdef(resultdef).floattype of
                  s32real:
                    begin
                      procname:='float32_sub';
                      fdef:=search_system_type('FLOAT32REC').typedef;
                    end;
                  s64real:
                    begin
                      procname:='float64_sub';
                      fdef:=search_system_type('FLOAT64').typedef;
                    end;
                  {!!! not yet implemented
                  s128real:
                  }
                  else
                    internalerror(2005082801);
                end;
                result:=ctypeconvnode.create_internal(ccallnode.createintern(procname,ccallparanode.create(
                  ctypeconvnode.create_internal(left,fDef),
                  ccallparanode.create(ctypeconvnode.create_internal(crealconstnode.create(0,resultdef),fdef),nil))),resultdef);
              end
            else
              begin
                case tfloatdef(resultdef).floattype of
                  s32real:
                    procname:='NEGS';
                  s64real:
                    procname:='NEGD';
                  {!!! not yet implemented
                  s128real:
                  }
                  else
                    internalerror(2005082802);
                end;
                result:=ccallnode.createintern(procname,ccallparanode.create(left,nil));
              end;

            left:=nil;
          end
        else
          begin
            if (left.resultdef.typ=floatdef) then
              expectloc:=LOC_FPUREGISTER
{$ifdef SUPPORT_MMX}
             else if (cs_mmx in current_settings.localswitches) and
               is_mmx_able_array(left.resultdef) then
              expectloc:=LOC_MMXREGISTER
{$endif SUPPORT_MMX}
             else if (left.resultdef.typ=orddef) then
               expectloc:=LOC_REGISTER;
          end;
      end;


{****************************************************************************
                               TNOTNODE
 ****************************************************************************}

    const
      boolean_reverse:array[ltn..unequaln] of Tnodetype=(
        gten,gtn,lten,ltn,unequaln,equaln
      );

    constructor tnotnode.create(expr : tnode);
      begin
         inherited create(notn,expr);
      end;


    function tnotnode.simplify:tnode;
      var
        v : tconstexprint;
        t : tnode;
        def : tdef;
      begin
        result:=nil;
        { Try optmimizing ourself away }
        if left.nodetype=notn then
          begin
            { Double not. Remove both }
            result:=Tnotnode(left).left;
            tnotnode(left).left:=nil;
            exit;
          end;

        if (left.nodetype in [ltn,lten,equaln,unequaln,gtn,gten]) then
         begin
           { Not of boolean expression. Turn around the operator and remove
             the not. This is not allowed for sets with the gten/lten,
             because there is no ltn/gtn support }
           if (taddnode(left).left.resultdef.typ<>setdef) or
              (left.nodetype in [equaln,unequaln]) then
            begin
              result:=left;
              left.nodetype:=boolean_reverse[left.nodetype];
              left:=nil;
              exit;
            end;
         end;

        { constant folding }
        if (left.nodetype=ordconstn) then
          begin
             v:=tordconstnode(left).value;
             def:=left.resultdef;
             case torddef(left.resultdef).ordtype of
               pasbool,
               bool8bit,
               bool16bit,
               bool32bit,
               bool64bit:
                 begin
                   v:=byte(not(boolean(int64(v))));
                   if (torddef(left.resultdef).ordtype<>pasbool) then
                     v:=-v;
                 end;
               uchar,
               uwidechar,
               u8bit,
               s8bit,
               u16bit,
               s16bit,
               s32bit,
{$ifdef cpu64bitaddr}
               u32bit,
{$endif cpu64bitaddr}
               s64bit:
                 begin
                   v:=int64(not int64(v));
                   if (torddef(left.resultdef).ordtype<>s64bit) then
                     def:=sinttype
                   else
                     def:=s64inttype;
                 end;
{$ifndef cpu64bitaddr}
               u32bit,
{$endif not cpu64bitaddr}
               u64bit :
                 begin
                   { Delphi-compatible: not dword = dword (not word = longint) }
                   { Extension: not qword = qword                              }
                   v:=qword(not qword(v));
                   { will be truncated by the ordconstnode for u32bit }
                 end;
               else
                 CGMessage(type_e_mismatch);
             end;
             t:=cordconstnode.create(v,def,false);
             result:=t;
             exit;
          end;
      end;


    function tnotnode.pass_typecheck : tnode;
      var
         t : tnode;
      begin
         result:=nil;
         typecheckpass(left);
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         { tp procvar support }
         maybe_call_procvar(left,true);

         resultdef:=left.resultdef;

         result:=simplify;
         if assigned(result) then
           exit;

         if is_boolean(resultdef) then
           begin
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in current_settings.localswitches) and
             is_mmx_able_array(left.resultdef) then
             begin
             end
         else
{$endif SUPPORT_MMX}
{$ifndef cpu64bitaddr}
           if is_64bitint(left.resultdef) then
             begin
             end
         else
{$endif not cpu64bitaddr}
           if is_integer(left.resultdef) then
             begin
             end
         else
           begin
             { allow operator overloading }
             t:=self;
             if isunaryoverloaded(t) then
               begin
                  result:=t;
                  exit;
               end;

             CGMessage(type_e_mismatch);
           end;
      end;


    function tnotnode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
           exit;

         expectloc:=left.expectloc;
         if is_boolean(resultdef) then
           begin
             if (expectloc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
               expectloc:=LOC_REGISTER;
            { before loading it into flags we need to load it into
              a register thus 1 register is need PM }
{$ifdef cpuflags}
             if left.expectloc<>LOC_JUMP then
               expectloc:=LOC_FLAGS;
{$endif def cpuflags}
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in current_settings.localswitches) and
             is_mmx_able_array(left.resultdef) then
             expectloc:=LOC_MMXREGISTER
         else
{$endif SUPPORT_MMX}
{$ifndef cpu64bitalu}
           if is_64bit(left.resultdef) then
             begin
                if (expectloc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
                  expectloc:=LOC_REGISTER;
             end
         else
{$endif not cpu64bitalu}
           if is_integer(left.resultdef) then
             expectloc:=LOC_REGISTER;
      end;

{$ifdef state_tracking}
    function Tnotnode.track_state_pass(exec_known:boolean):boolean;
      begin
        track_state_pass:=true;
        if left.track_state_pass(exec_known) then
          begin
            left.resultdef:=nil;
            do_typecheckpass(left);
          end;
      end;
{$endif}

begin
   cmoddivnode:=tmoddivnode;
   cshlshrnode:=tshlshrnode;
   cunaryminusnode:=tunaryminusnode;
   cnotnode:=tnotnode;
end.
