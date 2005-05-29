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
          function det_resulttype:tnode;override;
          function simplify : tnode;override;
         protected
{$ifndef cpu64bit}
          { override the following if you want to implement }
          { parts explicitely in the code generator (JM)    }
          function first_moddiv64bitint: tnode; virtual;
{$endif cpu64bit}
          function firstoptimize: tnode; virtual;
          function first_moddivint: tnode; virtual;
       end;
       tmoddivnodeclass = class of tmoddivnode;

       tshlshrnode = class(tbinopnode)
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function simplify : tnode;override;
{$ifndef cpu64bit}
          { override the following if you want to implement }
          { parts explicitely in the code generator (CEC)
            Should return nil, if everything will be handled
            in the code generator
          }
          function first_shlshr64bitint: tnode; virtual;
{$endif cpu64bit}
       end;
       tshlshrnodeclass = class of tshlshrnode;

       tunaryminusnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function simplify : tnode;override;
       end;
       tunaryminusnodeclass = class of tunaryminusnode;

       tnotnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
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
      globtype,
      symconst,symtype,symdef,defutil,
      htypechk,pass_1,
      cgbase,
      ncon,ncnv,ncal,nadd;

{****************************************************************************
                              TMODDIVNODE
 ****************************************************************************}

    function tmoddivnode.simplify:tnode;
      var
        t : tnode;
        rd,ld : torddef;
        rv,lv : tconstexprint;
      begin
        result:=nil;

        if is_constintnode(right) and is_constintnode(left) then
          begin
            rd:=torddef(right.resulttype.def);
            ld:=torddef(left.resulttype.def);

            rv:=tordconstnode(right).value;
            lv:=tordconstnode(left).value;

            case nodetype of
              modn:
                if (torddef(ld).typ <> u64bit) or
                   (torddef(rd).typ <> u64bit) then
                  t:=genintconstnode(lv mod rv)
                else
                  t:=genintconstnode(int64(qword(lv) mod qword(rv)));
              divn:
                if (torddef(ld).typ <> u64bit) or
                   (torddef(rd).typ <> u64bit) then
                  t:=genintconstnode(lv div rv)
                else
                  t:=genintconstnode(int64(qword(lv) div qword(rv)));
            end;
            result:=t;
            exit;
         end;
      end;


    function tmoddivnode.det_resulttype:tnode;
      var
        hp,t : tnode;
        rd,ld : torddef;
        rv : tconstexprint;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         set_varstate(left,vs_used,[vsf_must_be_valid]);
         set_varstate(right,vs_used,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         { we need 2 orddefs always }
         if (left.resulttype.def.deftype<>orddef) then
           inserttypeconv(right,sinttype);
         if (right.resulttype.def.deftype<>orddef) then
           inserttypeconv(right,sinttype);
         if codegenerror then
           exit;

         rd:=torddef(right.resulttype.def);
         ld:=torddef(left.resulttype.def);

         { check for division by zero }
         if is_constintnode(right) then
           begin
             rv:=tordconstnode(right).value;
             if (rv=0) then
               begin
                 Message(parser_e_division_by_zero);
                 { recover }
                 rv:=1;
               end;
            end;

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

         { if one operand is a cardinal and the other is a positive constant, convert the }
         { constant to a cardinal as well so we don't have to do a 64bit division (JM)    }
         { Do the same for qwords and positive constants as well, otherwise things like   }
         { "qword mod 10" are evaluated with int64 as result, which is wrong if the       }
         { "qword" was > high(int64) (JM)                                                 }
         if (rd.typ in [u32bit,u64bit]) and
            is_constintnode(left) and
            (tordconstnode(left).value >= 0) then
           begin
             inserttypeconv(left,right.resulttype);
             ld:=torddef(left.resulttype.def);
           end;
         if (ld.typ in [u32bit,u64bit]) and
            is_constintnode(right) and
            (tordconstnode(right).value >= 0) then
          begin
            inserttypeconv(right,left.resulttype);
            rd:=torddef(right.resulttype.def);
          end;

         { when there is one currency value, everything is done
           using currency }
         if (ld.typ=scurrency) or
            (rd.typ=scurrency) then
           begin
             if (ld.typ<>scurrency) then
              inserttypeconv(left,s64currencytype);
             if (rd.typ<>scurrency) then
              inserttypeconv(right,s64currencytype);
             resulttype:=left.resulttype;
           end
         else
{$ifndef cpu64bit}
          { when there is one 64bit value, everything is done
            in 64bit }
          if (is_64bitint(left.resulttype.def) or
              is_64bitint(right.resulttype.def)) then
           begin
             if is_signed(rd) or is_signed(ld) then
               begin
                  if (ld.typ<>s64bit) then
                    inserttypeconv(left,s64inttype);
                  if (rd.typ<>s64bit) then
                    inserttypeconv(right,s64inttype);
               end
             else
               begin
                  if (ld.typ<>u64bit) then
                    inserttypeconv(left,u64inttype);
                  if (rd.typ<>u64bit) then
                    inserttypeconv(right,u64inttype);
               end;
             resulttype:=left.resulttype;
           end
         else
          { when mixing cardinals and signed numbers, convert everythign to 64bit (JM) }
          if ((rd.typ = u32bit) and
              is_signed(ld)) or
             ((ld.typ = u32bit) and
              is_signed(rd)) then
           begin
              CGMessage(type_w_mixed_signed_unsigned);
              if (ld.typ<>s64bit) then
                inserttypeconv(left,s64inttype);
              if (rd.typ<>s64bit) then
                inserttypeconv(right,s64inttype);
              resulttype:=left.resulttype;
           end
         else
{$endif cpu64bit}
           begin
              { Make everything always default singed int }
              if not(rd.typ in [torddef(sinttype.def).typ,torddef(uinttype.def).typ]) then
                inserttypeconv(right,sinttype);
              if not(ld.typ in [torddef(sinttype.def).typ,torddef(uinttype.def).typ]) then
                inserttypeconv(left,sinttype);
              resulttype:=right.resulttype;
           end;

         { when the result is currency we need some extra code for
           division. this should not be done when the divn node is
           created internally }
         if (nodetype=divn) and
            not(nf_is_currency in flags) and
            is_currency(resulttype.def) then
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
        if is_signed(resulttype.def) then
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


{$ifndef cpu64bit}
    function tmoddivnode.first_moddiv64bitint: tnode;
      var
        procname: string[31];
      begin
        result := nil;

        { when currency is used set the result of the
          parameters to s64bit, so they are not converted }
        if is_currency(resulttype.def) then
          begin
            left.resulttype:=s64inttype;
            right.resulttype:=s64inttype;
          end;

        { otherwise create a call to a helper }
        if nodetype = divn then
          procname := 'fpc_div_'
        else
          procname := 'fpc_mod_';
        { only qword needs the unsigned code, the
          signed code is also used for currency }
        if is_signed(resulttype.def) then
          procname := procname + 'int64'
        else
          procname := procname + 'qword';

        result := ccallnode.createintern(procname,ccallparanode.create(left,
          ccallparanode.create(right,nil)));
        left := nil;
        right := nil;
        firstpass(result);
      end;
{$endif cpu64bit}


    function tmoddivnode.firstoptimize: tnode;
      var
        power{,shiftval} : longint;
        newtype: tnodetype;
      begin
        result := nil;
        { divide/mod a number by a constant which is a power of 2? }
        if (cs_optimize in aktglobalswitches) and
           (right.nodetype = ordconstn) and
{           ((nodetype = divn) or
            not is_signed(resulttype.def)) and}
           (not is_signed(resulttype.def)) and
           ispowerof2(tordconstnode(right).value,power) then
          begin
            if nodetype = divn then
              begin
(*
                if is_signed(resulttype.def) then
                  begin
                    if is_64bitint(left.resulttype.def) then
                      if not (cs_littlesize in aktglobalswitches) then
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
                          right.resulttype,false)));
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
                dec(tordconstnode(right).value);
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

{$ifndef cpu64bit}
         { 64bit }
         if (left.resulttype.def.deftype=orddef) and
            (right.resulttype.def.deftype=orddef) and
            (is_64bitint(left.resulttype.def) or is_64bitint(right.resulttype.def)) then
           begin
             result := first_moddiv64bitint;
             if assigned(result) then
               exit;
             expectloc:=LOC_REGISTER;
             calcregisters(self,2,0,0);
           end
         else
{$endif cpu64bit}
           begin
             result := first_moddivint;
             if assigned(result) then
               exit;
             left_right_max;
             if left.registersint<=right.registersint then
              inc(registersint);
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


    function tshlshrnode.det_resulttype:tnode;
      var
         t : tnode;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         set_varstate(right,vs_used,[vsf_must_be_valid]);
         set_varstate(left,vs_used,[vsf_must_be_valid]);
         if codegenerror then
           exit;

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
         if (not is_64bit(left.resulttype.def)) and
            (torddef(left.resulttype.def).typ<>u32bit) then
           inserttypeconv(left,s32inttype);

         inserttypeconv(right,sinttype);

         resulttype:=left.resulttype;
      end;


{$ifndef cpu64bit}
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
{$endif cpu64bit}


    function tshlshrnode.pass_1 : tnode;
      var
         regs : longint;
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

{$ifndef cpu64bit}
         { 64 bit ints have their own shift handling }
         if is_64bit(left.resulttype.def) then
           begin
             result := first_shlshr64bitint;
             if assigned(result) then
               exit;
             regs:=2;
           end
         else
{$endif cpu64bit}
           begin
             regs:=1
           end;

         if (right.nodetype<>ordconstn) then
           inc(regs);
         expectloc:=LOC_REGISTER;
         calcregisters(self,regs,0,0);
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
             result:=left;
             left:=nil;
             exit;
          end;
      end;


    function tunaryminusnode.det_resulttype : tnode;
      var
         t : tnode;
      begin
         result:=nil;
         resulttypepass(left);
         set_varstate(left,vs_used,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         result:=simplify;
         if assigned(result) then
           exit;

         resulttype:=left.resulttype;
         if (left.resulttype.def.deftype=floatdef) then
           begin
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(left.resulttype.def) then
             begin
               { if saturation is on, left.resulttype.def isn't
                 "mmx able" (FK)
               if (cs_mmx_saturation in aktlocalswitches^) and
                 (torddef(tarraydef(resulttype.def).definition).typ in
                 [s32bit,u32bit]) then
                 CGMessage(type_e_mismatch);
               }
             end
{$endif SUPPORT_MMX}
{$ifndef cpu64bit}
         else if is_64bitint(left.resulttype.def) then
           begin
           end
{$endif cpu64bit}
         else if (left.resulttype.def.deftype=orddef) then
           begin
              inserttypeconv(left,sinttype);
              resulttype:=left.resulttype;
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
      begin
         result:=nil;
         firstpass(left);
         if codegenerror then
           exit;

         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         if (left.resulttype.def.deftype=floatdef) then
           begin
              if (left.expectloc<>LOC_REGISTER) and
                 (registersfpu<1) then
                registersfpu:=1;
              expectloc:=LOC_FPUREGISTER;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(left.resulttype.def) then
             begin
               if (left.expectloc<>LOC_MMXREGISTER) and
                  (registersmmx<1) then
                 registersmmx:=1;
             end
{$endif SUPPORT_MMX}
{$ifndef cpu64bit}
         else if is_64bit(left.resulttype.def) then
           begin
              if (left.expectloc<>LOC_REGISTER) and
                 (registersint<2) then
                registersint:=2;
              expectloc:=LOC_REGISTER;
           end
{$endif cpu64bit}
         else if (left.resulttype.def.deftype=orddef) then
           begin
              if (left.expectloc<>LOC_REGISTER) and
                 (registersint<1) then
                registersint:=1;
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
        tt : ttype;
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
           if (taddnode(left).left.resulttype.def.deftype<>setdef) or
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
             tt:=left.resulttype;
             case torddef(left.resulttype.def).typ of
               bool8bit,
               bool16bit,
               bool32bit :
                 begin
                   { here we do a boolean(byte(..)) type cast because }
                   { boolean(<int64>) is buggy in 1.00                }
                   v:=byte(not(boolean(byte(v))));
                 end;
               uchar,
               uwidechar,
               u8bit,
               s8bit,
               u16bit,
               s16bit,
               u32bit,
               s32bit,
               s64bit,
               u64bit :
                 begin
                   v:=int64(not int64(v)); { maybe qword is required }
                   int_to_type(v,tt);
                 end;
               else
                 CGMessage(type_e_mismatch);
             end;
             t:=cordconstnode.create(v,tt,true);
             result:=t;
             exit;
          end;
      end;


    function tnotnode.det_resulttype : tnode;
      var
         t : tnode;
      begin
         result:=nil;
         resulttypepass(left);
         set_varstate(left,vs_used,[]);
         if codegenerror then
           exit;

         resulttype:=left.resulttype;

         result:=simplify;
         if assigned(result) then
           exit;

         if is_boolean(resulttype.def) then
           begin
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and
             is_mmx_able_array(left.resulttype.def) then
             begin
             end
         else
{$endif SUPPORT_MMX}
{$ifndef cpu64bit}
           if is_64bitint(left.resulttype.def) then
             begin
             end
         else
{$endif cpu64bit}
           if is_integer(left.resulttype.def) then
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
         registersint:=left.registersint;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if is_boolean(resulttype.def) then
           begin
             if (expectloc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
              begin
                expectloc:=LOC_REGISTER;
                if (registersint<1) then
                 registersint:=1;
              end;
            { before loading it into flags we need to load it into
              a register thus 1 register is need PM }
{$ifdef cpuflags}
             if left.expectloc<>LOC_JUMP then
               expectloc:=LOC_FLAGS;
{$endif def cpuflags}
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and
             is_mmx_able_array(left.resulttype.def) then
             begin
               if (left.expectloc<>LOC_MMXREGISTER) and
                 (registersmmx<1) then
                 registersmmx:=1;
             end
         else
{$endif SUPPORT_MMX}
{$ifndef cpu64bit}
           if is_64bit(left.resulttype.def) then
             begin
                if (expectloc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
                 begin
                   expectloc:=LOC_REGISTER;
                   if (registersint<2) then
                    registersint:=2;
                 end;
             end
         else
{$endif cpu64bit}
           if is_integer(left.resulttype.def) then
             begin
               if (left.expectloc<>LOC_REGISTER) and
                  (registersint<1) then
                 registersint:=1;
               expectloc:=LOC_REGISTER;
             end;
      end;

{$ifdef state_tracking}
    function Tnotnode.track_state_pass(exec_known:boolean):boolean;
      begin
        track_state_pass:=true;
        if left.track_state_pass(exec_known) then
          begin
            left.resulttype.def:=nil;
            do_resulttypepass(left);
          end;
      end;
{$endif}

begin
   cmoddivnode:=tmoddivnode;
   cshlshrnode:=tshlshrnode;
   cunaryminusnode:=tunaryminusnode;
   cnotnode:=tnotnode;
end.
