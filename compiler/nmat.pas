{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
       node;

    type
       tmoddivnode = class(tbinopnode)
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
         protected
          { override the following if you want to implement }
          { parts explicitely in the code generator (JM)    }
          function first_moddiv64bitint: tnode; virtual;
          function firstoptimize: tnode; virtual;
       end;
       tmoddivnodeclass = class of tmoddivnode;

       tshlshrnode = class(tbinopnode)
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tshlshrnodeclass = class of tshlshrnode;

       tunaryminusnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tunaryminusnodeclass = class of tunaryminusnode;

       tnotnode = class(tunarynode)
          constructor create(expr : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tnotnodeclass = class of tnotnode;

    var
       cmoddivnode : tmoddivnodeclass;
       cshlshrnode : tshlshrnodeclass;
       cunaryminusnode : tunaryminusnodeclass;
       cnotnode : tnotnodeclass;


implementation

    uses
      systems,tokens,
      verbose,globals,cutils,
      globtype,
      symconst,symtype,symtable,symdef,types,
      htypechk,pass_1,cpubase,
      cgbase,
      ncon,ncnv,ncal,nadd;

{****************************************************************************
                              TMODDIVNODE
 ****************************************************************************}

    function tmoddivnode.det_resulttype:tnode;
      var
         t : tnode;
         rd,ld : tdef;
         rv,lv : tconstexprint;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         set_varstate(left,true);
         set_varstate(right,true);
         if codegenerror then
           exit;

         { constant folding }
         if is_constintnode(left) and is_constintnode(right) then
           begin
              rv:=tordconstnode(right).value;
              lv:=tordconstnode(left).value;

              { check for division by zero }
              if (rv=0) then
               begin
                 Message(parser_e_division_by_zero);
                 { recover }
                 rv:=1;
               end;

              case nodetype of
                modn:
                  t:=genintconstnode(lv mod rv);
                divn:
                  t:=genintconstnode(lv div rv);
              end;
              result:=t;
              exit;
           end;

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
         if (left.resulttype.def.deftype=orddef) and (right.resulttype.def.deftype=orddef) then
           if (torddef(right.resulttype.def).typ in [u32bit,u64bit]) and
              is_constintnode(left) and
              (tordconstnode(left).value >= 0) then
             inserttypeconv(left,right.resulttype)
           else if (torddef(left.resulttype.def).typ in [u32bit,u64bit]) and
              is_constintnode(right) and
              (tordconstnode(right).value >= 0) then
             inserttypeconv(right,left.resulttype);

         if (left.resulttype.def.deftype=orddef) and (right.resulttype.def.deftype=orddef) and
            (is_64bitint(left.resulttype.def) or is_64bitint(right.resulttype.def) or
             { when mixing cardinals and signed numbers, convert everythign to 64bit (JM) }
             ((torddef(right.resulttype.def).typ = u32bit) and
              is_signed(left.resulttype.def)) or
             ((torddef(left.resulttype.def).typ = u32bit) and
              is_signed(right.resulttype.def))) then
           begin
              rd:=right.resulttype.def;
              ld:=left.resulttype.def;
              { issue warning if necessary }
              if not (is_64bitint(left.resulttype.def) or is_64bitint(right.resulttype.def)) then
                CGMessage(type_w_mixed_signed_unsigned);
              if is_signed(rd) or is_signed(ld) then
                begin
                   if (torddef(ld).typ<>s64bit) then
                     inserttypeconv(left,cs64bittype);
                   if (torddef(rd).typ<>s64bit) then
                     inserttypeconv(right,cs64bittype);
                end
              else
                begin
                   if (torddef(ld).typ<>u64bit) then
                     inserttypeconv(left,cu64bittype);
                   if (torddef(rd).typ<>u64bit) then
                     inserttypeconv(right,cu64bittype);
                end;
              resulttype:=left.resulttype;
           end
         else
           begin
              if not(right.resulttype.def.deftype=orddef) or
                 not(torddef(right.resulttype.def).typ in [s32bit,u32bit]) then
                inserttypeconv(right,s32bittype);

              if not(left.resulttype.def.deftype=orddef) or
                 not(torddef(left.resulttype.def).typ in [s32bit,u32bit]) then
                inserttypeconv(left,s32bittype);

              { the resulttype.def depends on the right side, because the left becomes }
              { always 64 bit                                                      }
              resulttype:=right.resulttype;
           end;
      end;


    function tmoddivnode.first_moddiv64bitint: tnode;
      var
        procname: string[31];
      begin
        result := nil;

        { otherwise create a call to a helper }
        if nodetype = divn then
          procname := 'fpc_div_'
        else
          procname := 'fpc_mod_';
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
                          cordconstnode.create(shiftval,s32bittype)),
                        cordconstnode.create(tordconstnode(right).value-1,
                          right.resulttype)));
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

         result := firstoptimize;
         if assigned(result) then
           exit;
         { 64bit }
         if (left.resulttype.def.deftype=orddef) and (right.resulttype.def.deftype=orddef) and
            (is_64bitint(left.resulttype.def) or is_64bitint(right.resulttype.def)) then
           begin
             result := first_moddiv64bitint;
             if assigned(result) then
               exit;
             location.loc:=LOC_REGISTER;
             calcregisters(self,2,0,0);
           end
         else
           begin
             left_right_max;
             if left.registers32<=right.registers32 then
              inc(registers32);
           end;
         location.loc:=LOC_REGISTER;
      end;



{****************************************************************************
                              TSHLSHRNODE
 ****************************************************************************}

    function tshlshrnode.det_resulttype:tnode;
      var
         t : tnode;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         set_varstate(right,true);
         set_varstate(left,true);
         if codegenerror then
           exit;

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

         { allow operator overloading }
         t:=self;
         if isbinaryoverloaded(t) then
           begin
              result:=t;
              exit;
           end;

         { 64 bit ints have their own shift handling }
         if not(is_64bitint(left.resulttype.def)) then
           begin
              if torddef(left.resulttype.def).typ <> u32bit then
               inserttypeconv(left,s32bittype);
           end;

         inserttypeconv(right,s32bittype);

         resulttype:=left.resulttype;
      end;


    function tshlshrnode.pass_1 : tnode;
      var
         regs : longint;
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;

         { 64 bit ints have their own shift handling }
         if not(is_64bitint(left.resulttype.def)) then
          regs:=1
         else
          regs:=2;

         if (right.nodetype<>ordconstn) then
          inc(regs);
         location.loc:=LOC_REGISTER;
         calcregisters(self,regs,0,0);
      end;


{****************************************************************************
                            TUNARYMINUSNODE
 ****************************************************************************}

    constructor tunaryminusnode.create(expr : tnode);
      begin
         inherited create(unaryminusn,expr);
      end;

    function tunaryminusnode.det_resulttype : tnode;
      var
         t : tnode;
         minusdef : pprocdeflist;
      begin
         result:=nil;
         resulttypepass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;

         { constant folding }
         if is_constintnode(left) then
           begin
              tordconstnode(left).value:=-tordconstnode(left).value;
              result:=left;
              left:=nil;
              exit;
           end;
         if is_constrealnode(left) then
           begin
              trealconstnode(left).value_real:=-trealconstnode(left).value_real;
              result:=left;
              left:=nil;
              exit;
           end;

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
         else if is_64bitint(left.resulttype.def) then
           begin
           end
         else if (left.resulttype.def.deftype=orddef) then
           begin
              inserttypeconv(left,s32bittype);
              resulttype:=left.resulttype;
           end
         else
           begin
              if assigned(overloaded_operators[_minus]) then
                minusdef:=overloaded_operators[_minus].defs
              else
                minusdef:=nil;
              while assigned(minusdef) do
                begin
                   if is_equal(tparaitem(minusdef^.def.para.first).paratype.def,left.resulttype.def) and
                      (tparaitem(minusdef^.def.para.first).next=nil) then
                     begin
                        t:=ccallnode.create(ccallparanode.create(left,nil),
                                            overloaded_operators[_minus],nil,nil);
                        left:=nil;
                        result:=t;
                        exit;
                     end;
                   minusdef:=minusdef^.next;
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

         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         if (left.resulttype.def.deftype=floatdef) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registersfpu<1) then
                registersfpu:=1;
              location.loc:=LOC_REGISTER;
           end
{$ifdef SUPPORT_MMX}
         else if (cs_mmx in aktlocalswitches) and
           is_mmx_able_array(left.resulttype.def) then
             begin
               if (left.location.loc<>LOC_MMXREGISTER) and
                  (registersmmx<1) then
                 registersmmx:=1;
             end
{$endif SUPPORT_MMX}
         else if is_64bitint(left.resulttype.def) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<2) then
                registers32:=2;
              location.loc:=LOC_REGISTER;
           end
         else if (left.resulttype.def.deftype=orddef) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<1) then
                registers32:=1;
              location.loc:=LOC_REGISTER;
           end;
      end;


{****************************************************************************
                               TNOTNODE
 ****************************************************************************}

    constructor tnotnode.create(expr : tnode);

      begin
         inherited create(notn,expr);
      end;

    function tnotnode.det_resulttype : tnode;
      var
         t : tnode;
         notdef : pprocdeflist;
         v : tconstexprint;
      begin
         result:=nil;
         resulttypepass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;

         { constant folding }
         if (left.nodetype=ordconstn) then
           begin
              v:=tordconstnode(left).value;
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
                u8bit :
                  v:=byte(not byte(v));
                s8bit :
                  v:=shortint(not shortint(v));
                uwidechar,
                u16bit :
                  v:=word(not word(v));
                s16bit :
                  v:=smallint(not smallint(v));
                u32bit :
                  v:=cardinal(not cardinal(v));
                s32bit :
                  v:=longint(not longint(v));
                u64bit :
                  v:=int64(not int64(v)); { maybe qword is required }
                s64bit :
                  v:=int64(not int64(v));
                else
                  CGMessage(type_e_mismatch);
              end;
              t:=cordconstnode.create(v,left.resulttype);
              result:=t;
              exit;
           end;

         resulttype:=left.resulttype;
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
           if is_64bitint(left.resulttype.def) then
             begin
             end
         else if is_integer(left.resulttype.def) then
           begin
           end
         else
           begin
              if assigned(overloaded_operators[_op_not]) then
                notdef:=overloaded_operators[_op_not].defs
              else
                notdef:=nil;
              while assigned(notdef) do
                begin
                   if is_equal(tparaitem(notdef^.def.para.first).paratype.def,left.resulttype.def) and
                      (tparaitem(notdef^.def.para.first).next=nil) then
                     begin
                        t:=ccallnode.create(ccallparanode.create(left,nil),
                                            overloaded_operators[_op_not],nil,nil);
                        left:=nil;
                        result:=t;
                        exit;
                     end;
                   notdef:=notdef^.next;
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

         location.loc:=left.location.loc;
         registers32:=left.registers32;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         if is_boolean(resulttype.def) then
           begin
             if (location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
              begin
                location.loc:=LOC_REGISTER;
                if (registers32<1) then
                 registers32:=1;
              end;
            { before loading it into flags we need to load it into
              a register thus 1 register is need PM }
{$ifdef i386}
             if left.location.loc<>LOC_JUMP then
               location.loc:=LOC_FLAGS;
{$endif def i386}
           end
         else
{$ifdef SUPPORT_MMX}
           if (cs_mmx in aktlocalswitches) and
             is_mmx_able_array(left.resulttype.def) then
             begin
               if (left.location.loc<>LOC_MMXREGISTER) and
                 (registersmmx<1) then
                 registersmmx:=1;
             end
         else
{$endif SUPPORT_MMX}
           if is_64bitint(left.resulttype.def) then
             begin
                if (location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER]) then
                 begin
                   location.loc:=LOC_REGISTER;
                   if (registers32<2) then
                    registers32:=2;
                 end;
             end
         else if is_integer(left.resulttype.def) then
           begin
              if (left.location.loc<>LOC_REGISTER) and
                 (registers32<1) then
                registers32:=1;
              location.loc:=LOC_REGISTER;
           end
      end;

begin
   cmoddivnode:=tmoddivnode;
   cshlshrnode:=tshlshrnode;
   cunaryminusnode:=tunaryminusnode;
   cnotnode:=tnotnode;
end.
{
  $Log$
  Revision 1.32  2002-05-14 19:34:43  peter
    * removed old logs and updated copyright year

  Revision 1.31  2002/04/07 13:26:10  carl
  + change unit use

  Revision 1.30  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.29  2002/03/04 19:10:11  peter
    * removed compiler warnings

  Revision 1.28  2002/02/11 11:45:51  michael
  * Compilation without mmx support fixed from Peter

}
