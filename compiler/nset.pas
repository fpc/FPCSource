{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Type checking and register allocation for set/case nodes

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
unit nset;

{$i defines.inc}

interface

    uses
       node,cpuinfo,aasm;

    type
      pcaserecord = ^tcaserecord;
      tcaserecord = record
          { range }
          _low,_high : TConstExprInt;

          { only used by gentreejmp }
          _at : pasmlabel;

          { label of instruction }
          statement : pasmlabel;

          { is this the first of an case entry, needed to release statement
            label (PFV) }
          firstlabel : boolean;

          { left and right tree node }
          less,greater : pcaserecord;
       end;

       tsetelementnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tinnode = class(tbinopnode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       trangenode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
       end;

       tcasenode = class(tbinarynode)
          nodes : pcaserecord;
          elseblock : tnode;
          constructor create(l,r : tnode;n : pcaserecord);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          function pass_1 : tnode;override;
       end;

    var
       csetelementnode : class of tsetelementnode;
       cinnode : class of tinnode;
       crangenode : class of trangenode;
       ccasenode : class of tcasenode;

    { counts the labels }
    function case_count_labels(root : pcaserecord) : longint;
    { searches the highest label }
    function case_get_max(root : pcaserecord) : longint;
    { searches the lowest label }
    function case_get_min(root : pcaserecord) : longint;

    function gencasenode(l,r : tnode;nodes : pcaserecord) : tnode;

implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      symconst,symdef,symsym,symtable,types,
      htypechk,pass_1,
      ncnv,ncon,cpubase,nld
{$ifdef newcg}
      ,cgbase
      ,tgcpu
{$else newcg}
      ,hcodegen
{$ifdef i386}
      ,tgeni386
{$endif}
{$ifdef m68k}
      ,tgen68k
{$endif}
{$endif newcg}
      ;

    function gencasenode(l,r : tnode;nodes : pcaserecord) : tnode;

      var
         t : tnode;

      begin
         t:=ccasenode.create(l,r,nodes);
         gencasenode:=t;
      end;

{*****************************************************************************
                           TSETELEMENTNODE
*****************************************************************************}

    constructor tsetelementnode.create(l,r : tnode);

      begin
         inherited create(setelementn,l,r);
      end;

    function tsetelementnode.pass_1 : tnode;

      begin
         pass_1:=nil;
         firstpass(left);
         set_varstate(left,true);
         if codegenerror then
          exit;

         if assigned(right) then
          begin
            firstpass(right);
            if codegenerror then
             exit;
          end;

         calcregisters(self,0,0,0);
         resulttype:=left.resulttype;
         set_location(location,left.location);
      end;


{*****************************************************************************
                              TINNODE
*****************************************************************************}

    constructor tinnode.create(l,r : tnode);

      begin
         inherited create(inn,l,r);
      end;

    function tinnode.pass_1 : tnode;
      type
        byteset = set of byte;
      var
        t : tnode;
        pst : pconstset;

    function createsetconst(psd : psetdef) : pconstset;
      var
        pcs : pconstset;
        pes : penumsym;
        i : longint;
      begin
        new(pcs);
        case psd^.elementtype.def^.deftype of
          enumdef :
            begin
              pes:=penumsym(penumdef(psd^.elementtype.def)^.firstenum);
              while assigned(pes) do
                begin
                  pcs^[pes^.value div 8]:=pcs^[pes^.value div 8] or (1 shl (pes^.value mod 8));
                  pes:=pes^.nextenum;
                end;
            end;
          orddef :
            begin
              for i:=porddef(psd^.elementtype.def)^.low to porddef(psd^.elementtype.def)^.high do
                begin
                  pcs^[i div 8]:=pcs^[i div 8] or (1 shl (i mod 8));
                end;
            end;
        end;
       createsetconst:=pcs;
      end;

      begin
         pass_1:=nil;
         location.loc:=LOC_FLAGS;
         resulttype:=booldef;

         firstpass(right);
         set_varstate(right,true);
         if codegenerror then
          exit;

         { Convert array constructor first to set }
         if is_array_constructor(right.resulttype) then
          begin
            arrayconstructor_to_set(tarrayconstructornode(right));
            firstpass(right);
            if codegenerror then
             exit;
          end;

         { if right is a typen then the def
         is in typenodetype PM }
         if right.nodetype=typen then
           right.resulttype:=ttypenode(right).typenodetype;

         if right.resulttype^.deftype<>setdef then
           CGMessage(sym_e_set_expected);
         if codegenerror then
           exit;

         if (right.nodetype=typen) then
           begin
             { we need to create a setconstn }
             pst:=createsetconst(psetdef(ttypenode(right).typenodetype));
             t:=gensetconstnode(pst,psetdef(ttypenode(right).typenodetype));
             dispose(pst);
             right.free;
             right:=t;
           end;

         firstpass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;

         { empty set then return false }
         if not assigned(psetdef(right.resulttype)^.elementtype.def) then
          begin
            t:=genordinalconstnode(0,booldef);
            firstpass(t);
            pass_1:=t;
            exit;
          end;

         { type conversion/check }
         left:=gentypeconvnode(left,psetdef(right.resulttype)^.elementtype.def);
         firstpass(left);
         if codegenerror then
           exit;

         { constant evaulation }
         if (left.nodetype=ordconstn) and (right.nodetype=setconstn) then
          begin
            t:=genordinalconstnode(byte(tordconstnode(left).value in byteset(tsetconstnode(right).value_set^)),booldef);
            firstpass(t);
            pass_1:=t;
            exit;
          end;

         left_right_max;
         { this is not allways true due to optimization }
         { but if we don't set this we get problems with optimizing self code }
         if psetdef(right.resulttype)^.settype<>smallset then
           procinfo^.flags:=procinfo^.flags or pi_do_call
         else
           begin
              { a smallset needs maybe an misc. register }
              if (left.nodetype<>ordconstn) and
                not(right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) and
                (right.registers32<1) then
                inc(registers32);
           end;
      end;


{*****************************************************************************
                              TRANGENODE
*****************************************************************************}

    constructor trangenode.create(l,r : tnode);

      begin
         inherited create(rangen,l,r);
      end;

    function trangenode.pass_1 : tnode;
      var
         ct : tconverttype;
      begin
         pass_1:=nil;
         firstpass(left);
         set_varstate(left,true);
         firstpass(right);
         set_varstate(right,true);
         if codegenerror then
           exit;
         { both types must be compatible }
         if not(is_equal(left.resulttype,right.resulttype)) and
            (isconvertable(left.resulttype,right.resulttype,ct,ordconstn,false)=0) then
           CGMessage(type_e_mismatch);
         { Check if only when its a constant set }
         if (left.nodetype=ordconstn) and (right.nodetype=ordconstn) then
          begin
          { upper limit must be greater or equal than lower limit }
          { not if u32bit }
            if (tordconstnode(left).value>tordconstnode(right).value) and
               ((tordconstnode(left).value<0) or (tordconstnode(right).value>=0)) then
              CGMessage(cg_e_upper_lower_than_lower);
          end;
        left_right_max;
        resulttype:=left.resulttype;
        set_location(location,left.location);
      end;


{*****************************************************************************
                              Case Helpers
*****************************************************************************}

    function case_count_labels(root : pcaserecord) : longint;
      var
         _l : longint;

      procedure count(p : pcaserecord);
        begin
           inc(_l);
           if assigned(p^.less) then
             count(p^.less);
           if assigned(p^.greater) then
             count(p^.greater);
        end;

      begin
         _l:=0;
         count(root);
         case_count_labels:=_l;
      end;


    function case_get_max(root : pcaserecord) : longint;
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.greater) do
           hp:=hp^.greater;
         case_get_max:=hp^._high;
      end;


    function case_get_min(root : pcaserecord) : longint;
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.less) do
           hp:=hp^.less;
         case_get_min:=hp^._low;
      end;

    procedure deletecaselabels(p : pcaserecord);

      begin
         if assigned(p^.greater) then
           deletecaselabels(p^.greater);
         if assigned(p^.less) then
           deletecaselabels(p^.less);
         dispose(p);
      end;

    function copycaserecord(p : pcaserecord) : pcaserecord;

      var
         n : pcaserecord;

      begin
         new(n);
         n^:=p^;
         if assigned(p^.greater) then
           n^.greater:=copycaserecord(p^.greater);
         if assigned(p^.less) then
           n^.less:=copycaserecord(p^.less);
         copycaserecord:=n;
      end;

{*****************************************************************************
                              TCASENODE
*****************************************************************************}

    constructor tcasenode.create(l,r : tnode;n : pcaserecord);

      begin
         inherited create(casen,l,r);
         nodes:=n;
         elseblock:=nil;
         set_file_line(l);
      end;

    destructor tcasenode.destroy;

      begin
         elseblock.free;
         deletecaselabels(nodes);
         inherited destroy;
      end;

    function tcasenode.pass_1 : tnode;
      var
         old_t_times : longint;
         hp : tbinarynode;
      begin
         pass_1:=nil;
         { evalutes the case expression }
{$ifdef newcg}
         tg.cleartempgen;
{$else newcg}
         cleartempgen;
{$endif newcg}
         firstpass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;
         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { walk through all instructions }

         {   estimates the repeat of each instruction }
         old_t_times:=t_times;
         if not(cs_littlesize in aktglobalswitches) then
           begin
              t_times:=t_times div case_count_labels(nodes);
              if t_times<1 then
                t_times:=1;
           end;
         { first case }
         hp:=tbinarynode(right);
         while assigned(hp) do
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(hp.right);

              { searchs max registers }
              if hp.right.registers32>registers32 then
                registers32:=hp.right.registers32;
              if hp.right.registersfpu>registersfpu then
                registersfpu:=hp.right.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp.right.registersmmx>registersmmx then
                registersmmx:=hp.right.registersmmx;
{$endif SUPPORT_MMX}

              hp:=tbinarynode(hp.left);
           end;

         { may be handle else tree }
         if assigned(elseblock) then
           begin
{$ifdef newcg}
              tg.cleartempgen;
{$else newcg}
              cleartempgen;
{$endif newcg}
              firstpass(elseblock);
              if codegenerror then
                exit;
              if registers32<elseblock.registers32 then
                registers32:=elseblock.registers32;
              if registersfpu<elseblock.registersfpu then
                registersfpu:=elseblock.registersfpu;
{$ifdef SUPPORT_MMX}
              if registersmmx<elseblock.registersmmx then
                registersmmx:=elseblock.registersmmx;
{$endif SUPPORT_MMX}
           end;
         t_times:=old_t_times;

         { there is one register required for the case expression    }
         { for 64 bit ints we cheat: the high dword is stored in EDI }
         { so we don't need an extra register                        }
         if registers32<1 then registers32:=1;
      end;

    function tcasenode.getcopy : tnode;

      var
         p : tcasenode;

      begin
         p:=tcasenode(inherited getcopy);
         if assigned(elseblock) then
           p.elseblock:=elseblock.getcopy
         else
           p.elseblock:=nil;
         p.nodes:=copycaserecord(nodes);
         getcopy:=p;
      end;

    procedure tcasenode.insertintolist(l : tnodelist);

      begin
      end;

begin
   csetelementnode:=tsetelementnode;
   cinnode:=tinnode;
   crangenode:=trangenode;
   ccasenode:=tcasenode;
end.
{
  $Log$
  Revision 1.7  2000-10-31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.6  2000/10/21 18:16:11  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.5  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.4  2000/10/01 19:48:25  peter
    * lot of compile updates for cg11

  Revision 1.3  2000/09/27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.2  2000/09/24 20:17:44  florian
    * more conversion work done

  Revision 1.1  2000/09/24 19:38:39  florian
    * initial implementation

}