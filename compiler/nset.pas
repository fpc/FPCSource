{
    $Id$
    Copyright (c) 2000-2001 by Florian Klaempfl

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
          _at : tasmlabel;

          { label of instruction }
          statement : tasmlabel;

          { is this the first of an case entry, needed to release statement
            label (PFV) }
          firstlabel : boolean;

          { left and right tree node }
          less,greater : pcaserecord;
       end;

       tsetelementnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       tsetelementnodeclass = class of tsetelementnode;

       tinnode = class(tbinopnode)
          constructor create(l,r : tnode);virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       tinnodeclass = class of tinnode;

       trangenode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
       end;
       trangenodeclass = class of trangenode;

       tcasenode = class(tbinarynode)
          nodes : pcaserecord;
          elseblock : tnode;
          constructor create(l,r : tnode;n : pcaserecord);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tcasenodeclass = class of tcasenode;

    var
       csetelementnode : tsetelementnodeclass;
       cinnode : tinnodeclass;
       crangenode : trangenodeclass;
       ccasenode : tcasenodeclass;

    { counts the labels }
    function case_count_labels(root : pcaserecord) : longint;
    { searches the highest label }
{$ifdef int64funcresok}
    function case_get_max(root : pcaserecord) : tconstexprint;
{$else int64funcresok}
    function case_get_max(root : pcaserecord) : longint;
{$endif int64funcresok}
    { searches the lowest label }
{$ifdef int64funcresok}
    function case_get_min(root : pcaserecord) : tconstexprint;
{$else int64funcresok}
    function case_get_min(root : pcaserecord) : longint;
{$endif int64funcresok}

    function gencasenode(l,r : tnode;nodes : pcaserecord) : tnode;

implementation

    uses
      globtype,systems,
      verbose,globals,
      symconst,symdef,symsym,types,
      htypechk,pass_1,
      ncnv,ncon,cpubase,nld,rgobj,cgbase;

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


    function tsetelementnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttypepass(left);
         if assigned(right) then
          resulttypepass(right);
         set_varstate(left,true);
         if codegenerror then
          exit;

         resulttype:=left.resulttype;
      end;


    function tsetelementnode.pass_1 : tnode;

      begin
         result:=nil;
         firstpass(left);
         if assigned(right) then
          firstpass(right);
         if codegenerror then
          exit;

         location_copy(location,left.location);
         calcregisters(self,0,0,0);
      end;


{*****************************************************************************
                              TINNODE
*****************************************************************************}

    constructor tinnode.create(l,r : tnode);
      begin
         inherited create(inn,l,r);
      end;


    function tinnode.det_resulttype:tnode;
      type
        byteset = set of byte;
      var
        t : tnode;
        pst : pconstset;

        function createsetconst(psd : tsetdef) : pconstset;
        var
          pcs : pconstset;
          pes : tenumsym;
          i : longint;
        begin
          new(pcs);
          case psd.elementtype.def.deftype of
            enumdef :
              begin
                pes:=tenumsym(tenumdef(psd.elementtype.def).firstenum);
                while assigned(pes) do
                  begin
                    pcs^[pes.value div 8]:=pcs^[pes.value div 8] or (1 shl (pes.value mod 8));
                    pes:=pes.nextenum;
                  end;
              end;
            orddef :
              begin
                for i:=torddef(psd.elementtype.def).low to torddef(psd.elementtype.def).high do
                  begin
                    pcs^[i div 8]:=pcs^[i div 8] or (1 shl (i mod 8));
                  end;
              end;
          end;
          createsetconst:=pcs;
        end;

      begin
         result:=nil;
         resulttype:=booltype;
         resulttypepass(right);
         set_varstate(right,true);
         if codegenerror then
          exit;

         { Convert array constructor first to set }
         if is_array_constructor(right.resulttype.def) then
          begin
            arrayconstructor_to_set(tarrayconstructornode(right));
            firstpass(right);
            if codegenerror then
             exit;
          end;

         if right.resulttype.def.deftype<>setdef then
           CGMessage(sym_e_set_expected);

         if (right.nodetype=typen) then
           begin
             { we need to create a setconstn }
             pst:=createsetconst(tsetdef(ttypenode(right).resulttype.def));
             t:=csetconstnode.create(pst,ttypenode(right).resulttype);
             dispose(pst);
             right.free;
             right:=t;
           end;

         resulttypepass(left);
         set_varstate(left,true);
         if codegenerror then
           exit;

         { type conversion/check }
         if assigned(tsetdef(right.resulttype.def).elementtype.def) then
          inserttypeconv(left,tsetdef(right.resulttype.def).elementtype);

         { empty set then return false }
         if not assigned(tsetdef(right.resulttype.def).elementtype.def) then
          begin
            t:=cordconstnode.create(0,booltype);
            resulttypepass(t);
            result:=t;
            exit;
          end;

         { constant evaulation }
         if (left.nodetype=ordconstn) and (right.nodetype=setconstn) then
          begin
            t:=cordconstnode.create(byte(tordconstnode(left).value in byteset(tsetconstnode(right).value_set^)),booltype);
            resulttypepass(t);
            result:=t;
            exit;
          end;
      end;


    function tinnode.pass_1 : tnode;
      begin
         result:=nil;
         location.loc:=LOC_FLAGS;

         firstpass(right);
         firstpass(left);
         if codegenerror then
           exit;

         left_right_max;
         { this is not allways true due to optimization }
         { but if we don't set this we get problems with optimizing self code }
         if tsetdef(right.resulttype.def).settype<>smallset then
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


    function trangenode.det_resulttype : tnode;
      var
         ct : tconverttype;
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         set_varstate(left,true);
         set_varstate(right,true);
         if codegenerror then
           exit;
         { both types must be compatible }
         if not(is_equal(left.resulttype.def,right.resulttype.def)) and
            (isconvertable(left.resulttype.def,right.resulttype.def,ct,ordconstn,false)=0) then
           CGMessage(type_e_mismatch);
         { Check if only when its a constant set }
         if (left.nodetype=ordconstn) and (right.nodetype=ordconstn) then
          begin
            { upper limit must be greater or equal than lower limit }
            if (tordconstnode(left).value>tordconstnode(right).value) and
               ((tordconstnode(left).value<0) or (tordconstnode(right).value>=0)) then
              CGMessage(cg_e_upper_lower_than_lower);
          end;
        resulttype:=left.resulttype;
      end;


    function trangenode.pass_1 : tnode;
      begin
         result:=nil;
         firstpass(left);
         firstpass(right);
         if codegenerror then
           exit;
        left_right_max;
        location_copy(location,left.location);
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


{$ifdef int64funcresok}
    function case_get_max(root : pcaserecord) : tconstexprint;
{$else int64funcresok}
    function case_get_max(root : pcaserecord) : longint;
{$endif int64funcresok}
      var
         hp : pcaserecord;
      begin
         hp:=root;
         while assigned(hp^.greater) do
           hp:=hp^.greater;
         case_get_max:=hp^._high;
      end;


{$ifdef int64funcresok}
    function case_get_min(root : pcaserecord) : tconstexprint;
{$else int64funcresok}
    function case_get_min(root : pcaserecord) : longint;
{$endif int64funcresok}
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


    function tcasenode.det_resulttype : tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;



    function tcasenode.pass_1 : tnode;
      var
         old_t_times : longint;
         hp : tbinarynode;
      begin
         result:=nil;
         { evalutes the case expression }
         rg.cleartempgen;
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
         old_t_times:=rg.t_times;
         if not(cs_littlesize in aktglobalswitches) then
           begin
              rg.t_times:=rg.t_times div case_count_labels(nodes);
              if rg.t_times<1 then
                rg.t_times:=1;
           end;
         { first case }
         hp:=tbinarynode(right);
         while assigned(hp) do
           begin
              rg.cleartempgen;
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
              rg.cleartempgen;
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
         rg.t_times:=old_t_times;

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

    function casenodesequal(n1,n2: pcaserecord): boolean;
      begin
        casenodesequal :=
          (not assigned(n1) and not assigned(n2)) or
          (assigned(n1) and assigned(n2) and
           (n1^._low = n2^._low) and
           (n1^._high = n2^._high) and
           { the rest of the fields don't matter for equality (JM) }
           casenodesequal(n1^.less,n2^.less) and
           casenodesequal(n1^.greater,n2^.greater))
      end;


    function tcasenode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          casenodesequal(nodes,tcasenode(p).nodes) and
          elseblock.isequal(tcasenode(p).elseblock);
      end;

begin
   csetelementnode:=tsetelementnode;
   cinnode:=tinnode;
   crangenode:=trangenode;
   ccasenode:=tcasenode;
end.
{
  $Log$
  Revision 1.19  2002-04-02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.18  2002/03/31 20:26:35  jonas
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

  Revision 1.17  2001/12/06 17:57:35  florian
    + parasym to tparaitem added

  Revision 1.16  2001/10/12 13:51:51  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes ("merged")
    * fixed bug in n386add (introduced after compilerproc changes for string
      operations) where calcregisters wasn't called for shortstring addnodes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.15  2001/09/02 21:12:07  peter
    * move class of definitions into type section for delphi

  Revision 1.14  2001/08/26 13:36:43  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.13  2001/04/13 01:22:10  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.12  2001/04/02 21:20:31  peter
    * resulttype rewrite

  Revision 1.11  2000/12/31 11:14:11  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.10  2000/12/18 17:44:26  jonas
    * more int64 case fixes

  Revision 1.9  2000/11/29 00:30:34  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.8  2000/11/04 14:25:20  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.7  2000/10/31 22:02:49  peter
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
