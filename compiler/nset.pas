{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    uses
       node,globals,
       aasmbase,aasmtai,
       symppu;

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
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
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
      verbose,
      symconst,symdef,symsym,defutil,defcmp,
      htypechk,pass_1,
      nbas,ncnv,ncon,cpubase,nld,rgobj,cgbase;

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

         expectloc:=left.expectloc;
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
                    include(pcs^,pes.value);
                    pes:=pes.nextenum;
                  end;
              end;
            orddef :
              begin
                for i:=torddef(psd.elementtype.def).low to torddef(psd.elementtype.def).high do
                  include(pcs^,i);
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
            arrayconstructor_to_set(right);
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

         if not assigned(left.resulttype.def) then
           internalerror(20021126);
         { insert a hint that a range check error might occur on non-byte
           elements.with the in operator.
         }
         if  (
               (left.resulttype.def.deftype = orddef) and not
               (torddef(left.resulttype.def).typ in [s8bit,u8bit,uchar,bool8bit])
             )
            or
             (
               (left.resulttype.def.deftype = enumdef)
              and (tenumdef(left.resulttype.def).size <> 1)
             )
          then
             Message(type_h_in_range_check);

         { type conversion/check }
         if assigned(tsetdef(right.resulttype.def).elementtype.def) then
           begin
             inserttypeconv(left,tsetdef(right.resulttype.def).elementtype);
           end;

         { empty set then return false }
         if not assigned(tsetdef(right.resulttype.def).elementtype.def) or
            ((right.nodetype = setconstn) and
             (tnormalset(tsetconstnode(right).value_set^) = [])) then
          begin
            t:=cordconstnode.create(0,booltype,false);
            resulttypepass(t);
            result:=t;
            exit;
          end;

         { constant evaluation }
         if (left.nodetype=ordconstn) and (right.nodetype=setconstn) then
          begin
            t:=cordconstnode.create(byte(tordconstnode(left).value in Tsetconstnode(right).value_set^),
               booltype,true);
            resulttypepass(t);
            result:=t;
            exit;
          end;
      end;


    { Warning : This is the first pass for the generic version }
    { the only difference is mainly the result location which  }
    { is changed, compared to the i386 version.                }
    { ALSO REGISTER ALLOC IS WRONG?                            }
    function tinnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_REGISTER;

         firstpass(right);
         firstpass(left);
         if codegenerror then
           exit;

         left_right_max;

         if tsetdef(right.resulttype.def).settype<>smallset then
           begin
             if registers32 < 3 then
               registers32 := 3;
           end
         else
           begin
              { a smallset needs maybe an misc. register }
              if (left.nodetype<>ordconstn) and
                not(right.expectloc in [LOC_CREGISTER,LOC_REGISTER]) and
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
      begin
         result:=nil;
         resulttypepass(left);
         resulttypepass(right);
         set_varstate(left,true);
         set_varstate(right,true);
         if codegenerror then
           exit;
         { both types must be compatible }
         if compare_defs(left.resulttype.def,right.resulttype.def,left.nodetype)=te_incompatible then
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
        expectloc:=left.expectloc;
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


    procedure ppuwritecaserecord(ppufile:tcompilerppufile;p : pcaserecord);
      var
        b : byte;
      begin
        ppufile.putexprint(p^._low);
        ppufile.putexprint(p^._high);
        ppufile.putasmsymbol(p^._at);
        ppufile.putasmsymbol(p^.statement);
        ppufile.putbyte(byte(p^.firstlabel));
        b:=0;
        if assigned(p^.greater) then
         b:=b or 1;
        if assigned(p^.less) then
         b:=b or 2;
        ppufile.putbyte(b);
        if assigned(p^.greater) then
          ppuwritecaserecord(ppufile,p^.greater);
        if assigned(p^.less) then
          ppuwritecaserecord(ppufile,p^.less);
      end;


    function ppuloadcaserecord(ppufile:tcompilerppufile):pcaserecord;
      var
        b : byte;
        p : pcaserecord;
      begin
        new(p);
        p^._low:=ppufile.getexprint;
        p^._high:=ppufile.getexprint;
        p^._at:=tasmlabel(ppufile.getasmsymbol);
        p^.statement:=tasmlabel(ppufile.getasmsymbol);
        p^.firstlabel:=boolean(ppufile.getbyte);
        b:=ppufile.getbyte;
        if (b and 1)=1 then
         p^.greater:=ppuloadcaserecord(ppufile)
        else
         p^.greater:=nil;
        if (b and 2)=2 then
         p^.less:=ppuloadcaserecord(ppufile)
        else
         p^.less:=nil;
        ppuloadcaserecord:=p;
      end;


    procedure ppuderefcaserecord(p : pcaserecord);
      begin
         objectlibrary.derefasmsymbol(tasmsymbol(p^._at));
         objectlibrary.derefasmsymbol(tasmsymbol(p^.statement));
         if assigned(p^.greater) then
           ppuderefcaserecord(p^.greater);
         if assigned(p^.less) then
           ppuderefcaserecord(p^.less);
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


    constructor tcasenode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        elseblock:=ppuloadnode(ppufile);
        nodes:=ppuloadcaserecord(ppufile);
      end;


    procedure tcasenode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppuwritenode(ppufile,elseblock);
        ppuwritecaserecord(ppufile,nodes);
      end;


    procedure tcasenode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(elseblock) then
          elseblock.derefimpl;
        ppuderefcaserecord(nodes);
      end;


    function tcasenode.det_resulttype : tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;



    function tcasenode.pass_1 : tnode;
      var
         old_t_times : longint;
         hp : tstatementnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         { evalutes the case expression }
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
         hp:=tstatementnode(right);
         while assigned(hp) do
           begin
              firstpass(hp.left);

              { searchs max registers }
              if hp.left.registers32>registers32 then
                registers32:=hp.left.registers32;
              if hp.left.registersfpu>registersfpu then
                registersfpu:=hp.left.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp.left.registersmmx>registersmmx then
                registersmmx:=hp.left.registersmmx;
{$endif SUPPORT_MMX}

              hp:=tstatementnode(hp.right);
           end;

         { may be handle else tree }
         if assigned(elseblock) then
           begin
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
         if assigned(nodes) then
           p.nodes:=copycaserecord(nodes)
         else
           p.nodes:=nil;
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
  Revision 1.45  2003-10-01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.44  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.43  2003/06/12 22:09:54  jonas
    * tcginnode.pass_2 doesn't call a helper anymore in any case
    * fixed ungetregisterfpu compilation problems

  Revision 1.42  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.41  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.40  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.39  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.38  2002/12/07 14:12:56  carl
    - removed unused variable

  Revision 1.37  2002/11/27 02:37:14  peter
    * case statement inlining added
    * fixed inlining of write()
    * switched statementnode left and right parts so the statements are
      processed in the correct order when getcopy is used. This is
      required for tempnodes

  Revision 1.36  2002/11/26 21:52:38  carl
    + hint for in operator with non byte sized operand

  Revision 1.35  2002/11/25 17:43:21  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.34  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.33  2002/09/07 12:16:03  carl
    * second part bug report 1996 fix, testrange in cordconstnode
      only called if option is set (also make parsing a tiny faster)

  Revision 1.32  2002/08/19 19:36:44  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.31  2002/08/17 09:23:38  florian
    * first part of current_procinfo rewrite

  Revision 1.30  2002/07/23 13:19:40  jonas
    * fixed evaluation of expressions with empty sets that are calculated
      at compile time

  Revision 1.29  2002/07/23 12:34:30  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.28  2002/07/22 11:48:04  daniel
  * Sets are now internally sets.

  Revision 1.27  2002/07/20 11:57:55  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.26  2002/07/06 20:19:25  carl
  + generic set handling

  Revision 1.25  2002/07/01 18:46:24  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.24  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.23  2002/05/16 19:46:39  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.21  2002/05/12 16:53:08  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.20  2002/04/07 13:27:50  carl
  + change unit use

  Revision 1.19  2002/04/02 17:11:29  peter
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

}
