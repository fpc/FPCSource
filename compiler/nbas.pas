{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    This unit implements some basic nodes

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
unit nbas;

{$i defines.inc}

interface

    uses
       aasm,symtype,node,cpubase;

    type
       tnothingnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tnothingnodeclass = class of tnothingnode;

       terrornode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       terrornodeclass = class of terrornode;

       tasmnode = class(tnode)
          p_asm : taasmoutput;
          constructor create(p : taasmoutput);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tasmnodeclass = class of tasmnode;

       tstatementnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
{$ifdef extdebug}
          procedure dowrite;override;
{$endif extdebug}
       end;
       tstatementnodeclass = class of tstatementnode;

       tblocknode = class(tunarynode)
          constructor create(l : tnode);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tblocknodeclass = class of tblocknode;

       { to allow access to the location by temp references even after the temp has }
       { already been disposed and to make sure the coherency between temps and     }
       { temp references is kept after a getcopy                                    }
       ptempinfo = ^ttempinfo;
       ttempinfo = record
         { set to the copy of a tempcreate pnode (if it gets copied) so that the }
         { refs and deletenode can hook to this copy once they get copied too    }
         hookoncopy: ptempinfo;
         ref: treference;
         restype: ttype;
         valid: boolean;
       end;

       { a node which will create a (non)persistent temp of a given type with a given  }
       { size (the size is separate to allow creating "void" temps with a custom size) }
       ttempcreatenode = class(tnode)
          size: longint;
          tempinfo: ptempinfo;
          { * persistent temps are used in manually written code where the temp }
          { be usable among different statements and where you can manually say }
          { when the temp has to be freed (using a ttempdeletenode)             }
          { * non-persistent temps are mostly used in typeconversion helpers,   }
          { where the node that receives the temp becomes responsible for       }
          { freeing it. In this last case, you should use only one reference    }
          { to it and *not* generate a ttempdeletenode                          }
          constructor create(const _restype: ttype; _size: longint; _persistent: boolean); virtual;
          function getcopy: tnode; override;
          function pass_1 : tnode; override;
          function det_resulttype: tnode; override;
          function docompare(p: tnode): boolean; override;
         protected
          persistent: boolean;
        end;
       ttempcreatenodeclass = class of ttempcreatenode;

        { a node which is a reference to a certain temp }
        ttemprefnode = class(tnode)
          constructor create(const temp: ttempcreatenode); virtual;
          function getcopy: tnode; override;
          function pass_1 : tnode; override;
          function det_resulttype : tnode; override;
          function docompare(p: tnode): boolean; override;
         protected
          tempinfo: ptempinfo;
        end;
       ttemprefnodeclass = class of ttemprefnode;

        { a node which removes a temp }
        ttempdeletenode = class(tnode)
          constructor create(const temp: ttempcreatenode);
          function getcopy: tnode; override;
          function pass_1: tnode; override;
          function det_resulttype: tnode; override;
          function docompare(p: tnode): boolean; override;
          destructor destroy; override;
         protected
          tempinfo: ptempinfo;
        end;
       ttempdeletenodeclass = class of ttempdeletenode;

    var
       cnothingnode : tnothingnodeclass;
       cerrornode : terrornodeclass;
       casmnode : tasmnodeclass;
       cstatementnode : tstatementnodeclass;
       cblocknode : tblocknodeclass;
       ctempcreatenode : ttempcreatenodeclass;
       ctemprefnode : ttemprefnodeclass;
       ctempdeletenode : ttempdeletenodeclass;

implementation

    uses
      cutils,
      verbose,globals,globtype,systems,
      symconst,symdef,symsym,types,
      pass_1,
      ncal,nflw,rgobj,cgbase
      ;

{*****************************************************************************
                             TFIRSTNOTHING
*****************************************************************************}

    constructor tnothingnode.create;
      begin
         inherited create(nothingn);
      end;

    function tnothingnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;
      end;

    function tnothingnode.pass_1 : tnode;
      begin
         result:=nil;
      end;


{*****************************************************************************
                             TFIRSTERROR
*****************************************************************************}

    constructor terrornode.create;

      begin
         inherited create(errorn);
      end;

    function terrornode.det_resulttype:tnode;
      begin
         result:=nil;
         include(flags,nf_error);
         codegenerror:=true;
         resulttype:=generrortype;
      end;

    function terrornode.pass_1 : tnode;
      begin
         result:=nil;
         codegenerror:=true;
      end;

{*****************************************************************************
                            TSTATEMENTNODE
*****************************************************************************}

    constructor tstatementnode.create(l,r : tnode);

      begin
         inherited create(statementn,l,r);
      end;

    function tstatementnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;

         { right is the statement itself calln assignn or a complex one }
         resulttypepass(right);
         if (not (cs_extsyntax in aktmoduleswitches)) and
            assigned(right.resulttype.def) and
            not((right.nodetype=calln) and
                (tcallnode(right).procdefinition.proctypeoption=potype_constructor)) and
            not(is_void(right.resulttype.def)) then
           CGMessage(cg_e_illegal_expression);
         if codegenerror then
           exit;

         { left is the next in the list }
         resulttypepass(left);
         if codegenerror then
           exit;
      end;

    function tstatementnode.pass_1 : tnode;
      begin
         result:=nil;
         { no temps over several statements }
         rg.cleartempgen;
         { right is the statement itself calln assignn or a complex one }
         firstpass(right);
         if codegenerror then
           exit;
         registers32:=right.registers32;
         registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=right.registersmmx;
{$endif SUPPORT_MMX}
         { left is the next in the list }
         firstpass(left);
         if codegenerror then
           exit;
         if right.registers32>registers32 then
           registers32:=right.registers32;
         if right.registersfpu>registersfpu then
           registersfpu:=right.registersfpu;
{$ifdef SUPPORT_MMX}
         if right.registersmmx>registersmmx then
           registersmmx:=right.registersmmx;
{$endif}
      end;

{$ifdef extdebug}
    procedure tstatementnode.dowrite;

      begin
         { can't use inherited dowrite, because that will use the
           binary which we don't want for statements }
         dowritenodetype;
         writeln(',');
         { write the statement }
         writenodeindention:=writenodeindention+'    ';
         writenode(right);
         writeln(')');
         delete(writenodeindention,1,4);
         { go on with the next statement }
         writenode(left);
      end;
{$endif}

{*****************************************************************************
                             TBLOCKNODE
*****************************************************************************}

    constructor tblocknode.create(l : tnode);

      begin
         inherited create(blockn,l);
      end;

    function tblocknode.det_resulttype:tnode;
      var
         hp : tstatementnode;
      begin
         result:=nil;
         resulttype:=voidtype;

         hp:=tstatementnode(left);
         while assigned(hp) do
           begin
              if assigned(hp.right) then
                begin
                   codegenerror:=false;
                   resulttypepass(hp.right);
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp.right.resulttype.def) and
                      not((hp.right.nodetype=calln) and
                          (tcallnode(hp.right).procdefinition.proctypeoption=potype_constructor)) and
                      not(is_void(hp.right.resulttype.def)) then
                     CGMessage(cg_e_illegal_expression);
                end;
              hp:=tstatementnode(hp.left);
           end;
      end;

    function tblocknode.pass_1 : tnode;
      var
         hp : tstatementnode;
         count : longint;
      begin
         result:=nil;
         count:=0;
         hp:=tstatementnode(left);
         while assigned(hp) do
           begin
              if cs_regalloc in aktglobalswitches then
                begin
                   { node transformations }

                   { concat function result to exit }
                   { this is wrong for string or other complex
                     result types !!! }
                   if {ret_in_acc(aktprocdef.rettype.def) and }
                      (is_ordinal(aktprocdef.rettype.def) or
                       is_smallset(aktprocdef.rettype.def)) and
                      assigned(hp.left) and
                      assigned(tstatementnode(hp.left).right) and
                      (tstatementnode(hp.left).right.nodetype=exitn) and
                      (hp.right.nodetype=assignn) and
                      { !!!! this tbinarynode should be tassignmentnode }
                      (tbinarynode(hp.right).left.nodetype=funcretn) then
                      begin
                         if assigned(texitnode(tstatementnode(hp.left).right).left) then
                           CGMessage(cg_n_inefficient_code)
                         else
                           begin
                              texitnode(tstatementnode(hp.left).right).left:=tstatementnode(hp.right).right;
                              tstatementnode(hp.right).right:=nil;
                              hp.right.free;
                              hp.right:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp.right.nodetype in
                     [exitn,breakn,continuen,goton]) and
                     { statement node (JM) }
                     assigned(hp.left) and
                     { kind of statement! (JM) }
                     assigned(tstatementnode(hp.left).right) and
                     (tstatementnode(hp.left).right.nodetype<>labeln) then
                     begin
                        { use correct line number }
                        aktfilepos:=hp.left.fileinfo;
                        hp.left.free;
                        hp.left:=nil;
                        CGMessage(cg_w_unreachable_code);
                        { old lines }
                        aktfilepos:=hp.right.fileinfo;
                     end;
                end;
              if assigned(hp.right) then
                begin
                   rg.cleartempgen;
                   codegenerror:=false;
                   firstpass(hp.right);

                   hp.registers32:=hp.right.registers32;
                   hp.registersfpu:=hp.right.registersfpu;
{$ifdef SUPPORT_MMX}
                   hp.registersmmx:=hp.right.registersmmx;
{$endif SUPPORT_MMX}
                end
              else
                hp.registers32:=0;

              if hp.registers32>registers32 then
                registers32:=hp.registers32;
              if hp.registersfpu>registersfpu then
                registersfpu:=hp.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp.registersmmx>registersmmx then
                registersmmx:=hp.registersmmx;
{$endif}
              inc(count);
              hp:=tstatementnode(hp.left);
           end;
      end;


{*****************************************************************************
                             TASMNODE
*****************************************************************************}

    constructor tasmnode.create(p : taasmoutput);

      begin
         inherited create(asmn);
         p_asm:=p;
      end;

    destructor tasmnode.destroy;
      begin
        if assigned(p_asm) then
         p_asm.free;
        inherited destroy;
      end;

    function tasmnode.getcopy: tnode;
      var
        n: tasmnode;
      begin
        n := tasmnode(inherited getcopy);
        if assigned(p_asm) then
          begin
            n.p_asm:=taasmoutput.create;
            n.p_asm.concatlistcopy(p_asm);
          end
        else n.p_asm := nil;
        getcopy := n;
      end;

    function tasmnode.det_resulttype:tnode;
      begin
         result:=nil;
         resulttype:=voidtype;
      end;

    function tasmnode.pass_1 : tnode;
      begin
         result:=nil;
         procinfo^.flags:=procinfo^.flags or pi_uses_asm;
      end;

    function tasmnode.docompare(p: tnode): boolean;
      begin
        { comparing of asmlists is not implemented (JM) }
        docompare := false;
      end;

{*****************************************************************************
                          TEMPCREATENODE
*****************************************************************************}

    constructor ttempcreatenode.create(const _restype: ttype; _size: longint; _persistent: boolean);
      begin
        inherited create(tempn);
        size := _size;
        new(tempinfo);
        fillchar(tempinfo^,sizeof(tempinfo^),0);
        tempinfo^.restype := _restype;
        persistent := _persistent;
      end;

    function ttempcreatenode.getcopy: tnode;
      var
        n: ttempcreatenode;
      begin
        n := ttempcreatenode(inherited getcopy);
        n.size := size;

        new(n.tempinfo);
        fillchar(n.tempinfo^,sizeof(n.tempinfo^),0);
        n.tempinfo^.restype := tempinfo^.restype;

        { signal the temprefs that the temp they point to has been copied, }
        { so that if the refs get copied as well, they can hook themselves }
        { to the copy of the temp                                          }
        tempinfo^.hookoncopy := n.tempinfo;

        result := n;
      end;

    function ttempcreatenode.pass_1 : tnode;
      begin
        result := nil;
      end;

    function ttempcreatenode.det_resulttype: tnode;
      begin
        result := nil;
        { a tempcreatenode doesn't have a resulttype, only temprefnodes do }
        resulttype := voidtype;
      end;

    function ttempcreatenode.docompare(p: tnode): boolean;
      begin
        result :=
          inherited docompare(p) and
          (ttempcreatenode(p).size = size) and
          is_equal(ttempcreatenode(p).tempinfo^.restype.def,tempinfo^.restype.def);
      end;

{*****************************************************************************
                             TEMPREFNODE
*****************************************************************************}

    constructor ttemprefnode.create(const temp: ttempcreatenode);
      begin
        inherited create(temprefn);
        tempinfo := temp.tempinfo;
      end;

    function ttemprefnode.getcopy: tnode;
      var
        n: ttemprefnode;
      begin
        n := ttemprefnode(inherited getcopy);

        if assigned(tempinfo^.hookoncopy) then
          { if the temp has been copied, assume it becomes a new }
          { temp which has to be hooked by the copied reference  }
          begin
            { hook the ref to the copied temp }
            n.tempinfo := tempinfo^.hookoncopy;
          end
        else
          { if the temp we refer to hasn't been copied, assume }
          { we're just a new reference to that temp            }
          begin
            n.tempinfo := tempinfo;
          end;

        result := n;
      end;

    function ttemprefnode.pass_1 : tnode;
      begin
        result := nil;
      end;

    function ttemprefnode.det_resulttype: tnode;
      begin
        { check if the temp is already resulttype passed }
        if not assigned(tempinfo^.restype.def) then
          internalerror(200108233);
        result := nil;
        resulttype := tempinfo^.restype;
      end;

    function ttemprefnode.docompare(p: tnode): boolean;
      begin
        result :=
          inherited docompare(p) and
          (ttemprefnode(p).tempinfo = tempinfo);
      end;

{*****************************************************************************
                             TEMPDELETENODE
*****************************************************************************}

    constructor ttempdeletenode.create(const temp: ttempcreatenode);
      begin
        inherited create(temprefn);
        tempinfo := temp.tempinfo;
      end;

    function ttempdeletenode.getcopy: tnode;
      var
        n: ttempdeletenode;
      begin
        n := ttempdeletenode(inherited getcopy);

        if assigned(tempinfo^.hookoncopy) then
          { if the temp has been copied, assume it becomes a new }
          { temp which has to be hooked by the copied deletenode }
          begin
            { hook the tempdeletenode to the copied temp }
            n.tempinfo := tempinfo^.hookoncopy;
          end
        else
          { if the temp we refer to hasn't been copied, we have a }
          { problem since that means we now have two delete nodes }
          { for one temp                                          }
          internalerror(200108234);
        result := n;
      end;

    function ttempdeletenode.pass_1 : tnode;
      begin
        result := nil;
      end;

    function ttempdeletenode.det_resulttype: tnode;
      begin
        result := nil;
        resulttype := voidtype;
      end;

    function ttempdeletenode.docompare(p: tnode): boolean;
      begin
        result :=
          inherited docompare(p) and
          (ttemprefnode(p).tempinfo = tempinfo);
      end;

    destructor ttempdeletenode.destroy;
      begin
        dispose(tempinfo);
      end;

begin
   cnothingnode:=tnothingnode;
   cerrornode:=terrornode;
   casmnode:=tasmnode;
   cstatementnode:=tstatementnode;
   cblocknode:=tblocknode;
   ctempcreatenode:=ttempcreatenode;
   ctemprefnode:=ttemprefnode;
   ctempdeletenode:=ttempdeletenode;
end.
{
  $Log$
  Revision 1.19  2002-03-31 20:26:33  jonas
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

  Revision 1.18  2001/11/02 22:58:01  peter
    * procsym definition rewrite

  Revision 1.17  2001/09/02 21:12:06  peter
    * move class of definitions into type section for delphi

  Revision 1.16  2001/08/26 13:36:38  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.15  2001/08/24 13:47:26  jonas
    * moved "reverseparameters" from ninl.pas to ncal.pas
    + support for non-persistent temps in ttempcreatenode.create, for use
      with typeconversion nodes

  Revision 1.14  2001/08/23 14:28:35  jonas
    + tempcreate/ref/delete nodes (allows the use of temps in the
      resulttype and first pass)
    * made handling of read(ln)/write(ln) processor independent
    * moved processor independent handling for str and reset/rewrite-typed
      from firstpass to resulttype pass
    * changed names of helpers in text.inc to be generic for use as
      compilerprocs + added "iocheck" directive for most of them
    * reading of ordinals is done by procedures instead of functions
      because otherwise FPC_IOCHECK overwrote the result before it could
      be stored elsewhere (range checking still works)
    * compilerprocs can now be used in the system unit before they are
      implemented
    * added note to errore.msg that booleans can't be read using read/readln

  Revision 1.13  2001/08/06 21:40:46  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.12  2001/06/11 17:41:12  jonas
    * fixed web bug 1501 in conjunction with -Or

  Revision 1.11  2001/05/18 22:31:06  peter
    * tasmnode.pass_2 is independent of cpu, moved to ncgbas
    * include ncgbas for independent nodes

  Revision 1.10  2001/04/13 01:22:08  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.9  2001/04/02 21:20:30  peter
    * resulttype rewrite

  Revision 1.8  2001/02/05 20:45:49  peter
    * fixed buf 1364

  Revision 1.7  2000/12/31 11:14:10  jonas
    + implemented/fixed docompare() mathods for all nodes (not tested)
    + nopt.pas, nadd.pas, i386/n386opt.pas: optimized nodes for adding strings
      and constant strings/chars together
    * n386add.pas: don't copy temp strings (of size 256) to another temp string
      when adding

  Revision 1.6  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.5  2000/11/29 00:30:31  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.4  2000/10/31 22:02:47  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/27 14:57:16  jonas
    + implementation for tasmnode.getcopy

  Revision 1.2  2000/10/14 21:52:54  peter
    * fixed memory leaks

  Revision 1.1  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

}
