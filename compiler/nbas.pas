{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    uses
       cpubase,
       aasmbase,aasmtai,aasmcpu,
       node,
       tgobj,
       symtype,symppu;

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
          procedure mark_write;override;
       end;
       terrornodeclass = class of terrornode;

       tasmnode = class(tnode)
          p_asm : taasmoutput;
          constructor create(p : taasmoutput);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
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
          procedure printnodetree(var t:text);override;
       end;
       tstatementnodeclass = class of tstatementnode;

       tblocknode = class(tunarynode)
          constructor create(l : tnode;releasetemp : boolean);virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
{$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
{$endif state_tracking}
       end;
       tblocknodeclass = class of tblocknode;

       { to allow access to the location by temp references even after the temp has }
       { already been disposed and to make sure the coherency between temps and     }
       { temp references is kept after a getcopy                                    }
       ptempinfo = ^ttempinfo;
       ttempinfo = record
         { set to the copy of a tempcreate pnode (if it gets copied) so that the }
         { refs and deletenode can hook to this copy once they get copied too    }
         hookoncopy                 : ptempinfo;
         ref                        : treference;
         restype                    : ttype;
         temptype                   : ttemptype;
         valid                      : boolean;
         nextref_set_hookoncopy_nil : boolean;
       end;

       { a node which will create a (non)persistent temp of a given type with a given  }
       { size (the size is separate to allow creating "void" temps with a custom size) }
       ttempcreatenode = class(tnode)
          size: longint;
          temptype: ttemptype;
          tempinfo: ptempinfo;
          { * persistent temps are used in manually written code where the temp }
          { be usable among different statements and where you can manually say }
          { when the temp has to be freed (using a ttempdeletenode)             }
          { * non-persistent temps are mostly used in typeconversion helpers,   }
          { where the node that receives the temp becomes responsible for       }
          { freeing it. In this last case, you should use only one reference    }
          { to it and *not* generate a ttempdeletenode                          }
          constructor create(const _restype: ttype; _size: longint; _temptype: ttemptype); virtual;
          function getcopy: tnode; override;
          function pass_1 : tnode; override;
          function det_resulttype: tnode; override;
          function docompare(p: tnode): boolean; override;
          procedure printnodedata(var t:text);override;
        end;
       ttempcreatenodeclass = class of ttempcreatenode;

        { a node which is a reference to a certain temp }
        ttemprefnode = class(tnode)
          constructor create(const temp: ttempcreatenode); virtual;
          constructor create_offset(const temp: ttempcreatenode;aoffset:longint);
          function getcopy: tnode; override;
          function pass_1 : tnode; override;
          function det_resulttype : tnode; override;
          procedure mark_write;override;
          function docompare(p: tnode): boolean; override;
          { Changes the location of this temp to ref. Useful when assigning }
          { another temp to this one. The current location will be freed.   }
          { Can only be called in pass 2 (since earlier, the temp location  }
          { isn't known yet)                                                }
          procedure changelocation(const ref: treference);
         protected
          tempinfo: ptempinfo;
          offset : longint;
        end;
       ttemprefnodeclass = class of ttemprefnode;

        { a node which removes a temp }
        ttempdeletenode = class(tnode)
          constructor create(const temp: ttempcreatenode);
          { this will convert the persistant temp to a normal temp
            for returning to the other nodes }
          constructor create_normal_temp(const temp: ttempcreatenode);
          function getcopy: tnode; override;
          function pass_1: tnode; override;
          function det_resulttype: tnode; override;
          function docompare(p: tnode): boolean; override;
          destructor destroy; override;
         protected
          tempinfo: ptempinfo;
          release_to_normal : boolean;
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

       { Create a blocknode and statement node for multiple statements
         generated internally by the parser }
       function  internalstatements(var laststatement:tstatementnode;releasetemp : boolean):tblocknode;
       procedure addstatement(var laststatement:tstatementnode;n:tnode);


implementation

    uses
      cutils,
      verbose,globals,globtype,systems,
      symconst,symdef,symsym,symutil,defutil,defcmp,
      pass_1,
      nld,ncal,nflw,rgobj,cginfo,cgbase
      ;


{*****************************************************************************
                                     Helpers
*****************************************************************************}

    function internalstatements(var laststatement:tstatementnode;releasetemp : boolean):tblocknode;
      begin
        { create dummy initial statement }
        laststatement := cstatementnode.create(cnothingnode.create,nil);
        internalstatements := cblocknode.create(laststatement,releasetemp);
      end;


    procedure addstatement(var laststatement:tstatementnode;n:tnode);
      begin
        if assigned(laststatement.right) then
         internalerror(200204201);
        laststatement.right:=cstatementnode.create(n,nil);
        laststatement:=tstatementnode(laststatement.right);
      end;


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
        expectloc:=LOC_VOID;
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
         expectloc:=LOC_VOID;
         codegenerror:=true;
      end;


    procedure terrornode.mark_write;
      begin
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

         { left is the statement itself calln assignn or a complex one }
         resulttypepass(left);
         if (not (cs_extsyntax in aktmoduleswitches)) and
            assigned(left.resulttype.def) and
            not((left.nodetype=calln) and
                { don't complain when funcretrefnode is set, because then the
                  value is already used. And also not for constructors }
                (assigned(tcallnode(left).funcretnode) or
                 (tcallnode(left).procdefinition.proctypeoption=potype_constructor))) and
            not(is_void(left.resulttype.def)) then
           CGMessage(cg_e_illegal_expression);
         if codegenerror then
           exit;

         { right is the next statement in the list }
         if assigned(right) then
           resulttypepass(right);
         if codegenerror then
           exit;
      end;


    function tstatementnode.pass_1 : tnode;
      begin
         result:=nil;
         { left is the statement itself calln assignn or a complex one }
         firstpass(left);
         if codegenerror then
           exit;
         expectloc:=left.expectloc;
         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
         { right is the next in the list }
         if assigned(right) then
           firstpass(right);
         if codegenerror then
           exit;
      end;


    procedure tstatementnode.printnodetree(var t:text);
      begin
        printnodelist(t);
      end;

{*****************************************************************************
                             TBLOCKNODE
*****************************************************************************}

    constructor tblocknode.create(l : tnode;releasetemp : boolean);

      begin
         inherited create(blockn,l);
    {$ifndef newra}
         if releasetemp then
           include(flags,nf_releasetemps);
    {$endif newra}
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
              if assigned(hp.left) then
                begin
                   codegenerror:=false;
                   resulttypepass(hp.left);
                   if (not (cs_extsyntax in aktmoduleswitches)) and
                      assigned(hp.left.resulttype.def) and
                      not((hp.left.nodetype=calln) and
                          { don't complain when funcretnode is set, because then the
                            value is already used. And also not for constructors }
                          (assigned(tcallnode(hp.left).funcretnode) or
                           (tcallnode(hp.left).procdefinition.proctypeoption=potype_constructor))) and
                      not(is_void(hp.left.resulttype.def)) then
                     CGMessagePos(hp.left.fileinfo,cg_e_illegal_expression);
                   { the resulttype of the block is the last type that is
                     returned. Normally this is a voidtype. But when the
                     compiler inserts a block of multiple statements then the
                     last entry can return a value }
                   resulttype:=hp.left.resulttype;
                end;
              hp:=tstatementnode(hp.right);
           end;
      end;


    function tblocknode.pass_1 : tnode;
      var
         hp : tstatementnode;
         count : longint;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         count:=0;
         hp:=tstatementnode(left);
         while assigned(hp) do
           begin
(*
              if cs_regalloc in aktglobalswitches then
                begin
                   { node transformations }

                   { concat function result to exit }
                   { this is wrong for string or other complex
                     result types !!! }
                   if {ret_in_acc(current_procdef.rettype.def) and }
                      (is_ordinal(current_procdef.rettype.def) or
                       is_smallset(current_procdef.rettype.def)) and
                      assigned(hp.right) and
                      assigned(tstatementnode(hp.right).left) and
                      (tstatementnode(hp.right).left.nodetype=exitn) and
                      (hp.left.nodetype=assignn) and
                      { !!!! this tbinarynode should be tassignmentnode }
                      (tbinarynode(hp.left).left.nodetype=loadn) and
                      (is_funcret_sym(tloadnode(tbinarynode(hp.left).left).symtableentry)) then
                      begin
                         if assigned(texitnode(tstatementnode(hp.right).left).left) then
                           CGMessage(cg_n_inefficient_code)
                         else
                           begin
                              texitnode(tstatementnode(hp.right).left).left:=tassignmentnode(hp.left).right;
                              tassignmentnode(hp.left).right:=nil;
                              hp.left.free;
                              hp.left:=nil;
                           end;
                      end
                   { warning if unreachable code occurs and elimate this }
                   else if (hp.left.nodetype in
                     [exitn,breakn,continuen,goton]) and
                     { statement node (JM) }
                     assigned(hp.right) and
                     { kind of statement! (JM) }
                     assigned(tstatementnode(hp.right).left) and
                     (tstatementnode(hp.right).left.nodetype<>labeln) then
                     begin
                        { use correct line number }
                        aktfilepos:=hp.right.fileinfo;
                        hp.right.free;
                        hp.right:=nil;
                        CGMessage(cg_w_unreachable_code);
                        { old lines }
                        aktfilepos:=hp.left.fileinfo;
                     end;
                end;
*)
              if assigned(hp.left) then
                begin
                   codegenerror:=false;
                   firstpass(hp.left);

                   hp.expectloc:=hp.left.expectloc;
                   hp.registers32:=hp.left.registers32;
                   hp.registersfpu:=hp.left.registersfpu;
{$ifdef SUPPORT_MMX}
                   hp.registersmmx:=hp.left.registersmmx;
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
              expectloc:=hp.expectloc;
              inc(count);
              hp:=tstatementnode(hp.right);
           end;
      end;

{$ifdef state_tracking}
      function Tblocknode.track_state_pass(exec_known:boolean):boolean;

      var hp:Tstatementnode;

      begin
        track_state_pass:=false;
        hp:=Tstatementnode(left);
        while assigned(hp) do
            begin
                if hp.left.track_state_pass(exec_known) then
                    track_state_pass:=true;
                hp:=Tstatementnode(hp.right);
            end;
      end;
{$endif state_tracking}

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


    constructor tasmnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      var
        hp : tai;
      begin
        inherited ppuload(t,ppufile);
        p_asm:=taasmoutput.create;
        repeat
          hp:=ppuloadai(ppufile);
          if hp=nil then
           break;
          p_asm.concat(hp);
        until false;
      end;


    procedure tasmnode.ppuwrite(ppufile:tcompilerppufile);
      var
        hp : tai;
      begin
        inherited ppuwrite(ppufile);
        hp:=tai(p_asm.first);
        while assigned(hp) do
         begin
           ppuwriteai(ppufile,hp);
           hp:=tai(hp.next);
         end;
        { end is marked by a nil }
        ppuwriteai(ppufile,nil);
      end;


    procedure tasmnode.derefimpl;
      var
        hp : tai;
      begin
        inherited derefimpl;
        hp:=tai(p_asm.first);
        while assigned(hp) do
         begin
           hp.derefimpl;
           hp:=tai(hp.next);
         end;
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
         include(current_procinfo.flags,pi_uses_asm);
      end;

    function tasmnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
      end;


    function tasmnode.docompare(p: tnode): boolean;
      begin
        { comparing of asmlists is not implemented (JM) }
        docompare := false;
      end;


{*****************************************************************************
                          TEMPCREATENODE
*****************************************************************************}

    constructor ttempcreatenode.create(const _restype: ttype; _size: longint; _temptype: ttemptype);
      begin
        inherited create(tempcreaten);
        size := _size;
        new(tempinfo);
        fillchar(tempinfo^,sizeof(tempinfo^),0);
        tempinfo^.restype := _restype;
        temptype := _temptype;
      end;

    function ttempcreatenode.getcopy: tnode;
      var
        n: ttempcreatenode;
      begin
        n := ttempcreatenode(inherited getcopy);
        n.size := size;
        n.temptype := temptype;

        new(n.tempinfo);
        fillchar(n.tempinfo^,sizeof(n.tempinfo^),0);
        n.tempinfo^.restype := tempinfo^.restype;

        { when the tempinfo has already a hookoncopy then it is not
          reset by a tempdeletenode }
        if assigned(tempinfo^.hookoncopy) then
          internalerror(200211262);

        { signal the temprefs that the temp they point to has been copied, }
        { so that if the refs get copied as well, they can hook themselves }
        { to the copy of the temp                                          }
        tempinfo^.hookoncopy := n.tempinfo;
        tempinfo^.nextref_set_hookoncopy_nil := false;

        result := n;
      end;

    function ttempcreatenode.pass_1 : tnode;
      begin
         result := nil;
         expectloc:=LOC_VOID;
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
          equal_defs(ttempcreatenode(p).tempinfo^.restype.def,tempinfo^.restype.def);
      end;


    procedure ttempcreatenode.printnodedata(var t:text);
      begin
        inherited printnodedata(t);
        writeln(t,printnodeindention,'size = ',size);
      end;


{*****************************************************************************
                             TEMPREFNODE
*****************************************************************************}

    constructor ttemprefnode.create(const temp: ttempcreatenode);
      begin
        inherited create(temprefn);
        tempinfo := temp.tempinfo;
        offset:=0;
      end;

    constructor ttemprefnode.create_offset(const temp: ttempcreatenode;aoffset:longint);
      begin
        self.create(temp);
        offset := aoffset;
      end;

    function ttemprefnode.getcopy: tnode;
      var
        n: ttemprefnode;
      begin
        n := ttemprefnode(inherited getcopy);
        n.offset := offset;

        if assigned(tempinfo^.hookoncopy) then
          { if the temp has been copied, assume it becomes a new }
          { temp which has to be hooked by the copied reference  }
          begin
            { hook the ref to the copied temp }
            n.tempinfo := tempinfo^.hookoncopy;
            { if we passed a ttempdeletenode that changed the temp }
            { from a persistent one into a normal one, we must be  }
            { the last reference (since our parent should free the }
            { temp (JM)                                            }
            if (tempinfo^.nextref_set_hookoncopy_nil) then
              tempinfo^.hookoncopy := nil;
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
        expectloc:=LOC_REFERENCE;
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
          (ttemprefnode(p).tempinfo = tempinfo) and
          (ttemprefnode(p).offset = offset);
      end;

    procedure Ttemprefnode.mark_write;

    begin
      include(flags,nf_write);
    end;


    procedure ttemprefnode.changelocation(const ref: treference);
      begin
        { check if the temp is valid }
        if not tempinfo^.valid then
          internalerror(200306081);
        if (tempinfo^.temptype = tt_persistent) then
          tg.ChangeTempType(exprasmlist,tempinfo^.ref,tt_normal);
        tg.ungettemp(exprasmlist,tempinfo^.ref);
        tempinfo^.ref := ref;
        tg.ChangeTempType(exprasmlist,tempinfo^.ref,tempinfo^.temptype);
        { adapt location }
        location.reference := ref;
        inc(location.reference.offset,offset);
      end;


{*****************************************************************************
                             TEMPDELETENODE
*****************************************************************************}

    constructor ttempdeletenode.create(const temp: ttempcreatenode);
      begin
        inherited create(tempdeleten);
        tempinfo := temp.tempinfo;
        release_to_normal := false;
      end;


    constructor ttempdeletenode.create_normal_temp(const temp: ttempcreatenode);
      begin
        inherited create(tempdeleten);
        tempinfo := temp.tempinfo;
        release_to_normal := true;
        if temp.temptype <> tt_persistent then
          internalerror(200204211);
      end;


    function ttempdeletenode.getcopy: tnode;
      var
        n: ttempdeletenode;
      begin
        n := ttempdeletenode(inherited getcopy);
        n.release_to_normal := release_to_normal;

        if assigned(tempinfo^.hookoncopy) then
          { if the temp has been copied, assume it becomes a new }
          { temp which has to be hooked by the copied deletenode }
          begin
            { hook the tempdeletenode to the copied temp }
            n.tempinfo := tempinfo^.hookoncopy;
            { the temp shall not be used, reset hookoncopy    }
            { Only if release_to_normal is false, otherwise   }
            { the temp can still be referenced once more (JM) }
            if (not release_to_normal) then
              tempinfo^.hookoncopy:=nil
            else
              tempinfo^.nextref_set_hookoncopy_nil := true;
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
         expectloc:=LOC_VOID;
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
  Revision 1.54  2003-06-08 18:27:15  jonas
    + ability to change the location of a ttempref node with changelocation()
      method. Useful to use instead of copying the contents from one temp to
      another
    + some shortstring optimizations in tassignmentnode that avoid some
      copying (required some shortstring optimizations to be moved from
      resulttype to firstpass, because they work on callnodes and string
      addnodes are only changed to callnodes in the firstpass)
    * allow setting/changing the funcretnode of callnodes after the
      resulttypepass has been done, funcretnode is now a property
    (all of the above should have a quite big effect on callparatemp)

  Revision 1.53  2003/05/30 21:01:44  jonas
    - disabled "result := value; exit;" -> exit(value) optimization because
      a) it was wrong
      b) exit(value) works now exactly the same as that
     (it was only activated with -Or)

  Revision 1.52  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.51  2003/05/17 13:30:08  jonas
    * changed tt_persistant to tt_persistent :)
    * tempcreatenode now doesn't accept a boolean anymore for persistent
      temps, but a ttemptype, so you can also create ansistring temps etc

  Revision 1.50  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.49  2003/05/11 14:45:12  peter
    * tloadnode does not support objectsymtable,withsymtable anymore
    * withnode cleanup
    * direct with rewritten to use temprefnode

  Revision 1.48  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.47  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.46  2002/04/25 20:15:39  florian
    * block nodes within expressions shouldn't release the used registers,
      fixed using a flag till the new rg is ready

  Revision 1.45  2003/04/23 08:41:34  jonas
    * fixed ttemprefnode.compare and .getcopy to take offset field into
      account

  Revision 1.44  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.43  2003/04/21 15:00:22  jonas
    * fixed tstatementnode.det_resulttype and tststatementnode.pass_1
    * fixed some getcopy issues with ttemp*nodes

  Revision 1.42  2003/04/17 07:50:24  daniel
    * Some work on interference graph construction

  Revision 1.41  2003/04/12 14:53:59  jonas
    * ttempdeletenode.create now sets the nodetype to tempdeleten instead of
      temprefn

  Revision 1.40  2003/03/17 20:30:46  peter
    * errornode.mark_write added

  Revision 1.39  2003/01/03 12:15:55  daniel
    * Removed ifdefs around notifications
      ifdefs around for loop optimizations remain

  Revision 1.38  2002/11/27 02:37:12  peter
    * case statement inlining added
    * fixed inlining of write()
    * switched statementnode left and right parts so the statements are
      processed in the correct order when getcopy is used. This is
      required for tempnodes

  Revision 1.37  2002/11/25 17:43:17  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.36  2002/10/05 15:15:19  peter
    * don't complain in X- mode for internal generated function calls
      with funcretrefnode set
    * give statement error at the correct line position instead of the
      block begin

  Revision 1.35  2002/09/01 08:01:16  daniel
   * Removed sets from Tcallnode.det_resulttype
   + Added read/write notifications of variables. These will be usefull
     for providing information for several optimizations. For example
     the value of the loop variable of a for loop does matter is the
     variable is read after the for loop, but if it's no longer used
     or written, it doesn't matter and this can be used to optimize
     the loop code generation.

  Revision 1.34  2002/08/18 20:06:23  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.33  2002/08/17 22:09:44  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.printnodetree
    * some ppc stuff fixed

  Revision 1.32  2002/08/17 09:23:34  florian
    * first part of procinfo rewrite

  Revision 1.31  2002/08/15 19:10:35  peter
    * first things tai,tnode storing in ppu

  Revision 1.30  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.29  2002/07/19 11:41:35  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.28  2002/07/14 18:00:43  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

  Revision 1.27  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.26  2002/06/24 12:43:00  jonas
    * fixed errors found with new -CR code from Peter when cycling with -O2p3r

  Revision 1.25  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.24  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.22  2002/04/23 19:16:34  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.21  2002/04/21 19:02:03  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.20  2002/04/04 19:05:57  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.19  2002/03/31 20:26:33  jonas
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
