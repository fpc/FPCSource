{
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
       globtype,
       cpuinfo,cpubase,cgbase,cgutils,
       aasmbase,aasmtai,aasmdata,aasmcpu,
       node,
       symtype;

    type
       tnothingnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
       end;
       tnothingnodeclass = class of tnothingnode;

       terrornode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          procedure mark_write;override;
       end;
       terrornodeclass = class of terrornode;

       tasmnode = class(tnode)
          p_asm : TAsmList;
          currenttai : tai;
          { Used registers in assembler block }
          used_regs_int,
          used_regs_fpu : tcpuregisterset;
          constructor create(p : TAsmList);virtual;
          constructor create_get_position;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function dogetcopy : tnode;override;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          function docompare(p: tnode): boolean; override;
       end;
       tasmnodeclass = class of tasmnode;

       tstatementnode = class(tbinarynode)
          constructor create(l,r : tnode);virtual;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
          procedure printnodetree(var t:text);override;
          property statement : tnode read left write left;
          property next : tnode read right write right;
       end;
       tstatementnodeclass = class of tstatementnode;

       tblocknode = class(tunarynode)
          constructor create(l : tnode);virtual;
          destructor destroy; override;
          function pass_1 : tnode;override;
          function pass_typecheck:tnode;override;
{$ifdef state_tracking}
          function track_state_pass(exec_known:boolean):boolean;override;
{$endif state_tracking}
          property statements : tnode read left write left;
       end;
       tblocknodeclass = class of tblocknode;

       ttempcreatenode = class;

       { to allow access to the location by temp references even after the temp has }
       { already been disposed and to make sure the coherency between temps and     }
       { temp references is kept after a getcopy                                    }
       ptempinfo = ^ttempinfo;
       ttempinfo = record
         { set to the copy of a tempcreate pnode (if it gets copied) so that the }
         { refs and deletenode can hook to this copy once they get copied too    }
         hookoncopy                 : ptempinfo;
         typedef                    : tdef;
         typedefderef               : tderef;
         temptype                   : ttemptype;
         owner                      : ttempcreatenode;
         may_be_in_reg              : boolean;
         valid                      : boolean;
         nextref_set_hookoncopy_nil : boolean;
         location                   : tlocation;
       end;

       { a node which will create a (non)persistent temp of a given type with a given  }
       { size (the size is separate to allow creating "void" temps with a custom size) }
       ttempcreatenode = class(tnode)
          size: aint;
          tempinfo: ptempinfo;
          { * persistent temps are used in manually written code where the temp }
          { be usable among different statements and where you can manually say }
          { when the temp has to be freed (using a ttempdeletenode)             }
          { * non-persistent temps are mostly used in typeconversion helpers,   }
          { where the node that receives the temp becomes responsible for       }
          { freeing it. In this last case, you must use only one reference      }
          { to it and *not* generate a ttempdeletenode                          }
          constructor create(_typedef: tdef; _size: aint; _temptype: ttemptype;allowreg:boolean); virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function dogetcopy: tnode; override;
          function pass_1 : tnode; override;
          function pass_typecheck: tnode; override;
          function docompare(p: tnode): boolean; override;
          procedure printnodedata(var t:text);override;
        end;
       ttempcreatenodeclass = class of ttempcreatenode;

        { a node which is a reference to a certain temp }
        ttemprefnode = class(tnode)
          tempinfo: ptempinfo;

          constructor create(const temp: ttempcreatenode); virtual;
          constructor create_offset(const temp: ttempcreatenode;aoffset:longint);
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function dogetcopy: tnode; override;
          procedure derefnode;override;
          function pass_1 : tnode; override;
          function pass_typecheck : tnode; override;
          procedure mark_write;override;
          function docompare(p: tnode): boolean; override;
          procedure printnodedata(var t:text);override;
         protected
          offset : longint;
         private
          tempidx : longint;
        end;
       ttemprefnodeclass = class of ttemprefnode;

        { a node which removes a temp }
        ttempdeletenode = class(tnode)
          tempinfo: ptempinfo;
          constructor create(const temp: ttempcreatenode); virtual;
          { this will convert the persistant temp to a normal temp
            for returning to the other nodes }
          constructor create_normal_temp(const temp: ttempcreatenode);
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          function dogetcopy: tnode; override;
          procedure derefnode;override;
          function pass_1: tnode; override;
          function pass_typecheck: tnode; override;
          function docompare(p: tnode): boolean; override;
          destructor destroy; override;
          procedure printnodedata(var t:text);override;
         protected
          release_to_normal : boolean;
        private
          tempidx : longint;
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
       function  internalstatements(var laststatement:tstatementnode):tblocknode;
       function  laststatement(block:tblocknode):tstatementnode;
       procedure addstatement(var laststatement:tstatementnode;n:tnode);


implementation

    uses
      cutils,
      verbose,globals,systems,
      symconst,symdef,defutil,defcmp,
      pass_1,
      nld,ncal,nflw,
      procinfo
      ;


{*****************************************************************************
                                     Helpers
*****************************************************************************}

    function internalstatements(var laststatement:tstatementnode):tblocknode;
      begin
        { create dummy initial statement }
        laststatement := cstatementnode.create(cnothingnode.create,nil);
        internalstatements := cblocknode.create(laststatement);
      end;


    function laststatement(block:tblocknode):tstatementnode;
      begin
        result:=tstatementnode(block.left);
        while assigned(result) and assigned(result.right) do
          result:=tstatementnode(result.right);
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


    function tnothingnode.pass_typecheck:tnode;
      begin
        result:=nil;
        resultdef:=voidtype;
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


    function terrornode.pass_typecheck:tnode;
      begin
         result:=nil;
         include(flags,nf_error);
         codegenerror:=true;
         resultdef:=generrordef;
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

    function tstatementnode.pass_typecheck:tnode;
      begin
         result:=nil;
         resultdef:=voidtype;

         { left is the statement itself calln assignn or a complex one }
         typecheckpass(left);
         if (not (cs_extsyntax in current_settings.moduleswitches)) and
            assigned(left.resultdef) and
            not((left.nodetype=calln) and
                { don't complain when funcretrefnode is set, because then the
                  value is already used. And also not for constructors }
                (assigned(tcallnode(left).funcretnode) or
                 (tcallnode(left).procdefinition.proctypeoption=potype_constructor))) and
            not(is_void(left.resultdef)) then
           CGMessage(parser_e_illegal_expression);
         if codegenerror then
           exit;

         { right is the next statement in the list }
         if assigned(right) then
           typecheckpass(right);
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
         registersint:=left.registersint;
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

    constructor tblocknode.create(l : tnode);

      begin
         inherited create(blockn,l);
      end;

    destructor tblocknode.destroy;

      var
        hp, next: tstatementnode;
      begin
        hp := tstatementnode(left);
        left := nil;
        while assigned(hp) do
          begin
            next := tstatementnode(hp.right);
            hp.right := nil;
            hp.free;
            hp := next;
          end;
        inherited destroy;
      end;

    function tblocknode.pass_typecheck:tnode;
      var
         hp : tstatementnode;
      begin
         result:=nil;
         resultdef:=voidtype;

         hp:=tstatementnode(left);
         while assigned(hp) do
           begin
              if assigned(hp.left) then
                begin
                   codegenerror:=false;
                   typecheckpass(hp.left);
                   if not(codegenerror) and
                      not(cs_extsyntax in current_settings.moduleswitches) and
                      (hp.left.nodetype=calln) and
                      not(is_void(hp.left.resultdef)) and
                      not(cnf_return_value_used in tcallnode(hp.left).callnodeflags) and
                      not((tcallnode(hp.left).procdefinition.proctypeoption=potype_constructor) and
                          assigned(tprocdef(tcallnode(hp.left).procdefinition)._class) and
                          is_object(tprocdef(tcallnode(hp.left).procdefinition)._class)) then
                     CGMessagePos(hp.left.fileinfo,parser_e_illegal_expression);
                   { the resultdef of the block is the last type that is
                     returned. Normally this is a voidtype. But when the
                     compiler inserts a block of multiple statements then the
                     last entry can return a value }
                   resultdef:=hp.left.resultdef;
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
              if assigned(hp.left) then
                begin
                   codegenerror:=false;
                   firstpass(hp.left);

                   hp.expectloc:=hp.left.expectloc;
                   hp.registersint:=hp.left.registersint;
                   hp.registersfpu:=hp.left.registersfpu;
{$ifdef SUPPORT_MMX}
                   hp.registersmmx:=hp.left.registersmmx;
{$endif SUPPORT_MMX}
                end
              else
                hp.registersint:=0;

              if hp.registersint>registersint then
                registersint:=hp.registersint;
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

    constructor tasmnode.create(p : TAsmList);
      begin
        inherited create(asmn);
        p_asm:=p;
        currenttai:=nil;
        used_regs_int:=[];
        used_regs_fpu:=[];
      end;


    constructor tasmnode.create_get_position;
      begin
        inherited create(asmn);
        p_asm:=nil;
        include(flags,nf_get_asm_position);
        currenttai:=nil;
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
        if not(nf_get_asm_position in flags) then
          begin
            p_asm:=TAsmList.create;
            repeat
              hp:=ppuloadai(ppufile);
              if hp=nil then
                break;
              p_asm.concat(hp);
            until false;
          end
        else
          p_asm:=nil;
        currenttai:=nil;
      end;


    procedure tasmnode.ppuwrite(ppufile:tcompilerppufile);
      var
        hp : tai;
      begin
        inherited ppuwrite(ppufile);
{$warning FIXME Add saving of register sets}
        if not(nf_get_asm_position in flags) then
          begin
            hp:=tai(p_asm.first);
            while assigned(hp) do
             begin
               ppuwriteai(ppufile,hp);
               hp:=tai(hp.next);
             end;
            { end is marked by a nil }
            ppuwriteai(ppufile,nil);
          end;
      end;


    procedure tasmnode.buildderefimpl;
      var
        hp : tai;
      begin
        inherited buildderefimpl;
        if not(nf_get_asm_position in flags) then
          begin
            hp:=tai(p_asm.first);
            while assigned(hp) do
             begin
               hp.buildderefimpl;
               hp:=tai(hp.next);
             end;
          end;
      end;


    procedure tasmnode.derefimpl;
      var
        hp : tai;
      begin
        inherited derefimpl;
        if not(nf_get_asm_position in flags) then
          begin
            hp:=tai(p_asm.first);
            while assigned(hp) do
             begin
               hp.derefimpl;
               hp:=tai(hp.next);
             end;
          end;
      end;


    function tasmnode.dogetcopy: tnode;
      var
        n: tasmnode;
      begin
        n := tasmnode(inherited dogetcopy);
        if assigned(p_asm) then
          begin
            n.p_asm:=TAsmList.create;
            n.p_asm.concatlistcopy(p_asm);
          end
        else n.p_asm := nil;
        n.currenttai:=currenttai;
        result:=n;
      end;


    function tasmnode.pass_typecheck:tnode;
      begin
        result:=nil;
        resultdef:=voidtype;
        if not(nf_get_asm_position in flags) then
          include(current_procinfo.flags,pi_has_assembler_block);
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

    constructor ttempcreatenode.create(_typedef:tdef; _size: aint; _temptype: ttemptype;allowreg:boolean);
      begin
        inherited create(tempcreaten);
        size := _size;
        new(tempinfo);
        fillchar(tempinfo^,sizeof(tempinfo^),0);
        tempinfo^.typedef := _typedef;
        tempinfo^.temptype := _temptype;
        tempinfo^.owner:=self;
        tempinfo^.may_be_in_reg:=
          allowreg and
          { temp must fit a single register }
          (tstoreddef(_typedef).is_fpuregable or
           (tstoreddef(_typedef).is_intregable and
            (_size<=TCGSize2Size[OS_64]))) and
          { size of register operations must be known }
          (def_cgsize(_typedef)<>OS_NO) and
          { no init/final needed }
          not (_typedef.needs_inittable) and
          ((_typedef.typ <> pointerdef) or
           (not tpointerdef(_typedef).pointeddef.needs_inittable));
      end;

    function ttempcreatenode.dogetcopy: tnode;
      var
        n: ttempcreatenode;
      begin
        n := ttempcreatenode(inherited dogetcopy);
        n.size := size;

        new(n.tempinfo);
        fillchar(n.tempinfo^,sizeof(n.tempinfo^),0);
        n.tempinfo^.owner:=n;
        n.tempinfo^.typedef := tempinfo^.typedef;
        n.tempinfo^.temptype := tempinfo^.temptype;

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


    constructor ttempcreatenode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);

        size:=ppufile.getlongint;
        new(tempinfo);
        fillchar(tempinfo^,sizeof(tempinfo^),0);
        tempinfo^.may_be_in_reg:=boolean(ppufile.getbyte);
        ppufile.getderef(tempinfo^.typedefderef);
        tempinfo^.temptype := ttemptype(ppufile.getbyte);
        tempinfo^.owner:=self;
      end;


    procedure ttempcreatenode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putlongint(size);
        ppufile.putbyte(byte(tempinfo^.may_be_in_reg));
        ppufile.putderef(tempinfo^.typedefderef);
        ppufile.putbyte(byte(tempinfo^.temptype));
      end;


    procedure ttempcreatenode.buildderefimpl;
      begin
        tempinfo^.typedefderef.build(tempinfo^.typedef);
      end;


    procedure ttempcreatenode.derefimpl;
      begin
        tempinfo^.typedef:=tdef(tempinfo^.typedefderef.resolve);
      end;


    function ttempcreatenode.pass_1 : tnode;
      begin
         result := nil;
         expectloc:=LOC_VOID;
         if (tempinfo^.typedef.needs_inittable) then
           include(current_procinfo.flags,pi_needs_implicit_finally);
      end;


    function ttempcreatenode.pass_typecheck: tnode;
      begin
        result := nil;
        { a tempcreatenode doesn't have a resultdef, only temprefnodes do }
        resultdef := voidtype;
      end;


    function ttempcreatenode.docompare(p: tnode): boolean;
      begin
        result :=
          inherited docompare(p) and
          (ttempcreatenode(p).size = size) and
          (ttempcreatenode(p).tempinfo^.may_be_in_reg = tempinfo^.may_be_in_reg) and
          equal_defs(ttempcreatenode(p).tempinfo^.typedef,tempinfo^.typedef);
      end;


    procedure ttempcreatenode.printnodedata(var t:text);
      begin
        inherited printnodedata(t);
        writeln(t,printnodeindention,'size = ',size,', temptypedef = "',tempinfo^.typedef.GetTypeName,'", tempinfo = $',hexstr(ptruint(tempinfo),sizeof(ptruint)*2));
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


    function ttemprefnode.dogetcopy: tnode;
      var
        n: ttemprefnode;
      begin
        n := ttemprefnode(inherited dogetcopy);
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

        if not assigned(n.tempinfo) then
          internalerror(2005071901);

        result := n;
      end;


    constructor ttemprefnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        tempidx:=ppufile.getlongint;
        offset:=ppufile.getlongint;
      end;


    procedure ttemprefnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putlongint(tempinfo^.owner.ppuidx);
        ppufile.putlongint(offset);
      end;


    procedure ttemprefnode.derefnode;
      var
        temp : ttempcreatenode;
      begin
        temp:=ttempcreatenode(nodeppuidxget(tempidx));
        if temp.nodetype<>tempcreaten then
          internalerror(200311075);
        tempinfo:=temp.tempinfo;
      end;


    function ttemprefnode.pass_1 : tnode;
      begin
        expectloc := LOC_REFERENCE;
        if not tempinfo^.typedef.needs_inittable and
           tempinfo^.may_be_in_reg then
          begin
            if tempinfo^.typedef.typ=floatdef then
              begin
                if (tempinfo^.temptype = tt_persistent) then
                  expectloc := LOC_CFPUREGISTER
                else
                  expectloc := LOC_FPUREGISTER;
              end
            else
              begin
                if (tempinfo^.temptype = tt_persistent) then
                  expectloc := LOC_CREGISTER
                else
                  expectloc := LOC_REGISTER;
              end;
          end;
        result := nil;
      end;

    function ttemprefnode.pass_typecheck: tnode;
      begin
        { check if the temp is already resultdef passed }
        if not assigned(tempinfo^.typedef) then
          internalerror(200108233);
        result := nil;
        resultdef := tempinfo^.typedef;
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

    procedure ttemprefnode.printnodedata(var t:text);
      begin
        inherited printnodedata(t);
        writeln(t,printnodeindention,'temptypedef = "',tempinfo^.typedef.GetTypeName,'", tempinfo = $',hexstr(ptruint(tempinfo),sizeof(ptruint)*2));
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
        if tempinfo^.temptype <> tt_persistent then
          internalerror(200204211);
      end;


    function ttempdeletenode.dogetcopy: tnode;
      var
        n: ttempdeletenode;
      begin
        n := ttempdeletenode(inherited dogetcopy);
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

    constructor ttempdeletenode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        tempidx:=ppufile.getlongint;
        release_to_normal:=(ppufile.getbyte<>0);
      end;


    procedure ttempdeletenode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putlongint(tempinfo^.owner.ppuidx);
        ppufile.putbyte(byte(release_to_normal));
      end;


    procedure ttempdeletenode.derefnode;
      var
        temp : ttempcreatenode;
      begin
        temp:=ttempcreatenode(nodeppuidxget(tempidx));
        if temp.nodetype<>tempcreaten then
          internalerror(200311075);
        tempinfo:=temp.tempinfo;
      end;


    function ttempdeletenode.pass_1 : tnode;
      begin
         expectloc:=LOC_VOID;
         result := nil;
      end;

    function ttempdeletenode.pass_typecheck: tnode;
      begin
        result := nil;
        resultdef := voidtype;
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

    procedure ttempdeletenode.printnodedata(var t:text);
      begin
        inherited printnodedata(t);
        writeln(t,printnodeindention,'release_to_normal: ',release_to_normal,', temptypedef = "',tempinfo^.typedef.GetTypeName,'", tempinfo = $',hexstr(ptruint(tempinfo),sizeof(ptruint)*2));
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
