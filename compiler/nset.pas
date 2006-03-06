{
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
       cclasses,
       node,globtype,globals,
       aasmbase,aasmtai,symtype;

    type
       pcaselabel = ^tcaselabel;
       tcaselabel = record
          { range }
          _low,
          _high   : TConstExprInt;
          { unique blockid }
          blockid : longint;
          { left and right tree node }
          less,
          greater : pcaselabel;
       end;

       pcaseblock = ^tcaseblock;
       tcaseblock = record
          { label (only used in pass_2) }
          blocklabel : tasmlabel;
          { instructions }
          statement  : tnode;
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

       tcasenode = class(tunarynode)
          labels    : pcaselabel;
          blocks    : TFPList;
          elseblock : tnode;
          constructor create(l:tnode);virtual;
          destructor destroy;override;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function _getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          function det_resulttype:tnode;override;
          function pass_1 : tnode;override;
          function docompare(p: tnode): boolean; override;
          procedure addlabel(blockid:longint;l,h : TConstExprInt);
          procedure addblock(blockid:longint;instr:tnode);
          procedure addelseblock(instr:tnode);
       end;
       tcasenodeclass = class of tcasenode;

    var
       csetelementnode : tsetelementnodeclass;
       cinnode : tinnodeclass;
       crangenode : trangenodeclass;
       ccasenode : tcasenodeclass;

    { counts the labels }
    function case_count_labels(root : pcaselabel) : longint;
    { searches the highest label }
    function case_get_max(root : pcaselabel) : tconstexprint;
    { searches the lowest label }
    function case_get_min(root : pcaselabel) : tconstexprint;


implementation

    uses
      systems,
      verbose,
      symconst,symdef,symsym,symtable,defutil,defcmp,
      htypechk,pass_1,
      nbas,ncnv,ncon,nld,cgobj,cgbase;


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
         set_varstate(left,vs_read,[vsf_must_be_valid]);
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
         set_varstate(right,vs_read,[vsf_must_be_valid]);
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
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;

         if not assigned(left.resulttype.def) then
           internalerror(20021126);

         if (m_fpc in aktmodeswitches) then
           begin
             { insert a hint that a range check error might occur on non-byte
               elements with the in operator.
             }
             if  (
                   (left.resulttype.def.deftype = orddef) and not
                   (torddef(left.resulttype.def).typ in [s8bit,u8bit,uchar,bool8bit])
                 )
                or
                 (
                   (left.resulttype.def.deftype = enumdef) and
                   (tenumdef(left.resulttype.def).maxval > 255)
                 )
              then
                 CGMessage(type_h_in_range_check);

             { type conversion/check }
             if assigned(tsetdef(right.resulttype.def).elementtype.def) then
               inserttypeconv(left,tsetdef(right.resulttype.def).elementtype);
           end
         else
           begin
             { insert explicit type conversion/check }
             if assigned(tsetdef(right.resulttype.def).elementtype.def) then
               inserttypeconv_internal(left,tsetdef(right.resulttype.def).elementtype);
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
             if registersint < 3 then
               registersint := 3;
           end
         else
           begin
              { a smallset needs maybe an misc. register }
              if (left.nodetype<>ordconstn) and
                not(right.expectloc in [LOC_CREGISTER,LOC_REGISTER]) and
                (right.registersint<1) then
                inc(registersint);
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
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         set_varstate(right,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;
         { both types must be compatible }
         if compare_defs(left.resulttype.def,right.resulttype.def,left.nodetype)=te_incompatible then
           IncompatibleTypes(left.resulttype.def,right.resulttype.def);
         { Check if only when its a constant set }
         if (left.nodetype=ordconstn) and (right.nodetype=ordconstn) then
          begin
            { upper limit must be greater or equal than lower limit }
            if (tordconstnode(left).value>tordconstnode(right).value) and
               ((tordconstnode(left).value<0) or (tordconstnode(right).value>=0)) then
              CGMessage(parser_e_upper_lower_than_lower);
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

    function case_count_labels(root : pcaselabel) : longint;
      var
         _l : longint;

      procedure count(p : pcaselabel);
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


    function case_get_max(root : pcaselabel) : tconstexprint;
      var
         hp : pcaselabel;
      begin
         hp:=root;
         while assigned(hp^.greater) do
           hp:=hp^.greater;
         case_get_max:=hp^._high;
      end;


    function case_get_min(root : pcaselabel) : tconstexprint;
      var
         hp : pcaselabel;
      begin
         hp:=root;
         while assigned(hp^.less) do
           hp:=hp^.less;
         case_get_min:=hp^._low;
      end;

    procedure deletecaselabels(p : pcaselabel);

      begin
         if assigned(p^.greater) then
           deletecaselabels(p^.greater);
         if assigned(p^.less) then
           deletecaselabels(p^.less);
         dispose(p);
      end;

    function copycaselabel(p : pcaselabel) : pcaselabel;

      var
         n : pcaselabel;

      begin
         new(n);
         n^:=p^;
         if assigned(p^.greater) then
           n^.greater:=copycaselabel(p^.greater);
         if assigned(p^.less) then
           n^.less:=copycaselabel(p^.less);
         copycaselabel:=n;
      end;


    procedure ppuwritecaselabel(ppufile:tcompilerppufile;p : pcaselabel);
      var
        b : byte;
      begin
        ppufile.putexprint(p^._low);
        ppufile.putexprint(p^._high);
        ppufile.putlongint(p^.blockid);
        b:=0;
        if assigned(p^.greater) then
         b:=b or 1;
        if assigned(p^.less) then
         b:=b or 2;
        ppufile.putbyte(b);
        if assigned(p^.greater) then
          ppuwritecaselabel(ppufile,p^.greater);
        if assigned(p^.less) then
          ppuwritecaselabel(ppufile,p^.less);
      end;


    function ppuloadcaselabel(ppufile:tcompilerppufile):pcaselabel;
      var
        b : byte;
        p : pcaselabel;
      begin
        new(p);
        p^._low:=ppufile.getexprint;
        p^._high:=ppufile.getexprint;
        p^.blockid:=ppufile.getlongint;
        b:=ppufile.getbyte;
        if (b and 1)=1 then
         p^.greater:=ppuloadcaselabel(ppufile)
        else
         p^.greater:=nil;
        if (b and 2)=2 then
         p^.less:=ppuloadcaselabel(ppufile)
        else
         p^.less:=nil;
        ppuloadcaselabel:=p;
      end;


{*****************************************************************************
                              TCASENODE
*****************************************************************************}

    constructor tcasenode.create(l:tnode);
      begin
         inherited create(casen,l);
         labels:=nil;
         blocks:=TFPList.create;
         elseblock:=nil;
      end;


    destructor tcasenode.destroy;
      var
        i : longint;
        hp : pcaseblock;
      begin
         elseblock.free;
         deletecaselabels(labels);
         for i:=0 to blocks.count-1 do
           begin
             pcaseblock(blocks[i])^.statement.free;
             hp:=pcaseblock(blocks[i]);
             dispose(hp);
           end;
         inherited destroy;
      end;


    constructor tcasenode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      var
        cnt,i : longint;
      begin
        inherited ppuload(t,ppufile);
        elseblock:=ppuloadnode(ppufile);
        cnt:=ppufile.getlongint();
        blocks:=TFPList.create;
        for i:=0 to cnt-1 do
          addblock(i,ppuloadnode(ppufile));
        labels:=ppuloadcaselabel(ppufile);
      end;


    procedure tcasenode.ppuwrite(ppufile:tcompilerppufile);
      var
        i : longint;
      begin
        inherited ppuwrite(ppufile);
        ppuwritenode(ppufile,elseblock);
        ppufile.putlongint(blocks.count);
        for i:=0 to blocks.count-1 do
          ppuwritenode(ppufile,pcaseblock(blocks[i])^.statement);
        ppuwritecaselabel(ppufile,labels);
      end;


    procedure tcasenode.buildderefimpl;
      var
        i : integer;
      begin
        inherited buildderefimpl;
        if assigned(elseblock) then
          elseblock.buildderefimpl;
        for i:=0 to blocks.count-1 do
          pcaseblock(blocks[i])^.statement.buildderefimpl;
      end;


    procedure tcasenode.derefimpl;
      var
        i : integer;
      begin
        inherited derefimpl;
        if assigned(elseblock) then
          elseblock.derefimpl;
        for i:=0 to blocks.count-1 do
          pcaseblock(blocks[i])^.statement.derefimpl;
      end;


    function tcasenode.det_resulttype : tnode;
      begin
        result:=nil;
        resulttype:=voidtype;
      end;



    function tcasenode.pass_1 : tnode;
      var
         old_t_times : longint;
         hp : tnode;
         i  : integer;
      begin
         result:=nil;
         expectloc:=LOC_VOID;
         { evalutes the case expression }
         firstpass(left);
         set_varstate(left,vs_read,[vsf_must_be_valid]);
         if codegenerror then
           exit;
         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { walk through all instructions }

         { estimates the repeat of each instruction }
         old_t_times:=cg.t_times;
         if not(cs_littlesize in aktglobalswitches) then
           begin
              cg.t_times:=cg.t_times div case_count_labels(labels);
              if cg.t_times<1 then
                cg.t_times:=1;
           end;
         { first case }
         for i:=0 to blocks.count-1 do
           begin

              firstpass(pcaseblock(blocks[i])^.statement);

              { searchs max registers }
              hp:=pcaseblock(blocks[i])^.statement;
              if hp.registersint>registersint then
                registersint:=hp.registersint;
              if hp.registersfpu>registersfpu then
                registersfpu:=hp.registersfpu;
{$ifdef SUPPORT_MMX}
              if hp.registersmmx>registersmmx then
                registersmmx:=hp.registersmmx;
{$endif SUPPORT_MMX}
           end;

         { may be handle else tree }
         if assigned(elseblock) then
           begin
              firstpass(elseblock);
              if registersint<elseblock.registersint then
                registersint:=elseblock.registersint;
              if registersfpu<elseblock.registersfpu then
                registersfpu:=elseblock.registersfpu;
{$ifdef SUPPORT_MMX}
              if registersmmx<elseblock.registersmmx then
                registersmmx:=elseblock.registersmmx;
{$endif SUPPORT_MMX}
           end;
         cg.t_times:=old_t_times;

         { there is one register required for the case expression    }
         { for 64 bit ints we cheat: the high dword is stored in EDI }
         { so we don't need an extra register                        }
         if registersint<1 then
           registersint:=1;
      end;


    function tcasenode._getcopy : tnode;

      var
         n : tcasenode;
         i : longint;
      begin
         n:=tcasenode(inherited _getcopy);
         if assigned(elseblock) then
           n.elseblock:=elseblock._getcopy
         else
           n.elseblock:=nil;
         if assigned(labels) then
           n.labels:=copycaselabel(labels)
         else
           n.labels:=nil;
         if assigned(blocks) then
           begin
             n.blocks:=TFPList.create;
             for i:=0 to blocks.count-1 do
               begin
                 if not assigned(blocks[i]) then
                   internalerror(200411302);
                 n.addblock(i,pcaseblock(blocks[i])^.statement._getcopy);
               end;
           end
         else
           n.labels:=nil;
         _getcopy:=n;
      end;

    procedure tcasenode.insertintolist(l : tnodelist);

      begin
      end;

    function caselabelsequal(n1,n2: pcaselabel): boolean;
      begin
        result :=
          (not assigned(n1) and not assigned(n2)) or
          (assigned(n1) and assigned(n2) and
           (n1^._low = n2^._low) and
           (n1^._high = n2^._high) and
           { the rest of the fields don't matter for equality (JM) }
           caselabelsequal(n1^.less,n2^.less) and
           caselabelsequal(n1^.greater,n2^.greater))
      end;


    function caseblocksequal(b1,b2:TFPList): boolean;
      var
        i : longint;
      begin
        result:=false;
        if b1.count<>b2.count then
          exit;
        for i:=0 to b1.count-1 do
          begin
            if not pcaseblock(b1[i])^.statement.isequal(pcaseblock(b2[i])^.statement) then
              exit;
          end;
        result:=true;
      end;


    function tcasenode.docompare(p: tnode): boolean;
      begin
        result :=
          inherited docompare(p) and
          caselabelsequal(labels,tcasenode(p).labels) and
          caseblocksequal(blocks,tcasenode(p).blocks) and
          elseblock.isequal(tcasenode(p).elseblock);
      end;


    procedure tcasenode.addblock(blockid:longint;instr:tnode);
      var
        hcaseblock : pcaseblock;
      begin
        new(hcaseblock);
        fillchar(hcaseblock^,sizeof(hcaseblock^),0);
        hcaseblock^.statement:=instr;
        if blockid>=blocks.count then
          blocks.count:=blockid+1;
        blocks[blockid]:=hcaseblock;
      end;


    procedure tcasenode.addelseblock(instr:tnode);
      begin
        elseblock:=instr;
      end;


    procedure tcasenode.addlabel(blockid:longint;l,h : TConstExprInt);
      var
        hcaselabel : pcaselabel;

        function insertlabel(var p : pcaselabel):pcaselabel;
          begin
             if p=nil then
               begin
                 p:=hcaselabel;
                 result:=p;
               end
             else
              if (p^._low>hcaselabel^._low) and
                 (p^._low>hcaselabel^._high) then
                begin
                  if (hcaselabel^.blockid = p^.blockid) and
                     (p^._low = hcaselabel^._high + 1) then
                    begin
                      p^._low := hcaselabel^._low;
                      dispose(hcaselabel);
                      result:=p;
                    end
                  else
                    result:=insertlabel(p^.less)
                end
             else
               if (p^._high<hcaselabel^._low) and
                  (p^._high<hcaselabel^._high) then
                 begin
                    if (hcaselabel^.blockid = p^.blockid) and
                       (p^._high+1 = hcaselabel^._low) then
                      begin
                        p^._high := hcaselabel^._high;
                        dispose(hcaselabel);
                        result:=p;
                      end
                    else
                      result:=insertlabel(p^.greater);
                 end
             else
               Message(parser_e_double_caselabel);
          end;

      begin
        new(hcaselabel);
        fillchar(hcaselabel^,sizeof(tcaselabel),0);
        hcaselabel^.blockid:=blockid;
        hcaselabel^._low:=l;
        hcaselabel^._high:=h;
        insertlabel(labels);
      end;

begin
   csetelementnode:=tsetelementnode;
   cinnode:=tinnode;
   crangenode:=trangenode;
   ccasenode:=tcasenode;
end.
