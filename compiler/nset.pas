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
       node,globtype,globals,
       aasmbase,aasmtai,symtype;

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
          procedure buildderefimpl;override;
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
      systems,
      verbose,
      symconst,symdef,symsym,symtable,defutil,defcmp,
      htypechk,pass_1,
      nbas,ncnv,ncon,nld,cgobj,cgbase;

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
         set_varstate(left,vs_used,true);
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
         set_varstate(right,vs_used,true);
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
         set_varstate(left,vs_used,true);
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
               (left.resulttype.def.deftype = enumdef) and
               (tenumdef(left.resulttype.def).maxval > 255)
             )
          then
             CGMessage(type_h_in_range_check);

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
         set_varstate(left,vs_used,true);
         set_varstate(right,vs_used,true);
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


    procedure tcasenode.buildderefimpl;
      begin
        inherited buildderefimpl;
        if assigned(elseblock) then
          elseblock.buildderefimpl;
        {ppubuildderefimplcaserecord(nodes);}
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
         set_varstate(left,vs_used,true);
         if codegenerror then
           exit;
         registersint:=left.registersint;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}

         { walk through all instructions }

         {   estimates the repeat of each instruction }
         old_t_times:=cg.t_times;
         if not(cs_littlesize in aktglobalswitches) then
           begin
              cg.t_times:=cg.t_times div case_count_labels(nodes);
              if cg.t_times<1 then
                cg.t_times:=1;
           end;
         { first case }
         hp:=tstatementnode(right);
         while assigned(hp) do
           begin
              firstpass(hp.left);

              { searchs max registers }
              if hp.left.registersint>registersint then
                registersint:=hp.left.registersint;
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
         if registersint<1 then registersint:=1;
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
  Revision 1.55  2004-06-20 08:55:29  florian
    * logs truncated

  Revision 1.54  2004/06/16 20:07:09  florian
    * dwarf branch merged

  Revision 1.53.2.2  2004/05/01 16:02:09  peter
    * POINTER_SIZE replaced with sizeof(aint)
    * aint,aword,tconst*int moved to globtype

  Revision 1.53.2.1  2004/04/28 19:55:51  peter
    * new warning for ordinal-pointer when size is different
    * fixed some cg_e_ messages to the correct section type_e_ or parser_e_

  Revision 1.53  2004/03/18 16:19:03  peter
    * fixed operator overload allowing for pointer-string
    * replaced some type_e_mismatch with more informational messages

  Revision 1.52  2004/02/03 22:32:54  peter
    * renamed xNNbittype to xNNinttype
    * renamed registers32 to registersint
    * replace some s32bit,u32bit with torddef([su]inttype).def.typ

}
