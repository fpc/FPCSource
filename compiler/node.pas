{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

    Basic node handling

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
unit node;

{$i defines.inc}

interface

    uses
       cclasses,
       globtype,globals,
       cpubase,
       aasm,
       symtype;

    type
       pconstset = ^tconstset;
       tconstset = array[0..31] of byte;
       pconst32bitset = ^tconst32bitset;
       tconst32bitset = array[0..7] of longint;

       tnodetype = (
          addn,     {Represents the + operator.}
          muln,     {Represents the * operator.}
          subn,     {Represents the - operator.}
          divn,     {Represents the div operator.}
          symdifn,       {Represents the >< operator.}
          modn,     {Represents the mod operator.}
          assignn,       {Represents an assignment.}
          loadn,           {Represents the use of a variabele.}
          rangen,         {Represents a range (i.e. 0..9).}
          ltn,       {Represents the < operator.}
          lten,     {Represents the <= operator.}
          gtn,       {Represents the > operator.}
          gten,     {Represents the >= operator.}
          equaln,         {Represents the = operator.}
          unequaln,     {Represents the <> operator.}
          inn,       {Represents the in operator.}
          orn,       {Represents the or operator.}
          xorn,     {Represents the xor operator.}
          shrn,     {Represents the shr operator.}
          shln,     {Represents the shl operator.}
          slashn,         {Represents the / operator.}
          andn,     {Represents the and operator.}
          subscriptn,      {??? Field in a record/object?}
          derefn,         {Dereferences a pointer.}
          addrn,           {Represents the @ operator.}
          doubleaddrn,     {Represents the @@ operator.}
          ordconstn,       {Represents an ordinal value.}
          typeconvn,       {Represents type-conversion/typecast.}
          calln,           {Represents a call node.}
          callparan,       {Represents a parameter.}
          realconstn,      {Represents a real value.}
          unaryminusn,     {Represents a sign change (i.e. -2).}
          asmn,     {Represents an assembler node }
          vecn,     {Represents array indexing.}
          pointerconstn,
          stringconstn,    {Represents a string constant.}
          funcretn,     {Represents the function result var.}
          selfn,           {Represents the self parameter.}
          notn,     {Represents the not operator.}
          inlinen,       {Internal procedures (i.e. writeln).}
          niln,     {Represents the nil pointer.}
          errorn,         {This part of the tree could not be
                            parsed because of a compiler error.}
          typen,           {A type name. Used for i.e. typeof(obj).}
          hnewn,           {The new operation, constructor call.}
          hdisposen,       {The dispose operation with destructor call.}
          newn,     {The new operation, constructor call.}
          simpledisposen,  {The dispose operation.}
          setelementn,     {A set element(s) (i.e. [a,b] and also [a..b]).}
          setconstn,       {A set constant (i.e. [1,2]).}
          blockn,         {A block of statements.}
          statementn,      {One statement in a block of nodes.}
          loopn,           { used in genloopnode, must be converted }
          ifn,       {An if statement.}
          breakn,         {A break statement.}
          continuen,       {A continue statement.}
          repeatn,       {A repeat until block.}
          whilen,         {A while do statement.}
          forn,     {A for loop.}
          exitn,           {An exit statement.}
          withn,           {A with statement.}
          casen,           {A case statement.}
          labeln,         {A label.}
          goton,           {A goto statement.}
          simplenewn,      {The new operation.}
          tryexceptn,      {A try except block.}
          raisen,         {A raise statement.}
          switchesn,       {??? Currently unused...}
          tryfinallyn,     {A try finally statement.}
          onn,       { for an on statement in exception code }
          isn,       {Represents the is operator.}
          asn,       {Represents the as typecast.}
          caretn,         {Represents the ^ operator.}
          failn,           {Represents the fail statement.}
          starstarn,       {Represents the ** operator exponentiation }
          procinlinen,     {Procedures that can be inlined }
          arrayconstructorn, {Construction node for [...] parsing}
          arrayconstructorrangen, {Range element to allow sets in array construction tree}
          tempn,     { for temps in the result/firstpass }
          temprefn,  { references to temps }
          { added for optimizations where we cannot suppress }
          addoptn,
          nothingn,
          loadvmtn,
          guidconstn,
          rttin       { rtti information so they can be accessed in result/firstpass }
       );

      const
        nodetype2str : array[tnodetype] of string[20] = (
          'addn',
          'muln',
          'subn',
          'divn',
          'symdifn',
          'modn',
          'assignn',
          'loadn',
          'rangen',
          'ltn',
          'lten',
          'gtn',
          'gten',
          'equaln',
          'unequaln',
          'inn',
          'orn',
          'xorn',
          'shrn',
          'shln',
          'slashn',
          'andn',
          'subscriptn',
          'derefn',
          'addrn',
          'doubleaddrn',
          'ordconstn',
          'typeconvn',
          'calln',
          'callparan',
          'realconstn',
          'umminusn',
          'asmn',
          'vecn',
          'pointerconstn',
          'stringconstn',
          'funcretn',
          'selfn',
          'notn',
          'inlinen',
          'niln',
          'errorn',
          'typen',
          'hnewn',
          'hdisposen',
          'newn',
          'simpledisposen',
          'setelementn',
          'setconstn',
          'blockn',
          'statementn',
          'loopn',
          'ifn',
          'breakn',
          'continuen',
          'repeatn',
          'whilen',
          'forn',
          'exitn',
          'withn',
          'casen',
          'labeln',
          'goton',
          'simplenewn',
          'tryexceptn',
          'raisen',
          'switchesn',
          'tryfinallyn',
          'onn',
          'isn',
          'asn',
          'caretn',
          'failn',
          'starstarn',
          'procinlinen',
          'arrayconstructn',
          'arrayconstructrangen',
          'tempn',
          'temprefn',
          'addoptn',
          'nothingn',
          'loadvmtn',
          'guidconstn',
          'rttin');

    type
       { all boolean field of ttree are now collected in flags }
       tnodeflags = (
         nf_needs_truefalselabel,
         nf_swapable,    { tbinop operands can be swaped }
         nf_swaped,      { tbinop operands are swaped    }
         nf_error,

         { flags used by tcallnode }
         nf_return_value_used,
         nf_static_call,

         { flags used by tcallparanode }
         nf_varargs_para,  { belongs this para to varargs }

         { flags used by loop nodes }
         nf_backward,  { set if it is a for ... downto ... do loop }
         nf_varstate,  { do we need to parse childs to set var state }

         { taddrnode }
         nf_procvarload,

         { tvecnode }
         nf_memindex,
         nf_memseg,
         nf_callunique,

         { twithnode }
         nf_islocal,

         { tloadnode }
         nf_absolute,
         nf_first,

         { tassignmentnode }
         nf_concat_string,

         { tfuncretnode }
         nf_is_first_funcret, { 20th }

         { tarrayconstructnode }
         nf_cargs,
         nf_cargswap,
         nf_forcevaria,
         nf_novariaallowed,

         { ttypeconvnode }
         nf_explizit,

         { tinlinenode }
         nf_inlineconst,

         { general }
         nf_isproperty,
         nf_varstateset,

         { tasmnode }
         nf_object_preserved,

         { taddnode }
         nf_use_strconcat
       );

       tnodeflagset = set of tnodeflags;

    const
       { contains the flags which must be equal for the equality }
       { of nodes                                                }
       flagsequal : tnodeflagset = [nf_error,nf_static_call,nf_backward];

    type
       tnodelist = class
       end;

       { later (for the newcg) tnode will inherit from tlinkedlist_item }
       tnode = class
       public
          { type of this node }
          nodetype : tnodetype;
          { type of the current code block, general/const/type }
          blocktype : tblock_type;
          { the location of the result of this node }
          location : tlocation;
          { the parent node of this is node    }
          { this field is set by concattolist  }
          parent : tnode;
          { there are some properties about the node stored }
          flags : tnodeflagset;
          { the number of registers needed to evalute the node }
          registers32,registersfpu : longint;  { must be longint !!!! }
{$ifdef SUPPORT_MMX}
          registersmmx,registerskni : longint;
{$endif SUPPORT_MMX}
          resulttype : ttype;
          fileinfo : tfileposinfo;
          localswitches : tlocalswitches;
{$ifdef extdebug}
          maxfirstpasscount,
          firstpasscount : longint;
{$endif extdebug}
          list : taasmoutput;
          constructor create(tt : tnodetype);
          { this constructor is only for creating copies of class }
          { the fields are copied by getcopy                      }
          constructor createforcopy;
          destructor destroy;override;

          { toggles the flag }
          procedure toggleflag(f : tnodeflags);

          { the 1.1 code generator may override pass_1 }
          { and it need not to implement det_* then    }
          { 1.1: pass_1 returns a value<>0 if the node has been transformed }
          { 2.0: runs det_resulttype and det_temp                           }
          function pass_1 : tnode;virtual;abstract;
          { dermines the resulttype of the node }
          function det_resulttype : tnode;virtual;abstract;
          { dermines the number of necessary temp. locations to evaluate
            the node }
          procedure det_temp;virtual;abstract;

          procedure pass_2;virtual;abstract;

          { comparing of nodes }
          function isequal(p : tnode) : boolean;
          { to implement comparisation, override this method }
          function docompare(p : tnode) : boolean;virtual;
          { gets a copy of the node }
          function getcopy : tnode;virtual;

          procedure insertintolist(l : tnodelist);virtual;
{$ifdef EXTDEBUG}
          { writes a node for debugging purpose, shouldn't be called }
          { direct, because there is no test for nil, use writenode  }
          { to write a complete tree                                 }
          procedure dowrite;virtual;
          procedure dowritenodetype;virtual;
{$endif EXTDEBUG}
          procedure concattolist(l : tlinkedlist);virtual;
          function ischild(p : tnode) : boolean;virtual;
          procedure set_file_line(from : tnode);
          procedure set_tree_filepos(const filepos : tfileposinfo);
       end;

       { this node is the anchestor for all nodes with at least   }
       { one child, you have to use it if you want to use         }
       { true- and falselabel                                     }
       tparentnode = class(tnode)
{$ifdef newcg}
          falselabel,truelabel : tasmlabel;
{$endif newcg}
       end;

       tnodeclass = class of tnode;

       punarynode = ^tunarynode;
       tunarynode = class(tparentnode)
          left : tnode;
          constructor create(tt : tnodetype;l : tnode);
          destructor destroy;override;
          procedure concattolist(l : tlinkedlist);override;
          function ischild(p : tnode) : boolean;override;
          function docompare(p : tnode) : boolean;override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          procedure left_max;
{$ifdef extdebug}
          procedure dowrite;override;
{$endif extdebug}
       end;

       pbinarynode = ^tbinarynode;
       tbinarynode = class(tunarynode)
          right : tnode;
          constructor create(tt : tnodetype;l,r : tnode);
          destructor destroy;override;
          procedure concattolist(l : tlinkedlist);override;
          function ischild(p : tnode) : boolean;override;
          function docompare(p : tnode) : boolean;override;
          procedure swapleftright;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          procedure left_right_max;
{$ifdef extdebug}
          procedure dowrite;override;
{$endif extdebug}
       end;

       pbinopnode = ^tbinopnode;
       tbinopnode = class(tbinarynode)
          constructor create(tt : tnodetype;l,r : tnode);virtual;
          function docompare(p : tnode) : boolean;override;
       end;

{$ifdef EXTDEBUG}
     var
       writenodeindention : string;

     procedure writenode(t:tnode);
{$endif EXTDEBUG}


implementation

    uses
       cutils;

{****************************************************************************
                                 TNODE
 ****************************************************************************}

    constructor tnode.create(tt : tnodetype);

      begin
         inherited create;
         nodetype:=tt;
         blocktype:=block_type;
         { this allows easier error tracing }
         location.loc:=LOC_INVALID;
         { save local info }
         fileinfo:=aktfilepos;
         localswitches:=aktlocalswitches;
         resulttype.reset;
         registers32:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
{$ifdef EXTDEBUG}
         maxfirstpasscount:=0;
         firstpasscount:=0;
{$endif EXTDEBUG}
         flags:=[];
      end;

    constructor tnode.createforcopy;

      begin
      end;

    procedure tnode.toggleflag(f : tnodeflags);

      begin
         if f in flags then
           exclude(flags,f)
         else
           include(flags,f);
      end;

    destructor tnode.destroy;

      begin
{$ifdef EXTDEBUG}
         if firstpasscount>maxfirstpasscount then
            maxfirstpasscount:=firstpasscount;
{$endif EXTDEBUG}
      end;


    procedure tnode.concattolist(l : tlinkedlist);

      begin
{$ifdef newcg}
         //!!!!!!! l^.concat(self);
         {$warning fixme}
{$endif newcg}
      end;

    function tnode.ischild(p : tnode) : boolean;

      begin
         ischild:=false;
      end;

{$ifdef EXTDEBUG}
    procedure tnode.dowrite;
      begin
        dowritenodetype;
      end;

    procedure tnode.dowritenodetype;
      begin
         write(writenodeindention,'(',nodetype2str[nodetype]);
      end;
{$endif EXTDEBUG}

    function tnode.isequal(p : tnode) : boolean;

      begin
         isequal:=
           (not assigned(self) and not assigned(p)) or
           (assigned(self) and assigned(p) and
            { optimized subclasses have the same nodetype as their        }
            { superclass (for compatibility), so also check the classtype (JM) }
            (p.classtype=classtype) and
            (p.nodetype=nodetype) and
            (flags*flagsequal=p.flags*flagsequal) and
            docompare(p));
      end;

    function tnode.docompare(p : tnode) : boolean;

      begin
         docompare:=true;
      end;


    function tnode.getcopy : tnode;

      var
         p : tnode;

      begin
         { this is quite tricky because we need a node of the current }
         { node type and not one of tnode!                            }
         p:=tnodeclass(classtype).createforcopy;
         p.nodetype:=nodetype;
         p.location:=location;
         p.parent:=parent;
         p.flags:=flags;
         p.registers32:=registers32;
         p.registersfpu:=registersfpu;
{$ifdef SUPPORT_MMX}
         p.registersmmx:=registersmmx;
         p.registerskni:=registerskni;
{$endif SUPPORT_MMX}
         p.resulttype:=resulttype;
         p.fileinfo:=fileinfo;
         p.localswitches:=localswitches;
{$ifdef extdebug}
         p.firstpasscount:=firstpasscount;
{$endif extdebug}
         p.list:=list;
         getcopy:=p;
      end;

    procedure tnode.insertintolist(l : tnodelist);

      begin
      end;

    procedure tnode.set_file_line(from : tnode);

      begin
         if assigned(from) then
           fileinfo:=from.fileinfo;
      end;

    procedure tnode.set_tree_filepos(const filepos : tfileposinfo);

      begin
         fileinfo:=filepos;
      end;


{****************************************************************************
                                 TUNARYNODE
 ****************************************************************************}

    constructor tunarynode.create(tt : tnodetype;l : tnode);

      begin
         inherited create(tt);
         left:=l;
      end;

    destructor tunarynode.destroy;
      begin
        left.free;
        inherited destroy;
      end;

    function tunarynode.docompare(p : tnode) : boolean;

      begin
         docompare:=(inherited docompare(p) and
           ((left=nil) or left.isequal(tunarynode(p).left))
         );
      end;

    function tunarynode.getcopy : tnode;

      var
         p : tunarynode;

      begin
         p:=tunarynode(inherited getcopy);
         if assigned(left) then
           p.left:=left.getcopy
         else
           p.left:=nil;
         getcopy:=p;
      end;

    procedure tunarynode.insertintolist(l : tnodelist);

      begin
      end;

{$ifdef extdebug}
    procedure tunarynode.dowrite;

      begin
         inherited dowrite;
         writeln(',');
         writenodeindention:=writenodeindention+'    ';
         writenode(left);
         write(')');
         delete(writenodeindention,1,4);
      end;
{$endif}

    procedure tunarynode.left_max;

      begin
         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
      end;

    procedure tunarynode.concattolist(l : tlinkedlist);

      begin
         left.parent:=self;
         left.concattolist(l);
         inherited concattolist(l);
      end;

    function tunarynode.ischild(p : tnode) : boolean;

      begin
         ischild:=p=left;
      end;

{****************************************************************************
                            TBINARYNODE
 ****************************************************************************}

    constructor tbinarynode.create(tt : tnodetype;l,r : tnode);

      begin
         inherited create(tt,l);
         right:=r
      end;

    destructor tbinarynode.destroy;
      begin
        right.free;
        inherited destroy;
      end;

    procedure tbinarynode.concattolist(l : tlinkedlist);

      begin
         { we could change that depending on the number of }
         { required registers                              }
         left.parent:=self;
         left.concattolist(l);
         left.parent:=self;
         left.concattolist(l);
         inherited concattolist(l);
      end;

    function tbinarynode.ischild(p : tnode) : boolean;

      begin
         ischild:=(p=right);
      end;

    function tbinarynode.docompare(p : tnode) : boolean;

      begin
         docompare:=(inherited docompare(p) and
             ((right=nil) or right.isequal(tbinarynode(p).right))
         );
      end;

    function tbinarynode.getcopy : tnode;

      var
         p : tbinarynode;

      begin
         p:=tbinarynode(inherited getcopy);
         if assigned(right) then
           p.right:=right.getcopy
         else
           p.right:=nil;
         getcopy:=p;
      end;

    procedure tbinarynode.insertintolist(l : tnodelist);

      begin
      end;

    procedure tbinarynode.swapleftright;

      var
         swapp : tnode;

      begin
         swapp:=right;
         right:=left;
         left:=swapp;
         if nf_swaped in flags then
           exclude(flags,nf_swaped)
         else
           include(flags,nf_swaped);
      end;

    procedure tbinarynode.left_right_max;
      begin
        if assigned(left) then
         begin
           if assigned(right) then
            begin
              registers32:=max(left.registers32,right.registers32);
              registersfpu:=max(left.registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(left.registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
            end
           else
            begin
              registers32:=left.registers32;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
            end;
         end;
      end;

{$ifdef extdebug}
    procedure tbinarynode.dowrite;

      begin
         inherited dowrite;
         writeln(',');
         writenodeindention:=writenodeindention+'    ';
         writenode(right);
         write(')');
         delete(writenodeindention,1,4);
      end;
{$endif}

{****************************************************************************
                            TBINOPYNODE
 ****************************************************************************}

    constructor tbinopnode.create(tt : tnodetype;l,r : tnode);

      begin
         inherited create(tt,l,r);
      end;

    function tbinopnode.docompare(p : tnode) : boolean;

      begin
         docompare:=(inherited docompare(p)) or
           { if that's in the flags, is p then always a tbinopnode (?) (JM) }
           ((nf_swapable in flags) and
            left.isequal(tbinopnode(p).right) and
            right.isequal(tbinopnode(p).left));
      end;


{****************************************************************************
                                 WRITENODE
 ****************************************************************************}

{$ifdef EXTDEBUG}
     procedure writenode(t:tnode);
     begin
       if assigned(t) then
        t.dowrite
       else
        write(writenodeindention,'nil');
       if writenodeindention='' then
        writeln;
     end;
{$endif EXTDEBUG}


end.
{
  $Log$
  Revision 1.21  2002-01-19 11:52:32  peter
    * dynarr:=nil support added

  Revision 1.20  2001/10/20 19:28:38  peter
    * interface 2 guid support
    * guid constants support

  Revision 1.19  2001/08/23 14:28:36  jonas
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

  Revision 1.18  2001/07/30 20:59:27  peter
    * m68k updates from v10 merged

  Revision 1.17  2001/06/04 18:14:16  peter
    * store blocktype info in tnode

  Revision 1.16  2001/06/04 11:53:13  peter
    + varargs directive

  Revision 1.15  2001/04/13 01:22:10  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.14  2001/04/02 21:20:31  peter
    * resulttype rewrite

  Revision 1.13  2001/01/13 00:08:09  peter
    * added missing addoptn

  Revision 1.12  2001/01/01 11:38:45  peter
    * forgot to remove node.inc and nodeh.inc that were merged into node.pas
      already.

  Revision 1.11  2000/12/25 00:07:26  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.10  2000/11/29 00:30:34  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.9  2000/10/31 22:02:49  peter
    * symtable splitted, no real code changes

  Revision 1.8  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.7  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.6  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.5  2000/09/27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.4  2000/09/24 15:06:19  peter
    * use defines.inc

  Revision 1.3  2000/09/22 21:45:35  florian
    * some updates e.g. getcopy added

  Revision 1.2  2000/09/20 21:52:38  florian
    * removed a lot of errors

  Revision 1.1  2000/08/26 12:27:35  florian
    * initial release
}
