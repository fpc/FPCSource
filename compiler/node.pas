{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    uses
       cclasses,
       globtype,globals,
       cpubase,
       aasmbase,
       symtype,symppu;

    type
       pconstset = ^tconstset;
{$ifdef oldset}
       tconstset = array[0..31] of byte;
       pconst32bitset = ^tconst32bitset;
       tconst32bitset = array[0..7] of longint;
{$else}
       tconstset = set of 0..255;
{$endif}

       tnodetype = (
          addn,             {Represents the + operator}
          muln,             {Represents the * operator}
          subn,             {Represents the - operator}
          divn,             {Represents the div operator}
          symdifn,          {Represents the >< operator}
          modn,             {Represents the mod operator}
          assignn,          {Represents an assignment}
          loadn,            {Represents the use of a variabele}
          rangen,           {Represents a range (i.e. 0..9)}
          ltn,              {Represents the < operator}
          lten,             {Represents the <= operator}
          gtn,              {Represents the > operator}
          gten,             {Represents the >= operator}
          equaln,           {Represents the = operator}
          unequaln,         {Represents the <> operator}
          inn,              {Represents the in operator}
          orn,              {Represents the or operator}
          xorn,             {Represents the xor operator}
          shrn,             {Represents the shr operator}
          shln,             {Represents the shl operator}
          slashn,           {Represents the / operator}
          andn,             {Represents the and operator}
          subscriptn,       {Field in a record/object}
          derefn,           {Dereferences a pointer}
          addrn,            {Represents the @ operator}
          doubleaddrn,      {Represents the @@ operator}
          ordconstn,        {Represents an ordinal value}
          typeconvn,        {Represents type-conversion/typecast}
          calln,            {Represents a call node}
          callparan,        {Represents a parameter}
          realconstn,       {Represents a real value}
          unaryminusn,      {Represents a sign change (i.e. -2)}
          asmn,             {Represents an assembler node }
          vecn,             {Represents array indexing}
          pointerconstn,    {Represents a pointer constant}
          stringconstn,     {Represents a string constant}
          funcretn,         {Represents the function result var}
          selfn,            {Represents the self parameter}
          notn,             {Represents the not operator}
          inlinen,          {Internal procedures (i.e. writeln)}
          niln,             {Represents the nil pointer}
          errorn,           {This part of the tree could not be
                             parsed because of a compiler error}
          typen,            {A type name. Used for i.e. typeof(obj)}
          hnewn,            {The new operation, constructor call}
          hdisposen,        {The dispose operation with destructor call}
          setelementn,      {A set element(s) (i.e. [a,b] and also [a..b])}
          setconstn,        {A set constant (i.e. [1,2])}
          blockn,           {A block of statements}
          statementn,       {One statement in a block of nodes}
          ifn,              {An if statement}
          breakn,           {A break statement}
          continuen,        {A continue statement}
          whilerepeatn,     {A while or repeat statement}
          forn,             {A for loop}
          exitn,            {An exit statement}
          withn,            {A with statement}
          casen,            {A case statement}
          labeln,           {A label}
          goton,            {A goto statement}
          tryexceptn,       {A try except block}
          raisen,           {A raise statement}
          tryfinallyn,      {A try finally statement}
          onn,              {For an on statement in exception code}
          isn,              {Represents the is operator}
          asn,              {Represents the as typecast}
          caretn,           {Represents the ^ operator}
          failn,            {Represents the fail statement}
          starstarn,        {Represents the ** operator exponentiation }
          procinlinen,      {Procedures that can be inlined }
          arrayconstructorn, {Construction node for [...] parsing}
          arrayconstructorrangen, {Range element to allow sets in array construction tree}
          tempcreaten,      { for temps in the result/firstpass }
          temprefn,         { references to temps }
          tempdeleten,      { for temps in the result/firstpass }
          addoptn,          { added for optimizations where we cannot suppress }
          nothingn,         {NOP, Do nothing}
          loadvmtn,         {Load the address of the VMT of a class/object}
          guidconstn,       {A GUID COM Interface constant }
          rttin             {Rtti information so they can be accessed in result/firstpass}
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
          'unaryminusn',
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
          'setelementn',
          'setconstn',
          'blockn',
          'statementn',
          'ifn',
          'breakn',
          'continuen',
          'whilerepeatn',
          'forn',
          'exitn',
          'withn',
          'casen',
          'labeln',
          'goton',
          'tryexceptn',
          'raisen',
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
          'tempcreaten',
          'temprefn',
          'tempdeleten',
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
         nf_backward,   { set if it is a for ... downto ... do loop }
         nf_varstate,   { do we need to parse childs to set var state }
         nf_testatbegin,{ Do a test at the begin of the loop?}
         nf_checknegate,{ Negate the loop test?}

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
          constructor create(tt : tnodetype);
          { this constructor is only for creating copies of class }
          { the fields are copied by getcopy                      }
          constructor createforcopy;
          constructor load(tt : tnodetype;ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure write(ppufile:tcompilerppufile);virtual;

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
{$ifdef state_tracking}
      { Does optimizations by keeping track of the variable states
        in a procedure }
      function track_state_pass(exec_known:boolean):boolean;virtual;
{$endif}
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
          procedure dowrite;
          procedure dowritenodetype;virtual;
          procedure _dowrite;virtual;
{$endif EXTDEBUG}
          procedure concattolist(l : tlinkedlist);virtual;
          function ischild(p : tnode) : boolean;virtual;
          procedure set_file_line(from : tnode);
          procedure set_tree_filepos(const filepos : tfileposinfo);
       end;

       tnodeclass = class of tnode;

       tnodeclassarray = array[tnodetype] of tnodeclass;

       { this node is the anchestor for all nodes with at least   }
       { one child, you have to use it if you want to use         }
       { true- and falselabel                                     }
       punarynode = ^tunarynode;
       tunarynode = class(tnode)
          left : tnode;
          constructor create(tt : tnodetype;l : tnode);
          constructor load(tt:tnodetype;ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure concattolist(l : tlinkedlist);override;
          function ischild(p : tnode) : boolean;override;
          function docompare(p : tnode) : boolean;override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          procedure left_max;
{$ifdef extdebug}
          procedure _dowrite;override;
{$endif extdebug}
       end;

       pbinarynode = ^tbinarynode;
       tbinarynode = class(tunarynode)
          right : tnode;
          constructor create(tt : tnodetype;l,r : tnode);
          constructor load(tt:tnodetype;ppufile:tcompilerppufile);
          destructor destroy;override;
          procedure write(ppufile:tcompilerppufile);override;
          procedure concattolist(l : tlinkedlist);override;
          function ischild(p : tnode) : boolean;override;
          function docompare(p : tnode) : boolean;override;
          procedure swapleftright;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          procedure left_right_max;
{$ifdef extdebug}
          procedure _dowrite;override;
{$endif extdebug}
       end;

       tbinopnode = class(tbinarynode)
          constructor create(tt : tnodetype;l,r : tnode);virtual;
          function docompare(p : tnode) : boolean;override;
       end;

{$ifdef tempregdebug}
    type
      pptree = ^tnode;
    var
      curptree: pptree;
{$endif tempregdebug}

    var
      nodeclass : tnodeclassarray;
{$ifdef EXTDEBUG}
      writenodeindention : string;
{$endif EXTDEBUG}


    function ppuloadnode(ppufile:tcompilerppufile):tnode;
{$ifdef EXTDEBUG}
    procedure writenode(t:tnode);
{$endif EXTDEBUG}


implementation

    uses
       cutils,verbose;

{****************************************************************************
                                 Helpers
 ****************************************************************************}

    function ppuloadnode(ppufile:tcompilerppufile):tnode;
      var
        b : byte;
        t : tnodetype;
      begin
        { marker }
        b:=ppufile.getbyte;
        if b<>255 then
          internalerror(200208151);
        { load nodetype }
        t:=tnodetype(ppufile.getbyte);
        if t>high(tnodetype) then
          internalerror(200208152);
        if not assigned(nodeclass[t]) then
          internalerror(200208153);
        { generate node of the correct class }
        ppuloadnode:=nodeclass[t].load(t,ppufile);
      end;


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

    constructor tnode.load(tt : tnodetype;ppufile:tcompilerppufile);

      begin
        { tnode fields }
        blocktype:=tblock_type(ppufile.getbyte);
        ppufile.getposinfo(fileinfo);
        ppufile.getsmallset(localswitches);
        ppufile.gettype(resulttype);
        ppufile.getsmallset(flags);
        { updated by firstpass }
        location.loc:=LOC_INVALID;
        registers32:=0;
        registersfpu:=0;
{$ifdef SUPPORT_MMX}
        registersmmx:=0;
{$endif SUPPORT_MMX}
{$ifdef EXTDEBUG}
        maxfirstpasscount:=0;
        firstpasscount:=0;
{$endif EXTDEBUG}
      end;


    procedure tnode.write(ppufile:tcompilerppufile);
      begin
        { marker, read by ppuloadnode }
        ppufile.putbyte($ff);
        { type, read by ppuloadnode }
        ppufile.putbyte(byte(nodetype));
        { tnode fields }
        ppufile.putbyte(byte(block_type));
        ppufile.putposinfo(aktfilepos);
        ppufile.putsmallset(localswitches);
        ppufile.puttype(resulttype);
        ppufile.putsmallset(flags);
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
      end;

    function tnode.ischild(p : tnode) : boolean;

      begin
         ischild:=false;
      end;

{$ifdef EXTDEBUG}
    procedure tnode._dowrite;
      begin
        dowritenodetype;
        system.write(',resulttype = "',resulttype.def.gettypename,'"');
        system.write(',location.loc = ',ord(location.loc));
        system.write(',registersfpu = ',registersfpu);
      end;

    procedure tnode.dowritenodetype;
      begin
          system.write(nodetype2str[nodetype]);
      end;

    procedure tnode.dowrite;
      begin
         system.write(writenodeindention,'(');
         writenodeindention:=writenodeindention+'    ';
         _dowrite;
         writeln(writenodeindention);
         delete(writenodeindention,1,4);
         system.write(writenodeindention,')');
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

{$ifdef state_tracking}
    function Tnode.track_state_pass(exec_known:boolean):boolean;

    begin
    track_state_pass:=false;
    end;
{$endif state_tracking}

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
{         p.list:=list; }
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


    constructor tunarynode.load(tt : tnodetype;ppufile:tcompilerppufile);
      begin
        inherited load(tt,ppufile);
        left:=ppuloadnode(ppufile);
      end;


    destructor tunarynode.destroy;
      begin
        left.free;
        inherited destroy;
      end;


    procedure tunarynode.write(ppufile:tcompilerppufile);
      begin
        inherited write(ppufile);
        left.write(ppufile);
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
    procedure tunarynode._dowrite;

      begin
         inherited _dowrite;
         writeln(',');
         writenode(left);
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


    constructor tbinarynode.load(tt : tnodetype;ppufile:tcompilerppufile);
      begin
        inherited load(tt,ppufile);
        right:=ppuloadnode(ppufile);
      end;


    destructor tbinarynode.destroy;
      begin
        right.free;
        inherited destroy;
      end;


    procedure tbinarynode.write(ppufile:tcompilerppufile);
      begin
        inherited write(ppufile);
        right.write(ppufile);
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
    procedure tbinarynode._dowrite;

      begin
         inherited _dowrite;
         writeln(',');
         writenode(right);
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

end.
{
  $Log$
  Revision 1.36  2002-08-17 22:09:46  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.35  2002/08/15 19:10:35  peter
    * first things tai,tnode storing in ppu

  Revision 1.34  2002/08/09 19:15:41  carl
     - removed newcg define

  Revision 1.33  2002/07/23 12:34:30  daniel
  * Readded old set code. To use it define 'oldset'. Activated by default
    for ppc.

  Revision 1.32  2002/07/22 11:48:04  daniel
  * Sets are now internally sets.

  Revision 1.31  2002/07/21 06:58:49  daniel
  * Changed booleans into flags

  Revision 1.30  2002/07/19 11:41:36  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.29  2002/07/14 18:00:44  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

  Revision 1.28  2002/07/01 18:46:24  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.27  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.26  2002/05/16 19:46:39  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.24  2002/04/21 19:02:04  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.23  2002/04/06 18:13:01  jonas
    * several powerpc-related additions and fixes

  Revision 1.22  2002/03/31 20:26:35  jonas
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

  Revision 1.21  2002/01/19 11:52:32  peter
    * dynarr:=nil support added

}
