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
       cpubase,cgbase,
       aasmbase,
       symtype,symppu;

    type
       pconstset = ^tconstset;
       tconstset = set of 0..255;

       tnodetype = (
          emptynode,        {No node (returns nil when loading from ppu)}
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
          notn,             {Represents the not operator}
          inlinen,          {Internal procedures (i.e. writeln)}
          niln,             {Represents the nil pointer}
          errorn,           {This part of the tree could not be
                             parsed because of a compiler error}
          typen,            {A type name. Used for i.e. typeof(obj)}
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
          starstarn,        {Represents the ** operator exponentiation }
          arrayconstructorn, {Construction node for [...] parsing}
          arrayconstructorrangen, {Range element to allow sets in array construction tree}
          tempcreaten,      { for temps in the result/firstpass }
          temprefn,         { references to temps }
          tempdeleten,      { for temps in the result/firstpass }
          addoptn,          { added for optimizations where we cannot suppress }
          nothingn,         {NOP, Do nothing}
          loadvmtaddrn,         {Load the address of the VMT of a class/object}
          guidconstn,       {A GUID COM Interface constant }
          rttin,             {Rtti information so they can be accessed in result/firstpass}
          loadparentfpn  { Load the framepointer of the parent for nested procedures }
       );

      const
        nodetype2str : array[tnodetype] of string[24] = (
          '<emptynode>',
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
          'notn',
          'inlinen',
          'niln',
          'errorn',
          'typen',
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
          'starstarn',
          'arrayconstructn',
          'arrayconstructrangen',
          'tempcreaten',
          'temprefn',
          'tempdeleten',
          'addoptn',
          'nothingn',
          'loadvmtaddrn',
          'guidconstn',
          'rttin',
          'loadparentfpn');

    type
       { all boolean field of ttree are now collected in flags }
       tnodeflag = (
         nf_swapable,    { tbinop operands can be swaped }
         nf_swaped,      { tbinop operands are swaped    }
         nf_error,

         { general }
         nf_write,       { Node is written to            }
         nf_first_use,   { First node that uses a variable after declared }
         nf_varstateset,
         nf_isproperty,

         { flags used by tcallnode }
         nf_return_value_used,
         nf_inherited,
         nf_anon_inherited,
         nf_new_call,
         nf_dispose_call,
         nf_member_call, { called with implicit methodpointer tree }

         { flags used by tcallparanode }
         nf_varargs_para,  { belongs this para to varargs }

         { taddrnode }
         nf_procvarload,

         { tvecnode }
         nf_memindex,
         nf_memseg,
         nf_callunique,

         { tloadnode }
         nf_absolute,
         nf_load_self_pointer,

         { taddnode }
         nf_is_currency,

         { tassignmentnode }
         nf_concat_string,
         nf_use_strconcat,

         { tarrayconstructnode }
         nf_cargs,
         nf_cargswap,
         nf_forcevaria,
         nf_novariaallowed,

         { ttypeconvnode }
         nf_explicit,

         { tinlinenode }
         nf_inlineconst,

         { tblocknode }
         nf_releasetemps
       );

       tnodeflags = set of tnodeflag;

    const
       { contains the flags which must be equal for the equality }
       { of nodes                                                }
       flagsequal : tnodeflags = [nf_error];

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
          { expected location of the result of this node (pass1) }
          expectloc : tcgloc;
          { the location of the result of this node (pass2) }
          location : tlocation;
          { the parent node of this is node    }
          { this field is set by concattolist  }
          parent : tnode;
          { there are some properties about the node stored }
          flags : tnodeflags;
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
          constructor create(t:tnodetype);
          { this constructor is only for creating copies of class }
          { the fields are copied by getcopy                      }
          constructor createforcopy;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);virtual;
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);virtual;
          procedure derefimpl;virtual;

          { toggles the flag }
          procedure toggleflag(f : tnodeflag);

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
          { For a t1:=t2 tree, mark the part of the tree t1 that gets
            written to (normally the loadnode) as write access. }
          procedure mark_write;virtual;
          procedure det_temp;virtual;abstract;

          procedure pass_2;virtual;abstract;

          { comparing of nodes }
          function isequal(p : tnode) : boolean;
          { to implement comparisation, override this method }
          function docompare(p : tnode) : boolean;virtual;
          { gets a copy of the node }
          function getcopy : tnode;virtual;

          procedure insertintolist(l : tnodelist);virtual;
          { writes a node for debugging purpose, shouldn't be called }
          { direct, because there is no test for nil, use printnode  }
          { to write a complete tree }
          procedure printnodeinfo(var t:text);
          procedure printnodedata(var t:text);virtual;
          procedure printnodetree(var t:text);virtual;
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
          constructor create(t:tnodetype;l : tnode);
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          procedure concattolist(l : tlinkedlist);override;
          function ischild(p : tnode) : boolean;override;
          function docompare(p : tnode) : boolean;override;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          procedure left_max;
          procedure printnodedata(var t:text);override;
       end;

       pbinarynode = ^tbinarynode;
       tbinarynode = class(tunarynode)
          right : tnode;
          constructor create(t:tnodetype;l,r : tnode);
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          destructor destroy;override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure derefimpl;override;
          procedure concattolist(l : tlinkedlist);override;
          function ischild(p : tnode) : boolean;override;
          function docompare(p : tnode) : boolean;override;
          procedure swapleftright;
          function getcopy : tnode;override;
          procedure insertintolist(l : tnodelist);override;
          procedure left_right_max;
          procedure printnodedata(var t:text);override;
          procedure printnodelist(var t:text);
       end;

       tbinopnode = class(tbinarynode)
          constructor create(t:tnodetype;l,r : tnode);virtual;
          function docompare(p : tnode) : boolean;override;
       end;

{$ifdef tempregdebug}
    type
      pptree = ^tnode;
    var
      curptree: pptree;
{$endif tempregdebug}

    var
      { array with all class types for tnodes }
      nodeclass : tnodeclassarray;

    function ppuloadnode(ppufile:tcompilerppufile):tnode;
    procedure ppuwritenode(ppufile:tcompilerppufile;n:tnode);

    const
      printnodespacing = '   ';
    var
      { indention used when writing the tree to the screen }
      printnodeindention : string;

    procedure printnodeindent;
    procedure printnodeunindent;
    procedure printnode(var t:text;n:tnode);



implementation

    uses
       cutils,verbose;

    const
      ppunodemarker = 255;


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
        if b<>ppunodemarker then
          internalerror(200208151);
        { load nodetype }
        t:=tnodetype(ppufile.getbyte);
        if t>high(tnodetype) then
          internalerror(200208152);
        if t<>emptynode then
         begin
           if not assigned(nodeclass[t]) then
             internalerror(200208153);
           //writeln('load: ',nodetype2str[t]);
           { generate node of the correct class }
           ppuloadnode:=nodeclass[t].ppuload(t,ppufile);
         end
        else
         ppuloadnode:=nil;
      end;


    procedure ppuwritenode(ppufile:tcompilerppufile;n:tnode);
      begin
        { marker, read by ppuloadnode }
        ppufile.putbyte(ppunodemarker);
        { type, read by ppuloadnode }
        if assigned(n) then
         begin
           ppufile.putbyte(byte(n.nodetype));
           //writeln('write: ',nodetype2str[n.nodetype]);
           n.ppuwrite(ppufile);
         end
        else
         ppufile.putbyte(byte(emptynode));
      end;


    procedure printnodeindent;
      begin
        printnodeindention:=printnodeindention+printnodespacing;
      end;


    procedure printnodeunindent;
      begin
        delete(printnodeindention,1,length(printnodespacing));
      end;


    procedure printnode(var t:text;n:tnode);
      begin
        if assigned(n) then
         n.printnodetree(t)
        else
         writeln(t,printnodeindention,'nil');
      end;


{****************************************************************************
                                 TNODE
 ****************************************************************************}

    constructor tnode.create(t:tnodetype);

      begin
         inherited create;
         nodetype:=t;
         blocktype:=block_type;
         { updated by firstpass }
         expectloc:=LOC_INVALID;
         { updated by secondpass }
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

    constructor tnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);

      begin
        nodetype:=t;
        { tnode fields }
        blocktype:=tblock_type(ppufile.getbyte);
        ppufile.getposinfo(fileinfo);
        ppufile.getsmallset(localswitches);
        ppufile.gettype(resulttype);
        ppufile.getsmallset(flags);
        { updated by firstpass }
        expectloc:=LOC_INVALID;
        { updated by secondpass }
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


    procedure tnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        ppufile.putbyte(byte(block_type));
        ppufile.putposinfo(fileinfo);
        ppufile.putsmallset(localswitches);
        ppufile.puttype(resulttype);
        ppufile.putsmallset(flags);
      end;


    procedure tnode.derefimpl;
      begin
        resulttype.resolve;
      end;


    procedure tnode.toggleflag(f : tnodeflag);
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


    procedure tnode.mark_write;
      begin
{$ifdef EXTDEBUG}
        Comment(V_Warning,'mark_write not implemented for '+nodetype2str[nodetype]);
{$endif EXTDEBUG}
      end;


    procedure tnode.printnodeinfo(var t:text);
      begin
        write(t,nodetype2str[nodetype]);
        if assigned(resulttype.def) then
          write(t,', resulttype = "',resulttype.def.gettypename,'"')
        else
          write(t,', resulttype = <nil>');
        writeln(t,', pos = (',fileinfo.line,',',fileinfo.column,')',
                  // ', loc = ',tcgloc2str[location.loc],
                  ', expectloc = ',tcgloc2str[expectloc],
                  ', intregs = ',registers32,
                  ', fpuregs = ',registersfpu);
      end;


    procedure tnode.printnodedata(var t:text);
      begin
      end;


    procedure tnode.printnodetree(var t:text);
      begin
         write(t,printnodeindention,'(');
         printnodeinfo(t);
         printnodeindent;
         printnodedata(t);
         printnodeunindent;
         writeln(t,printnodeindention,')');
      end;


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

    constructor tunarynode.create(t:tnodetype;l : tnode);
      begin
         inherited create(t);
         left:=l;
      end;


    constructor tunarynode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        left:=ppuloadnode(ppufile);
      end;


    destructor tunarynode.destroy;
      begin
        left.free;
        inherited destroy;
      end;


    procedure tunarynode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppuwritenode(ppufile,left);
      end;


    procedure tunarynode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(left) then
          left.derefimpl;
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


    procedure tunarynode.printnodedata(var t:text);
      begin
         inherited printnodedata(t);
         printnode(t,left);
      end;


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

    constructor tbinarynode.create(t:tnodetype;l,r : tnode);
      begin
         inherited create(t,l);
         right:=r
      end;


    constructor tbinarynode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        right:=ppuloadnode(ppufile);
      end;


    destructor tbinarynode.destroy;
      begin
        right.free;
        inherited destroy;
      end;


    procedure tbinarynode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppuwritenode(ppufile,right);
      end;


    procedure tbinarynode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(right) then
          right.derefimpl;
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


    procedure tbinarynode.printnodedata(var t:text);
      begin
         inherited printnodedata(t);
         printnode(t,right);
      end;


    procedure tbinarynode.printnodelist(var t:text);
      var
        hp : tbinarynode;
      begin
        hp:=self;
        while assigned(hp) do
         begin
           write(t,printnodeindention,'(');
           printnodeindent;
           hp.printnodeinfo(t);
           printnode(t,hp.left);
           printnodeunindent;
           writeln(t,printnodeindention,')');
           hp:=tbinarynode(hp.right);
         end;
      end;


{****************************************************************************
                            TBINOPYNODE
 ****************************************************************************}

    constructor tbinopnode.create(t:tnodetype;l,r : tnode);
      begin
         inherited create(t,l,r);
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
  Revision 1.68  2003-10-01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.67  2003/09/28 17:55:04  peter
    * parent framepointer changed to hidden parameter
    * tloadparentfpnode added

  Revision 1.66  2003/09/06 22:27:08  florian
    * fixed web bug 2669
    * cosmetic fix in printnode
    * tobjectdef.gettypename implemented

  Revision 1.65  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.64  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.63  2003/08/10 17:25:23  peter
    * fixed some reported bugs

  Revision 1.62  2003/05/26 21:17:17  peter
    * procinlinenode removed
    * aktexit2label removed, fast exit removed
    + tcallnode.inlined_pass_2 added

  Revision 1.61  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.60  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.59  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.58  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.57  2002/04/25 20:15:39  florian
    * block nodes within expressions shouldn't release the used registers,
      fixed using a flag till the new rg is ready

  Revision 1.56  2003/04/24 22:29:58  florian
    * fixed a lot of PowerPC related stuff

  Revision 1.55  2003/04/23 10:12:14  peter
    * allow multi pass2 changed to global boolean instead of node flag

  Revision 1.54  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.53  2003/04/22 09:52:00  peter
    * mark_write implemented for default with a warning in EXTDEBUG, this
      is required for error recovery where the left node can be also a non
      writable node

  Revision 1.52  2003/04/10 17:57:52  peter
    * vs_hidden released

  Revision 1.51  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.50  2003/03/17 16:54:41  peter
    * support DefaultHandler and anonymous inheritance fixed
      for message methods

  Revision 1.49  2003/01/04 15:54:03  daniel
    * Fixed mark_write for @ operator
      (can happen when compiling @procvar:=nil (Delphi mode construction))

  Revision 1.48  2003/01/03 21:03:02  peter
    * made mark_write dummy instead of abstract

  Revision 1.47  2003/01/03 12:15:56  daniel
    * Removed ifdefs around notifications
      ifdefs around for loop optimizations remain

  Revision 1.46  2002/12/26 18:24:33  jonas
  * fixed check for whether or not a high parameter was already generated
  * no type checking/conversions for invisible parameters

  Revision 1.45  2002/11/28 11:17:04  florian
    * loop node flags from node flags splitted

  Revision 1.44  2002/10/05 00:48:57  peter
    * support inherited; support for overload as it is handled by
      delphi. This is only for delphi mode as it is working is
      undocumented and hard to predict what is done

  Revision 1.43  2002/09/07 15:25:03  peter
    * old logs removed and tabs fixed

  Revision 1.42  2002/09/03 16:26:26  daniel
    * Make Tprocdef.defs protected

  Revision 1.41  2002/09/01 13:28:38  daniel
   - write_access fields removed in favor of a flag

  Revision 1.40  2002/09/01 08:01:16  daniel
   * Removed sets from Tcallnode.det_resulttype
   + Added read/write notifications of variables. These will be usefull
     for providing information for several optimizations. For example
     the value of the loop variable of a for loop does matter is the
     variable is read after the for loop, but if it's no longer used
     or written, it doesn't matter and this can be used to optimize
     the loop code generation.

  Revision 1.39  2002/08/22 11:21:45  florian
    + register32 is now written by tnode.dowrite
    * fixed write of value of tconstnode

  Revision 1.38  2002/08/19 19:36:44  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.37  2002/08/18 20:06:24  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.36  2002/08/17 22:09:46  florian
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

}
