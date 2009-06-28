{
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
       cpubase,cgbase,cgutils,
       aasmbase,
       symtype,
       optbase;

    type
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
          starstarn,        {Represents the ** operator exponentiation }
          arrayconstructorn, {Construction node for [...] parsing}
          arrayconstructorrangen, {Range element to allow sets in array construction tree}
          tempcreaten,      { for temps in the result/firstpass }
          temprefn,         { references to temps }
          tempdeleten,      { for temps in the result/firstpass }
          addoptn,          { added for optimizations where we cannot suppress }
          nothingn,         { NOP, Do nothing}
          loadvmtaddrn,     { Load the address of the VMT of a class/object}
          guidconstn,       { A GUID COM Interface constant }
          rttin,            { Rtti information so they can be accessed in result/firstpass}
          loadparentfpn,    { Load the framepointer of the parent for nested procedures }
          dataconstn,       { node storing some binary data }
          objcselectorn,    { node for an Objective-C message selector }
          objcmessagesendn  { node for message sent to an Objective-C instance (similar to a method call) }
       );

       tnodetypeset = set of tnodetype;
       pnodetypeset = ^tnodetypeset;

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
          'loadparentfpn',
          'dataconstn',
          'objcselectorn',
          'objcmessagesendn');

    type
       { all boolean field of ttree are now collected in flags }
       tnodeflag = (
         nf_swapable,    { tbinop operands can be swaped }
         nf_swapped,      { tbinop operands are swaped    }
         nf_error,

         { general }
         nf_pass1_done,
         nf_write,       { Node is written to    }
         nf_modify,      { Node is modified      }
         nf_is_funcret,
         nf_isproperty,
         nf_processing,

         { taddrnode }
         nf_typedaddr,

         { tderefnode }
         nf_no_checkpointer,

         { tvecnode }
         nf_memindex,
         nf_memseg,
         nf_callunique,

         { tloadnode }
         nf_absolute,
         nf_is_self,
         nf_load_self_pointer,
         nf_inherited,
         { the loadnode is generated internally and a varspez=vs_const should be ignore,
           this requires that the parameter is actually passed by value
           Be really carefull when using this flag! }
         nf_isinternal_ignoreconst,

         { taddnode }
         nf_is_currency,
         nf_has_pointerdiv,
         nf_short_bool,

         { tassignmentnode }
         nf_assign_done_in_right,

         { tarrayconstructnode }
         nf_forcevaria,
         nf_novariaallowed,

         { ttypeconvnode, and the first one also treal/ord/pointerconstn }
         { second one also for subtractions of u32-u32 implicitly upcasted to s64 }
         nf_explicit,
         nf_internal,  { no warnings/hints generated }
         nf_load_procvar,

         { tinlinenode }
         nf_inlineconst,

         { tasmnode }
         nf_get_asm_position,

         { tblocknode }
         nf_block_with_exit,

         { tloadvmtaddrnode }
         nf_ignore_for_wpo  { we know that this loadvmtaddrnode cannot be used to construct a class instance }

         { WARNING: there are now 32 elements in this type, and a set of this
             type is written to the PPU. So before adding any more elements,
             either move some flags to specific nodes, or stream a normalset
             to the ppu
         }

       );

       tnodeflags = set of tnodeflag;

    const
       { contains the flags which must be equal for the equality }
       { of nodes                                                }
       flagsequal : tnodeflags = [nf_error];

    type
       tnodelist = class
       end;

      pnode = ^tnode;
      { basic class for the intermediated representation fpc uses }
      tnode = class
      private
         fppuidx : longint;
         function getppuidx:longint;
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
         { next node in control flow on the same block level, i.e.
           for loop nodes, this is the next node after the end of the loop,
           same for if and case, if this field is nil, the next node is the procedure exit,
           for the last node in a loop this is set to the loop header
           this field is set only for control flow nodes }
         successor : tnode;
         { there are some properties about the node stored }
         flags  : tnodeflags;
         resultdef     : tdef;
         resultdefderef : tderef;
         fileinfo      : tfileposinfo;
         localswitches : tlocalswitches;
         verbosity     : longint;
         optinfo : poptinfo;
         constructor create(t:tnodetype);
         { this constructor is only for creating copies of class }
         { the fields are copied by getcopy                      }
         constructor createforcopy;
         constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);virtual;
         destructor destroy;override;
         procedure ppuwrite(ppufile:tcompilerppufile);virtual;
         procedure buildderefimpl;virtual;
         procedure derefimpl;virtual;
         procedure resolveppuidx;virtual;

         { toggles the flag }
         procedure toggleflag(f : tnodeflag);

         { the 1.1 code generator may override pass_1 }
         { and it need not to implement det_* then    }
         { 1.1: pass_1 returns a value<>0 if the node has been transformed }
         { 2.0: runs pass_typecheck and det_temp                           }
         function pass_1 : tnode;virtual;abstract;
         { dermines the resultdef of the node }
         function pass_typecheck : tnode;virtual;abstract;

         { tries to simplify the node, returns a value <>nil if a simplified
           node has been created }
         function simplify : tnode;virtual;
{$ifdef state_tracking}
         { Does optimizations by keeping track of the variable states
           in a procedure }
         function track_state_pass(exec_known:boolean):boolean;virtual;
{$endif}
         { For a t1:=t2 tree, mark the part of the tree t1 that gets
           written to (normally the loadnode) as write access. }
         procedure mark_write;virtual;
         { dermines the number of necessary temp. locations to evaluate
           the node }
         procedure det_temp;virtual;abstract;

         procedure pass_generate_code;virtual;abstract;

         { comparing of nodes }
         function isequal(p : tnode) : boolean;
         { to implement comparisation, override this method }
         function docompare(p : tnode) : boolean;virtual;
         { wrapper for getcopy }
         function getcopy : tnode;

         { does the real copying of a node }
         function dogetcopy : tnode;virtual;

         { returns the real loadn/temprefn a node refers to,
           skipping (absolute) equal type conversions        }
         function actualtargetnode: tnode;virtual;

         procedure insertintolist(l : tnodelist);virtual;
         { writes a node for debugging purpose, shouldn't be called }
         { direct, because there is no test for nil, use printnode  }
         { to write a complete tree }
         procedure printnodeinfo(var t:text);virtual;
         procedure printnodedata(var t:text);virtual;
         procedure printnodetree(var t:text);virtual;
         procedure concattolist(l : tlinkedlist);virtual;
         function ischild(p : tnode) : boolean;virtual;

         { ensures that the optimizer info record is allocated }
         function allocoptinfo : poptinfo;inline;
         property ppuidx:longint read getppuidx;
      end;

      tnodeclass = class of tnode;

      tnodeclassarray = array[tnodetype] of tnodeclass;

      { this node is the anchestor for all nodes with at least   }
      { one child, you have to use it if you want to use         }
      { true- and current_procinfo.CurrFalseLabel                                     }
      punarynode = ^tunarynode;
      tunarynode = class(tnode)
         left : tnode;
         constructor create(t:tnodetype;l : tnode);
         constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
         destructor destroy;override;
         procedure ppuwrite(ppufile:tcompilerppufile);override;
         procedure buildderefimpl;override;
         procedure derefimpl;override;
         procedure concattolist(l : tlinkedlist);override;
         function ischild(p : tnode) : boolean;override;
         function docompare(p : tnode) : boolean;override;
         function dogetcopy : tnode;override;
         procedure insertintolist(l : tnodelist);override;
         procedure printnodedata(var t:text);override;
      end;

      pbinarynode = ^tbinarynode;
      tbinarynode = class(tunarynode)
         right : tnode;
         constructor create(t:tnodetype;l,r : tnode);
         constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
         destructor destroy;override;
         procedure ppuwrite(ppufile:tcompilerppufile);override;
         procedure buildderefimpl;override;
         procedure derefimpl;override;
         procedure concattolist(l : tlinkedlist);override;
         function ischild(p : tnode) : boolean;override;
         function docompare(p : tnode) : boolean;override;
         procedure swapleftright;
         function dogetcopy : tnode;override;
         procedure insertintolist(l : tnodelist);override;
         procedure printnodedata(var t:text);override;
         procedure printnodelist(var t:text);
      end;

      ptertiarynode = ^ttertiarynode;
      ttertiarynode = class(tbinarynode)
         third : tnode;
         constructor create(_t:tnodetype;l,r,t : tnode);
         constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
         destructor destroy;override;
         procedure ppuwrite(ppufile:tcompilerppufile);override;
         procedure buildderefimpl;override;
         procedure derefimpl;override;
         procedure concattolist(l : tlinkedlist);override;
         function ischild(p : tnode) : boolean;override;
         function docompare(p : tnode) : boolean;override;
         function dogetcopy : tnode;override;
         procedure insertintolist(l : tnodelist);override;
         procedure printnodedata(var t:text);override;
      end;

      tbinopnode = class(tbinarynode)
         constructor create(t:tnodetype;l,r : tnode);virtual;
         function docompare(p : tnode) : boolean;override;
      end;

    var
      { array with all class types for tnodes }
      nodeclass : tnodeclassarray;

    function nodeppuidxget(i:longint):tnode;
    function ppuloadnode(ppufile:tcompilerppufile):tnode;
    procedure ppuwritenode(ppufile:tcompilerppufile;n:tnode);
    function ppuloadnodetree(ppufile:tcompilerppufile):tnode;
    procedure ppuwritenodetree(ppufile:tcompilerppufile;n:tnode);

    const
      printnodespacing = '   ';
    var
      { indention used when writing the tree to the screen }
      printnodeindention : string;

    procedure printnodeindent;
    procedure printnodeunindent;
    procedure printnode(var t:text;n:tnode);
    procedure printnode(n:tnode);

    function is_constnode(p : tnode) : boolean;
    function is_constintnode(p : tnode) : boolean;
    function is_constcharnode(p : tnode) : boolean;
    function is_constrealnode(p : tnode) : boolean;
    function is_constboolnode(p : tnode) : boolean;
    function is_constenumnode(p : tnode) : boolean;
    function is_constwidecharnode(p : tnode) : boolean;
    function is_constpointernode(p : tnode) : boolean;


implementation

    uses
       cutils,verbose,ppu,comphook,
       symconst,
       nutils,nflw,
       defutil;

    const
      ppunodemarker = 255;


{****************************************************************************
                                 Helpers
 ****************************************************************************}

    var
      nodeppulist : TFPObjectList;
      nodeppuidx  : longint;


    procedure nodeppuidxcreate;
      begin
        nodeppulist:=TFPObjectList.Create(false);
        nodeppuidx:=0;
      end;


    procedure nodeppuidxresolve;
      var
        i : longint;
        n : tnode;
      begin
        for i:=0 to nodeppulist.count-1 do
          begin
            n:=tnode(nodeppulist[i]);
            if assigned(n) then
              n.resolveppuidx;
          end;
      end;


    procedure nodeppuidxfree;
      begin
        nodeppulist.free;
        nodeppulist:=nil;
        nodeppuidx:=0;
      end;


    procedure nodeppuidxadd(n:tnode);
      var
        i : longint;
      begin
        i:=n.ppuidx;
        if i<=0 then
          internalerror(200311072);
        if i>=nodeppulist.capacity then
          nodeppulist.capacity:=((i div 1024)+1)*1024;
        if i>=nodeppulist.count then
          nodeppulist.count:=i+1;
        nodeppulist[i]:=n;
      end;


    function nodeppuidxget(i:longint):tnode;
      begin
        if i<=0 then
          internalerror(200311073);
        result:=tnode(nodeppulist[i]);
      end;


    function ppuloadnode(ppufile:tcompilerppufile):tnode;
      var
        b : byte;
        t : tnodetype;
        hppuidx : longint;
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
           hppuidx:=ppufile.getlongint;
           //writeln('load: ',nodetype2str[t]);
           { generate node of the correct class }
           result:=nodeclass[t].ppuload(t,ppufile);
           result.fppuidx:=hppuidx;
           nodeppuidxadd(result);
         end
        else
         result:=nil;
      end;


    procedure ppuwritenode(ppufile:tcompilerppufile;n:tnode);
      begin
        { marker, read by ppuloadnode }
        ppufile.putbyte(ppunodemarker);
        { type, read by ppuloadnode }
        if assigned(n) then
         begin
           ppufile.putbyte(byte(n.nodetype));
           ppufile.putlongint(n.ppuidx);
           //writeln('write: ',nodetype2str[n.nodetype]);
           n.ppuwrite(ppufile);
         end
        else
         ppufile.putbyte(byte(emptynode));
      end;


    function ppuloadnodetree(ppufile:tcompilerppufile):tnode;
      begin
        if ppufile.readentry<>ibnodetree then
          Message(unit_f_ppu_read_error);
        nodeppuidxcreate;
        result:=ppuloadnode(ppufile);
        nodeppuidxresolve;
        nodeppuidxfree;
      end;


    procedure ppuwritenodetree(ppufile:tcompilerppufile;n:tnode);
      begin
        nodeppuidxcreate;
        ppuwritenode(ppufile,n);
        ppufile.writeentry(ibnodetree);
        nodeppuidxfree;
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


    procedure printnode(n:tnode);
      begin
        printnode(output,n);
      end;


    function is_constnode(p : tnode) : boolean;
      begin
        is_constnode:=(p.nodetype in [niln,ordconstn,realconstn,stringconstn,setconstn,guidconstn]);
      end;


    function is_constintnode(p : tnode) : boolean;
      begin
         is_constintnode:=(p.nodetype=ordconstn) and is_integer(p.resultdef);
      end;


    function is_constcharnode(p : tnode) : boolean;
      begin
         is_constcharnode:=(p.nodetype=ordconstn) and is_char(p.resultdef);
      end;


    function is_constwidecharnode(p : tnode) : boolean;
      begin
         is_constwidecharnode:=(p.nodetype=ordconstn) and is_widechar(p.resultdef);
      end;


    function is_constrealnode(p : tnode) : boolean;
      begin
         is_constrealnode:=(p.nodetype=realconstn);
      end;


    function is_constboolnode(p : tnode) : boolean;
      begin
         is_constboolnode:=(p.nodetype=ordconstn) and is_boolean(p.resultdef);
      end;


    function is_constenumnode(p : tnode) : boolean;
      begin
         is_constenumnode:=(p.nodetype=ordconstn) and (p.resultdef.typ=enumdef);
      end;


    function is_constpointernode(p : tnode) : boolean;
      begin
         is_constpointernode:=(p.nodetype=pointerconstn);
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
         fileinfo:=current_filepos;
         localswitches:=current_settings.localswitches;
         verbosity:=status.verbosity;
         resultdef:=nil;
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
        verbosity:=ppufile.getlongint;
        ppufile.getderef(resultdefderef);
        ppufile.getsmallset(flags);
        { updated by firstpass }
        expectloc:=LOC_INVALID;
        { updated by secondpass }
        location.loc:=LOC_INVALID;
      end;


    procedure tnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        ppufile.putbyte(byte(block_type));
        ppufile.putposinfo(fileinfo);
        ppufile.putsmallset(localswitches);
        ppufile.putlongint(verbosity);
        ppufile.putderef(resultdefderef);
        ppufile.putsmallset(flags);
      end;


    function tnode.getppuidx:longint;
      begin
        if fppuidx=0 then
          begin
            inc(nodeppuidx);
            fppuidx:=nodeppuidx;
          end;
         result:=fppuidx;
       end;


    procedure tnode.resolveppuidx;
      begin
      end;


    procedure tnode.buildderefimpl;
      begin
        resultdefderef.build(resultdef);
      end;


    procedure tnode.derefimpl;
      begin
        resultdef:=tdef(resultdefderef.resolve);
      end;


    procedure tnode.toggleflag(f : tnodeflag);
      begin
         if f in flags then
           exclude(flags,f)
         else
           include(flags,f);
      end;


    function tnode.simplify : tnode;
      begin
        result:=nil;
      end;


    destructor tnode.destroy;
      begin
         if assigned(optinfo) then
           dispose(optinfo);
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
        if assigned(resultdef) then
          write(t,', resultdef = "',resultdef.GetTypeName,'"')
        else
          write(t,', resultdef = <nil>');
        write(t,', pos = (',fileinfo.line,',',fileinfo.column,')',
                  ', loc = ',tcgloc2str[location.loc],
                  ', expectloc = ',tcgloc2str[expectloc]);
      end;


    procedure tnode.printnodedata(var t:text);
      begin
      end;


    procedure tnode.printnodetree(var t:text);
      begin
         write(t,printnodeindention,'(');
         printnodeinfo(t);
         writeln(t);
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


    function cleanupcopiedto(var n : tnode;arg : pointer) : foreachnoderesult;
      begin
        result:=fen_true;
        if n.nodetype=labeln then
          tlabelnode(n).copiedto:=nil;
      end;


    function tnode.getcopy : tnode;
      begin
        result:=dogetcopy;
        foreachnodestatic(pm_postprocess,self,@cleanupcopiedto,nil);
      end;


    function tnode.dogetcopy : tnode;
      var
         p : tnode;
      begin
         { this is quite tricky because we need a node of the current }
         { node type and not one of tnode!                            }
         p:=tnodeclass(classtype).createforcopy;
         p.nodetype:=nodetype;
         p.expectloc:=expectloc;
         p.location:=location;
         p.parent:=parent;
         p.flags:=flags;
         p.resultdef:=resultdef;
         p.fileinfo:=fileinfo;
         p.localswitches:=localswitches;
         p.verbosity:=verbosity;
{         p.list:=list; }
         result:=p;
      end;


    function tnode.actualtargetnode: tnode;
      begin
        result:=self;
      end;


    procedure tnode.insertintolist(l : tnodelist);
      begin
      end;


    { ensures that the optimizer info record is allocated }
    function tnode.allocoptinfo : poptinfo;inline;
      begin
        if not(assigned(optinfo)) then
          new(optinfo);
        result:=optinfo;
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


    procedure tunarynode.buildderefimpl;
      begin
        inherited buildderefimpl;
        if assigned(left) then
          left.buildderefimpl;
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


    function tunarynode.dogetcopy : tnode;
      var
         p : tunarynode;
      begin
         p:=tunarynode(inherited dogetcopy);
         if assigned(left) then
           p.left:=left.dogetcopy
         else
           p.left:=nil;
         result:=p;
      end;


    procedure tunarynode.insertintolist(l : tnodelist);
      begin
      end;


    procedure tunarynode.printnodedata(var t:text);
      begin
         inherited printnodedata(t);
         printnode(t,left);
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


    procedure tbinarynode.buildderefimpl;
      begin
        inherited buildderefimpl;
        if assigned(right) then
          right.buildderefimpl;
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


    function tbinarynode.dogetcopy : tnode;
      var
         p : tbinarynode;
      begin
         p:=tbinarynode(inherited dogetcopy);
         if assigned(right) then
           p.right:=right.dogetcopy
         else
           p.right:=nil;
         result:=p;
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
         if nf_swapped in flags then
           exclude(flags,nf_swapped)
         else
           include(flags,nf_swapped);
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
           writeln(t);
           printnode(t,hp.left);
           writeln(t);
           printnodeunindent;
           writeln(t,printnodeindention,')');
           hp:=tbinarynode(hp.right);
         end;
      end;


{****************************************************************************
                                 TTERTIARYNODE
 ****************************************************************************}

    constructor ttertiarynode.create(_t:tnodetype;l,r,t : tnode);
      begin
         inherited create(_t,l,r);
         third:=t;
      end;


    constructor ttertiarynode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        third:=ppuloadnode(ppufile);
      end;


    destructor ttertiarynode.destroy;
      begin
        third.free;
        inherited destroy;
      end;


    procedure ttertiarynode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppuwritenode(ppufile,third);
      end;


    procedure ttertiarynode.buildderefimpl;
      begin
        inherited buildderefimpl;
        if assigned(third) then
          third.buildderefimpl;
      end;


    procedure ttertiarynode.derefimpl;
      begin
        inherited derefimpl;
        if assigned(third) then
          third.derefimpl;
      end;


    function ttertiarynode.docompare(p : tnode) : boolean;
      begin
         docompare:=(inherited docompare(p) and
           ((third=nil) or third.isequal(ttertiarynode(p).third))
         );
      end;


    function ttertiarynode.dogetcopy : tnode;
      var
         p : ttertiarynode;
      begin
         p:=ttertiarynode(inherited dogetcopy);
         if assigned(third) then
           p.third:=third.dogetcopy
         else
           p.third:=nil;
         result:=p;
      end;


    procedure ttertiarynode.insertintolist(l : tnodelist);
      begin
      end;


    procedure ttertiarynode.printnodedata(var t:text);
      begin
         inherited printnodedata(t);
         printnode(t,third);
      end;


    procedure ttertiarynode.concattolist(l : tlinkedlist);
      begin
         third.parent:=self;
         third.concattolist(l);
         inherited concattolist(l);
      end;


    function ttertiarynode.ischild(p : tnode) : boolean;
      begin
         ischild:=p=third;
      end;


{****************************************************************************
                            TBINOPNODE
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
