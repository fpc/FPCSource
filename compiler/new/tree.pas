{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    This units exports some routines to manage the parse tree

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

{$ifdef tp}
  {$E+,N+}
{$endif}
unit tree;

  interface

    uses
       globtype,cobjects,symtable,aasm
{$I cpuunit.inc}
       ;

    type
       pconstset = ^tconstset;
       tconstset = array[0..31] of byte;

       ttreetyp = (
          addn,            {Represents the + operator.}
          muln,            {Represents the * operator.}
          subn,            {Represents the - operator.}
          divn,            {Represents the div operator.}
          symdifn,         {Represents the >< operator.}
          modn,            {Represents the mod operator.}
          assignn,         {Represents an assignment.}
          loadn,           {Represents the use of a variabele.}
          rangen,          {Represents a range (i.e. 0..9).}
          ltn,             {Represents the < operator.}
          lten,            {Represents the <= operator.}
          gtn,             {Represents the > operator.}
          gten,            {Represents the >= operator.}
          equaln,          {Represents the = operator.}
          unequaln,        {Represents the <> operator.}
          inn,             {Represents the in operator.}
          orn,             {Represents the or operator.}
          xorn,            {Represents the xor operator.}
          shrn,            {Represents the shr operator.}
          shln,            {Represents the shl operator.}
          slashn,          {Represents the / operator.}
          andn,            {Represents the and operator.}
          subscriptn,      {access to a record/class/object field}
          derefn,          {Dereferences a pointer.}
          addrn,           {Represents the @ operator.}
          doubleaddrn,     {Represents the @@ operator.}
          ordconstn,       {Represents an ordinal value.}
          typeconvn,       {Represents type-conversion/typecast.}
          calln,           {Represents a call node.}
          callparan,       {Represents a parameter.}
          realconstn,      {Represents a real value.}
          fixconstn,       {Represents a fixed value.}
          umminusn,        {Represents a sign change (i.e. -2).}
          asmn,            {Represents an assembler node }
          vecn,            {Represents array indexing.}
          stringconstn,    {Represents a string constant.}
          funcretn,        {Represents the function result var.}
          selfn,           {Represents the self parameter.}
          notn,            {Represents the not operator.}
          inlinen,         {Internal procedures (i.e. writeln).}
          niln,            {Represents the nil pointer.}
          errorn,          {This part of the tree could not be
                            parsed because of a compiler error.}
          typen,           {A type name. Used for i.e. typeof(obj).}
          hnewn,           {The new operation, constructor call.}
          hdisposen,       {The dispose operation with destructor call.}
          newn,            {The new operation, constructor call.}
          simpledisposen,  {The dispose operation.}
          setelementn,     {A set element(s) (i.e. [a,b] and also [a..b]).}
          setconstn,       {A set constant (i.e. [1,2]).}
          blockn,          {A block of statements.}
          statementn,      {One statement in a block of nodes.}
          loopn,           { used in genloopnode, must be converted }
          ifn,             {An if statement.}
          breakn,          {A break statement.}
          continuen,       {A continue statement.}
          repeatn,         {A repeat until block.}
          whilen,          {A while do statement.}
          forn,            {A for loop.}
          exitn,           {An exit statement.}
          withn,           {A with statement.}
          casen,           {A case statement.}
          labeln,          {A label.}
          goton,           {A goto statement.}
          simplenewn,      {The new operation.}
          tryexceptn,      {A try except block.}
          raisen,          {A raise statement.}
          switchesn,       {??? Currently unused...}
          tryfinallyn,     {A try finally statement.}
          onn,             { for an on statement in exception code }
          isn,             {Represents the is operator.}
          asn,             {Represents the as typecast.}
          caretn,          {Represents the ^ operator.}
          failn,           {Represents the fail statement.}
          starstarn,       {Represents the ** operator exponentiation }
          procinlinen,     {Procedures that can be inlined }
          arrayconstructn, {Construction node for [...] parsing}
          arrayconstructrangen, {Range element to allow sets in array construction tree}
          { added for optimizations where we cannot suppress }
          nothingn,
          loadvmtn
       );

       tconverttype = (
          tc_equal,
          tc_not_possible,
          tc_string_2_string,
          tc_char_2_string,
          tc_pchar_2_string,
          tc_cchar_2_pchar,
          tc_cstring_2_pchar,
          tc_ansistring_2_pchar,
          tc_string_2_chararray,
          tc_chararray_2_string,
          tc_array_2_pointer,
          tc_pointer_2_array,
          tc_int_2_int,
          tc_bool_2_int,
          tc_int_2_bool,
          tc_real_2_real,
          tc_int_2_real,
          tc_int_2_fix,
          tc_real_2_fix,
          tc_fix_2_real,
          tc_proc_2_procvar,
          tc_arrayconstructor_2_set,
          tc_load_smallset
       );

      { different assignment types }

      tassigntyp = (at_normal,at_plus,at_minus,at_star,at_slash);

      pcaserecord = ^tcaserecord;
      tcaserecord = record

          { range }
          _low,_high : longint;

          { only used by gentreejmp }
          _at : plabel;

          { label of instruction }
          statement : plabel;

          { is this the first of an case entry, needed to release statement
            label (PFV) }
          firstlabel : boolean;

          { left and right tree node }
          less,greater : pcaserecord;
       end;

       pnode = ^tnode;
       tnode = object
          treetype : ttreetyp;
          { the location of the result of this node }
          location : tlocation;

          { the number of registers needed to evalute the node }
          registersint,registersfpu : longint;  { must be longint !!!! }
{$ifdef SUPPORT_MMX}
          registersmmx : longint;
{$endif SUPPORT_MMX}
          resulttype : pdef;
          fileinfo : tfileposinfo;
          localswitches : tlocalswitches;
{$ifdef extdebug}
          firstpasscount : longint;
{$endif extdebug}
          error : boolean;
          constructor init;
          destructor done;virtual;
          { runs det_resulttype and det_temp }
          procedure pass_1;
          { dermines the resulttype of the node }
          procedure det_resulttype;virtual;
          { dermines the number of necessary temp. locations to evalute
            the node }
          procedure det_temp;virtual;
          procedure secondpass;virtual;
{$ifdef EXTDEBUG}
          { writes a node for debugging purpose, shouldn't be called }
          { direct, because there is no test for nil, use writenode  }
          { to write a complete tree                                 }
          procedure dowrite;virtual;
{$endif EXTDEBUG}
       end;

       ploadnode = ^tloadnode;

       tloadnode = object(tnode)
          symtableentry : psym;
          symtable : psymtable;
          is_absolute,is_first,is_methodpointer : boolean;
          constructor init(v : pvarsym;st : psymtable);
          destructor done;virtual;
       end;

{$ifndef nooldtree}
       { allows to determine which elementes are to be replaced }
       tdisposetyp = (dt_nothing,dt_leftright,dt_left,
                      dt_mbleft,dt_typeconv,dt_inlinen,
                      dt_mbleft_and_method,dt_loop,dt_case,dt_with,dt_onn);

       ptree = ^ttree;
       ttree = record
          error : boolean;
          disposetyp : tdisposetyp;
          { is true, if the
           right and left operand are swaped }
          swaped : boolean;

          { the location of the result of this node }
          location : tlocation;

          { the number of registers needed to evalute the node }
          registers32,registersfpu : longint;  { must be longint !!!! }
{$ifdef SUPPORT_MMX}
          registersmmx : longint;
{$endif SUPPORT_MMX}
          left,right : ptree;
          resulttype : pdef;
          fileinfo : tfileposinfo;
          localswitches : tlocalswitches;
{$ifdef EXTDEBUG}
          firstpasscount : longint;
{$endif EXTDEBUG}
          case treetype : ttreetyp of
             addn : (use_strconcat : boolean;string_typ : tstringtype);
             callparan : (is_colon_para : boolean;exact_match_found : boolean);
             assignn : (assigntyp : tassigntyp;concat_string : boolean);
             loadn : (symtableentry : psym;symtable : psymtable;
                      is_absolute,is_first,is_methodpointer : boolean);
             calln : (symtableprocentry : psym;
                      symtableproc : psymtable;procdefinition : pprocdef;
                      methodpointer : ptree;
                      no_check,unit_specific,return_value_used : boolean);
             ordconstn : (value : longint);
             realconstn : (value_real : bestreal;lab_real : plabel;realtyp : tait);
             fixconstn : (value_fix: longint);
             funcretn : (funcretprocinfo : pointer;retdef : pdef);
             subscriptn : (vs : pvarsym);
             vecn : (memindex,memseg:boolean;callunique : boolean);
             stringconstn : (value_str : pchar;length : longint; lab_str : plabel;stringtype : tstringtype);
             typeconvn : (convtyp : tconverttype;explizit : boolean);
             typen : (typenodetype : pdef);
             inlinen : (inlinenumber : byte;inlineconst:boolean);
             procinlinen : (inlineprocdef : pprocdef;
                            retoffset,para_offset,para_size : longint);
             setconstn : (value_set : pconstset;lab_set:plabel);
             loopn : (t1,t2 : ptree;backward : boolean);
             asmn : (p_asm : paasmoutput;object_preserved : boolean);
             casen : (nodes : pcaserecord;elseblock : ptree);
             labeln,goton : (labelnr : plabel);
             withn : (withsymtable : psymtable;tablecount : longint);
             onn : (exceptsymtable : psymtable;excepttype : pobjectdef);
             arrayconstructn : (cargs,cargswap: boolean);
           end;
{$endif}
          punarynode = ^tunarynode;
          tunarynode = object(tnode)
             left : pnode;
             procedure dowrite;virtual;
          end;

          pbinarynode = ^tbinarynode;
          tbinarynode = object(tunarynode)
             right : pnode;
          end;

          pbinopnode = ^tbinopnode;
          tbinopnode = object(tbinarynode)
             { is true, if the right and left operand are swaped }
             { against the original order                        }
             swaped : boolean;
          end;

          pblocknode = ^tblocknode;
          tblocknode = object(tunarynode)
            constructor init;
          end;

{$ifdef dummy}
          case treetype : ttreetyp of
             addn : (use_strconcat : boolean;string_typ : tstringtype);
             callparan : (is_colon_para : boolean;exact_match_found : boolean);
             assignn : (assigntyp : tassigntyp;concat_string : boolean);
             calln : (symtableprocentry : psym;
                      symtableproc : psymtable;procdefinition : pprocdef;
                      methodpointer : ptree;
                      no_check,unit_specific,return_value_used : boolean);
             ordconstn : (value : longint);
             realconstn : (value_real : bestreal;lab_real : plabel;realtyp : tait);
             fixconstn : (value_fix: longint);
             funcretn : (funcretprocinfo : pointer;retdef : pdef);
             subscriptn : (vs : pvarsym);
             vecn : (memindex,memseg:boolean;callunique : boolean);
             stringconstn : (value_str : pchar;length : longint; lab_str : plabel;stringtype : tstringtype);
             typeconvn : (convtyp : tconverttype;explizit : boolean);
             typen : (typenodetype : pdef);
             inlinen : (inlinenumber : byte;inlineconst:boolean);
             procinlinen : (inlineprocdef : pprocdef;
                            retoffset,para_offset,para_size : longint);
             setconstn : (value_set : pconstset;lab_set:plabel);
             loopn : (t1,t2 : ptree;backward : boolean);
             casen : (nodes : pcaserecord;elseblock : ptree);
             labeln,goton : (labelnr : plabel);
             withn : (withsymtable : psymtable;tablecount : longint);
             onn : (exceptsymtable : psymtable;excepttype : pobjectdef);
             arrayconstructn : (cargs,cargswap: boolean);
{$endif dummy}

    function gennode(t : ttreetyp;l,r : ptree) : ptree;
    function genlabelnode(t : ttreetyp;nr : plabel) : ptree;
    function genloadnode(v : pvarsym;st : psymtable) : ptree;
    function genloadcallnode(v: pprocsym;st: psymtable): ptree;
    function gensinglenode(t : ttreetyp;l : ptree) : ptree;
    function gensubscriptnode(varsym : pvarsym;l : ptree) : ptree;
    function genordinalconstnode(v : longint;def : pdef) : ptree;
    function genfixconstnode(v : longint;def : pdef) : ptree;
    function gentypeconvnode(node : ptree;t : pdef) : ptree;
    function gentypenode(t : pdef) : ptree;
    function gencallparanode(expr,next : ptree) : ptree;
    function genrealconstnode(v : bestreal) : ptree;
    function gencallnode(v : pprocsym;st : psymtable) : ptree;
    function genmethodcallnode(v : pprocsym;st : psymtable;mp : ptree) : ptree;

    { allow pchar or string for defining a pchar node }
    function genstringconstnode(const s : string) : ptree;
    { length is required for ansistrings }
    function genpcharconstnode(s : pchar;length : longint) : ptree;
    { helper routine for conststring node }
    function getpcharcopy(p : ptree) : pchar;

    function genzeronode(t : ttreetyp) : ptree;
    function geninlinenode(number : byte;is_const:boolean;l : ptree) : ptree;
    function genprocinlinenode(callp,code : ptree) : ptree;
    function gentypedconstloadnode(sym : ptypedconstsym;st : psymtable) : ptree;
    function genenumnode(v : penumsym) : ptree;
    function genselfnode(_class : pdef) : ptree;
    function gensetconstnode(s : pconstset;settype : psetdef) : ptree;
    function genloopnode(t : ttreetyp;l,r,n1: ptree;back : boolean) : ptree;
    function genasmnode(p_asm : paasmoutput) : ptree;
    function gencasenode(l,r : ptree;nodes : pcaserecord) : ptree;
    function genwithnode(symtable : psymtable;l,r : ptree;count : longint) : ptree;

    function getcopy(p : ptree) : ptree;

    function equal_trees(t1,t2 : ptree) : boolean;

    procedure swaptree(p:Ptree);
    procedure disposetree(p : ptree);
    procedure putnode(p : ptree);
    function getnode : ptree;
    procedure set_file_line(from,_to : ptree);
    procedure set_tree_filepos(p : ptree;const filepos : tfileposinfo);
{$ifdef EXTDEBUG}
    procedure compare_trees(oldp,p : ptree);
    const
       maxfirstpasscount : longint = 0;

    { writes a complete tree, checks for nil }
    procedure writenode(n : pnode);
{$endif EXTDEBUG}

    { sets the callunique flag, if the node is a vecn, }
    { takes care of type casts etc.                    }
    procedure set_unique(p : ptree);

    { gibt den ordinalen Werten der Node zurueck oder falls sie }
    { keinen ordinalen Wert hat, wird ein Fehler erzeugt        }
    function get_ordinal_value(p : ptree) : longint;

    function is_constnode(p : ptree) : boolean;
    { true, if p is a pointer to a const int value }
    function is_constintnode(p : ptree) : boolean;
    function is_constboolnode(p : ptree) : boolean;
    function is_constrealnode(p : ptree) : boolean;
    function is_constcharnode(p : ptree) : boolean;
    function str_length(p : ptree) : longint;
    function is_emptyset(p : ptree):boolean;

{$I innr.inc}

  implementation

    uses
       systems,
       globals,verbose,files,types;

{$ifdef EXTDEBUG}

    const
       indention : string = '';

    procedure writenode(n : pnode);

      begin
         if assigned(n) then
           n^.dowrite
         else
           writeln(indention,'nil');
      end;
{$endif EXTDEBUG}
{****************************************************************************
                                 TNODE
 ****************************************************************************}

    constructor tnode.init;

      begin
         treetype:=nothingn;
         { this allows easier error tracing }
         location.loc:=LOC_INVALID;
         { save local info }
         fileinfo:=aktfilepos;
         localswitches:=aktlocalswitches;
         resulttype:=nil;
         registersint:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
      end;

    destructor tnode.done;

      begin
         { reference info }
         if (location.loc in [LOC_MEM,LOC_REFERENCE]) and
            assigned(location.reference.symbol) then
           stringdispose(location.reference.symbol);
{$ifdef EXTDEBUG}
         if firstpasscount>maxfirstpasscount then
            maxfirstpasscount:=firstpasscount;
{$endif EXTDEBUG}
      end;

    procedure tnode.pass_1;

      begin
         det_resulttype;
         det_temp;
      end;

    procedure tnode.det_resulttype;

      begin
         abstract;
      end;

    procedure tnode.det_temp;

      begin
         abstract;
      end;

    procedure tnode.secondpass;

      begin
         abstract;
      end;

{$ifdef EXTDEBUG}
    procedure tnode.dowrite;

      const treetype2str : array[ttreetyp] of string[20] = (
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
          'fixconstn',
          'umminusn',
          'asmn',
          'vecn',
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
          'nothingn',
          'loadvmtn');

      begin
         write(indention,'(',treetype2str[treetype]);
      end;
{$endif EXTDEBUG}

{****************************************************************************
                                 TLOADNODE
 ****************************************************************************}

    constructor tloadnode.init(v : pvarsym;st : psymtable);

      var
         p : ptree;

      begin
         inherited init;
         p^.treetype:=loadn;
         resulttype:=v^.definition;
         symtableentry:=v;
         symtable:=st;
         p^.is_first := False;
         p^.is_methodpointer:=false;

         { method pointer load nodes can use the left subtree }
         { !!!!! p^.left:=nil; }
      end;

    destructor tloadnode.done;

      begin
         inherited done;
         { method pointer load nodes can use the left subtree }
         { !!!!! dispose(left,done); }
      end;

{****************************************************************************
                                 TUNARYNODE
 ****************************************************************************}

    procedure tunarynode.dowrite;

      begin
         inherited dowrite;
         writeln(',');
         writenode(left);
         writeln(')');
         dec(byte(indention[0]),2);
      end;

{****************************************************************************
                                 TBLOCKNODE
 ****************************************************************************}

    constructor tblocknode.init;

      begin
         inherited init;
         treetype:=blockn;
      end;

{$ifdef dummy}
         { clean up the contents of a node }
         case p^.treetype of
          asmn : if assigned(p^.p_asm) then
                  dispose(p^.p_asm,done);
  stringconstn : begin
                   ansistringdispose(p^.value_str,p^.length);
                 end;
     setconstn : begin
                   if assigned(p^.value_set) then
                     dispose(p^.value_set);
                 end;
         end;
{$endif dummy}

    procedure deletecaselabels(p : pcaserecord);

      begin
         if assigned(p^.greater) then
           deletecaselabels(p^.greater);
         if assigned(p^.less) then
           deletecaselabels(p^.less);
         freelabel(p^._at);
         if p^.firstlabel then
          freelabel(p^.statement);
         dispose(p);
      end;

    procedure swaptree(p:Ptree);

    var swapp:Ptree;

    begin
        swapp:=p^.right;
        p^.right:=p^.left;
        p^.left:=swapp;
        p^.swaped:=not(p^.swaped);
    end;

    function gennode(t : ttreetyp;l,r : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_leftright;
         p^.treetype:=t;
         p^.left:=l;
         p^.right:=r;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         gennode:=p;
      end;

    procedure disposetree(p : ptree);

      var
         symt : psymtable;
         i : longint;

      begin
         if not(assigned(p)) then
           exit;
         case p^.disposetyp of
            dt_leftright :
              begin
                 if assigned(p^.left) then
                   disposetree(p^.left);
                 if assigned(p^.right) then
                   disposetree(p^.right);
              end;
            dt_case :
              begin
                 if assigned(p^.left) then
                   disposetree(p^.left);
                 if assigned(p^.right) then
                   disposetree(p^.right);
                 if assigned(p^.nodes) then
                   deletecaselabels(p^.nodes);
                 if assigned(p^.elseblock) then
                   disposetree(p^.elseblock);
              end;
            dt_nothing : ;
            dt_left    :
              if assigned(p^.left) then
                disposetree(p^.left);
            dt_mbleft :
              if assigned(p^.left) then
                disposetree(p^.left);
            dt_mbleft_and_method :
              begin
                 if assigned(p^.left) then disposetree(p^.left);
                 disposetree(p^.methodpointer);
              end;
            dt_typeconv : disposetree(p^.left);
            dt_inlinen :
              if assigned(p^.left) then
                disposetree(p^.left);
            dt_loop :
              begin
                 if assigned(p^.left) then
                   disposetree(p^.left);
                 if assigned(p^.right) then
                   disposetree(p^.right);
                 if assigned(p^.t1) then
                   disposetree(p^.t1);
                 if assigned(p^.t2) then
                   disposetree(p^.t2);
              end;
            dt_onn:
              begin
                 if assigned(p^.left) then
                   disposetree(p^.left);
                 if assigned(p^.right) then
                   disposetree(p^.right);
                 if assigned(p^.exceptsymtable) then
                   dispose(p^.exceptsymtable,done);
              end;
            dt_with :
              begin
                 if assigned(p^.left) then
                   disposetree(p^.left);
                 if assigned(p^.right) then
                   disposetree(p^.right);
                 symt:=p^.withsymtable;
                 for i:=1 to p^.tablecount do
                   begin
                      if assigned(symt) then
                        begin
                           p^.withsymtable:=symt^.next;
                           dispose(symt,done);
                        end;
                      symt:=p^.withsymtable;
                   end;
              end;
            else internalerror(12);
         end;
         putnode(p);
      end;

    procedure set_file_line(from,_to : ptree);

      begin
         if assigned(from) then
           _to^.fileinfo:=from^.fileinfo;
      end;

   procedure set_tree_filepos(p : ptree;const filepos : tfileposinfo);
     begin
        p^.fileinfo:=filepos;
     end;

    function getnode : ptree;

      var
         hp : ptree;

      begin
         new(hp);
         { makes error tracking easier }
         fillchar(hp^,sizeof(ttree),0);
         { reset }
         hp^.location.loc:=LOC_INVALID;
         { save local info }
         hp^.fileinfo:=aktfilepos;
         hp^.localswitches:=aktlocalswitches;
         getnode:=hp;
      end;


    procedure putnode(p : ptree);
      begin
         { clean up the contents of a node }
         case p^.treetype of
          asmn : if assigned(p^.p_asm) then
                  dispose(p^.p_asm,done);
  stringconstn : begin
                   ansistringdispose(p^.value_str,p^.length);
                 end;
     setconstn : begin
                   if assigned(p^.value_set) then
                     dispose(p^.value_set);
                 end;
         end;
         { reference info }
         if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
            assigned(p^.location.reference.symbol) then
           stringdispose(p^.location.reference.symbol);
{$ifdef extdebug}
         if p^.firstpasscount>maxfirstpasscount then
            maxfirstpasscount:=p^.firstpasscount;
{$endif extdebug}
         dispose(p);
      end;

    function getcopy(p : ptree) : ptree;

      var
         hp : ptree;

      begin
         hp:=getnode;
         hp^:=p^;
         if assigned(p^.location.reference.symbol) then
           hp^.location.reference.symbol:=stringdup(p^.location.reference.symbol^);
         case p^.disposetyp of
            dt_leftright :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
              end;
            dt_nothing : ;
            dt_left    :
              if assigned(p^.left) then
                hp^.left:=getcopy(p^.left);
            dt_mbleft :
              if assigned(p^.left) then
                hp^.left:=getcopy(p^.left);
            dt_mbleft_and_method :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 hp^.methodpointer:=getcopy(p^.methodpointer);
              end;
            dt_loop :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
                 if assigned(p^.t1) then
                   hp^.t1:=getcopy(p^.t1);
                 if assigned(p^.t2) then
                   hp^.t2:=getcopy(p^.t2);
              end;
            dt_typeconv : hp^.left:=getcopy(p^.left);
            dt_inlinen :
              if assigned(p^.left) then
                hp^.left:=getcopy(p^.left);
            else internalerror(11);
         end;
       { now check treetype }
         case p^.treetype of
  stringconstn : begin
                   hp^.value_str:=getpcharcopy(p);
                   hp^.length:=p^.length;
                 end;
     setconstn : begin
                   new(hp^.value_set);
                   hp^.value_set:=p^.value_set;
                 end;
         end;
         getcopy:=hp;
      end;

    function genloadnode(v : pvarsym;st : psymtable) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=loadn;
         p^.resulttype:=v^.definition;
         p^.symtableentry:=v;
         p^.symtable:=st;
         p^.is_first := False;
         p^.is_methodpointer:=false;
         { method pointer load nodes can use the left subtree }
         p^.disposetyp:=dt_left;
         p^.left:=nil;
         genloadnode:=p;
      end;

   function genwithnode(symtable : psymtable;l,r : ptree;count : longint) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_with;
         p^.treetype:=withn;
         p^.left:=l;
         p^.right:=r;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         p^.withsymtable:=symtable;
         p^.tablecount:=count;
         set_file_line(l,p);
         genwithnode:=p;
      end;

    function genfixconstnode(v : longint;def : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=fixconstn;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=def;
         p^.value:=v;
         genfixconstnode:=p;
      end;

    function gencallparanode(expr,next : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_leftright;
         p^.treetype:=callparan;
         p^.left:=expr;
         p^.right:=next;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.registersfpu:=0;
         p^.resulttype:=nil;
         p^.exact_match_found:=false;
         p^.is_colon_para:=false;
         set_file_line(expr,p);
         gencallparanode:=p;
      end;

    function gencasenode(l,r : ptree;nodes : pcaserecord) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_case;
         p^.treetype:=casen;
         p^.left:=l;
         p^.right:=r;
         p^.nodes:=nodes;
         p^.registers32:=0;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         set_file_line(l,p);
         gencasenode:=p;
      end;

    function genloopnode(t : ttreetyp;l,r,n1 : ptree;back : boolean) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_loop;
         p^.treetype:=t;
         p^.left:=l;
         p^.right:=r;
         p^.t1:=n1;
         p^.t2:=nil;
         p^.registers32:=0;
         p^.backward:=back;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         set_file_line(l,p);
         genloopnode:=p;
      end;

    function genordinalconstnode(v : longint;def : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=ordconstn;
         p^.registers32:=0;
         { p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=def;
         p^.value:=v;
         if p^.resulttype^.deftype=orddef then
          testrange(p^.resulttype,p^.value);
         genordinalconstnode:=p;
      end;

    function genenumnode(v : penumsym) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=ordconstn;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=v^.definition;
         p^.value:=v^.value;
         testrange(p^.resulttype,p^.value);
         genenumnode:=p;
      end;


    function genrealconstnode(v : bestreal) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=realconstn;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
{$ifdef i386}
         p^.resulttype:=c64floatdef;
         p^.value_real:=v;
         { default value is double }
         p^.realtyp:=ait_real_64bit;
{$endif}
{$ifdef m68k}
         p^.resulttype:=new(pfloatdef,init(s32real));
         p^.value_real:=v;
         { default value is double }
         p^.realtyp:=ait_real_32bit;
{$endif}
         p^.lab_real:=nil;
         genrealconstnode:=p;
      end;

    function genstringconstnode(const s : string) : ptree;

      var
         p : ptree;
         l : longint;
      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=stringconstn;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         l:=length(s);
         p^.length:=l;
         { stringdup write even past a #0 }
         getmem(p^.value_str,l+1);
         move(s[1],p^.value_str^,l);
         p^.value_str[l]:=#0;
         p^.lab_str:=nil;
         if cs_ansistrings in aktlocalswitches then
          begin
            p^.stringtype:=st_ansistring;
            p^.resulttype:=cansistringdef;
          end
         else
          begin
            p^.stringtype:=st_shortstring;
            p^.resulttype:=cshortstringdef;
          end;

         genstringconstnode:=p;
      end;

    function getpcharcopy(p : ptree) : pchar;
      var
         pc : pchar;
      begin
         pc:=nil;
         getmem(pc,p^.length+1);
         if pc=nil then
           Message(general_f_no_memory_left);
         move(p^.value_str^,pc^,p^.length+1);
         getpcharcopy:=pc;
      end;


    function genpcharconstnode(s : pchar;length : longint) : ptree;
      var
         p : ptree;
      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=stringconstn;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=cshortstringdef;
         p^.length:=length;
         p^.value_str:=s;
         p^.lab_str:=nil;
         genpcharconstnode:=p;
      end;


    function gensinglenode(t : ttreetyp;l : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_left;
         p^.treetype:=t;
         p^.left:=l;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         gensinglenode:=p;
      end;

    function genasmnode(p_asm : paasmoutput) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=asmn;
         p^.registers32:=4;
         p^.p_asm:=p_asm;
         p^.object_preserved:=false;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=8;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=8;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         genasmnode:=p;
      end;

    function genloadcallnode(v: pprocsym;st: psymtable): ptree;
      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=loadn;
         p^.resulttype:=v^.definition;
         p^.symtableentry:=v;
         p^.symtable:=st;
         p^.is_first := False;
         p^.disposetyp:=dt_nothing;
         genloadcallnode:=p;
      end;


    function gentypedconstloadnode(sym : ptypedconstsym;st : psymtable) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=loadn;
         p^.resulttype:=sym^.definition;
         p^.symtableentry:=pvarsym(sym);
         p^.symtable:=st;
         p^.disposetyp:=dt_nothing;
         gentypedconstloadnode:=p;
      end;

    function gentypeconvnode(node : ptree;t : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_typeconv;
         p^.treetype:=typeconvn;
         p^.left:=node;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.convtyp:=tc_equal;
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=t;
         p^.explizit:=false;
         set_file_line(node,p);
         gentypeconvnode:=p;
      end;

    function gentypenode(t : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=typen;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=generrordef;
         p^.typenodetype:=t;
         gentypenode:=p;
      end;

    function gencallnode(v : pprocsym;st : psymtable) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=calln;
         p^.symtableprocentry:=v;
         p^.symtableproc:=st;
         p^.unit_specific:=false;
         p^.no_check:=false;
         p^.return_value_used:=true;
         p^.disposetyp := dt_leftright;
         p^.methodpointer:=nil;
         p^.left:=nil;
         p^.right:=nil;
         p^.procdefinition:=nil;
         gencallnode:=p;
      end;

    function genmethodcallnode(v : pprocsym;st : psymtable;mp : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.treetype:=calln;
         p^.return_value_used:=true;
         p^.symtableprocentry:=v;
         p^.symtableproc:=st;
         p^.disposetyp:=dt_mbleft_and_method;
         p^.left:=nil;
         p^.right:=nil;
         p^.methodpointer:=mp;
         p^.procdefinition:=nil;
         genmethodcallnode:=p;
      end;

    function gensubscriptnode(varsym : pvarsym;l : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_left;
         p^.treetype:=subscriptn;
         p^.left:=l;
         p^.registers32:=0;
         p^.vs:=varsym;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         gensubscriptnode:=p;
      end;

   function genzeronode(t : ttreetyp) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=t;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         genzeronode:=p;
      end;

   function genlabelnode(t : ttreetyp;nr : plabel) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=t;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         { for security }
         { nr^.is_used:=true;}
         p^.labelnr:=nr;
         genlabelnode:=p;
      end;

    function genselfnode(_class : pdef) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_nothing;
         p^.treetype:=selfn;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=_class;
         genselfnode:=p;
      end;

   function geninlinenode(number : byte;is_const:boolean;l : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_inlinen;
         p^.treetype:=inlinen;
         p^.left:=l;
         p^.inlinenumber:=number;
         p^.inlineconst:=is_const;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=nil;
         geninlinenode:=p;
      end;


      { uses the callnode to create the new procinline node }
    function genprocinlinenode(callp,code : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_left;
         p^.treetype:=procinlinen;
         p^.inlineprocdef:=callp^.procdefinition;
         p^.retoffset:=-4; { less dangerous as zero (PM) }
         p^.para_offset:=0;
         p^.para_size:=p^.inlineprocdef^.para_size;
         if ret_in_param(p^.inlineprocdef^.retdef) then
           p^.para_size:=p^.para_size+target_os.size_of_pointer;
         { copy args }
         p^.left:=getcopy(code);
         p^.registers32:=code^.registers32;
         p^.registersfpu:=code^.registersfpu;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=p^.inlineprocdef^.retdef;
         genprocinlinenode:=p;
      end;

   function gensetconstnode(s : pconstset;settype : psetdef) : ptree;

     var
        p : ptree;

     begin
        p:=getnode;
        p^.disposetyp:=dt_nothing;
        p^.treetype:=setconstn;
        p^.registers32:=0;
        p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=settype;
         p^.left:=nil;
         new(p^.value_set);
         p^.value_set^:=s^;
         gensetconstnode:=p;
      end;

{$ifdef extdebug}
    procedure compare_trees(oldp,p : ptree);

      var
         error_found : boolean;

      begin
          if oldp^.resulttype<>p^.resulttype then
            begin
               error_found:=true;
               if is_equal(oldp^.resulttype,p^.resulttype) then
                 comment(v_debug,'resulttype fields are different but equal')
               else
                 comment(v_warning,'resulttype fields are really different');
            end;
         if oldp^.treetype<>p^.treetype then
           begin
              comment(v_warning,'treetype field different');
              error_found:=true;
           end
         else
           comment(v_debug,' treetype '+tostr(longint(oldp^.treetype)));
         if oldp^.error<>p^.error then
           begin
              comment(v_warning,'error field different');
              error_found:=true;
           end;
         if oldp^.disposetyp<>p^.disposetyp then
           begin
              comment(v_warning,'disposetyp field different');
              error_found:=true;
           end;
         { is true, if the right and left operand are swaped }
         if oldp^.swaped<>p^.swaped then
           begin
              comment(v_warning,'swaped field different');
              error_found:=true;
           end;

         { the location of the result of this node }
         if oldp^.location.loc<>p^.location.loc then
           begin
              comment(v_warning,'location.loc field different');
              error_found:=true;
           end;

          { the number of registers needed to evalute the node }
          if oldp^.registers32<>p^.registers32 then
           begin
              comment(v_warning,'registers32 field different');
              comment(v_warning,' old '+tostr(oldp^.registers32)+'<> new '+tostr(p^.registers32));
              error_found:=true;
           end;
          if oldp^.registersfpu<>p^.registersfpu then
           begin
              comment(v_warning,'registersfpu field different');
              error_found:=true;
           end;
{$ifdef SUPPORT_MMX}
          if oldp^.registersmmx<>p^.registersmmx then
           begin
              comment(v_warning,'registersmmx field different');
              error_found:=true;
           end;
{$endif SUPPORT_MMX}
          if oldp^.left<>p^.left then
           begin
              comment(v_warning,'left field different');
              error_found:=true;
           end;
          if oldp^.right<>p^.right then
           begin
              comment(v_warning,'right field different');
              error_found:=true;
           end;
          if oldp^.fileinfo.line<>p^.fileinfo.line then
            begin
               comment(v_warning,'fileinfo.line field different');
               error_found:=true;
            end;
          if oldp^.fileinfo.column<>p^.fileinfo.column then
            begin
               comment(v_warning,'fileinfo.column field different');
               error_found:=true;
            end;
          if oldp^.fileinfo.fileindex<>p^.fileinfo.fileindex then
            begin
               comment(v_warning,'fileinfo.fileindex field different');
               error_found:=true;
            end;
          if oldp^.localswitches<>p^.localswitches then
            begin
               comment(v_warning,'localswitches field different');
               error_found:=true;
            end;
{$ifdef extdebug}
          if oldp^.firstpasscount<>p^.firstpasscount then
            begin
               comment(v_warning,'firstpasscount field different');
               error_found:=true;
            end;
{$endif extdebug}
          if oldp^.treetype=p^.treetype then
          case oldp^.treetype of
             addn :
             begin
                if oldp^.use_strconcat<>p^.use_strconcat then
                  begin
                     comment(v_warning,'use_strconcat field different');
                     error_found:=true;
                  end;
                if oldp^.string_typ<>p^.string_typ then
                  begin
                     comment(v_warning,'stringtyp field different');
                     error_found:=true;
                  end;
             end;
             callparan :
             {(is_colon_para : boolean;exact_match_found : boolean);}
             begin
                if oldp^.is_colon_para<>p^.is_colon_para then
                  begin
                     comment(v_warning,'use_strconcat field different');
                     error_found:=true;
                  end;
                if oldp^.exact_match_found<>p^.exact_match_found then
                  begin
                     comment(v_warning,'exact_match_found field different');
                     error_found:=true;
                  end;
             end;
             assignn :
             {(assigntyp : tassigntyp;concat_string : boolean);}
             begin
                if oldp^.assigntyp<>p^.assigntyp then
                  begin
                     comment(v_warning,'assigntyp field different');
                     error_found:=true;
                  end;
                if oldp^.concat_string<>p^.concat_string then
                  begin
                     comment(v_warning,'concat_string field different');
                     error_found:=true;
                  end;
             end;
             loadn :
             {(symtableentry : psym;symtable : psymtable;
                      is_absolute,is_first : boolean);}
             begin
                if oldp^.symtableentry<>p^.symtableentry then
                  begin
                     comment(v_warning,'symtableentry field different');
                     error_found:=true;
                  end;
                if oldp^.symtable<>p^.symtable then
                  begin
                     comment(v_warning,'symtable field different');
                     error_found:=true;
                  end;
                if oldp^.is_absolute<>p^.is_absolute then
                  begin
                     comment(v_warning,'is_absolute field different');
                     error_found:=true;
                  end;
                if oldp^.is_first<>p^.is_first then
                  begin
                     comment(v_warning,'is_first field different');
                     error_found:=true;
                  end;
             end;
             calln :
             {(symtableprocentry : pprocsym;
                      symtableproc : psymtable;procdefinition : pprocdef;
                      methodpointer : ptree;
                      no_check,unit_specific : boolean);}
             begin
                if oldp^.symtableprocentry<>p^.symtableprocentry then
                  begin
                     comment(v_warning,'symtableprocentry field different');
                     error_found:=true;
                  end;
                if oldp^.symtableproc<>p^.symtableproc then
                  begin
                     comment(v_warning,'symtableproc field different');
                     error_found:=true;
                  end;
                if oldp^.procdefinition<>p^.procdefinition then
                  begin
                     comment(v_warning,'procdefinition field different');
                     error_found:=true;
                  end;
                if oldp^.methodpointer<>p^.methodpointer then
                  begin
                     comment(v_warning,'methodpointer field different');
                     error_found:=true;
                  end;
                if oldp^.no_check<>p^.no_check then
                  begin
                     comment(v_warning,'no_check field different');
                     error_found:=true;
                  end;
                if oldp^.unit_specific<>p^.unit_specific then
                  begin
                     error_found:=true;
                     comment(v_warning,'unit_specific field different');
                  end;
             end;
             ordconstn :
               begin
                  if oldp^.value<>p^.value then
                  begin
                     comment(v_warning,'value field different');
                     error_found:=true;
                  end;
               end;
             realconstn :
               begin
                  if oldp^.value_real<>p^.value_real then
                  begin
                     comment(v_warning,'valued field different');
                     error_found:=true;
                  end;
                  if oldp^.lab_real<>p^.lab_real then
                  begin
                     comment(v_warning,'labnumber field different');
                     error_found:=true;
                  end;
                  if oldp^.realtyp<>p^.realtyp then
                  begin
                     comment(v_warning,'realtyp field different');
                     error_found:=true;
                  end;
               end;
           end;
         if not error_found then
           comment(v_warning,'did not find difference in trees');

      end;
{$endif extdebug}

    function equal_trees(t1,t2 : ptree) : boolean;

      begin
         if t1^.treetype=t2^.treetype then
           begin
              case t1^.treetype of
                 addn,
                 muln,
                 equaln,
                 orn,
                 xorn,
                 andn,
                 unequaln:
                   begin
                      equal_trees:=(equal_trees(t1^.left,t2^.left) and
                                    equal_trees(t1^.right,t2^.right)) or
                                   (equal_trees(t1^.right,t2^.left) and
                                    equal_trees(t1^.left,t2^.right));
                   end;
                 subn,
                 divn,
                 modn,
                 assignn,
                 ltn,
                 lten,
                 gtn,
                 gten,
                 inn,
                 shrn,
                 shln,
                 slashn,
                 rangen:
                   begin
                      equal_trees:=(equal_trees(t1^.left,t2^.left) and
                                    equal_trees(t1^.right,t2^.right));
                   end;
                 umminusn,
                 notn,
                 derefn,
                 addrn:
                   begin
                      equal_trees:=(equal_trees(t1^.left,t2^.left));
                   end;
                loadn:
                   begin
                      equal_trees:=(t1^.symtableentry=t2^.symtableentry)
                        { not necessary
                                     and (t1^.symtable=t2^.symtable)};
                   end;
                {

                   subscriptn,
                   ordconstn,typeconvn,calln,callparan,
                   realconstn,asmn,vecn,
                   stringconstn,funcretn,selfn,
                   inlinen,niln,errorn,
                   typen,hnewn,hdisposen,newn,
                   disposen,setelen,setconstrn
                }
                else equal_trees:=false;
             end;
          end
        else
          equal_trees:=false;
     end;

    procedure set_unique(p : ptree);

      begin
         if assigned(p) then
           begin
              case p^.treetype of
                 vecn:
                    p^.callunique:=true;
                 typeconvn:
                    set_unique(p^.left);
              end;
           end;
      end;

    function get_ordinal_value(p : ptree) : longint;
      begin
         if p^.treetype=ordconstn then
           get_ordinal_value:=p^.value
         else
           Message(type_e_ordinal_expr_expected);
      end;


    function is_constnode(p : ptree) : boolean;
      begin
        is_constnode:=(p^.treetype in [ordconstn,realconstn,stringconstn,fixconstn,setconstn]);
      end;


    function is_constintnode(p : ptree) : boolean;
      begin
         is_constintnode:=(p^.treetype=ordconstn) and is_integer(p^.resulttype);
      end;


    function is_constcharnode(p : ptree) : boolean;

      begin
         is_constcharnode:=((p^.treetype=ordconstn) and
           (p^.resulttype^.deftype=orddef) and
           (porddef(p^.resulttype)^.typ=uchar));
      end;

    function is_constrealnode(p : ptree) : boolean;

      begin
         is_constrealnode:=(p^.treetype=realconstn);
      end;

    function is_constboolnode(p : ptree) : boolean;

      begin
         is_constboolnode:=((p^.treetype=ordconstn) and
           (p^.resulttype^.deftype=orddef) and
           (porddef(p^.resulttype)^.typ in [bool8bit,bool16bit,bool32bit]));
      end;

    function str_length(p : ptree) : longint;

      begin
         str_length:=p^.length;
      end;


    function is_emptyset(p : ptree):boolean;
    {
      return true if set s is empty
    }
      var
        i : longint;
      begin
        i:=0;
        if p^.treetype=setconstn then
         begin
           while (i<32) and (p^.value_set^[i]=0) do
            inc(i);
         end;
        is_emptyset:=(i=32);
      end;

end.
{
  $Log$
  Revision 1.3  1999-01-13 22:52:40  florian
    + YES, finally the new code generator is compilable, but it doesn't run yet :(

  Revision 1.2  1998/12/26 15:20:32  florian
    + more changes for the new version

  Revision 1.1  1998/12/15 22:21:53  florian
    * first rough conversion

}

