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
{$ifdef ag386bin}
       ,i386base
{$else}
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
{$ifdef alpha}
       ,alpha
{$endif}
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
          subscriptn,      {??? Field in a record/object?}
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

{$ifndef OLDCNV}
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
{$else}
       tconverttype = (tc_equal,tc_not_possible,tc_u8bit_2_s32bit,
                      tc_only_rangechecks32bit,tc_s8bit_2_s32bit,
                      tc_u16bit_2_s32bit,tc_s16bit_2_s32bit,
                      tc_s32bit_2_s16bit,tc_s32bit_2_u8bit,
                      tc_s32bit_2_u16bit,tc_string_2_string,
                      tc_cstring_2_pchar,tc_string_2_chararray,
                      tc_array_2_pointer,tc_pointer_2_array,
                      tc_char_2_string,tc_u8bit_2_s16bit,
                      tc_u8bit_2_u16bit,tc_s8bit_2_s16bit,
                      tc_s16bit_2_s8bit,tc_s16bit_2_u8bit,
                      tc_u16bit_2_s8bit,tc_u16bit_2_u8bit,
                      tc_s8bit_2_u16bit,tc_s32bit_2_s8bit,
                      tc_s32bit_2_u32bit,tc_s16bit_2_u32bit,
                      tc_s8bit_2_u32bit,tc_u16bit_2_u32bit,
                      tc_u8bit_2_u32bit,tc_u32bit_2_s32bit,
                      tc_u32bit_2_s8bit,tc_u32bit_2_u8bit,
                      tc_u32bit_2_s16bit,tc_u32bit_2_u16bit,
                      tc_bool_2_int,tc_int_2_bool,
                      tc_int_2_real,tc_real_2_fix,
                      tc_fix_2_real,tc_int_2_fix,tc_real_2_real,
                      tc_chararray_2_string,
                      tc_proc_2_procvar,tc_cchar_2_pchar,tc_load_smallset,
                      tc_ansistring_2_pchar,tc_pchar_2_string,
                      tc_arrayconstructor_2_set);
{$endif}

       { allows to determine which elementes are to be replaced }
       tdisposetyp = (dt_nothing,dt_leftright,dt_left,dt_leftrighthigh,
                      dt_mbleft,dt_typeconv,dt_inlinen,
                      dt_mbleft_and_method,dt_loop,dt_case,dt_with,dt_onn);

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

       ptree = ^ttree;
       ttree = record
          error : boolean;
          disposetyp : tdisposetyp;
          { is true, if the right and left operand are swaped }
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
{$ifdef extdebug}
          firstpasscount : longint;
{$endif extdebug}
          case treetype : ttreetyp of
             addn : (use_strconcat : boolean;string_typ : tstringtype);
             callparan : (is_colon_para : boolean;exact_match_found : boolean;hightree:ptree);
             assignn : (assigntyp : tassigntyp;concat_string : boolean);
             loadn : (symtableentry : psym;symtable : psymtable;
                      is_absolute,is_first : boolean);
             calln : (symtableprocentry : psym;
                      symtableproc : psymtable;procdefinition : pprocdef;
                      methodpointer : ptree;
                      no_check,unit_specific,
                      return_value_used,static_call : boolean);
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
             withn : (withsymtable : pwithsymtable;tablecount : longint;
                     pref : preference);
             onn : (exceptsymtable : psymtable;excepttype : pobjectdef);
             arrayconstructn : (cargs,cargswap: boolean);
           end;

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
    function genwithnode(symtable : pwithsymtable;l,r : ptree;count : longint) : ptree;

    function getcopy(p : ptree) : ptree;

    function equal_trees(t1,t2 : ptree) : boolean;

    procedure swaptree(p:Ptree);
    procedure disposetree(p : ptree);
    procedure putnode(p : ptree);
    function getnode : ptree;
    procedure clear_location(var loc : tlocation);
    procedure set_location(var destloc,sourceloc : tlocation);
    procedure swap_location(var destloc,sourceloc : tlocation);
    procedure set_file_line(from,_to : ptree);
    procedure set_tree_filepos(p : ptree;const filepos : tfileposinfo);
{$ifdef extdebug}
    procedure compare_trees(oldp,p : ptree);
    const
       maxfirstpasscount : longint = 0;
{$endif extdebug}

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
            dt_leftrighthigh :
              begin
                 if assigned(p^.left) then
                   hp^.left:=getcopy(p^.left);
                 if assigned(p^.right) then
                   hp^.right:=getcopy(p^.right);
                 if assigned(p^.hightree) then
                   hp^.left:=getcopy(p^.hightree);
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


    procedure disposetree(p : ptree);

      var
         symt : pwithsymtable;
         i : longint;

      begin
         if not(assigned(p)) then
           exit;
         if not(p^.treetype in [addn..loadvmtn]) then
           internalerror(26219);
         case p^.disposetyp of
            dt_leftright :
              begin
                 if assigned(p^.left) then
                   disposetree(p^.left);
                 if assigned(p^.right) then
                   disposetree(p^.right);
              end;
            dt_leftrighthigh :
              begin
                 if assigned(p^.left) then
                   disposetree(p^.left);
                 if assigned(p^.right) then
                   disposetree(p^.right);
                 if assigned(p^.hightree) then
                   disposetree(p^.hightree);
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
                           p^.withsymtable:=pwithsymtable(symt^.next);
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

   function genwithnode(symtable : pwithsymtable;l,r : ptree;count : longint) : ptree;

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
         p^.pref:=nil;
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
         p^.disposetyp:=dt_leftrighthigh;
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
         p^.hightree:=nil;
         set_file_line(expr,p);
         gencallparanode:=p;
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
         { method pointer load nodes can use the left subtree }
         p^.disposetyp:=dt_left;
         p^.left:=nil;
         genloadnode:=p;
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
         p^.left:=nil;
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
         p^.left:=nil;
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

    procedure clear_location(var loc : tlocation);

      begin
        if ((loc.loc=LOC_MEM) or (loc.loc=LOC_REFERENCE)) and
           assigned(loc.reference.symbol) then
          stringdispose(loc.reference.symbol);
        loc.loc:=LOC_INVALID;
      end;

    {This is needed if you want to be able to delete the string with the nodes !!}
    procedure set_location(var destloc,sourceloc : tlocation);

      begin
        if assigned(destloc.reference.symbol) then
          stringdispose(destloc.reference.symbol);
        destloc:= sourceloc;
        if sourceloc.loc in [LOC_MEM,LOC_REFERENCE] then
          begin
             if assigned(sourceloc.reference.symbol) then
               destloc.reference.symbol:=
                 stringdup(sourceloc.reference.symbol^);
          end
        else
          destloc.reference.symbol:=nil;
      end;

    procedure swap_location(var destloc,sourceloc : tlocation);

      var
         swapl : tlocation;

      begin
         swapl := destloc;
         destloc := sourceloc;
         sourceloc := swapl;
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
  Revision 1.66  1999-02-22 02:15:59  peter
    * updates for ag386bin

  Revision 1.65  1999/02/11 09:46:31  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.64  1999/01/27 12:57:22  pierre
   * memory leaks with hightree solved by adding a new disposetyp
     dt_leftrighthigh

  Revision 1.63  1999/01/27 00:14:00  florian
    * "procedure of object"-stuff fixed

  Revision 1.62  1999/01/21 22:10:52  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.61  1999/01/21 16:41:09  pierre
   * fix for constructor inside with statements

  Revision 1.60  1998/12/15 11:52:19  peter
    * fixed dup release of statement label in case

  Revision 1.59  1998/12/15 10:23:32  peter
    + -iSO, -iSP, -iTO, -iTP

  Revision 1.58  1998/12/11 00:04:02  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.57  1998/12/04 10:18:13  florian
    * some stuff for procedures of object added
    * bug with overridden virtual constructors fixed (reported by Italo Gomes)

  Revision 1.56  1998/12/02 16:23:32  jonas
    * changed "if longintvar in set" to case or "if () or () .." statements
    * tree.pas: changed inlinenumber (and associated constructor/vars) to a byte

  Revision 1.55  1998/11/29 12:40:20  peter
    * newcnv -> not oldcnv

  Revision 1.54  1998/11/26 13:10:44  peter
    * new int - int conversion -dNEWCNV
    * some function renamings

  Revision 1.53  1998/11/24 12:52:42  peter
    * sets are not written twice anymore
    * optimize for emptyset+single element which uses a new routine from
      set.inc FPC_SET_CREATE_ELEMENT

  Revision 1.52  1998/11/23 17:51:58  pierre
   * added checking before dispose of reference string

  Revision 1.51  1998/11/13 10:15:53  peter
    * fixed ptr() with constants

  Revision 1.50  1998/11/10 10:09:20  peter
    * va_list -> array of const

  Revision 1.49  1998/11/05 12:03:07  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.48  1998/10/21 15:12:59  pierre
    * bug fix for IOCHECK inside a procedure with iocheck modifier
    * removed the GPF for unexistant overloading
      (firstcall was called with procedinition=nil !)
    * changed typen to what Florian proposed
      gentypenode(p : pdef) sets the typenodetype field
      and resulttype is only set if inside bt_type block !

  Revision 1.47  1998/10/20 08:07:07  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.46  1998/10/08 17:17:37  pierre
    * current_module old scanner tagged as invalid if unit is recompiled
    + added ppheap for better info on tracegetmem of heaptrc
      (adds line column and file index)
    * several memory leaks removed ith help of heaptrc !!

  Revision 1.45  1998/10/05 21:33:33  peter
    * fixed 161,165,166,167,168

  Revision 1.44  1998/09/28 16:57:28  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.43  1998/09/27 10:16:28  florian
    * type casts pchar<->ansistring fixed
    * ansistring[..] calls does now an unique call

  Revision 1.42  1998/09/23 12:03:59  peter
    * overloading fix for array of const

  Revision 1.41  1998/09/23 09:58:55  peter
    * first working array of const things

  Revision 1.40  1998/09/22 15:34:07  peter
    + pchar -> string conversion

  Revision 1.39  1998/09/21 08:45:27  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.38  1998/09/16 01:06:47  carl
    * crash bugfix in firstaddr

  Revision 1.37  1998/09/08 10:38:04  pierre
    * some variable fields inside conditionnal were not updated

  Revision 1.36  1998/09/07 18:46:17  peter
    * update smartlinking, uses getdatalabel
    * renamed ptree.value vars to value_str,value_real,value_set

  Revision 1.35  1998/09/04 08:42:11  peter
    * updated some error messages

  Revision 1.34  1998/09/01 17:39:54  peter
    + internal constant functions

  Revision 1.33  1998/08/28 12:51:44  florian
    + ansistring to pchar type cast fixed

  Revision 1.32  1998/08/28 10:54:25  peter
    * fixed smallset generation from elements, it has never worked before!

  Revision 1.31  1998/08/21 14:08:58  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.30  1998/08/18 09:24:47  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.29  1998/08/14 18:18:48  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.28  1998/08/13 11:00:13  peter
    * fixed procedure<>procedure construct

  Revision 1.27  1998/08/10 14:50:35  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.26  1998/08/10 09:57:19  peter
    - Remove InitTree which was empty and obsolete

  Revision 1.25  1998/08/02 16:42:02  florian
    * on o : tobject do should also work now, the exceptsymtable shouldn't be
      disposed by dellexlevel

  Revision 1.24  1998/07/30 11:18:23  florian
    + first implementation of try ... except on .. do end;
    * limitiation of 65535 bytes parameters for cdecl removed

  Revision 1.23  1998/07/24 22:17:01  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.22  1998/07/20 10:23:05  florian
    * better ansi string assignement

  Revision 1.21  1998/07/14 21:46:56  peter
    * updated messages file

  Revision 1.20  1998/07/14 14:47:11  peter
    * released NEWINPUT

  Revision 1.19  1998/07/08 14:56:53  daniel
  * Fixed $ifdef TP.

  Revision 1.18  1998/07/07 11:20:18  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.17  1998/06/22 08:59:03  daniel
  - Removed pool of nodes.

  Revision 1.16  1998/06/12 14:50:49  peter
    * removed the tree dependency to types.pas
    * long_fil.pas support (not fully tested yet)

  Revision 1.15  1998/06/06 08:39:07  peter
    * it needs types

  Revision 1.14  1998/06/05 14:37:40  pierre
    * fixes for inline for operators
    * inline procedure more correctly restricted

  Revision 1.13  1998/06/04 09:55:49  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Revision 1.12  1998/06/03 22:49:06  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.10  1998/05/20 09:42:38  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.9  1998/05/12 10:47:00  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.8  1998/05/07 00:17:01  peter
    * smartlinking for sets
    + consts labels are now concated/generated in hcodegen
    * moved some cpu code to cga and some none cpu depended code from cga
      to tree and hcodegen and cleanup of hcodegen
    * assembling .. output reduced for smartlinking ;)

  Revision 1.7  1998/05/06 15:04:21  pierre
    + when trying to find source files of a ppufile
      check the includepathlist for included files
      the main file must still be in the same directory

  Revision 1.6  1998/05/06 08:38:52  pierre
    * better position info with UseTokenInfo
      UseTokenInfo greatly simplified
    + added check for changed tree after first time firstpass
      (if we could remove all the cases were it happen
      we could skip all firstpass if firstpasscount > 1)
      Only with ExtDebug

  Revision 1.5  1998/04/30 15:59:43  pierre
    * GDB works again better :
      correct type info in one pass
    + UseTokenInfo for better source position
    * fixed one remaining bug in scanner for line counts
    * several little fixes

  Revision 1.4  1998/04/29 10:34:08  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/21 10:16:49  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.2  1998/04/07 22:45:05  florian
    * bug0092, bug0115 and bug0121 fixed
    + packed object/class/array
}

