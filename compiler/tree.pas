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
  {E+,N+}
{$endif}
unit tree;

  interface

    uses
       cobjects,globals,symtable,aasm
{$ifdef i386}
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

       ttreetyp = (addn,            {Represents the + operator.}
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
                   setelen,         {A set element (i.e. [a,b]).}
                   setconstrn,      {A set constant (i.e. [1,2]).}
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
                   isn,             {Represents the is operator.}
                   asn,             {Represents the as typecast.}
                   caretn,          {Represents the ^ operator.}
                   failn,           {Represents the fail statement.}
                   starstarn,       {Represents the ** operator exponentiation }
                   procinlinen,      {Procedures that can be inlined }
                   { added for optimizations where we cannot suppress }
                   nothingn,
                   loadvmtn);       {???.}

       tconverttype = (tc_equal,tc_not_possible,tc_u8bit_2_s32bit,
                      tc_only_rangechecks32bit,tc_s8bit_2_s32bit,
                      tc_u16bit_2_s32bit,tc_s16bit_2_s32bit,
                      tc_s32bit_2_s16bit,tc_s32bit_2_u8bit,
                      tc_s32bit_2_u16bit,tc_string_to_string,
                      tc_cstring_charpointer,tc_string_chararray,
                      tc_array_to_pointer,tc_pointer_to_array,
                      tc_char_to_string,tc_u8bit_2_s16bit,
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
                      tc_proc2procvar,tc_cchar_charpointer);

       { allows to determine which elementes are to be replaced }
       tdisposetyp = (dt_nothing,dt_leftright,dt_left,
                      dt_mbleft,dt_string,dt_typeconv,dt_inlinen,
                      dt_mbleft_and_method,dt_constset,dt_loop,dt_case,
                      dt_with);

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
          { line : longint;
          fileindex,colon : word; }
          fileinfo : tfileposinfo;
          pragmas : Tcswitches;
{$ifdef extdebug}
        firstpasscount : longint;
{$endif extdebug}
          case treetype : ttreetyp of
             addn : (use_strconcat : boolean;string_typ : stringtype);
             callparan : (is_colon_para : boolean;exact_match_found : boolean);
             assignn : (assigntyp : tassigntyp;concat_string : boolean);
             loadn : (symtableentry : psym;symtable : psymtable;
                      is_absolute,is_first : boolean);
             calln : (symtableprocentry : pprocsym;
                      symtableproc : psymtable;procdefinition : pprocdef;
                      methodpointer : ptree;
                      no_check,unit_specific,return_value_used : boolean);
             ordconstn : (value : longint);
             realconstn : (valued : bestreal;labnumber : longint;realtyp : tait);
             fixconstn : (valuef: longint);
{$ifdef TEST_FUNCRET}
             funcretn : (funcretprocinfo : pointer;retdef : pdef);
{$endif TEST_FUNCRET}
             subscriptn : (vs : pvarsym);
             vecn : (memindex,memseg:boolean);
             { stringconstn : (length : longint; values : pstring;labstrnumber : longint); }
             { string const can be longer then 255 with ansistring !! }
{$ifdef UseAnsiString}
             stringconstn : (values : pchar;length : longint; labstrnumber : longint);
{$else UseAnsiString}
             stringconstn : (values : pstring; labstrnumber : longint);
{$endif UseAnsiString}
             typeconvn : (convtyp : tconverttype;explizit : boolean);
             inlinen : (inlinenumber : longint);
             procinlinen : (inlineprocdef : pprocdef;
                            retoffset,para_offset,para_size : longint);
             setconstrn : (constset : pconstset);
             loopn : (t1,t2 : ptree;backward : boolean);
             asmn : (p_asm : paasmoutput;object_preserved : boolean);
             casen : (nodes : pcaserecord;elseblock : ptree);
             labeln,goton : (labelnr : plabel);
             withn : (withsymtable : psymtable;tablecount : longint);
           end;

    procedure init_tree;
    function gennode(t : ttreetyp;l,r : ptree) : ptree;
    function genlabelnode(t : ttreetyp;nr : plabel) : ptree;
    function genloadnode(v : pvarsym;st : psymtable) : ptree;
    function gensinglenode(t : ttreetyp;l : ptree) : ptree;
    function gensubscriptnode(varsym : pvarsym;l : ptree) : ptree;
    function genordinalconstnode(v : longint;def : pdef) : ptree;
    function genfixconstnode(v : longint;def : pdef) : ptree;
    function gentypeconvnode(node : ptree;t : pdef) : ptree;
    function gencallparanode(expr,next : ptree) : ptree;
    function genrealconstnode(v : bestreal) : ptree;
    function gencallnode(v : pprocsym;st : psymtable) : ptree;
    function genmethodcallnode(v : pprocsym;st : psymtable;mp : ptree) : ptree;

    { allow pchar or string for defining a pchar node }
    function genstringconstnode(const s : string) : ptree;
{$ifdef UseAnsiString}
    { length is required for ansistrings }
    function genpcharconstnode(s : pchar;length : longint) : ptree;
    { helper routine for conststring node }
    function getpcharcopy(p : ptree) : pchar;
{$endif UseAnsiString}

    function genzeronode(t : ttreetyp) : ptree;
    function geninlinenode(number : longint;l : ptree) : ptree;
    function genprocinlinenode(callp,code : ptree) : ptree;
    function gentypedconstloadnode(sym : ptypedconstsym;st : psymtable) : ptree;
    function genenumnode(v : penumsym) : ptree;
    function genselfnode(_class : pdef) : ptree;
    function gensetconstruktnode(s : pconstset;settype : psetdef) : ptree;
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
    procedure clearnodes;
    procedure set_location(var destloc,sourceloc : tlocation);
    procedure swap_location(var destloc,sourceloc : tlocation);
    procedure set_file_line(from,_to : ptree);
    procedure set_current_file_line(_to : ptree);
    procedure set_tree_filepos(p : ptree;const filepos : tfileposinfo);
{$ifdef extdebug}
    procedure compare_trees(oldp,p : ptree);
    const
       maxfirstpasscount : longint = 0;
{$endif extdebug}

{$I innr.inc}

  implementation

    uses
{$ifdef extdebug}
       types,
{$endif extdebug}
       verbose,files;

{****************************************************************************
        this is a pool for the tree nodes to get more performance
 ****************************************************************************}

    var
       root : ptree;

    procedure init_tree;

      begin
         root:=nil;
      end;

    procedure clearnodes;

      var
         hp : ptree;

      begin
         hp:=root;
         while assigned(hp) do
           begin
              root:=hp^.left;
              dispose(hp);
              hp:=root;
           end;
      end;

    function getnode : ptree;

      var
         hp : ptree;

      begin
         if root=nil then
           new(hp)
         else
           begin
              hp:=root;
              root:=root^.left;
           end;

         { makes error tracking easier }
         fillchar(hp^,sizeof(ttree),#0);
         hp^.location.loc:=LOC_INVALID;

         { new node is error free }
         hp^.error:=false;

         { we know also the position }
         hp^.fileinfo:=tokenpos;
         hp^.pragmas:=aktswitches;
         getnode:=hp;
      end;

    procedure putnode(p : ptree);

      begin
         { clean up the contents of a node }
         if p^.treetype=asmn then
           if assigned(p^.p_asm) then
             dispose(p^.p_asm,done);

         if p^.treetype=setconstrn then
          if assigned(p^.constset) then
            dispose(p^.constset);

         if (p^.location.loc=LOC_MEM) or (p^.location.loc=LOC_REFERENCE) and
           assigned(p^.location.reference.symbol) then
           stringdispose(p^.location.reference.symbol);

{$ifndef UseAnsiString}
         if p^.disposetyp=dt_string then
           stringdispose(p^.values);
{$else UseAnsiString}
         if p^.disposetyp=dt_string then
           ansistringdispose(p^.values,p^.length);
{$endif UseAnsiString}
{$ifdef extdebug}
         if p^.firstpasscount>maxfirstpasscount then
            maxfirstpasscount:=p^.firstpasscount;
         dispose(p);
{$else extdebug}
         p^.left:=root;
         root:=p;
{$endif extdebug}
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
{$ifdef UseAnsiString}
            dt_string : begin
                           hp^.values:=getpcharcopy(p);
                           hp^.length:=p^.length;
                        end;
{$else UseAnsiString}
            dt_string : hp^.values:=stringdup(p^.values^);
{$endif UseAnsiString}
            dt_typeconv : hp^.left:=getcopy(p^.left);
            dt_inlinen :
              if assigned(p^.left) then
                hp^.left:=getcopy(p^.left);
            else internalerror(11);
         end;
         getcopy:=hp;
      end;

    procedure deletecaselabels(p : pcaserecord);

      begin
         if assigned(p^.greater) then
           deletecaselabels(p^.greater);
         if assigned(p^.less) then
           deletecaselabels(p^.less);
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
{$ifdef UseAnsiString}
            dt_string : ansistringdispose(p^.values,p^.length);
{$else UseAnsiString}
            dt_string : stringdispose(p^.values);
{$endif UseAnsiString}
            dt_constset :
              begin
                 if assigned(p^.constset) then
                   begin
                      dispose(p^.constset);
                      p^.constset:=nil;
                   end;
                 if assigned(p^.left) then
                   disposetree(p^.left);
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
            dt_with :
              begin
                 if assigned(p^.left) then
                   disposetree(p^.left);
                 if assigned(p^.right) then
                   disposetree(p^.right);
                 if assigned(p^.withsymtable) then
                   dispose(p^.withsymtable,done);
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

    procedure set_current_file_line(_to : ptree);

      begin
         current_module^.current_inputfile:=
           pinputfile(current_module^.sourcefiles.get_file(_to^.fileinfo.fileindex));
         current_module^.current_inputfile^.line_no:=_to^.fileinfo.line;
         current_module^.current_index:=_to^.fileinfo.fileindex;
      end;

   procedure set_tree_filepos(p : ptree;const filepos : tfileposinfo);
     begin
        p^.fileinfo:=filepos;
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
         p^.valued:=v;
         { default value is double }
         p^.realtyp:=ait_real_64bit;
{$endif}
{$ifdef m68k}
         p^.resulttype:=new(pfloatdef,init(s32real));
         p^.valued:=v;
         { default value is double }
         p^.realtyp:=ait_real_32bit;
{$endif}
         p^.labnumber:=-1;
         genrealconstnode:=p;
      end;

    function genstringconstnode(const s : string) : ptree;

      var
         p : ptree;
{$ifdef UseAnsiString}
         l : longint;
{$endif UseAnsiString}
      begin
         p:=getnode;
         p^.disposetyp:=dt_string;
         p^.treetype:=stringconstn;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=cstringdef;
{$ifdef UseAnsiString}
         l:=length(s);
         p^.length:=l;
         { stringdup write even past a #0 }
         getmem(p^.values,l+1);
         move(s[1],p^.values^,l);
         p^.values[l]:=#0;
{$else UseAnsiString}
         p^.values:=stringdup(s);
{$endif UseAnsiString}
         p^.labstrnumber:=-1;
         genstringconstnode:=p;
      end;

{$ifdef UseAnsiString}
    function getpcharcopy(p : ptree) : pchar;

      var
         pc : pchar;

      begin
         pc:=nil;
         getmem(pc,p^.length+1);
         { Peter can you change that ? }
         if pc=nil then
           comment(V_fatal,'No memory left');
         move(p^.values^,pc^,p^.length+1);
         getpcharcopy:=pc;
      end;

    function genpcharconstnode(s : pchar;length : longint) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_string;
         p^.treetype:=stringconstn;
         p^.registers32:=0;
{         p^.registers16:=0;
         p^.registers8:=0; }
         p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=cstringdef;
         p^.length:=length;
         p^.values:=s;
         p^.labstrnumber:=-1;
         genpcharconstnode:=p;
      end;
{$endif UseAnsiString}

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
         p^.disposetyp:=dt_nothing;
         genloadnode:=p;
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

   function geninlinenode(number : longint;l : ptree) : ptree;

      var
         p : ptree;

      begin
         p:=getnode;
         p^.disposetyp:=dt_inlinen;
         p^.treetype:=inlinen;
         p^.left:=l;
         p^.inlinenumber:=number;
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

   function gensetconstruktnode(s : pconstset;settype : psetdef) : ptree;

     var
        p : ptree;

     begin
        p:=getnode;
        p^.disposetyp:=dt_constset;
        p^.treetype:=setconstrn;
        p^.registers32:=0;
        p^.registersfpu:=0;
{$ifdef SUPPORT_MMX}
         p^.registersmmx:=0;
{$endif SUPPORT_MMX}
         p^.resulttype:=settype;
         p^.left:=nil;
         new(p^.constset);
         p^.constset^:=s^;
         gensetconstruktnode:=p;
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
          if oldp^.pragmas<>p^.pragmas then
            begin
               comment(v_warning,'pragmas field different');
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
                  if oldp^.valued<>p^.valued then
                  begin
                     comment(v_warning,'valued field different');
                     error_found:=true;
                  end;
                  if oldp^.labnumber<>p^.labnumber then
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
             (*realconstn : (valued : bestreal;labnumber : longint;realtyp : tait);
             fixconstn : (valuef: longint);
{$ifdef TEST_FUNCRET}
             funcretn : (funcretprocinfo : pointer;retdef : pdef);
{$endif TEST_FUNCRET}
             subscriptn : (vs : pvarsym);
             vecn : (memindex,memseg:boolean);
             { stringconstn : (length : longint; values : pstring;labstrnumber : longint); }
             { string const can be longer then 255 with ansistring !! }
{$ifdef UseAnsiString}
             stringconstn : (values : pchar;length : longint; labstrnumber : longint);
{$else UseAnsiString}
             stringconstn : (values : pstring; labstrnumber : longint);
{$endif UseAnsiString}
             typeconvn : (convtyp : tconverttype;explizit : boolean);
             inlinen : (inlinenumber : longint);
             procinlinen : (inlineprocdef : pprocdef);
             setconstrn : (constset : pconstset);
             loopn : (t1,t2 : ptree;backward : boolean);
             asmn : (p_asm : paasmoutput);
             casen : (nodes : pcaserecord;elseblock : ptree);
             labeln,goton : (labelnr : plabel);
             withn : (withsymtable : psymtable;tablecount : longint);
           end; *)
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
end.
{
  $Log$
  Revision 1.13  1998-06-04 09:55:49  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Come test_funcret improvements (not yet working)S: ----------------------------------------------------------------------

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

