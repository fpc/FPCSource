{
    $Id$
    Copyright (c) 1999-2000 by Florian Klaempfl

    The declarations of the nodes for the new code generator

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
    type
       pconstset = ^tconstset;
       tconstset = array[0..31] of byte;

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
          fixconstn,       {Represents a fixed value.}
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
          tc_int_2_bool,
          tc_bool_2_bool,
          tc_bool_2_int,
          tc_real_2_real,
          tc_int_2_real,
          tc_int_2_fix,
          tc_real_2_fix,
          tc_fix_2_real,
          tc_proc_2_procvar,
          tc_arrayconstructor_2_set,
          tc_load_smallset,
          tc_cord_2_pointer
       );

      pcaserecord = ^tcaserecord;
      tcaserecord = record
          { range }
          _low,_high : longint;

          { only used by gentreejmp }
          _at : pasmlabel;

          { label of instruction }
          statement : pasmlabel;

          { is this the first of an case entry, needed to release statement
            label (PFV) }
          firstlabel : boolean;

          { left and right tree node }
          less,greater : pcaserecord;
       end;

       { all boolean field of ttree are now collected in flags }
       tnodeflags = (
         nf_needs_truefalselabel,
         nf_swapable,    { tbinop operands can be swaped }
         nf_swaped,      { tbinop operands are swaped    }
         nf_error,

         { flags used by tcallnode }
         nf_no_check,
         nf_unit_specific,
         nf_return_value_used,
         nf_static_call,

         { flags used by tcallparanode }
         nf_exact_match_found,
         nf_convlevel1found,
         nf_convlevel2found,
         nf_is_colon_para,

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
         nf_absolute,   { 20th }
         nf_first,

         { tassignmentnode }
         nf_concat_string,

         { tfuncretnode }
         nf_is_first_funcret,

         { tarrayconstructnode }
         nf_cargs,
         nf_cargswap,
         nf_forcevaria,
         nf_novariaallowed,

         { ttypeconvnode }
         nf_explizit,

         { tinlinenode }
         nf_inlineconst
       );

       tnodeflagset = set of tnodeflags;

    const
       { contains the flags which must be equal for the equality }
       { of nodes                                                }
       flagsequal : tnodeflagset = [nf_error,nf_static_call,nf_backward];

    type
       { later (for the newcg) tnode will inherit from tlinkedlist_item }
       tnode = class
          nodetype : tnodetype;
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
          resulttype : pdef;
          fileinfo : tfileposinfo;
          localswitches : tlocalswitches;
{$ifdef extdebug}
          firstpasscount : longint;
{$endif extdebug}
          list : paasmoutput;
          constructor create(tt : tnodetype);
          { this constructor is only for creating copies of class }
          { the fields are copied by getcopy                      }
          constructor createforcopy;
          destructor destroy;virtual;

          { the 1.1 code generator may override pass_1 }
          { and it need not to implement det_* then    }
          { 1.1: pass_1 returns a value<>0 if the node has been transformed }
          { 2.0: runs det_resulttype and det_temp                           }
          function pass_1 : tnode;virtual;
          { dermines the resulttype of the node }
          procedure det_resulttype;virtual;abstract;
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
          procedure unset_varstate;virtual;
          procedure set_varstate(must_be_valid : boolean);virtual;
{$ifdef EXTDEBUG}
          { writes a node for debugging purpose, shouldn't be called }
          { direct, because there is no test for nil, use writenode  }
          { to write a complete tree                                 }
          procedure dowrite;virtual;
{$endif EXTDEBUG}
          procedure concattolist(l : plinkedlist);virtual;
          function ischild(p : tnode) : boolean;virtual;
          procedure set_file_line(from : tnode);
          procedure set_tree_filepos(const filepos : tfileposinfo);
       end;

       { this node is the anchestor for all nodes with at least   }
       { one child, you have to use it if you want to use         }
       { true- and falselabel                                     }
       tparentnode = class(tnode)
          falselabel,truelabel : pasmlabel;
       end;

       punarynode = ^tunarynode;
       tunarynode = class(tparentnode)
          left : tnode;
{$ifdef extdebug}
          procedure dowrite;override;
{$endif extdebug}
          constructor create(tt : tnodetype;l : tnode);
          procedure concattolist(l : plinkedlist);override;
          function ischild(p : tnode) : boolean;override;
          procedure det_resulttype;override;
          procedure det_temp;override;
          function docompare(p : tnode) : boolean;override;
          function getcopy : tnode;override;
       end;

       pbinarynode = ^tbinarynode;
       tbinarynode = class(tunarynode)
          right : tnode;
          constructor create(tt : tnodetype;l,r : tnode);
          procedure concattolist(l : plinkedlist);override;
          function ischild(p : tnode) : boolean;override;
          procedure det_resulttype;override;
          procedure det_temp;override;
          function docompare(p : tnode) : boolean;override;
          procedure swapleftright;
          function isbinaryoverloaded(var t : tnode) : boolean;
          function getcopy : tnode;override;
       end;

       pbinopnode = ^tbinopnode;
       tbinopnode = class(tbinarynode)
          constructor create(tt : tnodetype;l,r : tnode);
          function docompare(p : tnode) : boolean;override;
       end;

{
  $Log$
  Revision 1.7  2000-09-26 14:59:34  florian
    * more conversion work done

  Revision 1.6  2000/09/25 15:37:14  florian
    * more fixes

  Revision 1.5  2000/09/25 15:05:25  florian
    * some updates

  Revision 1.4  2000/09/24 21:15:34  florian
    * some errors fix to get more stuff compilable

  Revision 1.3  2000/09/22 21:45:36  florian
    * some updates e.g. getcopy added

  Revision 1.2  2000/09/20 21:52:38  florian
    * removed a lot of errors

  Revision 1.1  2000/08/26 12:27:04  florian
    * initial release
}