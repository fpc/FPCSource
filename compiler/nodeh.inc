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

       { allows to determine which elementes are to be replaced }
       tdisposetyp = (dt_nothing,dt_leftright,dt_left,dt_leftrighthigh,
                      dt_mbleft,dt_typeconv,dt_inlinen,dt_leftrightmethod,
                      dt_mbleft_and_method,dt_loop,dt_case,dt_with,dt_onn,
                      dt_leftrightframe);

      { different assignment types }
      tassigntyp = (at_normal,at_plus,at_minus,at_star,at_slash);

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

       { this will be used mainly for the newcg }
       tnodeflags = (
         nf_needs_truefalselabel,
         nf_callunique,
         nf_swapable,    { tbinop operands can be swaped }
         nf_swaped,      { tbinop operands are swaped    }
         nf_error
         );

       tnodeflagset = set of tnodeflags;

    const
       { contains the flags which must be equal for the equality }
       { of nodes                                                }
       flagsequal : tnodeflagset = [nf_error];

    type
       { later (for the newcg) tnode will inherit from tlinkedlist_item }
       tnode = class
          nodetype : tnodetype;
          { the location of the result of this node }
          location : tlocation;
          { do we need to parse childs to set var state }
          varstateset : boolean;
          { the parent node of this is node    }
          { this field is set by concattolist  }
          parent : pnode;
          { there are some properties about the node stored }
          flags : tnodeflagset;
          { the number of registers needed to evalute the node }
          registersint,registersfpu : longint;  { must be longint !!!! }
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
          constructor init(tt : tnodetype);virtual;
          destructor done;virtual;

          { the 1.1 code generator may override pass_1 }
          { and it need not to implement det_* then    }
          { 1.1: pass_1 returns a value<>0 if the node has been transformed }
          { 2.0: runs det_resulttype and det_temp                           }
          function pass_1 : tnode;virtual;
          { dermines the resulttype of the node }
          procedure det_resulttype;virtual;
          { dermines the number of necessary temp. locations to evaluate
            the node }
          procedure det_temp;virtual;

          procedure secondpass;virtual;

          { comparing of nodes }
          function isequal(p : tnode) : boolean;
          { to implement comparisation, override this method }
          function docompare(p : tnode) : boolean;virtual;
{$ifdef EXTDEBUG}
          { writes a node for debugging purpose, shouldn't be called }
          { direct, because there is no test for nil, use writenode  }
          { to write a complete tree                                 }
          procedure dowrite;virtual;
{$endif EXTDEBUG}
          procedure concattolist(l : plinkedlist);virtual;
          function ischild(p : tnode) : boolean;virtual;
       end;

       { this node is the anchestor for all nodes with at least   }
       { one child, you have to use it if you want to use         }
       { true- and falselabel                                     }
       tparentnode = class(tnode);
          falselabel,truelabel : plabel;
       end;

       punarynode = ^tunarynode;
       tunarynode = class(tparentnode)
          left : tnode;
{$ifdef extdebug}
          procedure dowrite;virtual;
{$endif extdebug}
          constructor init(tt : tnodetype;l : tnode);virtual
          procedure concattolist(l : plinkedlist);virtual;
          function ischild(p : tnode) : boolean;virtual;
          procedure det_resulttype;virtual;
          procedure det_temp;virtual;
       end;

       pbinarynode = ^tbinarynode;
       tbinarynode = class(tunarynode)
          right : tnode;
          constructor init(tt : tnodetype;l,r : pnode);virtual;
          procedure concattolist(l : plinkedlist);virtual;
          function ischild(p : pnode) : boolean;virtual;
          procedure det_resulttype;virtual;
          procedure det_temp;virtual;
       end;

       pbinopnode = ^tbinopnode;
       tbinopnode = class(tbinarynode)
          constructor init(tt : tnodetype;l,r : pnode);virtual;
       end;
{
  $Log$
  Revision 1.1  2000-08-26 12:27:04  florian
    * initial release

}
