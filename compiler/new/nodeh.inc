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
       tnodeflags = (nf_needs_truefalselabel,tf_callunique);

       tnodeflagset = set of tnodeflags;

       pnode = ^tnode;
       tnode = object(tlinkedlist_item)
          treetype : ttreetyp;
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
          error : boolean;
          list : paasmoutput;
          constructor init;
          destructor done;virtual;
          { runs det_resulttype and det_temp }
          procedure pass_1;
          { dermines the resulttype of the node }
          procedure det_resulttype;virtual;
          { dermines the number of necessary temp. locations to evaluate
            the node }
          procedure det_temp;virtual;
          procedure secondpass;virtual;
{$ifdef EXTDEBUG}
          { writes a node for debugging purpose, shouldn't be called }
          { direct, because there is no test for nil, use writenode  }
          { to write a complete tree                                 }
          procedure dowrite;virtual;
{$endif EXTDEBUG}
          procedure concattolist(l : plinkedlist);virtual;
          function ischild(p : pnode) : boolean;virtual;
       end;

       { this node is the anchestor for all classes with at least }
       { one child, you have to use it if you want to use         }
       { true- and falselabel                                     }
       punarynode = ^tunarynode;
       tunarynode = object(tnode)
          left : pnode;
          truelabel,falselabel : pasmlabel;
 {$ifdef extdebug}
          procedure dowrite;virtual;
 {$endif extdebug}
          constructor init(l : pnode);
          procedure concattolist(l : plinkedlist);virtual;
          function ischild(p : pnode) : boolean;virtual;
          procedure det_resulttype;virtual;
          procedure det_temp;virtual;
       end;

       pbinarynode = ^tbinarynode;
       tbinarynode = object(tunarynode)
          right : pnode;
          constructor init(l,r : pnode);
          procedure concattolist(l : plinkedlist);virtual;
          function ischild(p : pnode) : boolean;virtual;
          procedure det_resulttype;virtual;
          procedure det_temp;virtual;
       end;

       pvecnode = ^tvecnode;
       tvecnode = object(tbinarynode)
       end;


       pbinopnode = ^tbinopnode;
       tbinopnode = object(tbinarynode)
          { is true, if the right and left operand are swaped }
          { against the original order                        }
          swaped : boolean;
          constructor init(l,r : pnode);
       end;

{
  $Log$
  Revision 1.1  2000-02-24 18:40:42  peter
    * moved to new/

  Revision 1.1  2000/02/20 20:49:45  florian
    * newcg is compiling
    * fixed the dup id problem reported by Paul Y.

}