{
    $Id$
    Copyright (c) 1998-2002 by Jonas Maebe

    This unit implements optimized nodes

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
unit nopt;

{$i fpcdefs.inc}

interface

uses node, nadd;

type
  tsubnodetype = (
    addsstringcharoptn,  { shorstring + char }
    addsstringcsstringoptn   { shortstring + constant shortstring }
  );

  taddoptnode = class(taddnode)
     subnodetype: tsubnodetype;
     constructor create(ts: tsubnodetype; l,r : tnode); virtual;
     { pass_1 will be overridden by the separate subclasses    }
     { By default, pass_2 is the same as for addnode           }
     { Only if there's a processor specific implementation, it }
     { will be overridden.                                     }
     function getcopy: tnode; override;
     function docompare(p: tnode): boolean; override;
  end;

  taddsstringoptnode = class(taddoptnode)
    { maximum length of the string until now, allows us to skip a compare }
    { sometimes (it's initialized/updated by calling updatecurmaxlen)     }
    curmaxlen: byte;
    { pass_1 must be overridden, otherwise we get an endless loop }
    function det_resulttype: tnode; override;
    function pass_1: tnode; override;
    function getcopy: tnode; override;
    function docompare(p: tnode): boolean; override;
   protected
    procedure updatecurmaxlen;
  end;

  { add a char to a shortstring }
  taddsstringcharoptnode = class(taddsstringoptnode)
    constructor create(l,r : tnode); virtual;
  end;
  taddsstringcharoptnodeclass = class of taddsstringcharoptnode;

  { add a constant string to a short string }
  taddsstringcsstringoptnode = class(taddsstringoptnode)
    constructor create(l,r : tnode); virtual;
    function pass_1: tnode; override;
  end;
  taddsstringcsstringoptnodeclass = class of taddsstringcsstringoptnode;

function canbeaddsstringcharoptnode(p: taddnode): boolean;
function genaddsstringcharoptnode(p: taddnode): tnode;
function canbeaddsstringcsstringoptnode(p: taddnode): boolean;
function genaddsstringcsstringoptnode(p: taddnode): tnode;


function is_addsstringoptnode(p: tnode): boolean;

var
   caddsstringcharoptnode: taddsstringcharoptnodeclass;
   caddsstringcsstringoptnode: taddsstringcsstringoptnodeclass;

implementation

uses cutils, htypechk, defutil, defcmp, globtype, globals, cpubase, ncnv, ncon,ncal,
     verbose, symdef, cgbase, procinfo;


{*****************************************************************************
                             TADDOPTNODE
*****************************************************************************}

constructor taddoptnode.create(ts: tsubnodetype; l,r : tnode);
begin
  { we need to keep the addn nodetype, otherwise taddnode.pass_2 will be }
  { confused. Comparison for equal nodetypes therefore has to be         }
  { implemented using the classtype() method (JM)                        }
  inherited create(addn,l,r);
  subnodetype := ts;
end;

function taddoptnode.getcopy: tnode;
var
  hp: taddoptnode;
begin
  hp := taddoptnode(inherited getcopy);
  hp.subnodetype := subnodetype;
  getcopy := hp;
end;

function taddoptnode.docompare(p: tnode): boolean;
begin
  docompare :=
    inherited docompare(p) and
    (subnodetype = taddoptnode(p).subnodetype);
end;


{*****************************************************************************
                        TADDSSTRINGOPTNODE
*****************************************************************************}

function taddsstringoptnode.det_resulttype: tnode;
begin
  result := nil;
  updatecurmaxlen;
  { left and right are already firstpass'ed by taddnode.pass_1 }
  if not is_shortstring(left.resulttype.def) then
   inserttypeconv(left,cshortstringtype);
  if not is_shortstring(right.resulttype.def) then
   inserttypeconv(right,cshortstringtype);
  resulttype := left.resulttype;
end;

function taddsstringoptnode.pass_1: tnode;
begin
  pass_1 := nil;
  expectloc:= LOC_REFERENCE;
  calcregisters(self,0,0,0);
  { here we call STRCONCAT or STRCMP or STRCOPY }
  include(current_procinfo.flags,pi_do_call);
end;

function taddsstringoptnode.getcopy: tnode;
var
  hp: taddsstringoptnode;
begin
  hp := taddsstringoptnode(inherited getcopy);
  hp.curmaxlen := curmaxlen;
  getcopy := hp;
end;

function taddsstringoptnode.docompare(p: tnode): boolean;
begin
  docompare :=
    inherited docompare(p) and
    (curmaxlen = taddsstringcharoptnode(p).curmaxlen);
end;


function is_addsstringoptnode(p: tnode): boolean;
begin
  is_addsstringoptnode :=
    p.inheritsfrom(taddsstringoptnode);
end;

procedure taddsstringoptnode.updatecurmaxlen;
begin
  if is_addsstringoptnode(left) then
    begin
      { made it a separate block so no other if's are processed (would be a }
      { simple waste of time) (JM)                                          }
      if (taddsstringoptnode(left).curmaxlen < 255) then
        case subnodetype of
          addsstringcharoptn:
            curmaxlen := succ(taddsstringoptnode(left).curmaxlen);
          addsstringcsstringoptn:
            curmaxlen := min(taddsstringoptnode(left).curmaxlen +
                              tstringconstnode(right).len,255)
          else
            internalerror(291220001);
        end
      else curmaxlen := 255;
    end
  else if (left.nodetype = stringconstn) then
    curmaxlen := min(tstringconstnode(left).len,255)
  else if is_char(left.resulttype.def) then
    curmaxlen := 1
  else if (left.nodetype = typeconvn) then
    begin
      case ttypeconvnode(left).convtype of
        tc_char_2_string:
          curmaxlen := 1;
{       doesn't work yet, don't know why (JM)
        tc_chararray_2_string:
          curmaxlen :=
            min(ttypeconvnode(left).left.resulttype.def.size,255); }
        else curmaxlen := 255;
      end;
    end
  else
    curmaxlen := 255;
end;

{*****************************************************************************
                        TADDSSTRINGCHAROPTNODE
*****************************************************************************}


constructor taddsstringcharoptnode.create(l,r : tnode);
begin
  inherited create(addsstringcharoptn,l,r);
end;

{*****************************************************************************
                        TADDSSTRINGCSSTRINGOPTNODE
*****************************************************************************}


constructor taddsstringcsstringoptnode.create(l,r : tnode);
begin
  inherited create(addsstringcsstringoptn,l,r);
end;


function taddsstringcsstringoptnode.pass_1: tnode;
begin
  { create the call to the concat routine both strings as arguments }
  result := ccallnode.createintern('fpc_shortstr_append_shortstr',
    ccallparanode.create(left,ccallparanode.create(right,nil)));
  left:=nil;
  right:=nil;
end;


{*****************************************************************************
                                HELPERS
*****************************************************************************}

function canbeaddsstringcharoptnode(p: taddnode): boolean;
begin
  canbeaddsstringcharoptnode :=
    (cs_optimize in aktglobalswitches) and

{   the shortstring will be gotten through conversion if necessary (JM)
    is_shortstring(p.left.resulttype.def) and }
    ((p.nodetype = addn) and
     is_char(p.right.resulttype.def));
end;

function genaddsstringcharoptnode(p: taddnode): tnode;
var
  hp: tnode;
begin
  hp := caddsstringcharoptnode.create(p.left.getcopy,p.right.getcopy);
  hp.flags := p.flags;
  genaddsstringcharoptnode := hp;
end;



function canbeaddsstringcsstringoptnode(p: taddnode): boolean;
begin
  canbeaddsstringcsstringoptnode :=
    (cs_optimize in aktglobalswitches) and

{   the shortstring will be gotten through conversion if necessary (JM)
    is_shortstring(p.left.resulttype.def) and }
    ((p.nodetype = addn) and
     (p.right.nodetype = stringconstn));
end;

function genaddsstringcsstringoptnode(p: taddnode): tnode;
var
  hp: tnode;
begin
  hp := caddsstringcsstringoptnode.create(p.left.getcopy,p.right.getcopy);
  hp.flags := p.flags;
  genaddsstringcsstringoptnode := hp;
end;


begin
  caddsstringcharoptnode := taddsstringcharoptnode;
  caddsstringcsstringoptnode := taddsstringcsstringoptnode;
end.

{
  $Log$
  Revision 1.17  2003-10-01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.16  2003/05/26 21:15:18  peter
    * disable string node optimizations for the moment

  Revision 1.15  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.14  2003/04/26 09:12:55  peter
    * add string returns in LOC_REFERENCE

  Revision 1.13  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.12  2002/11/25 17:43:20  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.11  2002/08/17 09:23:37  florian
    * first part of current_procinfo rewrite

  Revision 1.10  2002/07/20 11:57:55  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.9  2002/05/18 13:34:10  peter
    * readded missing revisions

  Revision 1.8  2002/05/16 19:46:39  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.6  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods
}
