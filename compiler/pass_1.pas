{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit handles the typecheck and node conversion pass

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
unit pass_1;

{$i fpcdefs.inc}

interface

    uses
       node;

    procedure resulttypepass(var p : tnode);
    function  do_resulttypepass(var p : tnode) : boolean;

    procedure firstpass(var p : tnode);
    function  do_firstpass(var p : tnode) : boolean;
{$ifdef state_tracking}
    procedure  do_track_state_pass(p:Tnode);
{$endif}


implementation

    uses
      globtype,systems,cclasses,
      cutils,globals,
      cgbase,symdef
{$ifdef extdebug}
      ,verbose,htypechk
{$endif extdebug}
{$ifdef state_tracking}
      ,nstate
{$endif}
      ;

{*****************************************************************************
                            Global procedures
*****************************************************************************}

    procedure resulttypepass(var p : tnode);
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
         hp        : tnode;
      begin
        if (p.resulttype.def=nil) then
         begin
           oldcodegenerror:=codegenerror;
           oldpos:=aktfilepos;
           oldlocalswitches:=aktlocalswitches;
           codegenerror:=false;
           aktfilepos:=p.fileinfo;
           aktlocalswitches:=p.localswitches;
           hp:=p.det_resulttype;
           { should the node be replaced? }
           if assigned(hp) then
            begin
               p.free;
               { run resulttypepass }
               resulttypepass(hp);
               { switch to new node }
               p:=hp;
            end;
           aktlocalswitches:=oldlocalswitches;
           aktfilepos:=oldpos;
           if codegenerror then
            begin
              include(p.flags,nf_error);
              { default to errortype if no type is set yet }
              if p.resulttype.def=nil then
               p.resulttype:=generrortype;
            end;
           codegenerror:=codegenerror or oldcodegenerror;
         end
        else
         begin
           { update the codegenerror boolean with the previous result of this node }
           if (nf_error in p.flags) then
            codegenerror:=true;
         end;
      end;


    function do_resulttypepass(var p : tnode) : boolean;
      begin
         codegenerror:=false;
         resulttypepass(p);
         do_resulttypepass:=codegenerror;
      end;


    procedure firstpass(var p : tnode);
      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
         hp : tnode;
      begin
         if not(nf_error in p.flags) then
           begin
              oldcodegenerror:=codegenerror;
              oldpos:=aktfilepos;
              oldlocalswitches:=aktlocalswitches;
              codegenerror:=false;
              aktfilepos:=p.fileinfo;
              aktlocalswitches:=p.localswitches;
              { determine the resulttype if not done }
              if (p.resulttype.def=nil) then
               begin
                 aktfilepos:=p.fileinfo;
                 aktlocalswitches:=p.localswitches;
                 hp:=p.det_resulttype;
                 { should the node be replaced? }
                 if assigned(hp) then
                  begin
                     p.free;
                     { run resulttypepass }
                     resulttypepass(hp);
                     { switch to new node }
                     p:=hp;
                  end;
                 if codegenerror then
                  begin
                    include(p.flags,nf_error);
                    { default to errortype if no type is set yet }
                    if p.resulttype.def=nil then
                     p.resulttype:=generrortype;
                  end;
                 aktlocalswitches:=oldlocalswitches;
                 aktfilepos:=oldpos;
                 codegenerror:=codegenerror or oldcodegenerror;
               end;
              if not(nf_error in p.flags) then
               begin
                 { first pass }
                 aktfilepos:=p.fileinfo;
                 aktlocalswitches:=p.localswitches;
                 hp:=p.pass_1;
                 { should the node be replaced? }
                 if assigned(hp) then
                  begin
                    p.free;
                    { run firstpass }
                    firstpass(hp);
                    { switch to new node }
                    p:=hp;
                  end;
                 if codegenerror then
                  include(p.flags,nf_error)
                 else
                  begin
{$ifdef EXTDEBUG}
                    if (p.expectloc=LOC_INVALID) then
                      Comment(V_Warning,'Expectloc is not set in firstpass: '+nodetype2str[p.nodetype]);
{$endif EXTDEBUG}
                  end;
               end;
              codegenerror:=codegenerror or oldcodegenerror;
              aktlocalswitches:=oldlocalswitches;
              aktfilepos:=oldpos;
           end
         else
           codegenerror:=true;
      end;


    function do_firstpass(var p : tnode) : boolean;
      begin
         codegenerror:=false;
         firstpass(p);
{$ifdef state_tracking}
         writeln('TRACKSTART');
         writeln('before');
         writenode(p);
         do_track_state_pass(p);
         writeln('after');
         writenode(p);
         writeln('TRACKDONE');
{$endif}
         do_firstpass:=codegenerror;
      end;

{$ifdef state_tracking}
     procedure do_track_state_pass(p:Tnode);

     begin
        aktstate:=Tstate_storage.create;
        p.track_state_pass(true);
            aktstate.destroy;
     end;
{$endif}

end.
{
  $Log$
  Revision 1.32  2003-10-01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.31  2003/09/23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.30  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.29  2002/12/17 22:19:33  peter
    * fixed pushing of records>8 bytes with stdcall
    * simplified hightree loading

  Revision 1.28  2002/09/05 19:28:30  peter
    * removed repetitive pass counting
    * display heapsize also for extdebug

  Revision 1.27  2002/07/19 12:55:27  daniel
  * Further developed state tracking in whilerepeatn

  Revision 1.26  2002/07/19 11:41:36  daniel
  * State tracker work
  * The whilen and repeatn are now completely unified into whilerepeatn. This
    allows the state tracker to change while nodes automatically into
    repeat nodes.
  * Resulttypepass improvements to the notn. 'not not a' is optimized away and
    'not(a>b)' is optimized into 'a<=b'.
  * Resulttypepass improvements to the whilerepeatn. 'while not a' is optimized
    by removing the notn and later switchting the true and falselabels. The
    same is done with 'repeat until not a'.

  Revision 1.25  2002/07/14 18:00:44  daniel
  + Added the beginning of a state tracker. This will track the values of
    variables through procedures and optimize things away.

  Revision 1.24  2002/06/16 08:15:54  carl
  * commented out uncompilable debug code

  Revision 1.23  2002/05/18 13:34:11  peter
    * readded missing revisions

  Revision 1.22  2002/05/16 19:46:42  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.20  2002/04/04 19:06:00  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.19  2002/03/31 20:26:35  jonas
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
