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

    var
      resulttypepasscnt,
      multiresulttypepasscnt : longint;

    procedure resulttypepass(var p : tnode);
    function  do_resulttypepass(var p : tnode) : boolean;

    procedure firstpass(var p : tnode);
    function  do_firstpass(var p : tnode) : boolean;


implementation

    uses
      globtype,systems,
      cutils,globals,
      cgbase,symdef,
{$ifdef extdebug}
      htypechk,
{$endif extdebug}
      tgobj
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
        inc(resulttypepasscnt);
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
           inc(multiresulttypepasscnt);
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
{$ifdef extdebug}
         inc(total_of_firstpass);
{$endif extdebug}
         oldcodegenerror:=codegenerror;
         oldpos:=aktfilepos;
         oldlocalswitches:=aktlocalswitches;
{$ifdef extdebug}
         if p.firstpasscount>0 then
          inc(firstpass_several);
{$endif extdebug}
         if not(nf_error in p.flags) then
           begin
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
              { first pass }
              if not(nf_error in p.flags) then
               begin
                 aktfilepos:=p.fileinfo;
                 aktlocalswitches:=p.localswitches;
                 hp:=p.pass_1;
                 { should the node be replaced? }
                 if assigned(hp) then
                  begin
                    p.free;
                    p:=hp;
                  end;
                 if codegenerror then
                  include(p.flags,nf_error);
               end;
              codegenerror:=codegenerror or oldcodegenerror;
              aktlocalswitches:=oldlocalswitches;
              aktfilepos:=oldpos;
           end
         else
           codegenerror:=true;
{$ifdef extdebug}
         if count_ref then
           inc(p.firstpasscount);
{$endif extdebug}
      end;


    function do_firstpass(var p : tnode) : boolean;
      begin
         codegenerror:=false;
         firstpass(p);
         do_firstpass:=codegenerror;
      end;

end.
{
  $Log$
  Revision 1.23  2002-05-18 13:34:11  peter
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
