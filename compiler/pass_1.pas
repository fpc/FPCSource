{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
       node;

    procedure firstpass(var p : tnode);
    function  do_firstpass(var p : tnode) : boolean;

    var
       { the block node of the current exception block to check gotos }
       aktexceptblock : tnode;

implementation

    uses
      globtype,systems,
      cutils,cobjects,globals,
      hcodegen,
      tgcpu
{$ifdef newcg}
      ,cgbase
{$endif}
      ;

{*****************************************************************************
                            Global procedures
*****************************************************************************}

    procedure firstpass(var p : tnode);

      var
         oldcodegenerror  : boolean;
         oldlocalswitches : tlocalswitches;
         oldpos    : tfileposinfo;
         hp : tnode;
{$ifdef extdebug}
   {$ifdef dummy}
         str1,str2 : string;
         oldp      : tnode;
   {$endif}
         not_first : boolean;
{$endif extdebug}
      begin
{$ifdef extdebug}
         inc(total_of_firstpass);
         if (p.firstpasscount>0) and only_one_pass then
           exit;
{$endif extdebug}
         oldcodegenerror:=codegenerror;
         oldpos:=aktfilepos;
         oldlocalswitches:=aktlocalswitches;
{$ifdef extdebug}
         if p.firstpasscount>0 then
           begin
    {$ifdef dummy}
              move(p^,str1[1],sizeof(ttree));
              str1[0]:=char(sizeof(ttree));
              new(oldp);
              old^:=p^;
    {$endif}
              not_first:=true;
              inc(firstpass_several);
           end
         else
           not_first:=false;
{$endif extdebug}

         if not(nf_error in p.flags) then
           begin
              codegenerror:=false;
              aktfilepos:=p.fileinfo;
              aktlocalswitches:=p.localswitches;
              hp:=p.pass_1;
              { should the node be replaced? }
              if assigned(hp) then
                begin
                   p.free;
                   p:=hp;
                end;
              aktlocalswitches:=oldlocalswitches;
              aktfilepos:=oldpos;
              if codegenerror then
                include(p.flags,nf_error);
              codegenerror:=codegenerror or oldcodegenerror;
           end
         else
           codegenerror:=true;
{$ifdef extdebug}
         if not_first then
           begin
    {$ifdef dummy}
              { dirty trick to compare two ttree's (PM) }
              move(p^,str2[1],sizeof(ttree));
              str2[0]:=char(sizeof(ttree));
              if str1<>str2 then
                begin
                   comment(v_debug,'tree changed after first counting pass '
                     +tostr(longint(p.treetype)));
                   compare_trees(oldp,p);
                end;
              dispose(oldp);
    {$endif dummy}
           end;
         if count_ref then
           inc(p.firstpasscount);
{$endif extdebug}
      end;


    function do_firstpass(var p : tnode) : boolean;
      begin
         aktexceptblock:=nil;
         codegenerror:=false;
         firstpass(p);
         do_firstpass:=codegenerror;
      end;

end.
{
  $Log$
  Revision 1.10  2000-11-29 00:30:35  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.9  2000/10/14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.8  2000/10/01 19:48:25  peter
    * lot of compile updates for cg11

  Revision 1.7  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.6  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.5  2000/09/24 21:15:34  florian
    * some errors fix to get more stuff compilable

  Revision 1.4  2000/09/24 15:06:21  peter
    * use defines.inc

  Revision 1.3  2000/09/19 23:09:07  pierre
   * problems wih extdebug cond. solved

  Revision 1.2  2000/07/13 11:32:44  michael
  + removed logs

}
