{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the base object for temp. generator

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
{#@abstract(Temporary reference allocator unit)
  Temporary reference allocator unit. This unit contains
  all which is related to allocating temporary memory
  space on the stack, as required, by the code generator.
}

unit tgobj;

{$i fpcdefs.inc}

  interface

    uses
      globals,
      cpubase,
      cpuinfo,
      cclasses,globtype,cgbase,aasmbase,aasmtai,aasmcpu;

    type
      ttemptype = (tt_none,
                   tt_free,tt_normal,tt_persistant,
                   tt_noreuse,tt_freenoreuse,
                   tt_ansistring,tt_freeansistring,
                   tt_widestring,tt_freewidestring,
                   tt_interfacecom,tt_freeinterfacecom);
      ttemptypeset = set of ttemptype;

      ptemprecord = ^ttemprecord;
      ttemprecord = record
         temptype   : ttemptype;
         pos        : longint;
         size       : longint;
         next       : ptemprecord;
         nextfree   : ptemprecord; { for faster freeblock checking }
{$ifdef EXTDEBUG}
         posinfo,
         releaseposinfo : tfileposinfo;
{$endif}
      end;


       {# Generates temporary variables }
       ttgobj = class
       private
          { contains all free temps using nextfree links }
          tempfreelist  : ptemprecord;
          function AllocTemp(list: taasmoutput; size : longint; temptype : ttemptype) : longint;
          procedure FreeTemp(list: taasmoutput; pos:longint;temptypes:ttemptypeset);
       public
          { contains all temps }
          templist      : ptemprecord;
          { Offsets of the first/last temp }
          firsttemp,
          lasttemp      : longint;
          direction : shortint;
          constructor create;
          {# Clear and free the complete linked list of temporary memory
             locations. The list is set to nil.}
          procedure resettempgen;
          {# Sets the first offset from the frame pointer or stack pointer where
             the temporary references will be allocated. It is to note that this
             value should always be negative.

             @param(l start offset where temps will start in stack)
          }
          procedure setfirsttemp(l : longint);
          function gettempsize : longint;

          procedure GetTemp(list: taasmoutput; size : longint;temptype:ttemptype;var ref : treference);
          procedure UnGetTemp(list: taasmoutput; const ref : treference);

          function SizeOfTemp(const ref: treference): longint;
          procedure ChangeTempType(const ref:treference;temptype:ttemptype);

          {# Returns TRUE if the reference ref is allocated in temporary volatile memory space,
             otherwise returns FALSE.

             @param(ref reference to verify)
          }
          function istemp(const ref : treference) : boolean;
          {# Frees a reference @var(ref) which was allocated in the volatile temporary memory space.
             The freed space can later be reallocated and reused. If this reference
             is not in the temporary memory, it is simply not freed.
          }
          procedure ungetiftemp(list: taasmoutput; const ref : treference);
       end;

     var
       tg: ttgobj;


    implementation

    uses
       systems,
       verbose,cutils;


    const
      FreeTempTypes = [tt_free,tt_freenoreuse,tt_freeansistring,
                       tt_freewidestring,tt_freeinterfacecom];

{$ifdef EXTDEBUG}
      TempTypeStr : array[ttemptype] of string[18] = (
          '<none>',
          'free','normal','persistant',
          'noreuse','freenoreuse',
          'ansistring','freeansistring',
          'widestring','freewidestring',
          'interfacecom','freeinterfacecom'
      );
{$endif EXTDEBUG}

      Used2Free : array[ttemptype] of ttemptype = (
        tt_none,
        tt_none,tt_free,tt_free,
        tt_freenoreuse,tt_none,
        tt_freeansistring,tt_none,
        tt_freewidestring,tt_none,
        tt_freeinterfacecom,tt_none);


{*****************************************************************************
                                    TTGOBJ
*****************************************************************************}

    constructor ttgobj.create;

     begin
       tempfreelist:=nil;
       templist:=nil;
       { we could create a new child class for this but I don't if it is worth the effort (FK) }
{$ifdef powerpc}
       direction:=1;
{$else powerpc}
       direction:=-1;
{$endif powerpc}
     end;


    procedure ttgobj.resettempgen;
      var
         hp : ptemprecord;
      begin
        { Clear the old templist }
        while assigned(templist) do
         begin
{$ifdef EXTDEBUG}
           if not(templist^.temptype in FreeTempTypes) then
            begin
              Comment(V_Warning,'temp at pos '+tostr(templist^.pos)+
                      ' with size '+tostr(templist^.size)+' and type '+TempTypeStr[templist^.temptype]+
                      ' from pos '+tostr(templist^.posinfo.line)+':'+tostr(templist^.posinfo.column)+
                      ' not freed at the end of the procedure');
            end;
{$endif}
           hp:=templist;
           templist:=hp^.next;
           dispose(hp);
         end;
        templist:=nil;
        tempfreelist:=nil;
        firsttemp:=0;
        lasttemp:=0;
      end;


    procedure ttgobj.setfirsttemp(l : longint);
      begin
         { this is a negative value normally }
         if l*direction>=0 then
          begin
            if odd(l) then
             inc(l,direction);
          end
         else
           internalerror(200204221);
         firsttemp:=l;
         lasttemp:=l;
      end;


    function ttgobj.gettempsize : longint;
      var
        _align : longint;
      begin
        { align to 4 bytes at least
          otherwise all those subl $2,%esp are meaningless PM }
        _align:=target_info.alignment.localalignmin;
        if _align<4 then
          _align:=4;
{$ifdef testtemp}
        if firsttemp <> lasttemp then
           gettempsize:=Align(direction*(lasttemp-firsttemp),_align)
        else
           gettempsize := 0;
{$else}
        gettempsize:=Align(direction*lasttemp,_align);
{$endif}
      end;


    function ttgobj.AllocTemp(list: taasmoutput; size : longint; temptype : ttemptype) : longint;
      var
         tl,
         bestslot,bestprev,
         hprev,hp : ptemprecord;
         bestsize : longint;
         freetype : ttemptype;
      begin
         AllocTemp:=0;
         bestprev:=nil;
         bestslot:=nil;
         tl:=nil;
         bestsize:=0;
{$ifdef EXTDEBUG}
         if size=0 then
          begin
            Comment(V_Warning,'Temp of size 0 requested');
            size:=4;
          end;
{$endif}
         freetype:=Used2Free[temptype];
         if freetype=tt_none then
          internalerror(200208201);
         { Align needed size on 4 bytes }
         size:=Align(size,4);
         { First check the tmpfreelist, but not when
           we don't want to reuse an already allocated block }
         if assigned(tempfreelist) and
            (temptype<>tt_noreuse) then
          begin
            { Check for a slot with the same size first }
            hprev:=nil;
            hp:=tempfreelist;
            while assigned(hp) do
             begin
{$ifdef EXTDEBUG}
               if not(hp^.temptype in FreeTempTypes) then
                 Comment(V_Warning,'Temp in freelist is not set to tt_free');
{$endif}
               if (hp^.temptype=freetype) and
                  (hp^.size>=size) then
                begin
                  { Slot is the same size, then leave immediatly }
                  if hp^.size=size then
                   begin
                     bestprev:=hprev;
                     bestslot:=hp;
                     bestsize:=size;
                     break;
                   end
                  else
                   begin
                     if (bestsize=0) or (hp^.size<bestsize) then
                      begin
                        bestprev:=hprev;
                        bestslot:=hp;
                        bestsize:=hp^.size;
                      end;
                   end;
                end;
               hprev:=hp;
               hp:=hp^.nextfree;
             end;
          end;
         { Reuse an old temp ? }
         if assigned(bestslot) then
          begin
            if bestsize=size then
             begin
               tl:=bestslot;
               tl^.temptype:=temptype;
               { Remove from the tempfreelist }
               if assigned(bestprev) then
                 bestprev^.nextfree:=tl^.nextfree
               else
                 tempfreelist:=tl^.nextfree;
               tl^.nextfree:=nil;
             end
            else
             begin
               { Resize the old block }
               dec(bestslot^.size,size);
               { Create new block and link after bestslot }
               new(tl);
               tl^.temptype:=temptype;
               tl^.pos:=bestslot^.pos+bestslot^.size;
               tl^.size:=size;
               tl^.nextfree:=nil;
               { link the new block }
               tl^.next:=bestslot^.next;
               bestslot^.next:=tl;
             end;
          end
         else
          begin
            { create a new temp, we need to allocate at least a minimum of
              4 bytes, else we get two temps at the same position resulting
              in problems when finding the corresponding temprecord }
            if size<4 then
             size:=4;
            { now we can create the templist entry }
            new(tl);
            tl^.temptype:=temptype;

            if direction=-1 then
              begin
                 { Extend the temp }
                 dec(lasttemp,size);
                 tl^.pos:=lasttemp;
              end
            else
              begin
                 tl^.pos:=lasttemp;
                 { Extend the temp }
                 inc(lasttemp,size);
              end;

            tl^.size:=size;
            tl^.next:=templist;
            tl^.nextfree:=nil;
            templist:=tl;
          end;
{$ifdef EXTDEBUG}
         tl^.posinfo:=aktfilepos;
{$endif}
         list.concat(tai_tempalloc.alloc(tl^.pos,tl^.size));
         AllocTemp:=tl^.pos;
      end;


    procedure ttgobj.FreeTemp(list: taasmoutput; pos:longint;temptypes:ttemptypeset);
      var
         hp,hnext,hprev,hprevfree : ptemprecord;
      begin
         hp:=templist;
         hprev:=nil;
         hprevfree:=nil;
         while assigned(hp) do
          begin
            if (hp^.pos=pos) then
             begin
               { check if already freed }
               if hp^.temptype in FreeTempTypes then
                begin
{$ifdef EXTDEBUG}
                  Comment(V_Warning,'temp managment : (FreeTemp) temp at pos '+tostr(pos)+ ' is already free !');
{$endif}
                  exit;
                end;
               { check type that are allowed to be released }
               if not(hp^.temptype in temptypes) then
                begin
{$ifdef EXTDEBUG}
                  Comment(V_Debug,'temp managment : (Freetemp) temp at pos '+tostr(pos)+ ' has different type, not releasing');
{$endif}
                  exit;
                end;
               list.concat(tai_tempalloc.dealloc(hp^.pos,hp^.size));
               { set this block to free }
               hp^.temptype:=Used2Free[hp^.temptype];
               { Update tempfreelist }
               if assigned(hprevfree) then
                begin
                  { Connect With previous tt_free block? }
                  if assigned(hprev) and
                     (hp^.temptype=tt_free) and
                     (hprev^.temptype=tt_free) then
                   begin
                     inc(hprev^.size,hp^.size);
                     hprev^.next:=hp^.next;
                     dispose(hp);
                     hp:=hprev;
                   end
                  else
                   hprevfree^.nextfree:=hp;
                end
               else
                begin
                  hp^.nextfree:=tempfreelist;

                  tempfreelist:=hp;
                end;
               { Next block tt_free ? Yes, then concat }
               hnext:=hp^.next;
               if assigned(hnext) and
                  (hp^.temptype=tt_free) and
                  (hnext^.temptype=tt_free) then
                begin
                  inc(hp^.size,hnext^.size);
                  hp^.nextfree:=hnext^.nextfree;
                  hp^.next:=hnext^.next;
                  dispose(hnext);
                end;
               { Stop }
               exit;
             end;
            if (hp^.temptype=tt_free) then
              hprevfree:=hp;
            hprev:=hp;
            hp:=hp^.next;
          end;
      end;


    procedure ttgobj.GetTemp(list: taasmoutput; size : longint;temptype:ttemptype;var ref : treference);
      begin
         FillChar(ref,sizeof(treference),0);
         ref.base:=procinfo.framepointer;
         ref.offset:=AllocTemp(list,size,temptype);
      end;


    function ttgobj.istemp(const ref : treference) : boolean;
      begin
         { ref.index = R_NO was missing
           led to problems with local arrays
           with lower bound > 0 (PM) }
         istemp:=((ref.base=procinfo.framepointer) and
                  (ref.index=R_NO) and
                  (ref.offset*direction>firsttemp));
      end;


    function ttgobj.SizeOfTemp(const ref: treference): longint;
      var
         hp : ptemprecord;
      begin
         SizeOfTemp := -1;
         hp:=templist;
         while assigned(hp) do
           begin
             if (hp^.pos=ref.offset) then
               begin
                 SizeOfTemp := hp^.size;
                 exit;
               end;
             hp := hp^.next;
           end;
{$ifdef EXTDEBUG}
         Comment(V_Debug,'temp managment : SizeOfTemp temp at pos '+tostr(ref.offset)+ ' not found !');
{$endif}
      end;


    procedure ttgobj.ChangeTempType(const ref:treference;temptype:ttemptype);
      var
        hp : ptemprecord;
      begin
         hp:=templist;
         while assigned(hp) do
          begin
            if (hp^.pos=ref.offset) then
             begin
               if not(hp^.temptype in [tt_free,tt_freeansistring,tt_freewidestring,tt_freeinterfacecom]) then
                begin
{$ifdef EXTDEBUG}
                  if hp^.temptype=temptype then
                    Comment(V_Warning,'temp managment : ChangeTempType temp'+
                       ' at pos '+tostr(ref.offset)+ ' is already of the correct type !');
{$endif}
                  hp^.temptype:=temptype;
                end
               else
                begin
{$ifdef EXTDEBUG}
                   Comment(V_Warning,'temp managment : ChangeTempType temp'+
                      ' at pos '+tostr(ref.offset)+ ' is already freed !');
{$endif}
                end;
               exit;
             end;
            hp:=hp^.next;
          end;
{$ifdef EXTDEBUG}
         Comment(V_Warning,'temp managment : ChangeTempType temp'+
            ' at pos '+tostr(ref.offset)+ ' not found !');
{$endif}
      end;


    procedure ttgobj.UnGetTemp(list: taasmoutput; const ref : treference);
      begin
        FreeTemp(list,ref.offset,[tt_normal,tt_noreuse,tt_persistant,tt_ansistring,tt_widestring,tt_interfacecom]);
      end;


    procedure ttgobj.UnGetIfTemp(list: taasmoutput; const ref : treference);
      begin
        if istemp(ref) then
          FreeTemp(list,ref.offset,[tt_normal,tt_ansistring,tt_widestring,tt_interfacecom]);
      end;


initialization
  tg := ttgobj.create;
finalization
  tg.free;
end.
{
  $Log$
  Revision 1.18  2002-10-11 11:57:43  florian
  *** empty log message ***

  Revision 1.16  2002/09/07 18:25:00  florian
    + added tcg.direction to allow upwards growing temp areas
      i.e. temps with positive index

  Revision 1.15  2002/09/01 18:42:50  peter
    * reduced level of comment that type is wrong for release

  Revision 1.14  2002/09/01 12:14:53  peter
    * fixed some wrong levels in extdebug comments

  Revision 1.13  2002/08/24 18:35:04  peter
    * when reusing a block also update the temptype instead of forcing it
      to tt_normal

  Revision 1.12  2002/08/23 16:14:49  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.11  2002/08/17 09:23:44  florian
    * first part of procinfo rewrite

  Revision 1.10  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.9  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.8  2002/05/16 19:46:45  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.7  2002/05/14 19:34:52  peter
    * removed old logs and updated copyright year

  Revision 1.6  2002/04/15 19:08:22  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables

  Revision 1.5  2002/04/07 13:38:48  carl
  + update documentation

  Revision 1.4  2002/04/07 09:17:17  carl
  + documentation
  - clean-up

  Revision 1.3  2002/04/04 19:06:06  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.2  2002/04/02 17:11:32  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.1  2002/03/31 20:26:37  jonas
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
