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
      cpuasm,
      tainst,
      cclasses,globtype,cgbase,aasm;

    type
      ttemptype = (tt_none,tt_free,tt_normal,tt_persistant,
                   tt_ansistring,tt_freeansistring,tt_widestring,tt_freewidestring,
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
          { contains all temps }
          templist      : ptemprecord;
          { contains all free temps using nextfree links }
          tempfreelist  : ptemprecord;
          { Offsets of the first/last temp }
          firsttemp,
          lasttemp      : longint;
          lasttempofsize : ptemprecord;
          { tries to hold the amount of times which the current tree is processed  }
          t_times: longint;

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
          { special call for inlined procedures }
          function gettempofsizepersistant(list: taasmoutput; size : longint) : longint;
          procedure gettempofsizereferencepersistant(list: taasmoutput; l : longint;var ref : treference);

          procedure gettemppointerreferencefortype(list: taasmoutput; var ref : treference; const usedtype, freetype: ttemptype);
          function ungettemppointeriftype(list: taasmoutput; const ref : treference; const usedtype, freetype: ttemptype) : boolean;

          { for parameter func returns }
          procedure normaltemptopersistant(pos : longint);

          {# Searches the list of currently allocated persistent memory space
             as the specified address @var(pos) , and if found, converts this memory
             space to normal volatile memory space which can be freed and reused.

             @param(pos offset from current frame pointer to memory area to convert)
          }
          procedure persistanttemptonormal(pos : longint);

          {procedure ungettemp(pos : longint;size : longint);}
          procedure ungetpersistanttemp(list: taasmoutput; pos : longint);
          procedure ungetpersistanttempreference(list: taasmoutput; const ref : treference);

          {# This routine is used to assign and allocate extra temporary volatile memory space
             on the stack from a reference. @var(l) is the size of the persistent memory space to
             allocate, while @var(ref) is a reference entry which will be set to the correct offset
             and correct base register (which is the current @var(procinfo^.framepointer)) register.
             The offset and base fields of ref will be set appropriately in this routine, and can be
             considered valid on exit of this routine.

             @param(l size of the area to allocate)
             @param(ref allocated reference)
          }
          procedure gettempofsizereference(list: taasmoutput; l : longint;var ref : treference);
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
          function getsizeoftemp(const ref: treference): longint;

          function ungetiftempansi(list: taasmoutput; const ref : treference) : boolean;
          procedure gettempansistringreference(list: taasmoutput; var ref : treference);

          function ungetiftempwidestr(list: taasmoutput; const ref : treference) : boolean;
          procedure gettempwidestringreference(list: taasmoutput; var ref : treference);

          function ungetiftempintfcom(list: taasmoutput; const ref : treference) : boolean;
          procedure gettempintfcomreference(list: taasmoutput; var ref : treference);

       private
          function ungettemp(list: taasmoutput; pos:longint;allowtype:ttemptype):ttemptype;
          function newtempofsize(size : longint) : longint;
          function gettempofsize(list: taasmoutput; size : longint) : longint;
       end;

     var
       tg: ttgobj;


  implementation

    uses
       systems,
       verbose,cutils;

    constructor ttgobj.create;

     begin
       tempfreelist:=nil;
       templist:=nil;
       lasttempofsize := nil;
     end;


    procedure ttgobj.resettempgen;
      var
         hp : ptemprecord;
      begin
        { Clear the old templist }
        while assigned(templist) do
         begin
{$ifdef EXTDEBUG}
           case templist^.temptype of
             tt_normal,
             tt_persistant :
               Comment(V_Warning,'temporary assignment of size '+
                       tostr(templist^.size)+' from pos '+tostr(templist^.posinfo.line)+
                       ':'+tostr(templist^.posinfo.column)+
                       ' at pos '+tostr(templist^.pos)+
                       ' not freed at the end of the procedure');
             tt_ansistring :
               Comment(V_Warning,'temporary ANSI assignment of size '+
                       tostr(templist^.size)+' from pos '+tostr(templist^.posinfo.line)+
                       ':'+tostr(templist^.posinfo.column)+
                       ' at pos '+tostr(templist^.pos)+
                     ' not freed at the end of the procedure');
             tt_widestring :
               Comment(V_Warning,'temporary WIDE assignment of size '+
                       tostr(templist^.size)+' from pos '+tostr(templist^.posinfo.line)+
                       ':'+tostr(templist^.posinfo.column)+
                       ' at pos '+tostr(templist^.pos)+
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
         if l <= 0 then
          Begin
            if odd(l) then
             Dec(l);
          end
         else
           internalerror(20020422);
         firsttemp:=l;
         lasttemp:=l;
      end;


    function ttgobj.newtempofsize(size : longint) : longint;
      var
        tl : ptemprecord;
      begin
        { we need to allocate at least a minimum of 4 bytes, else
          we get two temps at the same position resulting in problems
          when finding the corresponding temprecord }
        if size=0 then
         size:=4;
        { Just extend the temp, everything below has been use
          already }
        dec(lasttemp,size);
        { now we can create the templist entry }
        new(tl);
        tl^.temptype:=tt_normal;
        tl^.pos:=lasttemp;
        tl^.size:=size;
        tl^.next:=templist;
        tl^.nextfree:=nil;
        templist:=tl;
        newtempofsize:=tl^.pos;
      end;

    function ttgobj.gettempofsize(list: taasmoutput; size : longint) : longint;
      var
         tl,
         bestslot,bestprev,
         hprev,hp : ptemprecord;
         bestsize,ofs : longint;
      begin
         bestprev:=nil;
         bestslot:=nil;
         tl:=nil;
         bestsize:=0;
{$ifdef EXTDEBUG}
         if size=0 then
          Comment(V_Warning,'Temp of size 0 requested');
{$endif}
         { Align needed size on 4 bytes }
         if (size mod 4)<>0 then
           size:=size+(4-(size mod 4));
         { First check the tmpfreelist }
         if assigned(tempfreelist) then
          begin
            { Check for a slot with the same size first }
            hprev:=nil;
            hp:=tempfreelist;
            while assigned(hp) do
             begin
{$ifdef EXTDEBUG}
               if hp^.temptype<>tt_free then
                 Comment(V_Warning,'Temp in freelist is not set to tt_free');
{$endif}
               if hp^.size>=size then
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
               bestslot^.temptype:=tt_normal;
               ofs:=bestslot^.pos;
               tl:=bestslot;
               { Remove from the tempfreelist }
               if assigned(bestprev) then
                 bestprev^.nextfree:=bestslot^.nextfree
               else
                 tempfreelist:=bestslot^.nextfree;
             end
            else
             begin
               { Resize the old block }
               dec(bestslot^.size,size);
               { Create new block and link after bestslot }
               new(tl);
               tl^.temptype:=tt_normal;
               tl^.pos:=bestslot^.pos+bestslot^.size;
               ofs:=tl^.pos;
               tl^.size:=size;
               tl^.nextfree:=nil;
               { link the new block }
               tl^.next:=bestslot^.next;
               bestslot^.next:=tl;
             end;
          end
         else
          begin
             ofs:=newtempofsize(size);
             tl:=templist;
          end;
         lasttempofsize:=tl;
{$ifdef EXTDEBUG}
         tl^.posinfo:=aktfilepos;
{$endif}
         list.concat(Taitempalloc.alloc(ofs,size));
         gettempofsize:=ofs;
      end;


    function ttgobj.gettempofsizepersistant(list: taasmoutput; size : longint) : longint;
      var
         l : longint;
      begin
         l:=gettempofsize(list, size);
         lasttempofsize^.temptype:=tt_persistant;
{$ifdef EXTDEBUG}
         Comment(V_Debug,'temp managment  : call to gettempofsizepersistant()'+
                     ' with size '+tostr(size)+' returned '+tostr(l));
{$endif}
         gettempofsizepersistant:=l;
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
           gettempsize:=Align(-(lasttemp-firsttemp),_align)
        else
           gettempsize := 0;
{$else}
        gettempsize:=Align(-lasttemp,_align);
{$endif}
      end;


    procedure ttgobj.gettempofsizereference(list: taasmoutput; l : longint;var ref : treference);
      begin
         { do a reset, because the reference isn't used }
         FillChar(ref,sizeof(treference),0);
         ref.offset:=gettempofsize(list,l);
         ref.base:=procinfo^.framepointer;
      end;

    procedure ttgobj.gettempofsizereferencepersistant(list: taasmoutput; l : longint;var ref : treference);
      begin
         { do a reset, because the reference isn't used }
         FillChar(ref,sizeof(treference),0);
         ref.offset:=gettempofsizepersistant(list,l);
         ref.base:=procinfo^.framepointer;
      end;


    procedure ttgobj.gettemppointerreferencefortype(list: taasmoutput; var ref : treference; const usedtype, freetype: ttemptype);
      var
         foundslot,tl : ptemprecord;
      begin
         { do a reset, because the reference isn't used }
         FillChar(ref,sizeof(treference),0);
         ref.base:=procinfo^.framepointer;
         { Reuse old slot ? }
         foundslot:=nil;
         tl:=templist;
         while assigned(tl) do
          begin
            if tl^.temptype=freetype then
             begin
               foundslot:=tl;
{$ifdef EXTDEBUG}
               tl^.posinfo:=aktfilepos;
{$endif}
               break;
             end;
            tl:=tl^.next;
          end;
         if assigned(foundslot) then
          begin
            foundslot^.temptype:=usedtype;
            ref.offset:=foundslot^.pos;
          end
         else
          begin
            ref.offset:=newtempofsize(pointer_size);
{$ifdef EXTDEBUG}
            templist^.posinfo:=aktfilepos;
{$endif}
            templist^.temptype:=usedtype;
          end;
         list.concat(Taitempalloc.alloc(ref.offset,pointer_size));
      end;

    function ttgobj.ungettemppointeriftype(list: taasmoutput; const ref : treference; const usedtype, freetype: ttemptype) : boolean;
      var
         tl : ptemprecord;
      begin
        ungettemppointeriftype:=false;
        tl:=templist;
        while assigned(tl) do
         begin
           if tl^.pos=ref.offset then
            begin
              if tl^.temptype=usedtype then
               begin
                 tl^.temptype:=freetype;
                 ungettemppointeriftype:=true;
                 list.concat(Taitempalloc.dealloc(tl^.pos,tl^.size));
                 exit;
{$ifdef EXTDEBUG}
               end
              else if (tl^.temptype=freetype) then
               begin
                 Comment(V_Debug,'temp managment problem : ungettemppointeriftype()'+
                     ' at pos '+tostr(ref.offset)+ ' already free !');
{$endif}
               end;
            end;
           tl:=tl^.next;
         end;
      end;


    procedure ttgobj.gettempansistringreference(list: taasmoutput; var ref : treference);
      begin
        gettemppointerreferencefortype(list,ref,tt_ansistring,tt_freeansistring);
      end;

    procedure ttgobj.gettempwidestringreference(list: taasmoutput; var ref : treference);
      begin
        gettemppointerreferencefortype(list,ref,tt_widestring,tt_freewidestring);
      end;

    function ttgobj.ungetiftempansi(list: taasmoutput; const ref : treference) : boolean;
      begin
        ungetiftempansi:=ungettemppointeriftype(list,ref,tt_ansistring,tt_freeansistring);
      end;

    function ttgobj.ungetiftempwidestr(list: taasmoutput; const ref : treference) : boolean;
      begin
        ungetiftempwidestr:=ungettemppointeriftype(list,ref,tt_widestring,tt_freewidestring);
      end;


    procedure ttgobj.gettempintfcomreference(list: taasmoutput; var ref : treference);
      begin
        gettemppointerreferencefortype(list,ref,tt_interfacecom,tt_freeinterfacecom);
      end;


    function ttgobj.ungetiftempintfcom(list: taasmoutput; const ref : treference) : boolean;
      begin
        ungetiftempintfcom:=ungettemppointeriftype(list,ref,tt_ansistring,tt_freeansistring);
      end;

    function ttgobj.istemp(const ref : treference) : boolean;

      begin
         { ref.index = R_NO was missing
           led to problems with local arrays
           with lower bound > 0 (PM) }
         istemp:=((ref.base=procinfo^.framepointer) and
                  (ref.index=R_NO) and
                  (ref.offset<firsttemp));
      end;


    procedure ttgobj.persistanttemptonormal(pos : longint);
      var
        hp : ptemprecord;
      begin
         hp:=templist;
         while assigned(hp) do
           if (hp^.pos=pos) and (hp^.temptype=tt_persistant) then
             begin
{$ifdef EXTDEBUG}
               Comment(V_Debug,'temp managment : persistanttemptonormal()'+
                  ' at pos '+tostr(pos)+ ' found !');
{$endif}
                hp^.temptype:=tt_normal;
                exit;
             end
           else
             hp:=hp^.next;
{$ifdef EXTDEBUG}
         Comment(V_Debug,'temp managment problem : persistanttemptonormal()'+
            ' at pos '+tostr(pos)+ ' not found !');
{$endif}
      end;


    procedure ttgobj.normaltemptopersistant(pos : longint);
      var
        hp : ptemprecord;
      begin
         hp:=templist;
         while assigned(hp) do
           if (hp^.pos=pos) and (hp^.temptype=tt_normal) then
             begin
{$ifdef EXTDEBUG}
               Comment(V_Debug,'temp managment : normaltemptopersistant()'+
                  ' at pos '+tostr(pos)+ ' found !');
{$endif}
                hp^.temptype:=tt_persistant;
                exit;
             end
           else
             hp:=hp^.next;
{$ifdef EXTDEBUG}
         Comment(V_Debug,'temp managment problem : normaltemptopersistant()'+
            ' at pos '+tostr(pos)+ ' not found !');
{$endif}
      end;


    function ttgobj.ungettemp(list: taasmoutput; pos:longint;allowtype:ttemptype):ttemptype;
      var
         hp,hnext,hprev,hprevfree : ptemprecord;
      begin
         ungettemp:=tt_none;
         hp:=templist;
         hprev:=nil;
         hprevfree:=nil;
         while assigned(hp) do
          begin
            if (hp^.pos=pos) then
             begin
               { check type }
               ungettemp:=hp^.temptype;
               if hp^.temptype<>allowtype then
                begin
                  exit;
                end;
               list.concat(Taitempalloc.dealloc(hp^.pos,hp^.size));
               { set this block to free }
               hp^.temptype:=tt_free;
               { Update tempfreelist }
               if assigned(hprevfree) then
                begin
                  { Connect with previous? }
                  if assigned(hprev) and (hprev^.temptype=tt_free) then
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
               { Next block free ? Yes, then concat }
               hnext:=hp^.next;
               if assigned(hnext) and (hnext^.temptype=tt_free) then
                begin
                  inc(hp^.size,hnext^.size);
                  hp^.nextfree:=hnext^.nextfree;
                  hp^.next:=hnext^.next;
                  dispose(hnext);
                end;
               exit;
             end;
            if (hp^.temptype=tt_free) then
             hprevfree:=hp;
            hprev:=hp;
            hp:=hp^.next;
          end;
        ungettemp:=tt_none;
      end;

    function ttgobj.getsizeoftemp(const ref: treference): longint;
      var
         hp : ptemprecord;
      begin
        hp:=templist;
        while assigned(hp) do
          begin
            if (hp^.pos=ref.offset) then
              begin
                getsizeoftemp := hp^.size;
                exit;
              end;
            hp := hp^.next;
          end;
        getsizeoftemp := -1;
      end;

    procedure ttgobj.ungetpersistanttemp(list: taasmoutput; pos : longint);
      begin
{$ifdef EXTDEBUG}
        if ungettemp(list,pos,tt_persistant)<>tt_persistant then
          Comment(V_Warning,'temp managment problem : ungetpersistanttemp()'+
                  ' at pos '+tostr(pos)+ ' not found !');
{$else}
        ungettemp(list,pos,tt_persistant);
{$endif}
      end;

    procedure ttgobj.ungetpersistanttempreference(list: taasmoutput; const ref : treference);

      begin
         ungetpersistanttemp(list, ref.offset);
      end;

    procedure ttgobj.ungetiftemp(list: taasmoutput; const ref : treference);
{$ifdef EXTDEBUG}
      var
         tt : ttemptype;
{$endif}
      begin
         if istemp(ref) then
           begin
              { first check if ansistring }
              if ungetiftempansi(list,ref) or
                 ungetiftempwidestr(list,ref) or
                 ungetiftempintfcom(list,ref) then
                exit;
{$ifndef EXTDEBUG}
              ungettemp(list,ref.offset,tt_normal);
{$else}
              tt:=ungettemp(list,ref.offset,tt_normal);
              if tt=tt_persistant then
                Comment(V_Debug,'temp at pos '+tostr(ref.offset)+ ' not released because persistant!');
              if tt=tt_none then
                Comment(V_Warning,'temp not found for release at offset '+tostr(ref.offset));
{$endif}
           end;
      end;


initialization
  tg := ttgobj.create;
finalization
  tg.free;
end.
{
  $Log$
  Revision 1.8  2002-05-16 19:46:45  carl
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
