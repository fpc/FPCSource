{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit handles the temporary variables stuff for i386

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
unit temp_gen;

{$i defines.inc}

interface

    uses
      cpubase,cpuinfo,globals,
      hcodegen,verbose,fmodule,aasm;

{$ifdef newcg}
    const
       countusableregint : byte = c_countusableregsint;
       countusableregfpu : byte = c_countusableregsfpu;
       countusableregmm  : byte = c_countusableregsmm;
{$endif newcg}

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

    var
      { contains all temps }
      templist      : ptemprecord;
      { contains all free temps using nextfree links }
      tempfreelist  : ptemprecord;
      { Offsets of the first/last temp }
      firsttemp,
      lasttemp      : longint;

    { generates temporary variables }
    procedure resettempgen;
    procedure setfirsttemp(l : longint);
    function gettempsize : longint;
    function newtempofsize(size : longint) : longint;
    function gettempofsize(size : longint) : longint;
    { special call for inlined procedures }
    function gettempofsizepersistant(size : longint) : longint;
    procedure gettempofsizereferencepersistant(l : longint;var ref : treference);

    { for parameter func returns }
    procedure normaltemptopersistant(pos : longint);
    procedure persistanttemptonormal(pos : longint);

    {procedure ungettemp(pos : longint;size : longint);}
    procedure ungetpersistanttemp(pos : longint);
    procedure ungetpersistanttempreference(const ref : treference);

    procedure gettempofsizereference(l : longint;var ref : treference);
    function istemp(const ref : treference) : boolean;
    procedure ungetiftemp(const ref : treference);
    function getsizeoftemp(const ref: treference): longint;

    function ungetiftempansi(const ref : treference) : boolean;
    procedure gettempansistringreference(var ref : treference);

    function ungetiftempwidestr(const ref : treference) : boolean;
    procedure gettempwidestringreference(var ref : treference);

    function ungetiftempintfcom(const ref : treference) : boolean;
    procedure gettempintfcomreference(var ref : treference);

  implementation

    uses
       cutils,systems;

    procedure resettempgen;
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


    procedure setfirsttemp(l : longint);
      begin
         { this is a negative value normally }
         if l < 0 then
          Begin
            if odd(l) then
             Dec(l);
          end
         else
          Begin
            if odd(l) then
             Inc(l);
          end;
         firsttemp:=l;
         lasttemp:=l;
      end;


    function newtempofsize(size : longint) : longint;
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

const
  lasttempofsize : ptemprecord = nil;

    function gettempofsize(size : longint) : longint;
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
         exprasmList.concat(Taitempalloc.alloc(ofs,size));
         gettempofsize:=ofs;
      end;


    function gettempofsizepersistant(size : longint) : longint;
      var
         l : longint;
      begin
         l:=gettempofsize(size);
         lasttempofsize^.temptype:=tt_persistant;
{$ifdef EXTDEBUG}
         Comment(V_Debug,'temp managment  : call to gettempofsizepersistant()'+
                     ' with size '+tostr(size)+' returned '+tostr(l));
{$endif}
         gettempofsizepersistant:=l;
      end;


    function gettempsize : longint;
      var
        _align : longint;
      begin
        { align to 4 bytes at least
          otherwise all those subl $2,%esp are meaningless PM }
        _align:=target_info.alignment.localalignmin;
        if _align<4 then
          _align:=4;
        gettempsize:=Align(-lasttemp,_align);
      end;


    procedure gettempofsizereference(l : longint;var ref : treference);
      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.offset:=gettempofsize(l);
         ref.base:=procinfo^.framepointer;
      end;

    procedure gettempofsizereferencepersistant(l : longint;var ref : treference);
      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.offset:=gettempofsizepersistant(l);
         ref.base:=procinfo^.framepointer;
      end;


    procedure gettemppointerreferencefortype(var ref : treference; const usedtype, freetype: ttemptype);
      var
         foundslot,tl : ptemprecord;
      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
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
            ref.offset:=newtempofsize(target_info.size_of_pointer);
{$ifdef EXTDEBUG}
            templist^.posinfo:=aktfilepos;
{$endif}
            templist^.temptype:=usedtype;
          end;
         exprasmList.concat(Taitempalloc.alloc(ref.offset,target_info.size_of_pointer));
      end;

    function ungettemppointeriftype(const ref : treference; const usedtype, freetype: ttemptype) : boolean;
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
                 exprasmList.concat(Taitempalloc.dealloc(tl^.pos,tl^.size));
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


    procedure gettempansistringreference(var ref : treference);
      begin
        gettemppointerreferencefortype(ref,tt_ansistring,tt_freeansistring);
      end;

    procedure gettempwidestringreference(var ref : treference);
      begin
        gettemppointerreferencefortype(ref,tt_widestring,tt_freewidestring);
      end;

    function ungetiftempansi(const ref : treference) : boolean;
      begin
        ungetiftempansi:=ungettemppointeriftype(ref,tt_ansistring,tt_freeansistring);
      end;

    function ungetiftempwidestr(const ref : treference) : boolean;
      begin
        ungetiftempwidestr:=ungettemppointeriftype(ref,tt_widestring,tt_widestring);
      end;


    procedure gettempintfcomreference(var ref : treference);
      begin
        gettemppointerreferencefortype(ref,tt_interfacecom,tt_freeinterfacecom);
      end;


    function ungetiftempintfcom(const ref : treference) : boolean;
      begin
        ungetiftempintfcom:=ungettemppointeriftype(ref,tt_ansistring,tt_freeansistring);
      end;

    function istemp(const ref : treference) : boolean;

      begin
         { ref.index = R_NO was missing
           led to problems with local arrays
           with lower bound > 0 (PM) }
         istemp:=((ref.base=procinfo^.framepointer) and
{$ifdef i386}
                  (ref.index=R_NO) and
{$endif}
                  (ref.offset<firsttemp));
      end;


    procedure persistanttemptonormal(pos : longint);
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


    procedure normaltemptopersistant(pos : longint);
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


    function ungettemp(pos:longint;allowtype:ttemptype):ttemptype;
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
               exprasmList.concat(Taitempalloc.dealloc(hp^.pos,hp^.size));
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

    function getsizeoftemp(const ref: treference): longint;
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

    procedure ungetpersistanttemp(pos : longint);
      begin
{$ifdef EXTDEBUG}
        if ungettemp(pos,tt_persistant)<>tt_persistant then
          Comment(V_Warning,'temp managment problem : ungetpersistanttemp()'+
                  ' at pos '+tostr(pos)+ ' not found !');
{$else}
        ungettemp(pos,tt_persistant);
{$endif}
      end;

    procedure ungetpersistanttempreference(const ref : treference);

      begin
         ungetpersistanttemp(ref.offset);
      end;

    procedure ungetiftemp(const ref : treference);
{$ifdef EXTDEBUG}
      var
         tt : ttemptype;
{$endif}
      begin
         if istemp(ref) then
           begin
              { first check if ansistring }
              if ungetiftempansi(ref) then
                exit;
{$ifndef EXTDEBUG}
              ungettemp(ref.offset,tt_normal);
{$else}
              tt:=ungettemp(ref.offset,tt_normal);
              if tt=tt_persistant then
                Comment(V_Debug,'temp at pos '+tostr(ref.offset)+ ' not released because persistant!');
              if tt=tt_none then
                Comment(V_Warning,'temp not found for release at offset '+tostr(ref.offset));
{$endif}
           end;
      end;


   procedure inittemps;
     begin
       tempfreelist:=nil;
       templist:=nil;
     end;

begin
  InitTemps;
end.
{
  $Log$
  Revision 1.16  2001-07-01 20:16:18  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.15  2001/06/02 19:20:10  peter
    * allocate at least 4 bytes, also for 0 byte temps. Give a warning
      with extdebug

  Revision 1.14  2001/05/27 14:30:55  florian
    + some widestring stuff added

  Revision 1.13  2001/04/18 22:02:00  peter
    * registration of targets and assemblers

  Revision 1.12  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.11  2001/01/05 17:36:58  florian
  * the info about exception frames is stored now on the stack
  instead on the heap

  Revision 1.10  2000/12/31 11:04:43  jonas
    + sizeoftemp() function

  Revision 1.9  2000/12/25 00:07:30  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.8  2000/11/30 22:16:50  florian
    * moved to i386

  Revision 1.7  2000/11/29 00:30:42  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.6  2000/11/04 14:25:22  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.5  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.4  2000/09/24 15:06:31  peter
    * use defines.inc

  Revision 1.3  2000/08/27 16:11:55  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:52  michael
  + removed logs
}
