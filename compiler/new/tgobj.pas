{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

unit tgobj;

  interface

    uses
      cpubase,
      cpuinfo,
      cpuasm,
      tainst,
      cobjects,globals,tree,cgbase,verbose,files,aasm;

    type
       tregisterset = set of tregister;

       tpushed = array[firstreg..lastreg] of boolean;
       tsaved = array[firstreg..lastreg] of longint;

       ttemptype = (tt_none,tt_free,tt_normal,tt_persistant,tt_ansistring,tt_freeansistring,tt_widestring,tt_freewidestring);
       ttemptypeset = set of ttemptype;

       ptemprecord = ^ttemprecord;
       ttemprecord = record
          temptype   : ttemptype;
          pos    : longint;
          size       : longint;
          next       : ptemprecord;
          nextfree   : ptemprecord; { for faster freeblock checking }
 {$ifdef EXTDEBUG}
          posinfo,
          releaseposinfo : tfileposinfo;
 {$endif}
       end;

       ttgobj = object
          unusedregsint,availabletempregsint : tregisterset;
          unusedregsfpu,availabletempregsfpu : tregisterset;
          unusedregsmm,availabletempregsmm : tregisterset;
          countusableregsint,
  	  countusableregsfpu,
 	  countusableregsmm : byte;
          c_countusableregsint,
          c_countusableregsfpu,
          c_countusableregsmm : byte;

          usedinproc : tregisterset;

          reg_pushes : array[firstreg..lastreg] of longint;
          is_reg_var : array[firstreg..lastreg] of boolean;
          { contains all temps }
          templist      : ptemprecord;
          { contains all free temps using nextfree links }
          tempfreelist  : ptemprecord;
          { Offsets of the first/last temp }
          firsttemp,
          lasttemp      : longint;
          constructor init;
          { generates temporary variables }
          procedure resettempgen;
          procedure setfirsttemp(l : longint);
          function gettempsize : longint;
          function newtempofsize(size : longint) : longint;
          function gettempofsize(size : longint) : longint;
          { special call for inlined procedures }
          function gettempofsizepersistant(size : longint) : longint;
          { for parameter func returns }
          procedure normaltemptopersistant(pos : longint);
          procedure persistanttemptonormal(pos : longint);
          procedure ungetpersistanttemp(pos : longint);
          procedure gettempofsizereference(l : longint;var ref : treference);
          function istemp(const ref : treference) : boolean;virtual;
          procedure ungetiftemp(const ref : treference);
          function ungetiftempansi(const ref : treference) : boolean;
          function gettempansistringreference(var ref : treference):boolean;

          { the following methods must be overriden }
          function getregisterint : tregister;virtual;
          procedure ungetregisterint(r : tregister);virtual;
          { tries to allocate the passed register, if possible }
          function getexplicitregisterint(r : tregister) : tregister;virtual;

          procedure ungetregister(r : tregister);virtual;

          procedure cleartempgen;virtual;
          procedure del_reference(const ref : treference);virtual;
          procedure del_locref(const location : tlocation);virtual;
          procedure del_location(const l : tlocation);virtual;

          { pushs and restores registers }
          procedure pushusedregisters(var pushed : tpushed;b : byte);virtual;
          procedure popusedregisters(const pushed : tpushed);virtual;

          { saves and restores used registers to temp. values }
          procedure saveusedregisters(var saved : tsaved;b : byte);virtual;
          procedure restoreusedregisters(const saved : tsaved);virtual;

          procedure clearregistercount;virtual;
          procedure resetusableregisters;virtual;
       private
          function ungettemp(pos:longint;allowtype:ttemptype):ttemptype;
       end;

  implementation

    uses
       scanner,systems;

    constructor ttgobj.init;

     begin
       tempfreelist:=nil;
       templist:=nil;
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


    function ttgobj.newtempofsize(size : longint) : longint;
      var
        tl : ptemprecord;
      begin
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


    function ttgobj.gettempofsize(size : longint) : longint;
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
{$ifdef EXTDEBUG}
             tl:=templist;
{$endif}
          end;
{$ifdef EXTDEBUG}
         tl^.posinfo:=aktfilepos;
{$endif}
         exprasmlist^.concat(new(paitempalloc,alloc(ofs,size)));
         gettempofsize:=ofs;
      end;


    function ttgobj.gettempofsizepersistant(size : longint) : longint;
      var
         l : longint;
      begin
         l:=gettempofsize(size);
         templist^.temptype:=tt_persistant;
{$ifdef EXTDEBUG}
         Comment(V_Debug,'temp managment  : call to gettempofsizepersistant()'+
                     ' with size '+tostr(size)+' returned '+tostr(l));
{$endif}
         gettempofsizepersistant:=l;
      end;


    function ttgobj.gettempsize : longint;
      begin
        gettempsize:=Align(-lasttemp,target_os.stackalignment);
      end;


    procedure ttgobj.gettempofsizereference(l : longint;var ref : treference);
      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.offset:=gettempofsize(l);
         ref.base:=procinfo^.framepointer;
      end;


    function ttgobj.gettempansistringreference(var ref : treference):boolean;
      var
         foundslot,tl : ptemprecord;
      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.base:=procinfo^.framepointer;
         { Reuse old ansi slot ? }
         foundslot:=nil;
         tl:=templist;
         while assigned(tl) do
          begin
            if tl^.temptype=tt_freeansistring then
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
            foundslot^.temptype:=tt_ansistring;
            ref.offset:=foundslot^.pos;
            { we're reusing an old slot then set the function result to true
              so that we can call a decr_ansistr }
            gettempansistringreference:=true;
          end
         else
          begin
            ref.offset:=newtempofsize(target_os.size_of_pointer);
{$ifdef EXTDEBUG}
            templist^.posinfo:=aktfilepos;
{$endif}
            templist^.temptype:=tt_ansistring;
            { set result to false, we don't need an decr_ansistr }
            gettempansistringreference:=true;
          end;
         exprasmlist^.concat(new(paitempalloc,alloc(ref.offset,target_os.size_of_pointer)));
      end;


    function ttgobj.ungetiftempansi(const ref : treference) : boolean;
      var
         tl : ptemprecord;
      begin
        ungetiftempansi:=false;
        tl:=templist;
        while assigned(tl) do
         begin
           if tl^.pos=ref.offset then
            begin
              if tl^.temptype=tt_ansistring then
               begin
                 tl^.temptype:=tt_freeansistring;
                 ungetiftempansi:=true;
                 exprasmlist^.concat(new(paitempalloc,dealloc(tl^.pos,tl^.size)));
                 exit;
{$ifdef EXTDEBUG}
               end
              else if (tl^.temptype=tt_freeansistring) then
               begin
                 Comment(V_Debug,'temp ansi managment problem : ungetiftempansi()'+
                     ' at pos '+tostr(ref.offset)+ ' already free !');
{$endif}
               end;
            end;
           tl:=tl^.next;
         end;
      end;

    function ttgobj.istemp(const ref : treference) : boolean;

      begin
         istemp:=((ref.base=procinfo^.framepointer) and
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


    function ttgobj.ungettemp(pos:longint;allowtype:ttemptype):ttemptype;
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
               exprasmlist^.concat(new(paitempalloc,dealloc(hp^.pos,hp^.size)));
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


    procedure ttgobj.ungetpersistanttemp(pos : longint);
      begin
{$ifdef EXTDEBUG}
        if ungettemp(pos,tt_persistant)<>tt_persistant then
          Comment(V_Warning,'temp managment problem : ungetpersistanttemp()'+
                  ' at pos '+tostr(pos)+ ' not found !');
{$else}
        ungettemp(pos,tt_persistant);
{$endif}
      end;


    procedure ttgobj.ungetiftemp(const ref : treference);
      var
         tt : ttemptype;
      begin
         if istemp(ref) then
           begin
              { first check if ansistring }
              if ungetiftempansi(ref) then
                exit;
              tt:=ungettemp(ref.offset,tt_normal);
{$ifdef EXTDEBUG}
              if tt=tt_persistant then
                Comment(V_Debug,'temp at pos '+tostr(ref.offset)+ ' not released because persistant!');
              if tt=tt_none then
                Comment(V_Warning,'temp not found for release at offset '+tostr(ref.offset));
{$endif}
           end;
      end;

    function ttgobj.getregisterint : tregister;

      var
         i : tregister;

      begin
         if countusableregsint=0 then
           internalerror(10);
         for i:=firstreg to lastreg do
           begin
              if i in unusedregsint then
                begin
                   exclude(unusedregsint,i);
                   include(usedinproc,i);
                   dec(countusableregsint);
                   exprasmlist^.concat(new(pairegalloc,alloc(i)));
                   exit;
                end;
           end;
         internalerror(28991);
      end;

    procedure ttgobj.ungetregisterint(r : tregister);

      begin
         { takes much time }
         if not(r in availabletempregsint) then
           exit;
         include(unusedregsint,r);
         inc(countusableregsint);
         exprasmlist^.concat(new(pairegalloc,dealloc(r)));
      end;

    { tries to allocate the passed register, if possible }
    function ttgobj.getexplicitregisterint(r : tregister) : tregister;

      begin
         if r in unusedregsint then
           begin
              dec(countusableregsint);
              exclude(unusedregsint,r);
              include(usedinproc,r);
              exprasmlist^.concat(new(pairegalloc,alloc(r)));
              getexplicitregisterint:=r;
           end
         else
           getexplicitregisterint:=getregisterint;
      end;

    procedure ttgobj.ungetregister(r : tregister);

      begin
         if r in intregs then
           ungetregisterint(r)
	 {!!!!!!!!
         else if r in fpuregs then
           ungetregisterfpu(r)
         else if r in mmregs then
           ungetregistermm(r)
         }
         else internalerror(18);
      end;

    procedure ttgobj.cleartempgen;

      begin
         countusableregsint:=c_countusableregsint;
         countusableregsfpu:=c_countusableregsfpu;
         countusableregsmm:=c_countusableregsmm;
         unusedregsint:=availabletempregsint;
         {!!!!!!!!
         unusedregsfpu:=availabletempregsfpu;
         unusedregsmm:=availabletempregsmm;
         }
      end;

    procedure ttgobj.del_reference(const ref : treference);

      begin
         ungetregister(ref.base);
      end;

    procedure ttgobj.del_locref(const location : tlocation);

      begin
         if (location.loc<>LOC_MEM) and (location.loc<>LOC_REFERENCE) then
           exit;
         del_reference(location.reference);
      end;

    procedure ttgobj.del_location(const l : tlocation);

      begin
         case l.loc of
           LOC_REGISTER :
             ungetregister(l.register);
           LOC_MEM,LOC_REFERENCE :
             del_reference(l.reference);
         end;
      end;

    { pushs and restores registers }
    procedure ttgobj.pushusedregisters(var pushed : tpushed;b : byte);

      begin
         runerror(255);
      end;

    procedure ttgobj.popusedregisters(const pushed : tpushed);

      begin
         runerror(255);
      end;

    { saves and restores used registers to temp. values }
    procedure ttgobj.saveusedregisters(var saved : tsaved;b : byte);

      begin
         runerror(255);
      end;

    procedure ttgobj.restoreusedregisters(const saved : tsaved);

      begin
         runerror(255);
      end;

    procedure ttgobj.clearregistercount;

      begin
         runerror(255);
      end;

    procedure ttgobj.resetusableregisters;

      begin
         runerror(255);
      end;

end.
{
  $Log$
  Revision 1.10  2000-02-17 14:48:36  florian
     * updated to use old firstpass

  Revision 1.9  2000/01/07 01:14:55  peter
    * updated copyright to 2000

  Revision 1.8  1999/10/14 14:57:54  florian
    - removed the hcodegen use in the new cg, use cgbase instead

  Revision 1.7  1999/10/12 21:20:47  florian
    * new codegenerator compiles again

  Revision 1.6  1999/09/10 18:48:11  florian
    * some bug fixes (e.g. must_be_valid and procinfo.funcret_is_valid)
    * most things for stored properties fixed

  Revision 1.5  1999/08/06 16:04:06  michael
  + introduced tainstruction

  Revision 1.4  1999/08/03 00:33:23  michael
  + Added cpuasm for alpha

  Revision 1.3  1999/08/03 00:32:13  florian
    * reg_vars and reg_pushes is now in tgobj

  Revision 1.2  1999/08/02 23:13:22  florian
    * more changes to compile for the Alpha

  Revision 1.1  1999/08/02 17:14:12  florian
    + changed the temp. generator to an object

}