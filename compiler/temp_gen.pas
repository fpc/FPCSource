{
    $Id$
    Copyright (C) 1993-98 by Florian Klaempfl

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

  interface

    uses
{$ifdef i386}
{$ifdef ag386bin}
      i386base,
{$else}
      i386,
{$endif}
{$endif i386}
{$ifdef m68k}
      m68k,
{$endif m68k}
       cobjects,globals,tree,hcodegen,verbose,files,aasm;

      type
{ this saves some memory }
{$ifdef FPC}
{$minenumsize 1}
{$endif FPC}
       ttemptype = (tt_normal,tt_ansistring,tt_widestring);
{$ifdef FPC}
{$minenumsize default}
{$endif FPC}
    { generates temporary variables }
    procedure resettempgen;
    procedure setfirsttemp(l : longint);
    function gettempsize : longint;
    function gettempofsize(size : longint) : longint;
    { special call for inlined procedures }
    function gettempofsizepersistant(size : longint) : longint;
    { for parameter func returns }
    procedure persistanttemptonormal(pos : longint);
    {procedure ungettemp(pos : longint;size : longint);}
    procedure ungetpersistanttemp(pos : longint;size : longint);
    procedure gettempofsizereference(l : longint;var ref : treference);
    procedure gettempslotreference(slottype : ttemptype;var ref : treference);
    function istemp(const ref : treference) : boolean;
    procedure ungetiftemp(const ref : treference);
    function ungetiftempansi(const ref : treference) : boolean;
    procedure gettempansistringreference(var ref : treference);

    type
       pfreerecord = ^tfreerecord;

       tfreerecord = record
          next : pfreerecord;
          pos : longint;
          size : longint;
          persistant : boolean; { used for inlined procedures }
          is_ansistring : boolean;
          is_freeansistring : boolean;
          temptype : ttemptype;
{$ifdef EXTDEBUG}
          posinfo,releaseposinfo : tfileposinfo;
{$endif}
       end;

     var
       tempansilist : pfreerecord;

  implementation

    uses
       scanner,systems
{$ifdef i386}
       ,cgai386
{$endif i386}
{$ifdef m68k}
       ,cga68k
{$endif m68k}
       ;

    var
       { contains all free temps }
       tmpfreelist : pfreerecord;
       { contains all used temps }
       templist : pfreerecord;
       { contains the slots for ansi/wide string temps }
       reftempslots : pfreerecord;
{$ifdef EXTDEBUG}
       tempfreedlist : pfreerecord;
{$endif}
       lastoccupied : longint;
       firsttemp, maxtemp : longint;

    procedure resettempgen;

      var
         hp : pfreerecord;

      begin
         while assigned(tmpfreelist) do
           begin
              hp:=tmpfreelist;
              tmpfreelist:=hp^.next;
              dispose(hp);
           end;
         while assigned(templist) do
           begin
{$ifdef EXTDEBUG}
              Comment(V_Warning,'temporary assignment of size '
                       +tostr(templist^.size)+' from pos '+tostr(templist^.posinfo.line)
                       +':'+tostr(templist^.posinfo.column)
                       +' at pos '+tostr(templist^.pos)+
                       ' not freed at the end of the procedure');
{$endif}
              hp:=templist;
              templist:=hp^.next;
              dispose(hp);
           end;
{$ifdef EXTDEBUG}
         while assigned(tempfreedlist) do
           begin
              hp:=tempfreedlist;
              tempfreedlist:=hp^.next;
              dispose(hp);
           end;
{$endif}
         while assigned(tempansilist) do
           begin
              hp:=tempansilist;
              tempansilist:=hp^.next;
              dispose(hp);
           end;
         firsttemp:=0;
         maxtemp:=0;
         lastoccupied:=0;
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
         maxtemp:=l;
         lastoccupied:=l;
      end;

    function gettempofsize(size : longint) : longint;

      var
         tl,last,hp : pfreerecord;
         ofs : longint;

      begin
         { this code comes from the heap management of FPC ... }
         if (size mod 4)<>0 then
           size:=size+(4-(size mod 4));
           ofs:=0;
           if assigned(tmpfreelist) then
             begin
                last:=nil;
                hp:=tmpfreelist;
                while assigned(hp) do
                  begin
                     { first fit }
                     if hp^.size>=size then
                       begin
                          ofs:=hp^.pos;
                          { the whole block is needed ? }
                          if hp^.size>size then
                            begin
                               hp^.size:=hp^.size-size;
                               hp^.pos:=hp^.pos-size;
                            end
                          else
                            begin
                               if assigned(last) then
                                 last^.next:=hp^.next
                               else
                                 tmpfreelist:=nil;
                               dispose(hp);
                            end;
                          break;
                       end;
                     last:=hp;
                     hp:=hp^.next;
                  end;
             end;
          { nothing free is big enough : expand temp }
          if ofs=0 then
            begin
              ofs:=lastoccupied-size;
              lastoccupied:=lastoccupied-size;
              if lastoccupied < maxtemp then
                maxtemp := lastoccupied;
            end;
         new(tl);
         tl^.pos:=ofs;
         tl^.size:=size;
         tl^.next:=templist;
         tl^.persistant:=false;
         tl^.temptype:=tt_normal;
         templist:=tl;
{$ifdef EXTDEBUG}
         tl^.posinfo:=aktfilepos;
{$endif}
         gettempofsize:=ofs;
      end;

    function gettempofsizepersistant(size : longint) : longint;

      var
         l : longint;

      begin
         l:=gettempofsize(size);
         templist^.persistant:=true;
{$ifdef EXTDEBUG}
         Comment(V_Debug,'temp managment  : call to gettempofsizepersistant()'+
                     ' with size '+tostr(size)+' returned '+tostr(l));
{$endif}
         gettempofsizepersistant:=l;
      end;

    function gettempsize : longint;

      begin
{$ifdef i386}

         { align local data to dwords }
         if (maxtemp mod 4)<>0 then
           dec(maxtemp,4+(maxtemp mod 4));
{$endif}
{$ifdef m68k}

         { we only push words and we want to stay on }
         { even stack addresses                      }
         { maxtemp is negative                       }
         if (maxtemp mod 2)<>0 then
           dec(maxtemp);
{$endif}

         gettempsize:=-maxtemp;
      end;

    procedure gettempofsizereference(l : longint;var ref : treference);

      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.offset:=gettempofsize(l);
         ref.base:=procinfo.framepointer;
      end;

    function gettempansioffset : longint;
      var
         ofs : longint;
         tl : pfreerecord;
      begin
         tl:=tempansilist;
         while assigned(tl) do
           begin
              if tl^.is_freeansistring then
                break;
              tl:=tl^.next;
           end;
         if assigned(tl) then
           begin
              tl^.is_freeansistring:=false;
              ofs:=tl^.pos;
           end
         else
           begin
              if lastoccupied<>maxtemp then
                begin
                  { we cannnot use already used temp
                    so we need to convert that space into
                    a tempfreeitem ! }
                    new(tl);
                    tl^.pos:=lastoccupied;
                    tl^.size:=lastoccupied-maxtemp;
                    tl^.next:=tmpfreelist;
                    lastoccupied:=maxtemp;
                    tl^.persistant:=false;
                    tl^.is_ansistring:=false;
                    tl^.is_freeansistring:=false;
                    tmpfreelist:=tl;
                end;
              ofs:=maxtemp-target_os.size_of_pointer;
              maxtemp:=maxtemp-target_os.size_of_pointer;
              new(tl);
              tl^.pos:=ofs;
              tl^.size:=target_os.size_of_pointer;
              tl^.next:=tempansilist;
              tl^.persistant:=false;
              tl^.is_ansistring:=true;
              tl^.is_freeansistring:=false;
              tempansilist:=tl;
           end;
         gettempansioffset:=ofs;
      end;
      
    procedure gettempansistringreference(var ref : treference);

      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.offset:=gettempansioffset;
         ref.base:=procinfo.framepointer;
      end;

    function ungetiftempansi(const ref : treference) : boolean;
      var
         tl : pfreerecord;
      begin
         ungetiftempansi:=false;
         tl:=tempansilist;
         while assigned(tl) do
           begin
              if tl^.pos=ref.offset then
                if tl^.is_ansistring and not tl^.is_freeansistring then
                  begin
                     tl^.is_freeansistring:=true;
                     ungetiftempansi:=true;
                     exit;
                  end
{$ifdef EXTDEBUG}
                else
                  begin
                   Comment(V_Debug,'temp ansi managment problem : ungetiftempansi()'+
                     ' at pos '+tostr(ref.offset)+ ' already free !');
                  end;
{$endif}
              tl:=tl^.next;
           end;
      end;

    procedure gettempslotreference(slottype : ttemptype;var ref : treference);
      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         { this is not enough in my opinion PM }
         { because it still can mix different types !! }
         ref.offset:=gettempofsize(4);
         ref.base:=procinfo.framepointer;
         templist^.temptype:=slottype;
      end;


    function istemp(const ref : treference) : boolean;

      begin
         { ref.index = R_NO was missing
           led to problems with local arrays
           with lower bound > 0 (PM) }
         istemp:=((ref.base=procinfo.framepointer) and
           (ref.offset<firsttemp) and (ref.index=R_NO));
      end;

    procedure persistanttemptonormal(pos : longint);

      var hp : pfreerecord;

      begin
         hp:=templist;
         while assigned(hp) do
           if (hp^.persistant) and (hp^.pos=pos) then
             begin
{$ifdef EXTDEBUG}
                   Comment(V_Debug,'temp managment : persistanttemptonormal()'+
                     ' at pos '+tostr(pos)+ ' found !');
{$endif}
                hp^.persistant:=false;
                exit;
             end
           else
             hp:=hp^.next;
{$ifdef EXTDEBUG}
                   Comment(V_Debug,'temp managment problem : persistanttemptonormal()'+
                     ' at pos '+tostr(pos)+ ' not found !');
{$endif}
      end;


    procedure ungettemp(pos : longint;size : longint);

      var
         hp,newhp : pfreerecord;

      begin
         if (size mod 4)<>0 then
           size:=size+(4-(size mod 4));
         if size = 0 then
           exit;

         if pos<=lastoccupied then
           if pos=lastoccupied then
             begin
                lastoccupied:=pos+size;
                hp:=tmpfreelist;
                newhp:=nil;
                while assigned(hp) do
                  begin
                     { conneting a free block }
                     if hp^.pos=lastoccupied then
                        begin
                           if assigned(newhp) then newhp^.next:=nil
                             else tmpfreelist:=nil;
                           lastoccupied:=lastoccupied+hp^.size;
                           dispose(hp);
                           break;
                        end;
                     newhp:=hp;
                     hp:=hp^.next;
                  end;
             end
           else
             begin
{$ifdef EXTDEBUG}
              Comment(V_Warning,'temp managment problem : ungettemp()'+
                'pos '+tostr(pos)+ '< lastoccupied '+tostr(lastoccupied)+' !');
{$endif}
             end
         else
           begin
              new(newhp);
              { size can be allways set }
              newhp^.size:=size;
              newhp^.pos := pos;
              { if there is no free list }
              if not assigned(tmpfreelist) then
                begin
                   { then generate one }
                   tmpfreelist:=newhp;
                   newhp^.next:=nil;
                   exit;
                end;
              { search the position to insert }
              hp:=tmpfreelist;
              while assigned(hp) do
                begin
                   { conneting two blocks ? }
                   if hp^.pos+hp^.size=pos then
                      begin
                         inc(hp^.size,size);
                         dispose(newhp);
                         break;
                      end
                   { if the end is reached, then concat }
                   else if hp^.next=nil then
                     begin
                        hp^.next:=newhp;
                        newhp^.next:=nil;
                        break;
                     end
                   { falls der n„chste Zeiger gr”áer ist, dann }
                   { Einh„ngen                                 }
                   else if hp^.next^.pos<=pos+size then
                     begin
                        { concat two blocks ? }
                        if pos+size=hp^.next^.pos then
                          begin
                             newhp^.next:=hp^.next^.next;
                             inc(newhp^.size,hp^.next^.size);
                             dispose(hp^.next);
                             hp^.next:=newhp;
                          end
                        else
                          begin
                             newhp^.next:=hp^.next;
                             hp^.next:=newhp;
                          end;
                        break;
                     end;
                   hp:=hp^.next;
                end;
           end;
      end;

    procedure ungetpersistanttemp(pos : longint;size : longint);
      var
         prev,hp : pfreerecord;

      begin
         ungettemp(pos,size);
         prev:=nil;
         hp:=templist;
         while assigned(hp) do
           begin
              if (hp^.persistant) and (hp^.pos=pos) and (hp^.size=size) then
                begin
                   if assigned(prev) then
                     prev^.next:=hp^.next
                   else
                     templist:=hp^.next;
{$ifdef EXTDEBUG}
                   Comment(V_Debug,'temp managment  : ungetpersistanttemp()'+
                     ' at pos '+tostr(pos)+ ' found !');
                   hp^.next:=tempfreedlist;
                   tempfreedlist:=hp;
                   hp^.releaseposinfo:=aktfilepos;
{$else}
                   dispose(hp);
{$endif}
                   exit;
                end;
              prev:=hp;
              hp:=hp^.next;
           end;
{$ifdef EXTDEBUG}
       Comment(V_Warning,'temp managment problem : ungetpersistanttemp()'+
                ' at pos '+tostr(pos)+ ' not found !');
{$endif}
      end;

    procedure ungetiftemp(const ref : treference);

      var
         tl,prev : pfreerecord;

      begin
         if istemp(ref) then
           begin
              { first check if ansistring }
              if ungetiftempansi(ref) then
                exit;
              prev:=nil;
              tl:=templist;
              while assigned(tl) do
                begin
                   { no release of persistant blocks this way!! }
                   if (tl^.persistant) or (tl^.temptype<>tt_normal) then
                     if (ref.offset>=tl^.pos) and
                        (ref.offset<tl^.pos+tl^.size) then
                       begin
{$ifdef EXTDEBUG}
                          Comment(V_Debug,'temp '+
                            ' at pos '+tostr(ref.offset)+ ' not released because persistant or slot!');
{$endif}
                          exit;
                       end;
                   if (ref.offset=tl^.pos) then
                     begin
                        ungettemp(ref.offset,tl^.size);
{$ifdef TEMPDEBUG}
                        Comment(V_Debug,'temp managment  : ungettemp()'+
                          ' at pos '+tostr(tl^.pos)+ ' found !');
{$endif}
                        if assigned(prev) then
                          prev^.next:=tl^.next
                        else
                          templist:=tl^.next;
{$ifdef EXTDEBUG}
                        tl^.next:=tempfreedlist;
                        tempfreedlist:=tl;
                        tl^.releaseposinfo:=aktfilepos;
{$else}
                        dispose(tl);
{$endif}
                        exit;
                     end
                   else
                     begin
                        prev:=tl;
                        tl:=tl^.next;
                     end;
                end;
{$ifdef EXTDEBUG}
              Comment(V_Warning,'Internal: temp managment problem : '+
                'temp not found for release at offset '+tostr(ref.offset));
              tl:=tempfreedlist;
              while assigned(tl) do
                begin
                   if (ref.offset=tl^.pos) then
                     begin
                        Comment(V_Warning,'Last temporary assignment of size '
                          +tostr(tl^.size)+' from pos '+tostr(tl^.posinfo.line)
                          +':'+tostr(tl^.posinfo.column)
                          +' at pos '+tostr(tl^.pos)+
                          ' has been already freed at '
                          +tostr(tl^.releaseposinfo.line)
                          +':'+tostr(tl^.releaseposinfo.column)
                          );
                        Exit;
                     end;
                   tl:=tl^.next;
                end;

{$endIf}
           end;
      end;

   procedure inittemps;

     begin
        { hp:=temp }
     end;

begin
   tmpfreelist:=nil;
   templist:=nil;
   reftempslots:=nil;
end.
{
  $Log$
  Revision 1.12  1999-04-08 23:52:59  pierre
   + tempansilist and gettempansistringreference

  Revision 1.11  1999/04/08 20:59:44  florian
    * fixed problem with default properties which are a class
    * case bug (from the mailing list with -O2) fixed, the
      distance of the case labels can be greater than the positive
      range of a longint => it is now a dword for fpc

  Revision 1.10  1999/04/06 11:19:49  peter
    * fixed temp reuse

  Revision 1.9  1999/02/22 02:15:56  peter
    * updates for ag386bin

  Revision 1.8  1999/02/11 09:35:19  pierre
   * ExtDebug conditionnal infinite loop on temp problem removed

  Revision 1.7  1999/02/02 23:52:33  florian
    * problem with calls to method pointers in methods fixed
    - double ansistrings temp management removed

  Revision 1.6  1999/01/15 11:34:23  pierre
   + better info for temp allocation debugging

  Revision 1.5  1998/11/30 09:43:24  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.4  1998/10/09 08:56:32  pierre
    * several memory leaks fixed

  Revision 1.3  1998/07/16 08:01:42  pierre
    * small bug correction due to newinput
      (only with tempdebug conditionnal)

  Revision 1.2  1998/07/10 10:51:05  peter
    * m68k updates

  Revision 1.1  1998/06/08 16:07:41  pierre
    * temp_gen contains all temporary var functions
      (processor independent)

}

