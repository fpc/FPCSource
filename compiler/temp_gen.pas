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
      i386,
{$endif i386}
{$ifdef m68k}
      m68k,
{$endif m68k}
       cobjects,globals,tree,hcodegen,verbose,files,aasm;

    { generates temporary variables }
    procedure resettempgen;
    procedure setfirsttemp(l : longint);
    function gettempsize : longint;
    function gettempofsize(size : longint) : longint;
    { special call for inlined procedures }
    function gettempofsizepersistant(size : longint) : longint;
    { for parameter func returns }
    procedure persistanttemptonormal(pos : longint);
    procedure ungettemp(pos : longint;size : longint);
    procedure ungetpersistanttemp(pos : longint;size : longint);
    procedure gettempofsizereference(l : longint;var ref : treference);
    function istemp(const ref : treference) : boolean;
    procedure ungetiftemp(const ref : treference);


  implementation

    type
       pfreerecord = ^tfreerecord;

       tfreerecord = record
          next : pfreerecord;
          pos : longint;
          size : longint;
          persistant : boolean; { used for inlined procedures }
{$ifdef EXTDEBUG}
          line : longint;
{$endif}
       end;

    var
       tmpfreelist : pfreerecord;
       templist : pfreerecord;
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
                       +tostr(templist^.size)+' from line '+tostr(templist^.line)+
                       +' at pos '+tostr(templist^.pos)+
                       ' not freed at the end of the procedure');
{$endif}
              hp:=templist;
              templist:=hp^.next;
{$ifndef EXTDEBUG}
              dispose(hp);
{$endif not EXTDEBUG}
           end;
         templist:=nil;
         tmpfreelist:=nil;
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
                          if hp^.pos-size < maxtemp then
                            maxtemp := hp^.size-size;
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
         templist:=tl;
{$ifdef EXTDEBUG}
         tl^.line:=current_module^.current_inputfile^.line_no;
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
{$endif}
                   dispose(hp);
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

    procedure ungetiftemp(const ref : treference);

      var
         tl,prev : pfreerecord;

      begin
         if istemp(ref) then
           begin
              prev:=nil;
              tl:=templist;
              while assigned(tl) do
                begin
                   { no release of persistant blocks this way!! }
                   if tl^.persistant then
                     if (ref.offset>=tl^.pos) and
                        (ref.offset<tl^.pos+tl^.size) then
                       begin
{$ifdef EXTDEBUG}
                          Comment(V_Debug,'temp '+
                            ' at pos '+tostr(ref.offset)+ ' not released because persistant !');
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
                        dispose(tl);
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
{$endIf}
           end;
      end;

begin
   tmpfreelist:=nil;
   templist:=nil;
end.
{
  $Log$
  Revision 1.2  1998-07-10 10:51:05  peter
    * m68k updates

  Revision 1.1  1998/06/08 16:07:41  pierre
    * temp_gen contains all temporary var functions
      (processor independent)

}

