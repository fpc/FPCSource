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
unit tgeni386;

  interface

    uses
       cobjects,globals,tree,hcodegen,verbose,files,aasm
{$ifdef i386}
       ,i386
{$endif}
       ;

    type
       tregisterset = set of tregister;

       tpushed = array[R_EAX..R_MM6] of boolean;

    const
       usablereg32 : byte = 4;
{$ifdef SUPPORT_MMX}
       usableregmmx : byte = 8;
{$endif SUPPORT_MMX}

    function getregister32 : tregister;
    procedure ungetregister32(r : tregister);
{$ifdef SUPPORT_MMX}
    function getregistermmx : tregister;
    procedure ungetregistermmx(r : tregister);
{$endif SUPPORT_MMX}

    procedure ungetregister(r : tregister);

    procedure cleartempgen;

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

    procedure del_reference(const ref : treference);
    procedure del_locref(const location : tlocation);


    { pushs and restores registers }
    procedure pushusedregisters(var pushed : tpushed;b : byte);
    procedure popusedregisters(const pushed : tpushed);

    var
       unused,usableregs : tregisterset;
       c_usableregs : longint;

       { uses only 1 byte while a set uses in FPC 32 bytes }
       usedinproc : byte;

       { count, how much a register must be pushed if it is used as register }
       { variable                                                            }
{$ifdef SUPPORT_MMX}
       reg_pushes : array[R_EAX..R_MM6] of longint;
       is_reg_var : array[R_EAX..R_MM6] of boolean;
{$else SUPPORT_MMX}
       reg_pushes : array[R_EAX..R_EDI] of longint;
       is_reg_var : array[R_EAX..R_EDI] of boolean;
{$endif SUPPORT_MMX}
  implementation

    procedure pushusedregisters(var pushed : tpushed;b : byte);

      var
         r : tregister;
{$ifdef SUPPORT_MMX}
         hr : preference;
{$endif SUPPORT_MMX}

      begin
         usedinproc:=usedinproc or b;
         for r:=R_EAX to R_EBX do
           begin
              pushed[r]:=false;
              { if the register is used by the calling subroutine    }
              if ((b and ($80 shr byte(r)))<>0) then
                begin
                   { and is present in use }
                   if not(r in unused) then
                     begin
                        { then save it }
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,r)));
                        { here was a big problem  !!!!!}
                        { you cannot do that for a register that is
                        globally assigned to a var
                        this also means that you must push it much more
                        often, but there must be a better way
                        maybe by putting the value back to the stack !! }
                        if not(is_reg_var[r]) then
                          unused:=unused+[r];
                        pushed[r]:=true;
                     end;
                end;
           end;
{$ifdef SUPPORT_MMX}
         for r:=R_MM0 to R_MM6 do
           begin
              pushed[r]:=false;
              { if the mmx register is in use, save it }
              if not(r in unused) then
                begin
                   exprasmlist^.concat(new(pai386,op_const_reg(
                     A_SUB,S_L,8,R_ESP)));
                   new(hr);
                   reset_reference(hr^);
                   hr^.base:=R_ESP;
                   exprasmlist^.concat(new(pai386,op_reg_ref(
                     A_MOVQ,S_NO,r,hr)));
                   if not(is_reg_var[r]) then
                     unused:=unused+[r];
                   pushed[r]:=true;
                end;
           end;
{$endif SUPPORT_MMX}
      end;

    procedure popusedregisters(const pushed : tpushed);

      var
         r : tregister;
{$ifdef SUPPORT_MMX}
         hr : preference;
{$endif SUPPORT_MMX}
      begin
         { restore in reverse order: }
{$ifdef SUPPORT_MMX}
         for r:=R_MM6 downto R_MM0 do
           begin
              if pushed[r] then
                begin
                   new(hr);
                   reset_reference(hr^);
                   hr^.base:=R_ESP;
                   exprasmlist^.concat(new(pai386,op_ref_reg(
                     A_MOVQ,S_NO,hr,r)));
                   exprasmlist^.concat(new(pai386,op_const_reg(
                     A_ADD,S_L,8,R_ESP)));
                   unused:=unused-[r];
                end;
           end;
{$endif SUPPORT_MMX}
         for r:=R_EBX downto R_EAX do
           if pushed[r] then
             begin
                exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,r)));
                unused:=unused-[r];
             end;
      end;

    procedure ungetregister(r : tregister);

      begin
         if r in [R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI] then
           ungetregister32(r)
         else if r in [R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI] then
           ungetregister32(reg16toreg32(r))
         else if r in [R_AL,R_BL,R_CL,R_DL] then
           ungetregister32(reg8toreg32(r))
{$ifdef SUPPORT_MMX}
         else if r in [R_MM0..R_MM6] then
           ungetregistermmx(r)
{$endif SUPPORT_MMX}
         else internalerror(18);
      end;

    procedure ungetregister32(r : tregister);

      begin
         if cs_maxoptimieren in aktswitches then
           begin
              { takes much time }
              if not(r in usableregs) then
                exit;
              unused:=unused+[r];
              inc(usablereg32);
           end
         else
           begin
              if not(r in [R_EAX,R_EBX,R_ECX,R_EDX]) then
                exit;
              unused:=unused+[r];
              inc(usablereg32);
           end;
{$ifdef REGALLOC}
         exprasmlist^.concat(new(pairegdealloc,init(r)));
{$endif REGALLOC}
      end;

{$ifdef SUPPORT_MMX}
    function getregistermmx : tregister;

      var
         r : tregister;

      begin
         dec(usableregmmx);
         for r:=R_MM0 to R_MM6 do
           if r in unused then
             begin
                unused:=unused-[r];
                usedinproc:=usedinproc or ($80 shr byte(R_EAX));
                getregistermmx:=r;
                exit;
             end;
         internalerror(10);
      end;

    procedure ungetregistermmx(r : tregister);

      begin
         if cs_maxoptimieren in aktswitches then
           begin
              { takes much time }
              if not(r in usableregs) then
                exit;
              unused:=unused+[r];
              inc(usableregmmx);
           end
         else
           begin
              unused:=unused+[r];
              inc(usableregmmx);
           end;
      end;
{$endif SUPPORT_MMX}

    procedure del_reference(const ref : treference);

      begin
         if ref.isintvalue then
           exit;
         ungetregister32(ref.base);
         ungetregister32(ref.index);
         { ref.segment:=R_DEFAULT_SEG; }
      end;

    procedure del_locref(const location : tlocation);

      begin
         if (location.loc<>loc_mem) and (location.loc<>loc_reference) then
           exit;
         if location.reference.isintvalue then
           exit;
         ungetregister32(location.reference.base);
         ungetregister32(location.reference.index);
         { ref.segment:=R_DEFAULT_SEG; }
      end;

    function getregister32 : tregister;

      begin
         dec(usablereg32);
         if R_EAX in unused then
           begin
              unused:=unused-[R_EAX];
              usedinproc:=usedinproc or ($80 shr byte(R_EAX));
              getregister32:=R_EAX;
           end
         else if R_EDX in unused then
           begin
              unused:=unused-[R_EDX];
              usedinproc:=usedinproc or ($80 shr byte(R_EDX));
              getregister32:=R_EDX;
           end
         else if R_EBX in unused then
           begin
              unused:=unused-[R_EBX];
              usedinproc:=usedinproc or ($80 shr byte(R_EBX));
              getregister32:=R_EBX;
           end
         else if R_ECX in unused then
           begin
              unused:=unused-[R_ECX];
              usedinproc:=usedinproc or ($80 shr byte(R_ECX));
              getregister32:=R_ECX;
           end
         else internalerror(10);
      end;

    procedure cleartempgen;

      begin
         unused:=usableregs;
         usablereg32:=c_usableregs;
      end;

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
         { generates problems
         if (l mod 4 <> 0) then dec(l,l mod 4);}
         firsttemp:=l;
         maxtemp := l;
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
         { align local data to dwords }
         if (maxtemp mod 4)<>0 then
           dec(maxtemp,4+(maxtemp mod 4));
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
   usableregs:=[R_EAX,R_EBX,R_ECX,R_EDX];
{$ifdef SUPPORT_MMX}
   usableregs:=usableregs+[R_MM0..R_MM6];
{$endif SUPPORT_MMX}
   c_usableregs:=4;
   tmpfreelist:=nil;
   templist:=nil;
end.
{
  $Log$
  Revision 1.6  1998-05-20 09:42:38  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.5  1998/05/11 13:07:58  peter
    + $ifdef NEWPPU for the new ppuformat
    + $define GDB not longer required
    * removed all warnings and stripped some log comments
    * no findfirst/findnext anymore to remove smartlink *.o files

  Revision 1.4  1998/04/29 10:34:08  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/09 22:16:36  florian
    * problem with previous REGALLOC solved
    * improved property support

  Revision 1.2  1998/04/09 15:46:39  florian
    + register allocation tracing stuff added
}

