{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Carl Eric Codere

    This unit handles the temporary variables stuff for m68k

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
unit tgen68k;

  interface

    uses
       cobjects,globals,tree,hcodegen,verbose,files,aasm
{$ifdef m68k}
       ,m68k
{$endif}
       ;

    type
       tregisterset = set of tregister;
       tpushed = array[R_D0..R_A6] of boolean;

    const
       { D2 to D5 usable as scratch registers }
       usablereg32 : byte = 4;
       { A2 to A4 usable as address registers }
       usableaddress: byte = 3;
       { FP2 to FP7 usable as FPU registers   }
       usablefloatreg : byte = 6;

    function getregister32 : tregister;
    procedure ungetregister32(r : tregister);
    { return a free 32-bit address register }
    function getaddressreg: tregister;

    procedure ungetregister(r : tregister);

    procedure cleartempgen;

    { generates temporary variables }
    procedure resettempgen;
    procedure setfirsttemp(l : longint);
    function gettempsize : longint;
    function gettempofsize(size : longint) : longint;
    procedure gettempofsizereference(l : longint;var ref : treference);
    function istemp(const ref : treference) : boolean;
    procedure ungetiftemp(const ref : treference);
    function getfloatreg: tregister;
    { returns a free floating point register }
    { used in real, fpu mode, otherwise we   }
    { must use standard register allocation  }

    procedure del_reference(const ref : treference);
    procedure del_locref(const location : tlocation);


    { pushs and restores registers }
    procedure pushusedregisters(var pushed : tpushed;b : word);
    procedure popusedregisters(const pushed : tpushed);

    var
       unused,usableregs : tregisterset;
       c_usableregs : longint;

       usedinproc : word;

       { count, how much a register must be pushed if it is used as register }
       { variable                                                            }
       reg_pushes : array[R_D0..R_A6] of longint;
       is_reg_var : array[R_D0..R_A6] of boolean;

  implementation

    procedure pushusedregisters(var pushed : tpushed;b : word);

      var
         r : tregister;

      begin
         { the following registers can be pushed }
         { D0, D1, D2, D3, D4, D5, D6, D7, A0    }
         { A1, A2, A3, A4                        }
         for r:=R_D2 to R_A4 do
           begin
              pushed[r]:=false;
              { if the register is used by the calling subroutine    }
              if ((b and ($800 shr word(r)))<>0) then
                begin
                   { and is present in use }
                   if not(r in unused) then
                     begin
                        { then save it }
                        { then save it on the stack }
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,r,R_SPPUSH)));
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
      end;

    procedure popusedregisters(const pushed : tpushed);

      var
         r : tregister;

      begin
         for r:=R_A4 downto R_D2 do
           if pushed[r] then
             begin
                exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_SPPULL,r)));
                unused:=unused-[r];
             end;
      end;

    procedure ungetregister(r : tregister);

      begin
           ungetregister32(r)
      end;


    procedure del_reference(const ref : treference);

      begin
         if ref.isintvalue then
           exit;
         ungetregister(ref.base);
         ungetregister32(ref.index);
      end;

    procedure del_locref(const location : tlocation);

      begin
         if (location.loc<>loc_mem) and (location.loc<>loc_reference) then
           exit;
         if location.reference.isintvalue then
           exit;
         ungetregister(location.reference.base);
         ungetregister32(location.reference.index);
      end;

    procedure ungetregister32(r : tregister);

      begin
         if r in [R_D2,R_D3,R_D4,R_D5,R_D7] then
          begin
             unused:=unused+[r];
             inc(usablereg32);
          end
         else
         if r in [R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7] then
         begin
              unused:=unused+[r];
              inc(usablefloatreg);
         end
         else
         if r in [R_A2,R_A3,R_A4,R_A6,R_SP] then
           begin
              unused:=unused+[r];
              inc(usableaddress);
{$ifdef EXTDEBUG}
           end
         else
         begin
           if not (r in [R_NO]) then
           begin
            Comment(V_Debug,'ungetregister32() deallocation of reserved register.');
         end;
         end;
{$ELSE}
           end;
{$ENDIF}
      end;


    function getfloatreg: tregister;
    { returns a free floating point register }
    { used in real, fpu mode, otherwise we   }
    { must use standard register allocation  }
    var
     i:tregister;
    begin
      dec(usablefloatreg);
      if usablefloatreg = 0 then
       Message(cg_f_internal_error_in_getfloatreg);
      for i:=R_FP2 to R_FP7 do
      begin
         if i in unused then
         begin
           unused := unused-[i];
           getfloatreg := i;
           exit;
         end;
      end;
      { if we are here, then there was an allocation failure }
      Message(cg_f_internal_error_in_getfloatreg);
    end;


    function getaddressreg: tregister;

     begin
         dec(usableaddress);
         if R_A2 in unused then
           begin
              unused:=unused-[R_A2];
              usedinproc:=usedinproc or ($800 shr word(R_A2));
              getaddressreg:=R_A2;
           end
         else
         if R_A3 in unused then
           begin
              unused:=unused-[R_A3];
              usedinproc:=usedinproc or ($800 shr word(R_A3));
              getaddressreg:=R_A3;
           end
         else
         if R_A4 in unused then
           begin
              unused:=unused-[R_A4];
              usedinproc:=usedinproc or ($800 shr word(R_A4));
              getaddressreg:=R_A4;
           end
         else
         begin
           internalerror(10);
         end;

     end;

    function getregister32 : tregister;
      begin
         dec(usablereg32);
         if R_D2 in unused then
           begin
              unused:=unused-[R_D2];
              usedinproc:=usedinproc or ($800 shr word(R_D2));
              getregister32:=R_D2;
           end
         else if R_D3 in unused then
           begin
              unused:=unused-[R_D3];
              usedinproc:=usedinproc or ($800 shr word(R_D3));
              getregister32:=R_D3;
           end
         else if R_D4 in unused then
           begin
              unused:=unused-[R_D4];
              usedinproc:=usedinproc or ($800 shr word(R_D4));
              getregister32:=R_D4;
           end
         else if R_D5 in unused then
           begin
             unused:=unused-[R_D5];
             usedinproc:=usedinproc or ($800 shr word(R_D5));
             getregister32:=R_D5;
           end
         else if R_D7 in unused then
           begin
             unused:=unused-[R_D7];
             usedinproc:=usedinproc or ($800 shr word(R_D7));
             getregister32:=R_D7;
           end
         else
         begin
          internalerror(10);
         end;
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
                       +tostr(templist^.size)+' from '+tostr(templist^.line)+
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
         if odd(l) then
          l:=l+1;
         firsttemp:=l;
         maxtemp := l;
         lastoccupied:=l;
      end;

    function gettempofsize(size : longint) : longint;

      var
         last,hp : pfreerecord;

      begin
         { this code comes from the heap management of FPC ... }
         if (size mod 4)<>0 then
           size:=size+(4-(size mod 4));
           if assigned(tmpfreelist) then
             begin
                last:=nil;
                hp:=tmpfreelist;
                while assigned(hp) do
                  begin
                     { first fit }
                     if hp^.size>=size then
                       begin
                          gettempofsize:=hp^.pos;
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
                          exit;
                       end;
                     last:=hp;
                     hp:=hp^.next;
                  end;
             end;
          { nothing free is big enough : expand temp }
          gettempofsize:=lastoccupied-size;
          lastoccupied:=lastoccupied-size;
          if lastoccupied < maxtemp then
            maxtemp := lastoccupied;
      end;

    function gettempsize : longint;

      begin
         { we only push words and we want to stay on }
         { even stack addresses                      }
         { maxtemp is negative                       }
         if (maxtemp mod 2)<>0 then
           dec(maxtemp);
         gettempsize:=-maxtemp;
      end;

    procedure gettempofsizereference(l : longint;var ref : treference);

      var
         tl : pfreerecord;

      begin
         { do a reset, because the reference isn't used }
         reset_reference(ref);
         ref.offset:=gettempofsize(l);
         ref.base:=procinfo.framepointer;
         new(tl);
         tl^.pos:=ref.offset;
         tl^.size:=l;
         tl^.next:=templist;
         templist:=tl;
{$ifdef EXTDEBUG}
         tl^.line:=current_module^.current_inputfile^.line_no;
{$endif}
      end;

    function istemp(const ref : treference) : boolean;

      begin
         istemp:=((ref.base=procinfo.framepointer) and
           (ref.offset<firsttemp));
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
              Comment(V_Warning,'temp managment problem : ungettemp() pos < lastoccupied !');
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
                   if ref.offset=tl^.pos then
                     begin
                        ungettemp(ref.offset,tl^.size);
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
   { contains both information on Address registers and data registers }
   { even if they are allocated separately.                            }
   usableregs:=[R_D0,R_D1,R_D2,R_D3,R_D4,R_D5,R_D6,R_D7,R_A0,R_A1,R_A2,R_A3,R_A4,
               R_FP0,R_FP1,R_FP2,R_FP3,R_FP4,R_FP5,R_FP6,R_FP7];
   c_usableregs:=4;
   tmpfreelist:=nil;
   templist:=nil;
end.
{
  $Log$
  Revision 1.1  1998-03-25 11:18:15  root
  Initial revision

  Revision 1.12  1998/03/22 12:45:38  florian
    * changes of Carl-Eric to m68k target commit:
      - wrong nodes because of the new string cg in intel, I had to create
        this under m68k also ... had to work it out to fix potential alignment
        problems --> this removes the crash of the m68k compiler.
      - added absolute addressing in m68k assembler (required for Amiga startup)
      - fixed alignment problems (because of byte return values, alignment
        would not be always valid) -- is this ok if i change the offset if odd in
        setfirsttemp ?? -- it seems ok...

  Revision 1.11  1998/03/10 04:21:15  carl
    * fixed extdebug problems

  Revision 1.10  1998/03/10 01:17:30  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.9  1998/03/06 00:53:00  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.8  1998/03/02 01:49:35  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.7  1998/02/13 10:35:51  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.6  1998/01/11 03:40:16  carl
    + added fpu register allocation

  Revision 1.3  1997/12/09 14:13:07  carl
  * bugfix of free register list.

  Revision 1.2  1997/11/28 18:14:49  pierre
   working version with several bug fixes

  Revision 1.1.1.1  1997/11/27 08:33:03  michael
  FPC Compiler CVS start

  Pre-CVS log:

  + feature added
  - removed
  * bug fixed or changed

  History (started with version 0.9.0):
       7th december 1996:
         * some code from Pierre Muller inserted
           makes the use of the stack more efficient
   5th september 1997:
        + Converted for Motorola MC68000 output (C. E. Codere)
   24nd september 1997:
        + Reserved register list modified. (CEC)
   26 september 1997:
        + Converted to work with v093 (CEC)
        * Knowing that base is in address register, modified routines
          accordingly. (CEC)
   27 september 1997:
      + pushusedregisters now pushes only non-scratch registers.
    2nd october 1997:
      + added strict error checking when extdebug defined.
   23 october 1997:
      - it seems that sp, and the base pointer can be freed in ungetregister,
        removed warning accordingly. (CEC).
      * bugfix of address register in usableregs set. (They were not defined...) (CEC).
      * other stupid bug! When I changed the register conventions, I forgot to change
        getaddressreg to reflect those changes!! (CEC).

}
