{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    Contains the 386 binary writer for debugging purposes

    * This code was inspired by the NASM sources
      The Netwide Assembler is copyright (C) 1996 Simon Tatham and
      Julian Hall. All rights reserved.

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
unit og386dbg;

  interface
    uses
       systems,aasm,cpubase,og386;

    type
       pdbgoutput = ^tdbgoutput;
       tdbgoutput = object(tobjectoutput)
         nsyms   : longint;
         rawidx  : longint;
         constructor init(smart:boolean);
         destructor  done;virtual;
         procedure initwriting(Aplace:tcutplace);virtual;
         procedure donewriting;virtual;
         procedure writebytes(var data;len:longint);virtual;
         procedure writealloc(len:longint);virtual;
         procedure writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);virtual;
         procedure writesymbol(p:pasmsymbol);virtual;
         procedure writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);virtual;
       end;

  implementation

{****************************************************************************
                                Tdbgoutput
****************************************************************************}

    constructor tdbgoutput.init(smart:boolean);
      begin
        inherited init(smart);
        rawidx:=-1;
        nsyms:=0;
      end;


    destructor tdbgoutput.done;
      begin
      end;


    procedure tdbgoutput.initwriting(Aplace:tcutplace);
      begin
        inherited initwriting(Aplace);
        writeln('initwriting '+Objfile);
      end;


    procedure tdbgoutput.donewriting;
      begin
        if rawidx<>-1 then
         begin
           writeln;
           rawidx:=-1;
         end;
        writeln('donewriting');
      end;


    procedure tdbgoutput.writesymbol(p:pasmsymbol);
      begin
        if rawidx<>-1 then
         begin
           writeln;
           rawidx:=-1;
         end;
        p^.idx:=nsyms;
        write('symbol [',nsyms,'] '+p^.name+' (',target_asm.secnames[p^.section],',',p^.address,',',p^.size,',');
        case p^.typ of
          AS_LOCAL :
            writeln('local)');
          AS_GLOBAL :
            writeln('global)');
          AS_EXTERNAL :
            writeln('extern)');
        else
          writeln('unknown)');
        end;
        inc(nsyms);
      end;


    procedure tdbgoutput.writereloc(data,len:longint;p:pasmsymbol;relative:relative_type);
      begin
        if rawidx<>-1 then
         begin
           writeln;
           rawidx:=-1;
         end;
        if assigned(p) then
          write('reloc: ',data,' [',target_asm.secnames[p^.section],',',p^.address,']')
        else
          write('reloc: ',data);
        case relative of
          relative_true : writeln(' relative');
          relative_false: writeln(' not relative');
          relative_rva  : writeln(' relative virtual address');
        end;
      end;


    procedure tdbgoutput.writebytes(var data;len:longint);

        function hexstr(val : longint;cnt : byte) : string;
        const
          HexTbl : array[0..15] of char='0123456789ABCDEF';
        var
          i : longint;
        begin
          hexstr[0]:=char(cnt);
          for i:=cnt downto 1 do
           begin
             hexstr[i]:=hextbl[val and $f];
             val:=val shr 4;
           end;
        end;

      var
        p : pchar;
        i : longint;
      begin
        if len=0 then
         exit;
        p:=@data;
        if rawidx=-1 then
         begin
           write('raw: ');
           rawidx:=0;
         end;
        for i:=1to len do
         begin
           if rawidx>=16 then
            begin
              writeln;
              write('raw: ');
              rawidx:=0;
            end;
           write(hexstr(ord(p[i-1]),2),' ');
           inc(rawidx);
         end;
      end;

    procedure tdbgoutput.writealloc(len:longint);
      begin
        writeln('alloc: ',len);
      end;

    procedure tdbgoutput.writestabs(section:tsection;offset:longint;p:pchar;nidx,nother,line:longint;reloc:boolean);
      begin
        writeln('stabs: ',line,',',nidx,'"',p,'"');
      end;


end.
{
  $Log$
  Revision 1.7  2000-01-07 01:14:27  peter
    * updated copyright to 2000

  Revision 1.6  1999/11/02 15:06:57  peter
    * import library fixes for win32
    * alignment works again

  Revision 1.5  1999/08/04 00:23:06  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.4  1999/07/03 00:29:53  peter
    * new link writing to the ppu, one .ppu is needed for all link types,
      static (.o) is now always created also when smartlinking is used

  Revision 1.3  1999/05/05 17:34:32  peter
    * output is more like as 2.9.1
    * stabs really working for go32v2

  Revision 1.2  1999/05/02 22:41:55  peter
    * moved section names to systems
    * fixed nasm,intel writer

  Revision 1.1  1999/05/01 13:24:24  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.6  1999/03/10 13:41:11  pierre
   + partial implementation for win32 !
     winhello works but pp still does not !

  Revision 1.5  1999/03/08 14:51:10  peter
    + smartlinking for ag386bin

  Revision 1.4  1999/03/05 13:09:53  peter
    * first things for tai_cut support for ag386bin

  Revision 1.3  1999/03/02 02:56:28  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.2  1999/02/25 21:03:11  peter
    * ag386bin updates
    + coff writer

  Revision 1.1  1999/02/16 17:59:39  peter
    + initial files

}
