{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl, Pierre Muller

    Implementation of the reading of PPU Files for the symtable

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
unit symppu;
interface

    uses
       globtype,globals,
       symbase,
       ppu;

    var
       current_ppu       : pppufile;    { Current ppufile which is read }

    procedure writebyte(b:byte);
    procedure writeword(w:word);
    procedure writelong(l:longint);
    procedure writereal(d:bestreal);
    procedure writestring(const s:string);
    procedure writenormalset(var s); {You cannot pass an array[0..31] of byte!}
    procedure writesmallset(var s);
    procedure writeguid(var g: tguid);
    procedure writeposinfo(const p:tfileposinfo);
    procedure writederef(p : tsymtableentry);

    function readbyte:byte;
    function readword:word;
    function readlong:longint;
    function readreal : bestreal;
    function readstring : string;
    procedure readnormalset(var s);   {You cannot pass an array [0..31] of byte.}
    procedure readsmallset(var s);
    procedure readguid(var g: tguid);
    procedure readposinfo(var p:tfileposinfo);
    function readderef : tsymtableentry;

    procedure closecurrentppu;


implementation

    uses
       symconst,
       verbose;

{*****************************************************************************
                                 PPU Writing
*****************************************************************************}

    procedure writebyte(b:byte);
      begin
        current_ppu^.putbyte(b);
      end;


    procedure writeword(w:word);
      begin
        current_ppu^.putword(w);
      end;


    procedure writelong(l:longint);
      begin
        current_ppu^.putlongint(l);
      end;


    procedure writereal(d:bestreal);
      begin
        current_ppu^.putreal(d);
      end;


    procedure writestring(const s:string);
      begin
        current_ppu^.putstring(s);
      end;


    procedure writenormalset(var s); {You cannot pass an array[0..31] of byte!}
      begin
        current_ppu^.putdata(s,sizeof(tnormalset));
      end;


    procedure writesmallset(var s);
      begin
        current_ppu^.putdata(s,4);
      end;


    { posinfo is not relevant for changes in PPU }
    procedure writeposinfo(const p:tfileposinfo);
      var
        oldcrc : boolean;
      begin
        oldcrc:=current_ppu^.do_crc;
        current_ppu^.do_crc:=false;
        current_ppu^.putword(p.fileindex);
        current_ppu^.putlongint(p.line);
        current_ppu^.putword(p.column);
        current_ppu^.do_crc:=oldcrc;
      end;


    procedure writeguid(var g: tguid);
      begin
        current_ppu^.putdata(g,sizeof(g));
      end;

    procedure writederef(p : tsymtableentry);
      begin
        if p=nil then
         current_ppu^.putbyte(ord(derefnil))
        else
         begin
           { Static symtable ? }
           if p.owner.symtabletype=staticsymtable then
            begin
              current_ppu^.putbyte(ord(derefaktstaticindex));
              current_ppu^.putword(p.indexnr);
            end
           { Local record/object symtable ? }
           else if (p.owner=aktrecordsymtable) then
            begin
              current_ppu^.putbyte(ord(derefaktrecordindex));
              current_ppu^.putword(p.indexnr);
            end
           { Local local/para symtable ? }
           else if (p.owner=aktlocalsymtable) then
            begin
              current_ppu^.putbyte(ord(derefaktlocal));
              current_ppu^.putword(p.indexnr);
            end
           else
            begin
              current_ppu^.putbyte(ord(derefindex));
              current_ppu^.putword(p.indexnr);
           { Current unit symtable ? }
              repeat
                if not assigned(p) then
                 internalerror(556655);
                case p.owner.symtabletype of
                 { when writing the pseudo PPU file
                   to get CRC values the globalsymtable is not yet
                   a unitsymtable PM }
                  globalsymtable :
                    begin
                      { check if the unit is available in the uses
                        clause, else it's an error }
                      if p.owner.unitid=$ffff then
                       internalerror(55665566);
                      current_ppu^.putbyte(ord(derefunit));
                      current_ppu^.putword(p.owner.unitid);
                      break;
                    end;
                  staticsymtable :
                    begin
                      current_ppu^.putbyte(ord(derefaktstaticindex));
                      current_ppu^.putword(p.indexnr);
                      break;
                    end;
                  localsymtable :
                    begin
                      p:=p.owner.defowner;
                      current_ppu^.putbyte(ord(dereflocal));
                      current_ppu^.putword(p.indexnr);
                    end;
                  parasymtable :
                    begin
                      p:=p.owner.defowner;
                      current_ppu^.putbyte(ord(derefpara));
                      current_ppu^.putword(p.indexnr);
                    end;
                  objectsymtable,
                  recordsymtable :
                    begin
                      p:=p.owner.defowner;
                      current_ppu^.putbyte(ord(derefrecord));
                      current_ppu^.putword(p.indexnr);
                    end;
                  else
                    internalerror(556656);
                end;
              until false;
            end;
         end;
      end;

    procedure closecurrentppu;
      begin
{$ifdef Test_Double_checksum}
         if assigned(current_ppu^.crc_test) then
           dispose(current_ppu^.crc_test);
         if assigned(current_ppu^.crc_test2) then
           dispose(current_ppu^.crc_test2);
{$endif Test_Double_checksum}
       { close }
         current_ppu^.close;
         dispose(current_ppu,done);
         current_ppu:=nil;
      end;


{*****************************************************************************
                                 PPU Reading
*****************************************************************************}

    function readbyte:byte;
      begin
        readbyte:=current_ppu^.getbyte;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    function readword:word;
      begin
        readword:=current_ppu^.getword;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    function readlong:longint;
      begin
        readlong:=current_ppu^.getlongint;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    function readreal : bestreal;
      begin
        readreal:=current_ppu^.getreal;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    function readstring : string;
      begin
        readstring:=current_ppu^.getstring;
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    procedure readnormalset(var s);   {You cannot pass an array [0..31] of byte.}
      begin
        current_ppu^.getdata(s,sizeof(tnormalset));
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    procedure readsmallset(var s);
      begin
        current_ppu^.getdata(s,4);
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;


    procedure readguid(var g: tguid);
      begin
        current_ppu^.getdata(g,sizeof(g));
        if current_ppu^.error then
         Message(unit_f_ppu_read_error);
      end;

    procedure readposinfo(var p:tfileposinfo);
      begin
        p.fileindex:=current_ppu^.getword;
        p.line:=current_ppu^.getlongint;
        p.column:=current_ppu^.getword;
      end;


    function readderef : tsymtableentry;
      var
        hp,p : tderef;
        b : tdereftype;
      begin
        p:=nil;
        repeat
          hp:=p;
          b:=tdereftype(current_ppu^.getbyte);
          case b of
            derefnil :
              break;
            derefunit,
            derefaktrecordindex,
            derefaktlocal,
            derefaktstaticindex :
              begin
                p:=tderef.create(b,current_ppu^.getword);
                p.next:=hp;
                break;
              end;
            derefindex,
            dereflocal,
            derefpara,
            derefrecord :
              begin
                p:=tderef.create(b,current_ppu^.getword);
                p.next:=hp;
              end;
          end;
        until false;
        readderef:=tsymtableentry(p);
      end;

end.
{
  $Log$
  Revision 1.5  2001-04-13 01:22:16  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.4  2000/12/25 00:07:29  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.3  2000/11/29 00:30:41  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.2  2000/11/04 14:25:22  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.1  2000/10/31 22:02:52  peter
    * symtable splitted, no real code changes

}
