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

{$i defines.inc}

interface

    uses
       globtype,globals,
       symbase,symtype,
       ppu;

    type
       tcompilerppufile=class(tppufile)
         procedure checkerror;
         procedure getguid(var g: tguid);
         procedure getposinfo(var p:tfileposinfo);
         function  getderef : tsymtableentry;
         function  getsymlist:tsymlist;
         procedure gettype(var t:ttype);
         procedure putguid(const g: tguid);
         procedure putposinfo(const p:tfileposinfo);
         procedure putderef(p : tsymtableentry);
         procedure putsymlist(p:tsymlist);
         procedure puttype(const t:ttype);
       end;


implementation

    uses
       symconst,
       verbose;

{*****************************************************************************
                            TCompilerPPUFile
*****************************************************************************}

    procedure tcompilerppufile.checkerror;
      begin
        if error then
         Message(unit_f_ppu_read_error);
      end;


    procedure tcompilerppufile.getguid(var g: tguid);
      begin
        getdata(g,sizeof(g));
      end;


    procedure tcompilerppufile.getposinfo(var p:tfileposinfo);
      begin
        p.fileindex:=getword;
        p.line:=getlongint;
        p.column:=getword;
      end;


    function tcompilerppufile.getderef : tsymtableentry;
      var
        hp,p : tderef;
        b : tdereftype;
      begin
        p:=nil;
        repeat
          hp:=p;
          b:=tdereftype(getbyte);
          case b of
            derefnil :
              break;
            derefunit,
            derefaktrecordindex,
            derefaktlocal,
            derefaktstaticindex :
              begin
                p:=tderef.create(b,getword);
                p.next:=hp;
                break;
              end;
            derefindex,
            dereflocal,
            derefpara,
            derefrecord :
              begin
                p:=tderef.create(b,getword);
                p.next:=hp;
              end;
          end;
        until false;
        getderef:=tsymtableentry(p);
      end;


    function tcompilerppufile.getsymlist:tsymlist;
      var
        sym : tsym;
        p   : tsymlist;
      begin
        p:=tsymlist.create;
        p.def:=tdef(getderef);
        repeat
          sym:=tsym(getderef);
          if sym=nil then
           break;
          p.addsym(sym);
        until false;
        getsymlist:=tsymlist(p);
      end;


    procedure tcompilerppufile.gettype(var t:ttype);
      begin
        t.def:=tdef(getderef);
        t.sym:=tsym(getderef);
      end;


    procedure tcompilerppufile.putposinfo(const p:tfileposinfo);
      var
        oldcrc : boolean;
      begin
        { posinfo is not relevant for changes in PPU }
        oldcrc:=do_crc;
        do_crc:=false;
        putword(p.fileindex);
        putlongint(p.line);
        putword(p.column);
        do_crc:=oldcrc;
      end;


    procedure tcompilerppufile.putguid(const g: tguid);
      begin
        putdata(g,sizeof(g));
      end;


    procedure tcompilerppufile.putderef(p : tsymtableentry);
      begin
        if p=nil then
         putbyte(ord(derefnil))
        else
         begin
           { Static symtable ? }
           if p.owner.symtabletype=staticsymtable then
            begin
              putbyte(ord(derefaktstaticindex));
              putword(p.indexnr);
            end
           { Local record/object symtable ? }
           else if (p.owner=aktrecordsymtable) then
            begin
              putbyte(ord(derefaktrecordindex));
              putword(p.indexnr);
            end
           { Local local/para symtable ? }
           else if (p.owner=aktlocalsymtable) then
            begin
              putbyte(ord(derefaktlocal));
              putword(p.indexnr);
            end
           else
            begin
              putbyte(ord(derefindex));
              putword(p.indexnr);
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
                      putbyte(ord(derefunit));
                      putword(p.owner.unitid);
                      break;
                    end;
                  staticsymtable :
                    begin
                      putbyte(ord(derefaktstaticindex));
                      putword(p.indexnr);
                      break;
                    end;
                  localsymtable :
                    begin
                      p:=p.owner.defowner;
                      putbyte(ord(dereflocal));
                      putword(p.indexnr);
                    end;
                  parasymtable :
                    begin
                      p:=p.owner.defowner;
                      putbyte(ord(derefpara));
                      putword(p.indexnr);
                    end;
                  objectsymtable,
                  recordsymtable :
                    begin
                      p:=p.owner.defowner;
                      putbyte(ord(derefrecord));
                      putword(p.indexnr);
                    end;
                  else
                    internalerror(556656);
                end;
              until false;
            end;
         end;
      end;


    procedure tcompilerppufile.putsymlist(p:tsymlist);
      var
        hp : psymlistitem;
      begin
        putderef(p.def);
        hp:=p.firstsym;
        while assigned(hp) do
         begin
           putderef(hp^.sym);
           hp:=hp^.next;
         end;
        putderef(nil);
      end;


    procedure tcompilerppufile.puttype(const t:ttype);
      begin
        { Don't write symbol references for the current unit
          and for the system unit }
        if assigned(t.sym) and
           (t.sym.owner.unitid<>0) and
           (t.sym.owner.unitid<>1) then
         begin
           putderef(nil);
           putderef(t.sym);
         end
        else
         begin
           putderef(t.def);
           putderef(nil);
         end;
      end;

end.
{
  $Log$
  Revision 1.6  2001-05-06 14:49:17  peter
    * ppu object to class rewrite
    * move ppu read and write stuff to fppu

  Revision 1.5  2001/04/13 01:22:16  peter
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
