{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    This unit implements some support routines for the win32 target like
    import/export handling

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
unit win_targ;

  interface

  uses import;

  type
    pimportlibwin32=^timportlibwin32;
    timportlibwin32=object(timportlib)
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure generatelib;virtual;
    end;

  implementation

    uses
       aasm,files,strings,globals,cobjects
{$ifdef i386}
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ;

    procedure timportlibwin32.preparelib(const s : string);

      begin
         if not(assigned(importssection)) then
           importssection:=new(paasmoutput,init);
      end;

    procedure timportlibwin32.importprocedure(const func,module : string;index : longint;const name : string);

      var
         hp1 : pimportlist;
         hp2 : pimported_procedure;

      begin
         { search for the module }
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              if module=hp1^.dllname^ then
                break;
              hp1:=pimportlist(hp1^.next);
           end;
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              hp1:=new(pimportlist,init(module));
              current_module^.imports^.concat(hp1);
           end;
         hp2:=new(pimported_procedure,init(func,name,index));
         hp1^.imported_procedures^.concat(hp2);
      end;

    procedure timportlibwin32.generatelib;

      var
         hp1 : pimportlist;
         hp2 : pimported_procedure;
         l1,l2,l3,l4 : plabel;
         r : preference;

      begin
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              getlabel(l1);
              getlabel(l2);
              getlabel(l3);
              { create import directory entry }
              importssection^.concat(new(pai_section,init_idata(2)));
              { pointer to procedure names }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str
                (l2)))));
              { two empty entries follow }
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { pointer to dll name }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str
                (l1)))));
              { pointer to fixups }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str
                (l3)))));

              { now walk through all imported procedures }
              { we could that do in one while loop, but  }
              { this would give too much idata* entries  }

              { first write the name references }
              importssection^.concat(new(pai_section,init_idata(4)));
              importssection^.concat(new(pai_label,init(l2)));
              hp2:=pimported_procedure(hp1^.imported_procedures^.first);
              while assigned(hp2) do
                begin
                   getlabel(plabel(hp2^.lab));
                   importssection^.concat(new(pai_const,init_rva(strpnew(lab2str
                     (hp2^.lab)))));
                   hp2:=pimported_procedure(hp2^.next);
                end;
              { finalize the names ... }
              importssection^.concat(new(pai_const,init_32bit(0)));

              { then the addresses and create also the indirect jump }
              importssection^.concat(new(pai_section,init_idata(5)));
              importssection^.concat(new(pai_label,init(l3)));
              hp2:=pimported_procedure(hp1^.imported_procedures^.first);
              while assigned(hp2) do
                begin
                   getlabel(l4);
                   { text segment should be aligned }
                   codesegment^.concat(new(pai_align,init_op(4,$90)));
                   codesegment^.concat(new(pai_symbol,init_global(hp2^.func^)));
                   { the indirect jump }
                   new(r);
                   reset_reference(r^);
                   r^.symbol:=stringdup(lab2str(l4));
{$ifdef i386}
                   codesegment^.concat(new(pai386,op_ref(A_JMP,S_NO,r)));
{$endif}
                   importssection^.concat(new(pai_label,init(l4)));
                   importssection^.concat(new(pai_const,init_rva(strpnew(lab2str
                      (hp2^.lab)))));
                   hp2:=pimported_procedure(hp2^.next);
                end;
              { finalize the addresses }
              importssection^.concat(new(pai_const,init_32bit(0)));

              { finally the import information }
              importssection^.concat(new(pai_section,init_idata(6)));
              hp2:=pimported_procedure(hp1^.imported_procedures^.first);
              while assigned(hp2) do
                begin
                   importssection^.concat(new(pai_label,init(hp2^.lab)));
                   { the ordinal number }
                   importssection^.concat(new(pai_const,init_16bit(hp2^.ordnr)));
                   importssection^.concat(new(pai_string,init(hp2^.name^+#0)));
                   hp2:=pimported_procedure(hp2^.next);
                end;
              { create import dll name }
              importssection^.concat(new(pai_section,init_idata(7)));
              importssection^.concat(new(pai_label,init(l1)));
              importssection^.concat(new(pai_string,init(hp1^.dllname^+#0)));

              hp1:=pimportlist(hp1^.next);
           end;
      end;

end.
{
  $Log$
  Revision 1.2  1998-05-06 18:36:55  peter
    * tai_section extended with code,data,bss sections and enumerated type
    * ident 'compiled by FPC' moved to pmodules
    * small fix for smartlink

}
