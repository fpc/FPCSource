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

  uses import,export;

  type
    pimportlibwin32=^timportlibwin32;
    timportlibwin32=object(timportlib)
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure generatelib;virtual;
      procedure generatesmartlib;
    end;

    pexportlibwin32=^texportlibwin32;
    texportlibwin32=object(texportlib)
      procedure preparelib(const s:string);virtual;
      procedure generatelib;virtual;
    end;

    { sets some flags of the executable }
    procedure postprocessexecutable(n : string);

  implementation

    uses
       aasm,files,strings,globals,cobjects,systems,verbose
{$ifdef GDB}
       ,gdb
{$endif}
{$ifdef i386}
       ,i386
{$endif}
       ;

    type
       tdosheader = packed record
          e_magic : word;
          e_cblp : word;
          e_cp : word;
          e_crlc : word;
          e_cparhdr : word;
          e_minalloc : word;
          e_maxalloc : word;
          e_ss : word;
          e_sp : word;
          e_csum : word;
          e_ip : word;
          e_cs : word;
          e_lfarlc : word;
          e_ovno : word;
          e_res : array[0..3] of word;
          e_oemid : word;
          e_oeminfo : word;
          e_res2 : array[0..9] of word;
          e_lfanew : longint;
       end;

       tpeheader = packed record
          PEMagic : array[0..3] of char;
          Machine : word;
          NumberOfSections : word;
          TimeDateStamp : longint;
          PointerToSymbolTable : longint;
          NumberOfSymbols : longint;
          SizeOfOptionalHeader : word;
          Characteristics : word;
          Magic : word;
          MajorLinkerVersion : byte;
          MinorLinkerVersion : byte;
          SizeOfCode : longint;
          SizeOfInitializedData : longint;
          SizeOfUninitializedData : longint;
          AddressOfEntryPoint : longint;
          BaseOfCode : longint;
          BaseOfData : longint;
          ImageBase : longint;
          SectionAlignment : longint;
          FileAlignment : longint;
          MajorOperatingSystemVersion : word;
          MinorOperatingSystemVersion : word;
          MajorImageVersion : word;
          MinorImageVersion : word;
          MajorSubsystemVersion : word;
          MinorSubsystemVersion : word;
          Reserved1 : longint;
          SizeOfImage : longint;
          SizeOfHeaders : longint;
          CheckSum : longint;
          Subsystem : word;
          DllCharacteristics : word;
          SizeOfStackReserve : longint;
          SizeOfStackCommit : longint;
          SizeOfHeapReserve : longint;
          SizeOfHeapCommit : longint;
          LoaderFlags : longint;
          NumberOfRvaAndSizes : longint;
          { DataDirectory : array[0..(IMAGE_NUMBEROF_DIRECTORY_ENTRIES)-1] of IMAGE_DATA_DIRECTORY; }
       end;

    procedure timportlibwin32.preparelib(const s : string);

      begin
         if not(assigned(importssection)) then
           importssection:=new(paasmoutput,init);
      end;

    procedure timportlibwin32.importprocedure(const func,module : string;index : longint;const name : string);

      var
         hp1 : pimportlist;
         hp2 : pimported_procedure;
         hs  : string;
      begin
         hs:=SplitName(module);
         { search for the module }
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              if hs=hp1^.dllname^ then
                break;
              hp1:=pimportlist(hp1^.next);
           end;
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              hp1:=new(pimportlist,init(hs));
              current_module^.imports^.concat(hp1);
           end;
         hp2:=new(pimported_procedure,init(func,name,index));
         hp1^.imported_procedures^.concat(hp2);
      end;


    procedure timportlibwin32.generatesmartlib;
      var
         hp1 : pimportlist;
         hp2 : pimported_procedure;
         lhead,lname,lcode,
         lidata4,lidata5 : plabel;
         r : preference;
      begin
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              importssection^.concat(new(pai_cut,init));
              codesegment^.concat(new(pai_cut,init));
            { create header for this importmodule }
              { Get labels for the sections }
              getdatalabel(lhead);
              getdatalabel(lname);
              getlabel(lidata4);
              getlabel(lidata5);
              importssection^.concat(new(pai_section,init_idata(2)));
              importssection^.concat(new(pai_label,init(lhead)));
              { pointer to procedure names }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(lidata4)))));
              { two empty entries follow }
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { pointer to dll name }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(lname)))));
              { pointer to fixups }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(lidata5)))));
              { first write the name references }
              importssection^.concat(new(pai_section,init_idata(4)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_label,init(lidata4)));
              { then the addresses and create also the indirect jump }
              importssection^.concat(new(pai_section,init_idata(5)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_label,init(lidata5)));

              { write final section }
              importssection^.concat(new(pai_cut,init_end));
              { end of name references }
              importssection^.concat(new(pai_section,init_idata(4)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { end if addresses }
              importssection^.concat(new(pai_section,init_idata(5)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { dllname }
              importssection^.concat(new(pai_section,init_idata(7)));
              importssection^.concat(new(pai_label,init(lname)));
              importssection^.concat(new(pai_string,init(hp1^.dllname^+target_os.sharedlibext+#0)));

              { create procedures }
              hp2:=pimported_procedure(hp1^.imported_procedures^.first);
              while assigned(hp2) do
                begin
                  { insert cuts }
                  importssection^.concat(new(pai_cut,init));
                  { create indirect jump }
                  getlabel(lcode);
                  new(r);
                  reset_reference(r^);
                  r^.symbol:=stringdup(lab2str(lcode));
                  { place jump in codesegment, insert a code section in the
                    importsection to reduce the amount of .s files (PFV) }
                  importssection^.concat(new(pai_section,init(sec_code)));
{$IfDef GDB}
                  if (cs_debuginfo in aktmoduleswitches) then
                   importssection^.concat(new(pai_stab_function_name,init(nil)));
{$EndIf GDB}
                  importssection^.concat(new(pai_align,init_op(4,$90)));
                  importssection^.concat(new(pai_symbol,init_global(hp2^.func^)));
                  importssection^.concat(new(pai386,op_ref(A_JMP,S_NO,r)));
                  { create head link }
                  importssection^.concat(new(pai_section,init_idata(7)));
                  importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(lhead)))));
                  { fixup }
                  getlabel(plabel(hp2^.lab));
                  importssection^.concat(new(pai_section,init_idata(4)));
                  importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(hp2^.lab)))));
                  { add jump field to importsection }
                  importssection^.concat(new(pai_section,init_idata(5)));
                  importssection^.concat(new(pai_label,init(lcode)));
                  importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(hp2^.lab)))));
                  { finally the import information }
                  importssection^.concat(new(pai_section,init_idata(6)));
                  importssection^.concat(new(pai_label,init(hp2^.lab)));
                  importssection^.concat(new(pai_const,init_16bit(hp2^.ordnr)));
                  importssection^.concat(new(pai_string,init(hp2^.name^+#0)));

                  hp2:=pimported_procedure(hp2^.next);
                end;
              hp1:=pimportlist(hp1^.next);
           end;
       end;


    procedure timportlibwin32.generatelib;
      var
         hp1 : pimportlist;
         hp2 : pimported_procedure;
         l1,l2,l3,l4 : plabel;
         r : preference;
      begin
         if (cs_smartlink in aktmoduleswitches) then
          begin
            generatesmartlib;
            exit;
          end;

         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              { Insert cuts for smartlinking }
              if (cs_smartlink in aktmoduleswitches) then
                begin
                  importssection^.concat(new(pai_cut,init));
                  codesegment^.concat(new(pai_cut,init));
                end;
{$IfDef GDB}
              if (cs_debuginfo in aktmoduleswitches) then
                codesegment^.concat(new(pai_stab_function_name,init(nil)));
{$EndIf GDB}

              { Get labels for the sections }
              getlabel(l1);
              getlabel(l2);
              getlabel(l3);
              importssection^.concat(new(pai_section,init_idata(2)));
              { pointer to procedure names }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(l2)))));
              { two empty entries follow }
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { pointer to dll name }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(l1)))));
              { pointer to fixups }
              importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(l3)))));

              { only create one section for each else it will
                create a lot of idata* }

              { first write the name references }
              importssection^.concat(new(pai_section,init_idata(4)));
              importssection^.concat(new(pai_label,init(l2)));

              hp2:=pimported_procedure(hp1^.imported_procedures^.first);
              while assigned(hp2) do
                begin
                   getlabel(plabel(hp2^.lab));
                   importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(hp2^.lab)))));
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
                   getdatalabel(l4);
                   { create indirect jump }
                   new(r);
                   reset_reference(r^);
                   r^.symbol:=stringdup(lab2str(l4));
                   { place jump in codesegment }
                   codesegment^.concat(new(pai_align,init_op(4,$90)));
                   codesegment^.concat(new(pai_symbol,init_global(hp2^.func^)));
                   codesegment^.concat(new(pai386,op_ref(A_JMP,S_NO,r)));
                   { add jump field to importsection }
                   importssection^.concat(new(pai_label,init(l4)));
                   importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(hp2^.lab)))));
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
              importssection^.concat(new(pai_string,init(hp1^.dllname^+target_os.sharedlibext+#0)));

              hp1:=pimportlist(hp1^.next);
           end;
      end;

    procedure texportlibwin32.preparelib(const s:string);

      begin
         if not(assigned(exportssection)) then
           exportssection:=new(paasmoutput,init);
      end;

    procedure texportlibwin32.generatelib;

      var
         ordinal_base,entries,named_entries : longint;
         l1,l2,l3,l4 : plabel;

      begin
         ordinal_base:=0;
         getlabel(l1);
         getlabel(l2);
         getlabel(l3);
         getlabel(l4);
         { export flags }
         exportssection^.concat(new(pai_const,init_32bit(0)));
         { date/time stamp }
         exportssection^.concat(new(pai_const,init_32bit(0)));
         { major version }
         exportssection^.concat(new(pai_const,init_16bit(0)));
         { minor version }
         exportssection^.concat(new(pai_const,init_16bit(0)));
         { pointer to dll name }
         importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(l1)))));
         { ordinal base }
         exportssection^.concat(new(pai_const,init_32bit(0)));
         { number of entries }
         exportssection^.concat(new(pai_const,init_32bit(entries)));
         { number of named entries }
         exportssection^.concat(new(pai_const,init_32bit(named_entries)));
         { address of export address table }
         importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(l2)))));
         { address of name pointer pointers }
         importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(l3)))));
         { address of ordinal number pointers }
         importssection^.concat(new(pai_const,init_rva(strpnew(lab2str(l4)))));
         { the name }
         importssection^.concat(new(pai_label,init(l1)));
         importssection^.concat(new(pai_string,init(current_module^.modulename^+target_os.sharedlibext+#0)));

      end;

    procedure postprocessexecutable(n : string);

      var
         f : file;
         dosheader : tdosheader;
         peheader : tpeheader;
         peheaderpos : longint;

      begin
         assign(f,n);
         {$i-}
         reset(f,1);
         if ioresult<>0 then
           Message1(execinfo_f_cant_open_executable,n);
         blockread(f,dosheader,sizeof(tdosheader));
         peheaderpos:=dosheader.e_lfanew;
         seek(f,peheaderpos);
         blockread(f,peheader,sizeof(tpeheader));

         { write info }
         Message1(execinfo_x_codesize,tostr(peheader.SizeOfCode));
         Message1(execinfo_x_initdatasize,tostr(peheader.SizeOfInitializedData));
         Message1(execinfo_x_uninitdatasize,tostr(peheader.SizeOfUninitializedData));
         Message1(execinfo_x_stackreserve,tostr(peheader.SizeOfStackReserve));
         Message1(execinfo_x_stackcommit,tostr(peheader.SizeOfStackCommit));

         { change the header }

         { sub system }
         { gui=2 }
         { cui=3 }
         if apptype=at_gui then
           peheader.Subsystem:=2
         else if apptype=at_cui then
           peheader.Subsystem:=3;
         seek(f,peheaderpos);
         blockwrite(f,peheader,sizeof(tpeheader));
         close(f);
         if ioresult<>0 then
           Message1(execinfo_f_cant_process_executable,n);
      end;

end.
{
  $Log$
  Revision 1.13  1998-10-29 11:35:54  florian
    * some dll support for win32
    * fixed assembler writing for PalmOS

  Revision 1.12  1998/10/27 10:22:35  florian
    + First things for win32 export sections

  Revision 1.11  1998/10/22 17:54:09  florian
    + switch $APPTYPE for win32 added

  Revision 1.10  1998/10/22 15:18:51  florian
    + switch -vx for win32 added

  Revision 1.9  1998/10/19 15:41:03  peter
    * better splitname to support glib-1.1.dll alike names

  Revision 1.8  1998/09/07 18:33:35  peter
    + smartlinking for win95 imports

  Revision 1.7  1998/09/03 17:39:06  florian
    + better code for type conversation longint/dword to real type

  Revision 1.6  1998/08/10 14:50:38  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.5  1998/06/10 10:43:18  peter
    * write also the .dll extension (needed for NT)

  Revision 1.4  1998/06/08 22:59:56  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.3  1998/06/04 23:52:06  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.2  1998/05/06 18:36:55  peter
    * tai_section extended with code,data,bss sections and enumerated type
    * ident 'compiled by FPC' moved to pmodules
    * small fix for smartlink

}
