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
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
      procedure generatesmartlib;
    end;

    pexportlibwin32=^texportlibwin32;
    texportlibwin32=object(texportlib)
      st : string;
      last_index : longint;
      procedure preparelib(const s:string);virtual;
      procedure exportprocedure(hp : pexported_item);virtual;
      procedure exportvar(hp : pexported_item);virtual;
      procedure generatelib;virtual;
    end;

    { sets some flags of the executable }
    procedure postprocessexecutable(n : string);

  implementation

    uses
       aasm,files,strings,globtype,globals,cobjects,systems,verbose
{$ifdef GDB}
       ,gdb
{$endif}
{$ifdef i386}
{$ifndef OLDASM}
       ,i386base,i386asm
{$else}
       ,i386
{$endif}
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

    function DllName(Const Name : string) : string;
      var n : string;
      begin
         n:=Upper(SplitExtension(Name));
         if (n='.DLL') or (n='.DRV') or (n='.EXE') then
           DllName:=Name
         else
           DllName:=Name+target_os.sharedlibext;
      end;
    procedure timportlibwin32.preparelib(const s : string);

      begin
         if not(assigned(importssection)) then
           importssection:=new(paasmoutput,init);
      end;

    procedure timportlibwin32.importprocedure(const func,module : string;index : longint;const name : string);
      var
         hp1 : pimportlist;
         hp2 : pimported_item;
         hs  : string;
      begin
         { that IS wrong for DRV files
         hs:=SplitName(module); }
         hs:=DllName(module);
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
         hp2:=new(pimported_item,init(func,name,index));
         hp1^.imported_items^.concat(hp2);
      end;


    procedure timportlibwin32.importvariable(const varname,module:string;const name:string);
      var
         hp1 : pimportlist;
         hp2 : pimported_item;
         hs  : string;
      begin
         hs:=DllName(module);
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
         hp2:=new(pimported_item,init_var(varname,name));
         hp1^.imported_items^.concat(hp2);
      end;


    procedure timportlibwin32.generatesmartlib;
      var
         hp1 : pimportlist;
         hp2 : pimported_item;
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
              importssection^.concat(new(pai_section,init(sec_idata2)));
              importssection^.concat(new(pai_label,init(lhead)));
              { pointer to procedure names }
              importssection^.concat(new(pai_const_symbol,init_rva(lab2str(lidata4))));
              { two empty entries follow }
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { pointer to dll name }
              importssection^.concat(new(pai_const_symbol,init_rva(lab2str(lname))));
              { pointer to fixups }
              importssection^.concat(new(pai_const_symbol,init_rva(lab2str(lidata5))));
              { first write the name references }
              importssection^.concat(new(pai_section,init(sec_idata4)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_label,init(lidata4)));
              { then the addresses and create also the indirect jump }
              importssection^.concat(new(pai_section,init(sec_idata5)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_label,init(lidata5)));

              { write final section }
              importssection^.concat(new(pai_cut,init_end));
              { end of name references }
              importssection^.concat(new(pai_section,init(sec_idata4)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { end if addresses }
              importssection^.concat(new(pai_section,init(sec_idata5)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { dllname }
              importssection^.concat(new(pai_section,init(sec_idata7)));
              importssection^.concat(new(pai_label,init(lname)));
              importssection^.concat(new(pai_string,init(hp1^.dllname^+{target_os.sharedlibext+}#0)));

              { create procedures }
              hp2:=pimported_item(hp1^.imported_items^.first);
              while assigned(hp2) do
                begin
                  { insert cuts }
                  importssection^.concat(new(pai_cut,init));
                  { create indirect jump }
                  if not hp2^.is_var then
                   begin
                     getlabel(lcode);
                     new(r);
                     reset_reference(r^);
                     r^.symbol:=newasmsymbol(lab2str(lcode));
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
                   end;
                  { create head link }
                  importssection^.concat(new(pai_section,init(sec_idata7)));
                  importssection^.concat(new(pai_const_symbol,init_rva(lab2str(lhead))));
                  { fixup }
                  getlabel(plabel(hp2^.lab));
                  importssection^.concat(new(pai_section,init(sec_idata4)));
                  importssection^.concat(new(pai_const_symbol,init_rva(lab2str(hp2^.lab))));
                  { add jump field to importsection }
                  importssection^.concat(new(pai_section,init(sec_idata5)));
                  if hp2^.is_var then
                   importssection^.concat(new(pai_symbol,init_global(hp2^.func^)))
                  else
                   importssection^.concat(new(pai_label,init(lcode)));
                   if hp2^.name^<>'' then
                     importssection^.concat(new(pai_const_symbol,init_rva(lab2str(hp2^.lab))))
                   else
                     importssection^.concat(new(pai_const,init_32bit($80000000 or hp2^.ordnr)));
                  { finally the import information }
                  importssection^.concat(new(pai_section,init(sec_idata6)));
                  importssection^.concat(new(pai_label,init(hp2^.lab)));
                  importssection^.concat(new(pai_const,init_16bit(hp2^.ordnr)));
                  importssection^.concat(new(pai_string,init(hp2^.name^+#0)));
                  importssection^.concat(new(pai_align,init_op(2,0)));
                  hp2:=pimported_item(hp2^.next);
                end;
              hp1:=pimportlist(hp1^.next);
           end;
       end;


    procedure timportlibwin32.generatelib;
      var
         hp1 : pimportlist;
         hp2 : pimported_item;
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
              importssection^.concat(new(pai_section,init(sec_idata2)));
              { pointer to procedure names }
              importssection^.concat(new(pai_const_symbol,init_rva(lab2str(l2))));
              { two empty entries follow }
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { pointer to dll name }
              importssection^.concat(new(pai_const_symbol,init_rva(lab2str(l1))));
              { pointer to fixups }
              importssection^.concat(new(pai_const_symbol,init_rva(lab2str(l3))));

              { only create one section for each else it will
                create a lot of idata* }

              { first write the name references }
              importssection^.concat(new(pai_section,init(sec_idata4)));
              importssection^.concat(new(pai_label,init(l2)));

              hp2:=pimported_item(hp1^.imported_items^.first);
              while assigned(hp2) do
                begin
                   getlabel(plabel(hp2^.lab));
                   if hp2^.name^<>'' then
                     importssection^.concat(new(pai_const_symbol,init_rva(lab2str(hp2^.lab))))
                   else
                     importssection^.concat(new(pai_const,init_32bit($80000000 or hp2^.ordnr)));
                   hp2:=pimported_item(hp2^.next);
                end;
              { finalize the names ... }
              importssection^.concat(new(pai_const,init_32bit(0)));

              { then the addresses and create also the indirect jump }
              importssection^.concat(new(pai_section,init(sec_idata5)));
              importssection^.concat(new(pai_label,init(l3)));
              hp2:=pimported_item(hp1^.imported_items^.first);
              while assigned(hp2) do
                begin
                   if not hp2^.is_var then
                    begin
                      getdatalabel(l4);
                      { create indirect jump }
                      new(r);
                      reset_reference(r^);
                      r^.symbol:=newasmsymbol(lab2str(l4));
                      { place jump in codesegment }
                      codesegment^.concat(new(pai_align,init_op(4,$90)));
                      codesegment^.concat(new(pai_symbol,init_global(hp2^.func^)));
                      codesegment^.concat(new(pai386,op_ref(A_JMP,S_NO,r)));
                      { add jump field to importsection }
                      importssection^.concat(new(pai_label,init(l4)));
                    end
                   else
                    begin
                      importssection^.concat(new(pai_symbol,init_global(hp2^.func^)));
                    end;
                   importssection^.concat(new(pai_const_symbol,init_rva(lab2str(hp2^.lab))));
                   hp2:=pimported_item(hp2^.next);
                end;
              { finalize the addresses }
              importssection^.concat(new(pai_const,init_32bit(0)));

              { finally the import information }
              importssection^.concat(new(pai_section,init(sec_idata6)));
              hp2:=pimported_item(hp1^.imported_items^.first);
              while assigned(hp2) do
                begin
                   importssection^.concat(new(pai_label,init(hp2^.lab)));
                   { the ordinal number }
                   importssection^.concat(new(pai_const,init_16bit(hp2^.ordnr)));
                   importssection^.concat(new(pai_string,init(hp2^.name^+#0)));
                   importssection^.concat(new(pai_align,init_op(2,0)));
                   hp2:=pimported_item(hp2^.next);
                end;
              { create import dll name }
              importssection^.concat(new(pai_section,init(sec_idata7)));
              importssection^.concat(new(pai_label,init(l1)));
              importssection^.concat(new(pai_string,init(hp1^.dllname^+{target_os.sharedlibext+}#0)));

              hp1:=pimportlist(hp1^.next);
           end;
      end;

    procedure texportlibwin32.preparelib(const s:string);

      begin
         if not(assigned(exportssection)) then
           exportssection:=new(paasmoutput,init);
         last_index:=0;
      end;



    procedure texportlibwin32.exportvar(hp : pexported_item);
      begin
         { same code used !! PM }
         exportprocedure(hp);
      end;


    procedure texportlibwin32.exportprocedure(hp : pexported_item);

      { must be ordered at least for win32 !! }
      var hp2 : pexported_item;

    begin
        hp2:=pexported_item(current_module^._exports^.first);
        { first test the index value }
        if (hp^.options and eo_index)<>0 then
          begin
             if (hp^.index<=0) or (hp^.index>$ffff) then
               message1(parser_e_export_invalid_index,tostr(hp^.index))
             else while assigned(hp2) do
               begin
                  if (hp^.index=hp2^.index) then
                    if ((hp2^.options and eo_index)<>0) then
                      message1(parser_e_export_ordinal_double,tostr(hp^.index))
                    else
                      begin
                         inc(last_index);
                         hp2^.index:=last_index;
                      end;
                  hp2:=pexported_item(hp2^.next);
               end;
             if hp^.index=last_index+1 then
               inc(last_index);
          end
        else
          begin
             inc(last_index);
             hp^.index:=last_index;
          end;
        { use pascal name is none specified }
        if (hp^.options and eo_name)=0 then
          begin
             hp^.name:=stringdup(hp^.sym^.name);
             hp^.options:=hp^.options or eo_name;
          end;

        { now place in correct order }
        hp2:=pexported_item(current_module^._exports^.first);
        while assigned(hp2) and
           (hp^.name^>hp2^.name^) do
          hp2:=pexported_item(hp2^.next);
        { insert hp there !! }
        if assigned(hp2) and (hp2^.name^=hp^.name^) then
          begin
             { this is not allowed !! }
             message1(parser_e_export_name_double,hp^.name^);
          end;
        if hp2=pexported_item(current_module^._exports^.first) then
          current_module^._exports^.insert(hp)
        else if assigned(hp2) then
          begin
             hp^.next:=hp2;
             hp^.previous:=hp2^.previous;
             if assigned(hp2^.previous) then
               hp2^.previous^.next:=hp;
             hp2^.previous:=hp;
          end
        else
          current_module^._exports^.concat(hp);
    end;


    procedure texportlibwin32.generatelib;

      var
         ordinal_base,ordinal_max,ordinal_min : longint;
         current_index : longint;
         entries,named_entries : longint;
         name_label,dll_name_label,export_address_table : plabel;
         export_name_table_pointers,export_ordinal_table : plabel;
         hp,hp2 : pexported_item;
         tempexport : plinkedlist;
         address_table,name_table_pointers,
         name_table,ordinal_table : paasmoutput;

      begin
         ordinal_max:=0;
         ordinal_min:=$7FFFFFFF;
         entries:=0;
         named_entries:=0;
         getlabel(dll_name_label);
         getlabel(export_address_table);
         getlabel(export_name_table_pointers);
         getlabel(export_ordinal_table);

         hp:=pexported_item(current_module^._exports^.first);

         { count entries }
         while assigned(hp) do
           begin
              inc(entries);
              if (hp^.index>ordinal_max) then
                ordinal_max:=hp^.index;
              if (hp^.index>0) and (hp^.index<ordinal_min) then
                ordinal_min:=hp^.index;
              if assigned(hp^.name) then
                inc(named_entries);
              hp:=pexported_item(hp^.next);
           end;

         { no support for higher ordinal base yet !! }
         ordinal_base:=1;
         current_index:=ordinal_base;
         { we must also count the holes !! }
         entries:=ordinal_max-ordinal_base+1;

         exportssection^.concat(new(pai_section,init(sec_edata)));
         { export flags }
         exportssection^.concat(new(pai_const,init_32bit(0)));
         { date/time stamp }
         exportssection^.concat(new(pai_const,init_32bit(0)));
         { major version }
         exportssection^.concat(new(pai_const,init_16bit(0)));
         { minor version }
         exportssection^.concat(new(pai_const,init_16bit(0)));
         { pointer to dll name }
         exportssection^.concat(new(pai_const_symbol,init_rva(lab2str(dll_name_label))));
         { ordinal base normally set to 1 }
         exportssection^.concat(new(pai_const,init_32bit(ordinal_base)));
         { number of entries }
         exportssection^.concat(new(pai_const,init_32bit(entries)));
         { number of named entries }
         exportssection^.concat(new(pai_const,init_32bit(named_entries)));
         { address of export address table }
         exportssection^.concat(new(pai_const_symbol,init_rva(lab2str(export_address_table))));
         { address of name pointer pointers }
         exportssection^.concat(new(pai_const_symbol,init_rva(lab2str(export_name_table_pointers))));
         { address of ordinal number pointers }
         exportssection^.concat(new(pai_const_symbol,init_rva(lab2str(export_ordinal_table))));
         { the name }
         exportssection^.concat(new(pai_label,init(dll_name_label)));
         if st='' then
           exportssection^.concat(new(pai_string,init(current_module^.modulename^+target_os.sharedlibext+#0)))
         else
           exportssection^.concat(new(pai_string,init(st+target_os.sharedlibext+#0)));

         {  export address table }
         address_table:=new(paasmoutput,init);
         address_table^.concat(new(pai_align,init_op(4,0)));
         address_table^.concat(new(pai_label,init(export_address_table)));
         name_table_pointers:=new(paasmoutput,init);
         name_table_pointers^.concat(new(pai_align,init_op(4,0)));
         name_table_pointers^.concat(new(pai_label,init(export_name_table_pointers)));
         ordinal_table:=new(paasmoutput,init);
         ordinal_table^.concat(new(pai_align,init_op(4,0)));
         ordinal_table^.concat(new(pai_label,init(export_ordinal_table)));
         name_table:=new(paasmoutput,init);
         name_table^.concat(new(pai_align,init_op(4,0)));
         { write each address }
         hp:=pexported_item(current_module^._exports^.first);
         while assigned(hp) do
           begin
              if (hp^.options and eo_name)<>0 then
                begin
                   getlabel(name_label);
                   name_table_pointers^.concat(new(pai_const_symbol,init_rva(lab2str(name_label))));
                   ordinal_table^.concat(new(pai_const,init_16bit(hp^.index-ordinal_base)));
                   name_table^.concat(new(pai_align,init_op(2,0)));
                   name_table^.concat(new(pai_label,init(name_label)));
                   name_table^.concat(new(pai_string,init(hp^.name^+#0)));
                end;
              hp:=pexported_item(hp^.next);
           end;
         { order in increasing ordinal values }
         { into tempexport list }
         tempexport:=new(plinkedlist,init);
         hp:=pexported_item(current_module^._exports^.first);
         while assigned(hp) do
           begin
              current_module^._exports^.remove(hp);
              hp2:=pexported_item(tempexport^.first);
              while assigned(hp2) and (hp^.index>hp2^.index) do
                begin
                   hp2:=pexported_item(hp2^.next);
                end;
              if hp2=pexported_item(tempexport^.first) then
                 tempexport^.insert(hp)
              else
                begin
                   if assigned(hp2) then
                     begin
                        hp^.next:=hp2;
                        hp^.previous:=hp2^.previous;
                        hp2^.previous:=hp;
                        if assigned(hp^.previous) then
                          hp^.previous^.next:=hp;
                      end
                    else
                      tempexport^.concat(hp);
                end;
              hp:=pexported_item(current_module^._exports^.first);;
           end;



         { write the export adress table }
         current_index:=ordinal_base;
         hp:=pexported_item(tempexport^.first);
         while assigned(hp) do
           begin
              { fill missing values }
              while current_index<hp^.index do
                begin
                   address_table^.concat(new(pai_const,init_32bit(0)));
                   inc(current_index);
                end;
              address_table^.concat(new(pai_const_symbol,init_rva(hp^.sym^.mangledname)));
              inc(current_index);
              hp:=pexported_item(hp^.next);
           end;

         exportssection^.concatlist(address_table);
         exportssection^.concatlist(name_table_pointers);
         exportssection^.concatlist(ordinal_table);
         exportssection^.concatlist(name_table);
         dispose(address_table,done);
         dispose(name_table_pointers,done);
         dispose(ordinal_table,done);
         dispose(name_table,done);
         dispose(tempexport,done);
      end;

    procedure postprocessexecutable(n : string);

      var
         f : file;
         dosheader : tdosheader;
         peheader : tpeheader;
         peheaderpos : longint;

      begin
         { when -s is used quit, because there is no .exe }
         if cs_link_extern in aktglobalswitches then
          exit;
         { open file }
         assign(f,n);
         {$I-}
          reset(f,1);
         if ioresult<>0 then
           Message1(execinfo_f_cant_open_executable,n);
         { read headers }
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
         {$I+}
      end;

end.
{
  $Log$
  Revision 1.24  1999-05-01 13:25:04  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.23  1999/04/07 14:18:32  pierre
   * typo correction

  Revision 1.22  1999/04/07 14:04:40  pierre
    * adds .dll as library suffix only if
      the name does not end with .dll .drv or .exe !

  Revision 1.21  1999/02/25 21:02:59  peter
    * ag386bin updates
    + coff writer

  Revision 1.20  1999/02/22 02:44:14  peter
    * ag386bin doesn't use i386.pas anymore

  Revision 1.19  1998/12/11 00:04:06  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.18  1998/12/02 10:26:13  pierre
    * writing of .edata was wrong for indexes above number of exported items
    * importing by index only did not work !

  Revision 1.17  1998/12/01 23:35:43  pierre
   * alignment fixes

  Revision 1.16  1998/11/30 13:26:26  pierre
    * the code for ordering the exported procs/vars was buggy
    + added -WB to force binding (Ozerski way of creating DLL)
      this is off by default as direct writing of .edata section seems
      OK

  Revision 1.15  1998/11/30 09:43:25  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.14  1998/11/28 16:21:00  peter
    + support for dll variables

  Revision 1.13  1998/10/29 11:35:54  florian
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
