{
    $Id$
    Copyright (c) 1998-2000 by Peter Vreman

    This unit implements support import,export,link routines
    for the (i386) Win32 target

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
unit t_win32;

  interface

  uses
    import,export,link;

  const
     winstackpagesize = 4096;

  type
    pimportlibwin32=^timportlibwin32;
    timportlibwin32=object(timportlib)
      procedure preparelib(const s:string);virtual;
      procedure importprocedure(const func,module:string;index:longint;const name:string);virtual;
      procedure importvariable(const varname,module:string;const name:string);virtual;
      procedure generatelib;virtual;
      procedure generatesmartlib;virtual;
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

    plinkerwin32=^tlinkerwin32;
    tlinkerwin32=object(tlinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
       Function  PostProcessExecutable(const fn:string;isdll:boolean) : Boolean;
    public
       Constructor Init;
       Procedure SetDefaultInfo;virtual;
       function  MakeExecutable:boolean;virtual;
       function  MakeSharedLibrary:boolean;virtual;
    end;


  implementation

    uses
{$ifdef PAVEL_LINKLIB}
{$ifdef Delphi}
      dmisc,
{$else Delphi}
      dos,
{$endif Delphi}
       impdef,
{$endif PAVEL_LINKLIB}
       aasm,files,globtype,globals,cobjects,systems,verbose,
       script,gendef,
       cpubase,cpuasm
{$ifdef GDB}
       ,gdb
{$endif}
       ;

    function DllName(Const Name : string) : string;
      var n : string;
      begin
         n:=Upper(SplitExtension(Name));
         if (n='.DLL') or (n='.DRV') or (n='.EXE') then
           DllName:=Name
         else
           DllName:=Name+target_os.sharedlibext;
      end;


{*****************************************************************************
                             TIMPORTLIBWIN32
*****************************************************************************}

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
         { search for reuse of old import item }
         hp2:=pimported_item(hp1^.imported_items^.first);
         while assigned(hp2) do
          begin
            if hp2^.func^=func then
             break;
            hp2:=pimported_item(hp2^.next);
          end;
         if not assigned(hp2) then
          begin
            hp2:=new(pimported_item,init(func,name,index));
            hp1^.imported_items^.concat(hp2);
          end;
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
         lidata4,lidata5 : pasmlabel;
         r : preference;
      begin
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
           { Get labels for the sections }
             getdatalabel(lhead);
             getdatalabel(lname);
             getaddrlabel(lidata4);
             getaddrlabel(lidata5);
           { create header for this importmodule }
             importssection^.concat(new(pai_cut,init_begin));
             importssection^.concat(new(pai_section,init(sec_idata2)));
             importssection^.concat(new(pai_label,init(lhead)));
             { pointer to procedure names }
             importssection^.concat(new(pai_const_symbol,init_rva(lidata4)));
             { two empty entries follow }
             importssection^.concat(new(pai_const,init_32bit(0)));
             importssection^.concat(new(pai_const,init_32bit(0)));
             { pointer to dll name }
             importssection^.concat(new(pai_const_symbol,init_rva(lname)));
             { pointer to fixups }
             importssection^.concat(new(pai_const_symbol,init_rva(lidata5)));
             { first write the name references }
             importssection^.concat(new(pai_section,init(sec_idata4)));
             importssection^.concat(new(pai_const,init_32bit(0)));
             importssection^.concat(new(pai_label,init(lidata4)));
             { then the addresses and create also the indirect jump }
             importssection^.concat(new(pai_section,init(sec_idata5)));
             importssection^.concat(new(pai_const,init_32bit(0)));
             importssection^.concat(new(pai_label,init(lidata5)));

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
                    r^.symbol:=lcode;
                    { place jump in codesegment, insert a code section in the
                      importsection to reduce the amount of .s files (PFV) }
                    importssection^.concat(new(pai_section,init(sec_code)));
{$IfDef GDB}
                    if (cs_debuginfo in aktmoduleswitches) then
                     importssection^.concat(new(pai_stab_function_name,init(nil)));
{$EndIf GDB}
                    importssection^.concat(new(pai_symbol,initname_global(hp2^.func^,0)));
                    importssection^.concat(new(paicpu,op_ref(A_JMP,S_NO,r)));
                    importssection^.concat(new(pai_align,init_op(4,$90)));
                  end;
                 { create head link }
                 importssection^.concat(new(pai_section,init(sec_idata7)));
                 importssection^.concat(new(pai_const_symbol,init_rva(lhead)));
                 { fixup }
                 getlabel(pasmlabel(hp2^.lab));
                 importssection^.concat(new(pai_section,init(sec_idata4)));
                 importssection^.concat(new(pai_const_symbol,init_rva(hp2^.lab)));
                 { add jump field to importsection }
                 importssection^.concat(new(pai_section,init(sec_idata5)));
                 if hp2^.is_var then
                  importssection^.concat(new(pai_symbol,initname_global(hp2^.func^,0)))
                 else
                  importssection^.concat(new(pai_label,init(lcode)));
                  if hp2^.name^<>'' then
                    importssection^.concat(new(pai_const_symbol,init_rva(hp2^.lab)))
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
              importssection^.concat(new(pai_string,init(hp1^.dllname^+#0)));

              hp1:=pimportlist(hp1^.next);
           end;
       end;


    procedure timportlibwin32.generatelib;
      var
         hp1 : pimportlist;
         hp2 : pimported_item;
         l1,l2,l3,l4 : pasmlabel;
         r : preference;
      begin
         hp1:=pimportlist(current_module^.imports^.first);
         while assigned(hp1) do
           begin
              { align codesegment for the jumps }
              importssection^.concat(new(pai_section,init(sec_code)));
              importssection^.concat(new(pai_align,init_op(4,$90)));
              { Get labels for the sections }
              getlabel(l1);
              getlabel(l2);
              getlabel(l3);
              importssection^.concat(new(pai_section,init(sec_idata2)));
              { pointer to procedure names }
              importssection^.concat(new(pai_const_symbol,init_rva(l2)));
              { two empty entries follow }
              importssection^.concat(new(pai_const,init_32bit(0)));
              importssection^.concat(new(pai_const,init_32bit(0)));
              { pointer to dll name }
              importssection^.concat(new(pai_const_symbol,init_rva(l1)));
              { pointer to fixups }
              importssection^.concat(new(pai_const_symbol,init_rva(l3)));

              { only create one section for each else it will
                create a lot of idata* }

              { first write the name references }
              importssection^.concat(new(pai_section,init(sec_idata4)));
              importssection^.concat(new(pai_label,init(l2)));

              hp2:=pimported_item(hp1^.imported_items^.first);
              while assigned(hp2) do
                begin
                   getlabel(pasmlabel(hp2^.lab));
                   if hp2^.name^<>'' then
                     importssection^.concat(new(pai_const_symbol,init_rva(hp2^.lab)))
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
                      getlabel(l4);
                      { create indirect jump }
                      new(r);
                      reset_reference(r^);
                      r^.symbol:=l4;
                      { place jump in codesegment }
                      importssection^.concat(new(pai_section,init(sec_code)));
                      importssection^.concat(new(pai_symbol,initname_global(hp2^.func^,0)));
                      importssection^.concat(new(paicpu,op_ref(A_JMP,S_NO,r)));
                      importssection^.concat(new(pai_align,init_op(4,$90)));
                      { add jump field to importsection }
                      importssection^.concat(new(pai_section,init(sec_idata5)));
                      importssection^.concat(new(pai_label,init(l4)));
                    end
                   else
                    begin
                      importssection^.concat(new(pai_symbol,initname_global(hp2^.func^,0)));
                    end;
                   importssection^.concat(new(pai_const_symbol,init_rva(hp2^.lab)));
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
              importssection^.concat(new(pai_string,init(hp1^.dllname^+#0)));

              hp1:=pimportlist(hp1^.next);
           end;
      end;


{*****************************************************************************
                             TEXPORTLIBWIN32
*****************************************************************************}

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
      var
        hp2 : pexported_item;
      begin
        { first test the index value }
        if (hp^.options and eo_index)<>0 then
          begin
             if (hp^.index<=0) or (hp^.index>$ffff) then
               begin
                 message1(parser_e_export_invalid_index,tostr(hp^.index));
                 exit;
               end;
             if (hp^.index<=last_index) then
               begin
                 message1(parser_e_export_ordinal_double,tostr(hp^.index));
                 { disregard index value }
                 inc(last_index);
                 hp^.index:=last_index;
                 exit;
               end
             else
               begin
                 last_index:=hp^.index;
               end;
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
             exit;
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
         name_label,dll_name_label,export_address_table : pasmlabel;
         export_name_table_pointers,export_ordinal_table : pasmlabel;
         hp,hp2 : pexported_item;
         tempexport : plinkedlist;
         address_table,name_table_pointers,
         name_table,ordinal_table : paasmoutput;
      begin

         hp:=pexported_item(current_module^._exports^.first);
         if not assigned(hp) then
           exit;

         ordinal_max:=0;
         ordinal_min:=$7FFFFFFF;
         entries:=0;
         named_entries:=0;
         getlabel(dll_name_label);
         getlabel(export_address_table);
         getlabel(export_name_table_pointers);
         getlabel(export_ordinal_table);

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
         exportssection^.concat(new(pai_const_symbol,init_rva(dll_name_label)));
         { ordinal base normally set to 1 }
         exportssection^.concat(new(pai_const,init_32bit(ordinal_base)));
         { number of entries }
         exportssection^.concat(new(pai_const,init_32bit(entries)));
         { number of named entries }
         exportssection^.concat(new(pai_const,init_32bit(named_entries)));
         { address of export address table }
         exportssection^.concat(new(pai_const_symbol,init_rva(export_address_table)));
         { address of name pointer pointers }
         exportssection^.concat(new(pai_const_symbol,init_rva(export_name_table_pointers)));
         { address of ordinal number pointers }
         exportssection^.concat(new(pai_const_symbol,init_rva(export_ordinal_table)));
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
                   name_table_pointers^.concat(new(pai_const_symbol,init_rva(name_label)));
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
              address_table^.concat(new(pai_const_symbol,initname_rva(hp^.sym^.mangledname)));
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


{****************************************************************************
                              TLINKERWIN32
****************************************************************************}


Constructor TLinkerWin32.Init;
begin
  Inherited Init;
  { allow duplicated libs (PM) }
  SharedLibFiles.doubles:=true;
  StaticLibFiles.doubles:=true;
  If not ForceDeffileForExport then
    UseDeffileForExport:=false;
end;

Procedure TLinkerWin32.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ldw $OPT $STRIP $APPTYPE $IMAGEBASE $RELOC -o $EXE $RES';
     DllCmd[1]:='ldw $OPT $STRIP --dll $APPTYPE $IMAGEBASE $RELOC -o $EXE $RES';
     if RelocSection or UseDeffileForExport then
       begin
          { ExeCmd[2]:='dlltool --as $ASBIN --dllname $EXE --output-exp exp.$$$ $RELOC $DEF';
            use short forms to avoid 128 char limitation problem }
          ExeCmd[2]:='dlltool -S $ASBIN -D $EXE -e exp.$$$ $RELOC $DEF';
          ExeCmd[3]:='ldw $OPT $STRIP $APPTYPE $IMAGEBASE -o $EXE $RES exp.$$$';
          { DllCmd[2]:='dlltool --as $ASBIN --dllname $EXE --output-exp exp.$$$ $RELOC $DEF'; }
          DllCmd[2]:='dlltool -S $ASBIN -D $EXE -e exp.$$$ $RELOC $DEF';
          DllCmd[3]:='ldw $OPT $STRIP --dll $APPTYPE $IMAGEBASE -o $EXE $RES exp.$$$';
       end;
   end;
end;

{$ifndef PAVEL_LINKLIB}
Function TLinkerWin32.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  i        : longint;
{$IFDEF NEWST}
  HPath    : PStringItem;
{$ELSE}
  HPath    : PStringQueueItem;
{$ENDIF NEWST}
  s,s2        : string;
  found,linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+GetShortName(HPath^.Data^)+')');
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+GetShortName(HPath^.Data^)+')');
     HPath:=HPath^.Next;
   end;

  { add objectfiles, start with prt0 always }
  LinkRes.Add('INPUT(');
  if isdll then
   LinkRes.AddFileName(GetShortName(FindObjectFile('wdllprt0','')))
  else
   LinkRes.AddFileName(GetShortName(FindObjectFile('wprt0','')));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.AddFileName(GetShortName(s));
   end;
  LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        LinkRes.AddFileName(GetShortName(s));
      end;
     LinkRes.Add(')');
   end;

  { Write sharedlibraries like -l<lib>, also add the needed dynamic linker
    here to be sure that it gets linked this is needed for glibc2 systems (PFV) }
  if not SharedLibFiles.Empty then
   begin
     linklibc:=false;
     LinkRes.Add('INPUT(');
     While not SharedLibFiles.Empty do
      begin
        S:=SharedLibFiles.Get;
        if pos('.',s)=0 then
          { we never directly link a DLL
            its allways through an import library PM }
          { libraries created by C compilers have .a extensions }
          s2:=s+'.a'{ target_os.sharedlibext }
        else
          s2:=s;
        s2:=FindLibraryFile(s2,'',found);
        if found then
          begin
            LinkRes.Add(s2);
            continue;
          end;
        if pos(target_os.libprefix,s)=1 then
          s:=copy(s,length(target_os.libprefix)+1,255);
        if s<>'c' then
         begin
           i:=Pos(target_os.sharedlibext,S);
           if i>0 then
            Delete(S,i,255);
           LinkRes.Add('-l'+s);
         end
        else
         begin
           LinkRes.Add('-l'+s);
           linklibc:=true;
         end;
      end;
     { be sure that libc is the last lib }
     if linklibc then
      LinkRes.Add('-lc');
     LinkRes.Add(')');
   end;
{ Write and Close response }
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;
{$else PAVEL_LINKLIB}
Function TLinkerWin32.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres  : TLinkRes;
  HPath    : {$ifdef NEWST} PStringItem {$else} PStringQueueItem {$endif};
  s,s2        : string;
  success : boolean;
function ExpandName(const s:string):string;
var
  sysdir:string;
procedure GetSysDir;
  begin
   sysdir:=GetEnv('windir');
   if sysdir<>''then
    begin
     if not(sysdir[length(sysdir)]in['\','/'])then
      sysdir:=sysdir+dirsep;
    end;
  end;
function IsFile(d:string;var PathToDll:string):longbool;
  var
   f:file;
   attr:word;
  begin
   PathToDll:='';
   if d<>''then
    if d[length(d)]<>dirsep then
     d:=d+dirsep;
   d:=d+s;
   assign(f,d);
   GetFattr(f,Attr);
   if DOSerror<>0 then
    IsFile:=false
   else
    begin
     if(attr and directory)=0 then
      begin
       IsFile:=true;
       PathToDll:=GetShortName(d);
      end
     else
      IsFile:=false;
    end;
  end;
var
  PathToDll:string;
begin
  if not isFile('',PathToDll)then
   begin
    HPath:=LibrarySearchPath.First;
     while assigned(HPath) do
      begin
       if isFile(GetShortName(HPath^.Data^),PathToDll)then
        break;
       HPath:=HPath^.Next;
      end;
    if PathToDll='' then
     begin
      GetSysDir;
      if not isFile(sysdir,PathToDll)then
       if not isFile(sysdir+'system32',PathToDll)then
        if not isFile(sysdir+'system',PathToDll)then
         begin
          message1(exec_w_libfile_not_found,S2);
          PathToDll:=S2;
         end;
     end;
   end;
  ExpandName:=PathToDll;
end;
function DotPos(const s:string):longint;
var
  i:longint;
begin
  DotPos:=0;
  for i:=length(s)downto 1 do
   begin
    if s[i]in['/','\',':']then
     exit
    else if s[i]='.'then
     begin
      DotPos:=i;
      exit;
     end;
   end;
end;
procedure strip(var s:string);
  var
   d:dirstr;
   n:namestr;
   e:extstr;
  begin
   fsplit(s,d,n,e);
   s:=n;
  end;
function do_makedef(const s:string):longbool;
  begin
   if cs_link_extern in aktglobalswitches then
    do_makedef:=DoExec(FindUtil('fpimpdef'),'-o deffile.$$$ -i '+s,false,false)
   else
    do_makedef:=makedef(s,'deffile.$$$');
  end;
begin
  WriteResponseFile:=False;
  While not SharedLibFiles.Empty do
   begin
     S:=SharedLibFiles.Get;
     if DotPos(s)=0 then
      s2:=s+target_os.sharedlibext
     else
      s2:=s;
     strip(s);
     if not do_makedef(ExpandName(s2))then
      begin
       Message(exec_w_error_while_linking);
       aktglobalswitches:=aktglobalswitches+[cs_link_extern];
      end
     else
      begin
       s:=target_os.libprefix+s+target_os.staticlibext;
       success:=DoExec(FindUtil('dlltool'),'-l '+s+' -D '+s2+' -d deffile.$$$',false,false);
       ObjectFiles.insert(s);
       if not success then
        break;
      end;
   end;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=current_module^.locallibrarysearchpath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+GetShortName(HPath^.Data^)+')');
     HPath:=HPath^.Next;
   end;
  HPath:=LibrarySearchPath.First;
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+GetShortName(HPath^.Data^)+')');
     HPath:=HPath^.Next;
   end;

  { add objectfiles, start with prt0 always }
  LinkRes.Add('INPUT(');
  if isdll then
   LinkRes.AddFileName(GetShortName(FindObjectFile('wdllprt0')))
  else
   LinkRes.AddFileName(GetShortName(FindObjectFile('wprt0')));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.Get;
     if s<>'' then
      LinkRes.AddFileName(GetShortName(s));
   end;
  LinkRes.Add(')');

  { Write staticlibraries }
  if not StaticLibFiles.Empty then
   begin
     LinkRes.Add('GROUP(');
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.Get;
        LinkRes.AddFileName(GetShortName(s));
      end;
     LinkRes.Add(')');
   end;

{ Write and Close response }
  linkres.writetodisk;
  linkres.done;

  WriteResponseFile:=True;
end;
{$endif PAVEL_LINKLIB}


function TLinkerWin32.MakeExecutable:boolean;
var
  binstr,
  cmdstr  : string;
  found,
  success : boolean;
  i       : longint;
  AsBinStr     : string[80];
  StripStr,
  RelocStr,
  AppTypeStr,
  ImageBaseStr : string[40];
begin
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.exefilename^);

{ Create some replacements }
  RelocStr:='';
  AppTypeStr:='';
  ImageBaseStr:='';
  StripStr:='';
  AsBinStr:=FindExe('asw',found);
  if RelocSection then
   { Using short form to avoid problems with 128 char limitation under Dos. }
   RelocStr:='-b base.$$$';
  if apptype=at_gui then
   AppTypeStr:='--subsystem windows';
  if assigned(DLLImageBase) then
   ImageBaseStr:='--image-base=0x'+DLLImageBase^;
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';

{ Write used files and libraries }
  WriteResponseFile(false);

{ Call linker }
  success:=false;
  for i:=1 to 3 do
   begin
     SplitBinCmd(Info.ExeCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        Replace(cmdstr,'$EXE',current_module^.exefilename^);
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',outputexedir+Info.ResName);
        Replace(cmdstr,'$APPTYPE',AppTypeStr);
        Replace(cmdstr,'$ASBIN',AsbinStr);
        Replace(cmdstr,'$RELOC',RelocStr);
        Replace(cmdstr,'$IMAGEBASE',ImageBaseStr);
        Replace(cmdstr,'$STRIP',StripStr);
        if not DefFile.Empty {and UseDefFileForExport} then
          begin
            DefFile.WriteFile;
            Replace(cmdstr,'$DEF','-d '+deffile.fname);
          end
        else
          Replace(cmdstr,'$DEF','');
        success:=DoExec(FindUtil(binstr),cmdstr,(i=1),false);
        if not success then
         break;
      end;
   end;

{ Post process }
  if success then
   success:=PostProcessExecutable(current_module^.exefilename^,false);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   begin
     RemoveFile(outputexedir+Info.ResName);
     RemoveFile('base.$$$');
     RemoveFile('exp.$$$');
     RemoveFile('deffile.$$$');
   end;

  MakeExecutable:=success;   { otherwise a recursive call to link method }
end;


Function TLinkerWin32.MakeSharedLibrary:boolean;
var
  binstr,
  cmdstr  : string;
  found,
  success : boolean;
  i       : longint;
  AsBinStr     : string[80];
  StripStr,
  RelocStr,
  AppTypeStr,
  ImageBaseStr : string[40];
begin
  MakeSharedLibrary:=false;
  if not(cs_link_extern in aktglobalswitches) then
   Message1(exec_i_linking,current_module^.sharedlibfilename^);

{ Create some replacements }
  RelocStr:='';
  AppTypeStr:='';
  ImageBaseStr:='';
  StripStr:='';
  AsBinStr:=FindExe('asw',found);
  if RelocSection then
   { Using short form to avoid problems with 128 char limitation under Dos. }
   RelocStr:='-b base.$$$';
  if apptype=at_gui then
   AppTypeStr:='--subsystem windows';
  if assigned(DLLImageBase) then
   ImageBaseStr:='--image-base=0x'+DLLImageBase^;
  if (cs_link_strip in aktglobalswitches) then
   StripStr:='-s';

{ Write used files and libraries }
  WriteResponseFile(true);

{ Call linker }
  success:=false;
  for i:=1 to 3 do
   begin
     SplitBinCmd(Info.DllCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        Replace(cmdstr,'$EXE',current_module^.sharedlibfilename^);
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',outputexedir+Info.ResName);
        Replace(cmdstr,'$APPTYPE',AppTypeStr);
        Replace(cmdstr,'$ASBIN',AsbinStr);
        Replace(cmdstr,'$RELOC',RelocStr);
        Replace(cmdstr,'$IMAGEBASE',ImageBaseStr);
        Replace(cmdstr,'$STRIP',StripStr);
        if not DefFile.Empty {and UseDefFileForExport} then
          begin
            DefFile.WriteFile;
            Replace(cmdstr,'$DEF','-d '+deffile.fname);
          end
        else
          Replace(cmdstr,'$DEF','');
        success:=DoExec(FindUtil(binstr),cmdstr,(i=1),false);
        if not success then
         break;
      end;
   end;

{ Post process }
  if success then
   success:=PostProcessExecutable(current_module^.sharedlibfilename^,true);

{ Remove ReponseFile }
  if (success) and not(cs_link_extern in aktglobalswitches) then
   begin
     RemoveFile(outputexedir+Info.ResName);
     RemoveFile('base.$$$');
     RemoveFile('exp.$$$');
   end;
  MakeSharedLibrary:=success;   { otherwise a recursive call to link method }
end;


function tlinkerwin32.postprocessexecutable(const fn : string;isdll:boolean):boolean;
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
     DataDirectory : array[1..$80] of byte;
  end;
  tcoffsechdr=packed record
    name     : array[0..7] of char;
    vsize    : longint;
    rvaofs   : longint;
    datalen  : longint;
    datapos  : longint;
    relocpos : longint;
    lineno1  : longint;
    nrelocs  : word;
    lineno2  : word;
    flags    : longint;
  end;
  psecfill=^tsecfill;
  tsecfill=record
    fillpos,
    fillsize : longint;
    next : psecfill;
  end;
var
  f : file;
  cmdstr : string;
  dosheader : tdosheader;
  peheader : tpeheader;
  firstsecpos,
  maxfillsize,
  l,peheaderpos : longint;
  coffsec : tcoffsechdr;
  secroot,hsecroot : psecfill;
  zerobuf : pointer;
begin
  postprocessexecutable:=false;
  { when -s is used or it's a dll then quit }
  if (cs_link_extern in aktglobalswitches) then
   begin
     if apptype=at_gui then
       cmdstr:='--subsystem gui'
     else if apptype=at_cui then
       cmdstr:='--subsystem console';
     if dllversion<>'' then
       cmdstr:=cmdstr+' --version '+dllversion;
     cmdstr:=cmdstr+' --input '+fn;
     cmdstr:=cmdstr+' --stack '+tostr(stacksize);
     DoExec(FindUtil('postw32'),cmdstr,false,false);
     postprocessexecutable:=true;
     exit;
   end;
  { open file }
  assign(f,fn);
  {$I-}
   reset(f,1);
  if ioresult<>0 then
    Message1(execinfo_f_cant_open_executable,fn);
  { read headers }
  blockread(f,dosheader,sizeof(tdosheader));
  peheaderpos:=dosheader.e_lfanew;
  seek(f,peheaderpos);
  blockread(f,peheader,sizeof(tpeheader));
  { write info }
  Message1(execinfo_x_codesize,tostr(peheader.SizeOfCode));
  Message1(execinfo_x_initdatasize,tostr(peheader.SizeOfInitializedData));
  Message1(execinfo_x_uninitdatasize,tostr(peheader.SizeOfUninitializedData));
  { change stack size (PM) }
  { I am not sure that the default value is adequate !! }
  peheader.SizeOfStackReserve:=stacksize;
  { change the header }
  { sub system }
  { gui=2 }
  { cui=3 }
  if apptype=at_gui then
    peheader.Subsystem:=2
  else if apptype=at_cui then
    peheader.Subsystem:=3;
  if dllversion<>'' then
    begin
     peheader.MajorImageVersion:=dllmajor;
     peheader.MinorImageVersion:=dllminor;
    end;
  { reset timestamp }
  peheader.TimeDateStamp:=0;
  { write header back }
  seek(f,peheaderpos);
  blockwrite(f,peheader,sizeof(tpeheader));
  if ioresult<>0 then
    Message1(execinfo_f_cant_process_executable,fn);
  seek(f,peheaderpos);
  blockread(f,peheader,sizeof(tpeheader));
  { write the value after the change }
  Message1(execinfo_x_stackreserve,tostr(peheader.SizeOfStackReserve));
  Message1(execinfo_x_stackcommit,tostr(peheader.SizeOfStackCommit));
  { read section info }
  maxfillsize:=0;
  firstsecpos:=0;
  secroot:=nil;
  for l:=1 to peheader.NumberOfSections do
   begin
     blockread(f,coffsec,sizeof(tcoffsechdr));
     if coffsec.datapos>0 then
      begin
        if secroot=nil then
         firstsecpos:=coffsec.datapos;
        new(hsecroot);
        hsecroot^.fillpos:=coffsec.datapos+coffsec.vsize;
        hsecroot^.fillsize:=coffsec.datalen-coffsec.vsize;
        hsecroot^.next:=secroot;
        secroot:=hsecroot;
        if secroot^.fillsize>maxfillsize then
         maxfillsize:=secroot^.fillsize;
      end;
   end;
  if firstsecpos>0 then
   begin
     l:=firstsecpos-filepos(f);
     if l>maxfillsize then
      maxfillsize:=l;
   end
  else
   l:=0;
  { get zero buffer }
  getmem(zerobuf,maxfillsize);
  fillchar(zerobuf^,maxfillsize,0);
  { zero from sectioninfo until first section }
  blockwrite(f,zerobuf^,l);
  { zero section alignments }
  while assigned(secroot) do
   begin
     seek(f,secroot^.fillpos);
     blockwrite(f,zerobuf^,secroot^.fillsize);
     hsecroot:=secroot;
     secroot:=secroot^.next;
     dispose(hsecroot);
   end;
  freemem(zerobuf,maxfillsize);
  close(f);
  {$I+}
  if ioresult<>0 then;
  postprocessexecutable:=true;
end;

end.
{
  $Log$
  Revision 1.3  2000-07-21 15:14:02  jonas
    + added is_addr field for labels, if they are only used for getting the address
       (e.g. for io checks) and corresponding getaddrlabel() procedure

  Revision 1.2  2000/07/13 11:32:50  michael
  + removed logs

}
