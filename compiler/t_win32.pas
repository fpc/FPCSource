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

{$i defines.inc}

interface

  uses
    import,export,link;

  const
     winstackpagesize = 4096;

  type
    timportlibwin32=class(timportlib)
      procedure preparelib(const s:string);override;
      procedure importprocedure(const func,module:string;index:longint;const name:string);override;
      procedure importvariable(const varname,module:string;const name:string);override;
      procedure generatelib;override;
      procedure generatesmartlib;override;
    end;

    texportlibwin32=class(texportlib)
      st : string;
      last_index : longint;
      procedure preparelib(const s:string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure generatelib;override;
    end;

    tlinkerwin32=class(tlinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
       Function  PostProcessExecutable(const fn:string;isdll:boolean) : Boolean;
    public
       Constructor Create;
       Procedure SetDefaultInfo;override;
       function  MakeExecutable:boolean;override;
       function  MakeSharedLibrary:boolean;override;
    end;


implementation

    uses
{$ifdef Delphi}
       dmisc,
{$else Delphi}
       dos,
{$endif Delphi}
       cutils,cclasses,
       aasm,fmodule,globtype,globals,systems,verbose,
       script,gendef,impdef,
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


    function FindDLL(const s:string):string;
      var
        sysdir : string;
        FoundDll : string;
        Found : boolean;
      begin
        Found:=false;
        { Look for DLL in:
          1. Current dir
          2. Library Path
          3. windir,windir/system,windir/system32 }
        FoundDll:=FindFile(s,'.'+DirSep,found)+s;
        if (not found) then
         FoundDll:=includesearchpath.FindFile(s,found)+s;
        if (not found) then
         begin
           sysdir:=FixPath(GetEnv('windir'),false);
           FoundDll:=FindFile(s,sysdir+';'+sysdir+'system'+DirSep+';'+sysdir+'system32'+DirSep,found)+s;
         end;
        if (not found) then
         begin
           message1(exec_w_libfile_not_found,s);
           FoundDll:=s;
         end;
        FindDll:=FoundDll;
      end;


{*****************************************************************************
                             TIMPORTLIBWIN32
*****************************************************************************}

    procedure timportlibwin32.preparelib(const s : string);
      begin
         if not(assigned(importssection)) then
           importssection:=TAAsmoutput.create;
      end;


    procedure timportlibwin32.importprocedure(const func,module : string;index : longint;const name : string);
      var
         hp1 : timportlist;
         hp2 : timported_item;
         hs  : string;
      begin
         hs:=DllName(module);
         { search for the module }
         hp1:=timportlist(current_module.imports.first);
         while assigned(hp1) do
           begin
              if hs=hp1.dllname^ then
                break;
              hp1:=timportlist(hp1.next);
           end;
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              hp1:=timportlist.create(hs);
              current_module.imports.concat(hp1);
           end;
         { search for reuse of old import item }
         hp2:=timported_item(hp1.imported_items.first);
         while assigned(hp2) do
          begin
            if hp2.func^=func then
             break;
            hp2:=timported_item(hp2.next);
          end;
         if not assigned(hp2) then
          begin
            hp2:=timported_item.create(func,name,index);
            hp1.imported_items.concat(hp2);
          end;
      end;


    procedure timportlibwin32.importvariable(const varname,module:string;const name:string);
      var
         hp1 : timportlist;
         hp2 : timported_item;
         hs  : string;
      begin
         hs:=DllName(module);
         { search for the module }
         hp1:=timportlist(current_module.imports.first);
         while assigned(hp1) do
           begin
              if hs=hp1.dllname^ then
                break;
              hp1:=timportlist(hp1.next);
           end;
         { generate a new item ? }
         if not(assigned(hp1)) then
           begin
              hp1:=timportlist.create(hs);
              current_module.imports.concat(hp1);
           end;
         hp2:=timported_item.create_var(varname,name);
         hp1.imported_items.concat(hp2);
      end;


    procedure timportlibwin32.generatesmartlib;
      var
         hp1 : timportlist;
         hp2 : timported_item;
         lhead,lname,lcode,
         lidata4,lidata5 : pasmlabel;
         r : preference;
      begin
         hp1:=timportlist(current_module.imports.first);
         while assigned(hp1) do
           begin
           { Get labels for the sections }
             getdatalabel(lhead);
             getdatalabel(lname);
             getaddrlabel(lidata4);
             getaddrlabel(lidata5);
           { create header for this importmodule }
             importsSection.concat(Tai_cut.Create_begin);
             importsSection.concat(Tai_section.Create(sec_idata2));
             importsSection.concat(Tai_label.Create(lhead));
             { pointer to procedure names }
             importsSection.concat(Tai_const_symbol.Create_rva(lidata4));
             { two empty entries follow }
             importsSection.concat(Tai_const.Create_32bit(0));
             importsSection.concat(Tai_const.Create_32bit(0));
             { pointer to dll name }
             importsSection.concat(Tai_const_symbol.Create_rva(lname));
             { pointer to fixups }
             importsSection.concat(Tai_const_symbol.Create_rva(lidata5));
             { first write the name references }
             importsSection.concat(Tai_section.Create(sec_idata4));
             importsSection.concat(Tai_const.Create_32bit(0));
             importsSection.concat(Tai_label.Create(lidata4));
             { then the addresses and create also the indirect jump }
             importsSection.concat(Tai_section.Create(sec_idata5));
             importsSection.concat(Tai_const.Create_32bit(0));
             importsSection.concat(Tai_label.Create(lidata5));

             { create procedures }
             hp2:=timported_item(hp1.imported_items.first);
             while assigned(hp2) do
               begin
                 { insert cuts }
                 importsSection.concat(Tai_cut.Create);
                 { create indirect jump }
                 if not hp2.is_var then
                  begin
                    getlabel(lcode);
                    new(r);
                    reset_reference(r^);
                    r^.symbol:=lcode;
                    { place jump in codesegment, insert a code section in the
                      importsection to reduce the amount of .s files (PFV) }
                    importsSection.concat(Tai_section.Create(sec_code));
{$IfDef GDB}
                    if (cs_debuginfo in aktmoduleswitches) then
                     importsSection.concat(Tai_stab_function_name.Create(nil));
{$EndIf GDB}
                    importsSection.concat(Tai_symbol.Createname_global(hp2.func^,0));
                    importsSection.concat(Taicpu.Op_ref(A_JMP,S_NO,r));
                    importsSection.concat(Tai_align.Create_op(4,$90));
                  end;
                 { create head link }
                 importsSection.concat(Tai_section.Create(sec_idata7));
                 importsSection.concat(Tai_const_symbol.Create_rva(lhead));
                 { fixup }
                 getlabel(pasmlabel(hp2.lab));
                 importsSection.concat(Tai_section.Create(sec_idata4));
                 importsSection.concat(Tai_const_symbol.Create_rva(hp2.lab));
                 { add jump field to importsection }
                 importsSection.concat(Tai_section.Create(sec_idata5));
                 if hp2.is_var then
                  importsSection.concat(Tai_symbol.Createname_global(hp2.func^,0))
                 else
                  importsSection.concat(Tai_label.Create(lcode));
                  if hp2.name^<>'' then
                    importsSection.concat(Tai_const_symbol.Create_rva(hp2.lab))
                  else
                    importsSection.concat(Tai_const.Create_32bit($80000000 or hp2.ordnr));
                 { finally the import information }
                 importsSection.concat(Tai_section.Create(sec_idata6));
                 importsSection.concat(Tai_label.Create(hp2.lab));
                 importsSection.concat(Tai_const.Create_16bit(hp2.ordnr));
                 importsSection.concat(Tai_string.Create(hp2.name^+#0));
                 importsSection.concat(Tai_align.Create_op(2,0));
                 hp2:=timported_item(hp2.next);
               end;

              { write final section }
              importsSection.concat(Tai_cut.Create_end);
              { end of name references }
              importsSection.concat(Tai_section.Create(sec_idata4));
              importsSection.concat(Tai_const.Create_32bit(0));
              { end if addresses }
              importsSection.concat(Tai_section.Create(sec_idata5));
              importsSection.concat(Tai_const.Create_32bit(0));
              { dllname }
              importsSection.concat(Tai_section.Create(sec_idata7));
              importsSection.concat(Tai_label.Create(lname));
              importsSection.concat(Tai_string.Create(hp1.dllname^+#0));

              hp1:=timportlist(hp1.next);
           end;
       end;


    procedure timportlibwin32.generatelib;
      var
         hp1 : timportlist;
         hp2 : timported_item;
         l1,l2,l3,l4 : pasmlabel;
         r : preference;
      begin
         hp1:=timportlist(current_module.imports.first);
         while assigned(hp1) do
           begin
              { align codesegment for the jumps }
              importsSection.concat(Tai_section.Create(sec_code));
              importsSection.concat(Tai_align.Create_op(4,$90));
              { Get labels for the sections }
              getlabel(l1);
              getlabel(l2);
              getlabel(l3);
              importsSection.concat(Tai_section.Create(sec_idata2));
              { pointer to procedure names }
              importsSection.concat(Tai_const_symbol.Create_rva(l2));
              { two empty entries follow }
              importsSection.concat(Tai_const.Create_32bit(0));
              importsSection.concat(Tai_const.Create_32bit(0));
              { pointer to dll name }
              importsSection.concat(Tai_const_symbol.Create_rva(l1));
              { pointer to fixups }
              importsSection.concat(Tai_const_symbol.Create_rva(l3));

              { only create one section for each else it will
                create a lot of idata* }

              { first write the name references }
              importsSection.concat(Tai_section.Create(sec_idata4));
              importsSection.concat(Tai_label.Create(l2));

              hp2:=timported_item(hp1.imported_items.first);
              while assigned(hp2) do
                begin
                   getlabel(pasmlabel(hp2.lab));
                   if hp2.name^<>'' then
                     importsSection.concat(Tai_const_symbol.Create_rva(hp2.lab))
                   else
                     importsSection.concat(Tai_const.Create_32bit($80000000 or hp2.ordnr));
                   hp2:=timported_item(hp2.next);
                end;
              { finalize the names ... }
              importsSection.concat(Tai_const.Create_32bit(0));

              { then the addresses and create also the indirect jump }
              importsSection.concat(Tai_section.Create(sec_idata5));
              importsSection.concat(Tai_label.Create(l3));
              hp2:=timported_item(hp1.imported_items.first);
              while assigned(hp2) do
                begin
                   if not hp2.is_var then
                    begin
                      getlabel(l4);
                      { create indirect jump }
                      new(r);
                      reset_reference(r^);
                      r^.symbol:=l4;
                      { place jump in codesegment }
                      importsSection.concat(Tai_section.Create(sec_code));
                      importsSection.concat(Tai_symbol.Createname_global(hp2.func^,0));
                      importsSection.concat(Taicpu.Op_ref(A_JMP,S_NO,r));
                      importsSection.concat(Tai_align.Create_op(4,$90));
                      { add jump field to importsection }
                      importsSection.concat(Tai_section.Create(sec_idata5));
                      importsSection.concat(Tai_label.Create(l4));
                    end
                   else
                    begin
                      importsSection.concat(Tai_symbol.Createname_global(hp2.func^,0));
                    end;
                   importsSection.concat(Tai_const_symbol.Create_rva(hp2.lab));
                   hp2:=timported_item(hp2.next);
                end;
              { finalize the addresses }
              importsSection.concat(Tai_const.Create_32bit(0));

              { finally the import information }
              importsSection.concat(Tai_section.Create(sec_idata6));
              hp2:=timported_item(hp1.imported_items.first);
              while assigned(hp2) do
                begin
                   importsSection.concat(Tai_label.Create(hp2.lab));
                   { the ordinal number }
                   importsSection.concat(Tai_const.Create_16bit(hp2.ordnr));
                   importsSection.concat(Tai_string.Create(hp2.name^+#0));
                   importsSection.concat(Tai_align.Create_op(2,0));
                   hp2:=timported_item(hp2.next);
                end;
              { create import dll name }
              importsSection.concat(Tai_section.Create(sec_idata7));
              importsSection.concat(Tai_label.Create(l1));
              importsSection.concat(Tai_string.Create(hp1.dllname^+#0));

              hp1:=timportlist(hp1.next);
           end;
      end;


{*****************************************************************************
                             TEXPORTLIBWIN32
*****************************************************************************}

    procedure texportlibwin32.preparelib(const s:string);
      begin
         if not(assigned(exportssection)) then
           exportssection:=TAAsmoutput.create;
         last_index:=0;
      end;



    procedure texportlibwin32.exportvar(hp : texported_item);
      begin
         { same code used !! PM }
         exportprocedure(hp);
      end;


    procedure texportlibwin32.exportprocedure(hp : texported_item);
      { must be ordered at least for win32 !! }
      var
        hp2 : texported_item;
      begin
        { first test the index value }
        if (hp.options and eo_index)<>0 then
          begin
             if (hp.index<=0) or (hp.index>$ffff) then
               begin
                 message1(parser_e_export_invalid_index,tostr(hp.index));
                 exit;
               end;
             if (hp.index<=last_index) then
               begin
                 message1(parser_e_export_ordinal_double,tostr(hp.index));
                 { disregard index value }
                 inc(last_index);
                 hp.index:=last_index;
                 exit;
               end
             else
               begin
                 last_index:=hp.index;
               end;
          end
        else
          begin
             inc(last_index);
             hp.index:=last_index;
          end;
        { now place in correct order }
        hp2:=texported_item(current_module._exports.first);
        while assigned(hp2) and
           (hp.name^>hp2.name^) do
          hp2:=texported_item(hp2.next);
        { insert hp there !! }
        if assigned(hp2) and (hp2.name^=hp.name^) then
          begin
             { this is not allowed !! }
             message1(parser_e_export_name_double,hp.name^);
             exit;
          end;
        if hp2=texported_item(current_module._exports.first) then
          current_module._exports.concat(hp)
        else if assigned(hp2) then
          begin
             hp.next:=hp2;
             hp.previous:=hp2.previous;
             if assigned(hp2.previous) then
               hp2.previous.next:=hp;
             hp2.previous:=hp;
          end
        else
          current_module._exports.concat(hp);
      end;


    procedure texportlibwin32.generatelib;
      var
         ordinal_base,ordinal_max,ordinal_min : longint;
         current_index : longint;
         entries,named_entries : longint;
         name_label,dll_name_label,export_address_table : pasmlabel;
         export_name_table_pointers,export_ordinal_table : pasmlabel;
         hp,hp2 : texported_item;
         temtexport : TLinkedList;
         address_table,name_table_pointers,
         name_table,ordinal_table : TAAsmoutput;
      begin

         hp:=texported_item(current_module._exports.first);
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
              if (hp.index>ordinal_max) then
                ordinal_max:=hp.index;
              if (hp.index>0) and (hp.index<ordinal_min) then
                ordinal_min:=hp.index;
              if assigned(hp.name) then
                inc(named_entries);
              hp:=texported_item(hp.next);
           end;

         { no support for higher ordinal base yet !! }
         ordinal_base:=1;
         current_index:=ordinal_base;
         { we must also count the holes !! }
         entries:=ordinal_max-ordinal_base+1;

         exportsSection.concat(Tai_section.Create(sec_edata));
         { export flags }
         exportsSection.concat(Tai_const.Create_32bit(0));
         { date/time stamp }
         exportsSection.concat(Tai_const.Create_32bit(0));
         { major version }
         exportsSection.concat(Tai_const.Create_16bit(0));
         { minor version }
         exportsSection.concat(Tai_const.Create_16bit(0));
         { pointer to dll name }
         exportsSection.concat(Tai_const_symbol.Create_rva(dll_name_label));
         { ordinal base normally set to 1 }
         exportsSection.concat(Tai_const.Create_32bit(ordinal_base));
         { number of entries }
         exportsSection.concat(Tai_const.Create_32bit(entries));
         { number of named entries }
         exportsSection.concat(Tai_const.Create_32bit(named_entries));
         { address of export address table }
         exportsSection.concat(Tai_const_symbol.Create_rva(export_address_table));
         { address of name pointer pointers }
         exportsSection.concat(Tai_const_symbol.Create_rva(export_name_table_pointers));
         { address of ordinal number pointers }
         exportsSection.concat(Tai_const_symbol.Create_rva(export_ordinal_table));
         { the name }
         exportsSection.concat(Tai_label.Create(dll_name_label));
         if st='' then
           exportsSection.concat(Tai_string.Create(current_module.modulename^+target_os.sharedlibext+#0))
         else
           exportsSection.concat(Tai_string.Create(st+target_os.sharedlibext+#0));

         {  export address table }
         address_table:=TAAsmoutput.create;
         address_table.concat(Tai_align.Create_op(4,0));
         address_table.concat(Tai_label.Create(export_address_table));
         name_table_pointers:=TAAsmoutput.create;
         name_table_pointers.concat(Tai_align.Create_op(4,0));
         name_table_pointers.concat(Tai_label.Create(export_name_table_pointers));
         ordinal_table:=TAAsmoutput.create;
         ordinal_table.concat(Tai_align.Create_op(4,0));
         ordinal_table.concat(Tai_label.Create(export_ordinal_table));
         name_table:=TAAsmoutput.Create;
         name_table.concat(Tai_align.Create_op(4,0));
         { write each address }
         hp:=texported_item(current_module._exports.first);
         while assigned(hp) do
           begin
              if (hp.options and eo_name)<>0 then
                begin
                   getlabel(name_label);
                   name_table_pointers.concat(Tai_const_symbol.Create_rva(name_label));
                   ordinal_table.concat(Tai_const.Create_16bit(hp.index-ordinal_base));
                   name_table.concat(Tai_align.Create_op(2,0));
                   name_table.concat(Tai_label.Create(name_label));
                   name_table.concat(Tai_string.Create(hp.name^+#0));
                end;
              hp:=texported_item(hp.next);
           end;
         { order in increasing ordinal values }
         { into temtexport list }
         temtexport:=TLinkedList.Create;
         hp:=texported_item(current_module._exports.first);
         while assigned(hp) do
           begin
              current_module._exports.remove(hp);
              hp2:=texported_item(temtexport.first);
              while assigned(hp2) and (hp.index>hp2.index) do
                begin
                   hp2:=texported_item(hp2.next);
                end;
              if hp2=texported_item(temtexport.first) then
                 temtexport.insert(hp)
              else
                begin
                   if assigned(hp2) then
                     begin
                        hp.next:=hp2;
                        hp.previous:=hp2.previous;
                        hp2.previous:=hp;
                        if assigned(hp.previous) then
                          hp.previous.next:=hp;
                      end
                    else
                      temtexport.concat(hp);
                end;
              hp:=texported_item(current_module._exports.first);;
           end;

         { write the export adress table }
         current_index:=ordinal_base;
         hp:=texported_item(temtexport.first);
         while assigned(hp) do
           begin
              { fill missing values }
              while current_index<hp.index do
                begin
                   address_table.concat(Tai_const.Create_32bit(0));
                   inc(current_index);
                end;
              address_table.concat(Tai_const_symbol.Createname_rva(hp.sym^.mangledname));
              inc(current_index);
              hp:=texported_item(hp.next);
           end;

         exportsSection.concatlist(address_table);
         exportsSection.concatlist(name_table_pointers);
         exportsSection.concatlist(ordinal_table);
         exportsSection.concatlist(name_table);
         address_table.Free;
         name_table_pointers.free;
         ordinal_table.free;
         name_table.free;
         temtexport.free;
      end;


{****************************************************************************
                              TLINKERWIN32
****************************************************************************}


Constructor TLinkerWin32.Create;
begin
  Inherited Create;
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



Function TLinkerWin32.WriteResponseFile(isdll:boolean) : Boolean;

  function do_makedef(const DllName,LibName:string):boolean;
  var
    CmdLine : string;
  begin
    if (not do_build) and
       FileExists(LibName) then
     begin
       if GetNamedFileTime(LibName)>GetNamedFileTime(DllName) then
        begin
          do_makedef:=true;
          exit;
        end;
     end;
    asw_name:=FindUtil('asw');
    arw_name:=FindUtil('arw');
    if cs_link_extern in aktglobalswitches then
     begin
       CmdLine:='-l '+LibName+' -i '+DLLName;
       if asw_name<>'' then
        CmdLine:=CmdLine+' -a '+asw_name;
       if arw_name<>'' then
        CmdLine:=CmdLine+' -r '+arw_name;
       do_makedef:=DoExec(FindUtil('fpimpdef'),CmdLine,false,false);
     end
    else
     do_makedef:=makedef(DLLName,LIbName);
  end;

Var
  linkres  : TLinkRes;
  i        : longint;
  HPath    : TStringListItem;
  s,s2     : string;
  found,
  linklibc : boolean;
begin
  WriteResponseFile:=False;

  { Create static import libraries for DLL that are
    included using the $linklib directive }
  While not SharedLibFiles.Empty do
   begin
     s:=SharedLibFiles.GetFirst;
     s2:=AddExtension(s,target_os.sharedlibext);
     s:=target_os.libprefix+SplitName(s)+target_os.staticlibext;
     if Do_makedef(FindDLL(s2),s) then
      begin
        if s<>''then
         StaticLibFiles.insert(s);
      end
     else
      begin
        Message(exec_w_error_while_linking);
        aktglobalswitches:=aktglobalswitches+[cs_link_extern];
      end;
   end;

  { Open link.res file }
  LinkRes.Init(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+GetShortName(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+GetShortName(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;

  { add objectfiles, start with prt0 always }
  LinkRes.Add('INPUT(');
  if isdll then
   LinkRes.AddFileName(GetShortName(FindObjectFile('wdllprt0','')))
  else
   LinkRes.AddFileName(GetShortName(FindObjectFile('wprt0','')));
  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
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
        S:=StaticLibFiles.GetFirst;
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
        S:=SharedLibFiles.GetFirst;
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
   Message1(exec_i_linking,current_module.exefilename^);

{ Create some replacements }
  RelocStr:='';
  AppTypeStr:='';
  ImageBaseStr:='';
  StripStr:='';
  AsBinStr:=FindExe('asw',found);
  if RelocSection then
   { Using short form to avoid problems with 128 char limitation under Dos. }
   RelocStr:='-b base.$$$';
  if apptype=app_gui then
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
        Replace(cmdstr,'$EXE',current_module.exefilename^);
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
   success:=PostProcessExecutable(current_module.exefilename^,false);

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
   Message1(exec_i_linking,current_module.sharedlibfilename^);

{ Create some replacements }
  RelocStr:='';
  AppTypeStr:='';
  ImageBaseStr:='';
  StripStr:='';
  AsBinStr:=FindExe('asw',found);
  if RelocSection then
   { Using short form to avoid problems with 128 char limitation under Dos. }
   RelocStr:='-b base.$$$';
  if apptype=app_gui then
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
        Replace(cmdstr,'$EXE',current_module.sharedlibfilename^);
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
   success:=PostProcessExecutable(current_module.sharedlibfilename^,true);

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
     case apptype of
       app_gui :
         cmdstr:='--subsystem gui';
       app_cui :
         cmdstr:='--subsystem console';
     end;
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
  case apptype of
    app_gui :
      peheader.Subsystem:=2;
    app_cui :
      peheader.Subsystem:=3;
  end;
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
  Revision 1.9  2001-01-13 00:09:22  peter
    * made Pavel O. happy ;)

  Revision 1.8  2000/12/30 22:53:25  peter
    * export with the case provided in the exports section

  Revision 1.7  2000/12/25 00:07:30  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.6  2000/11/12 22:20:37  peter
    * create generic toutputsection for binary writers

  Revision 1.5  2000/09/24 15:06:31  peter
    * use defines.inc

  Revision 1.4  2000/08/27 16:11:54  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/07/21 15:14:02  jonas
    + added is_addr field for labels, if they are only used for getting the address
       (e.g. for io checks) and corresponding getaddrlabel() procedure

  Revision 1.2  2000/07/13 11:32:50  michael
  + removed logs

}
