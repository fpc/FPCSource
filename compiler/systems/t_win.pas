{
    Copyright (c) 1998-2002 by Peter Vreman

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
unit t_win;

{$i fpcdefs.inc}

interface
    uses
       dos,
       cutils,cclasses,
       aasmbase,aasmtai,aasmcpu,fmodule,globtype,globals,systems,verbose,
       symconst,symdef,symsym,
       script,gendef,
       cpubase,
{$ifdef GDB}
       gdb,
{$endif}
       import,export,link,cgobj,i_win;


  const
     MAX_DEFAULT_EXTENSIONS = 3;

  type
     tStr4=array[1..MAX_DEFAULT_EXTENSIONS]of string[4];
     pStr4=^tStr4;

    twin32imported_item = class(timported_item)
       procdef : tprocdef;
    end;

    timportlibwin32=class(timportlib)
    private
      procedure win32importproc(aprocdef:tprocdef;const func,module : string;index : longint;const name : string);
      procedure importvariable_str(const s:string;const name,module:string);
      procedure importprocedure_str(const func,module:string;index:longint;const name:string);
    public
      procedure preparelib(const s:string);override;
      procedure importprocedure(aprocdef:tprocdef;const module:string;index:longint;const name:string);override;
      procedure importvariable(vs:tglobalvarsym;const name,module:string);override;
      procedure generatelib;override;
      procedure generatenasmlib;virtual;
      procedure generatesmartlib;override;
    end;

    texportlibwin32=class(texportlib)
      st : string;
      EList_indexed:tList;
      EList_nonindexed:tList;
      procedure preparelib(const s:string);override;
      procedure exportprocedure(hp : texported_item);override;
      procedure exportvar(hp : texported_item);override;
      procedure exportfromlist(hp : texported_item);
      procedure generatelib;override;
      procedure generatenasmlib;virtual;
    end;

    tlinkerwin32=class(texternallinker)
    private
       Function  WriteResponseFile(isdll:boolean) : Boolean;
       Function  PostProcessExecutable(const fn:string;isdll:boolean) : Boolean;
    public
       Constructor Create;override;
       Procedure SetDefaultInfo;override;
       function  MakeExecutable:boolean;override;
       function  MakeSharedLibrary:boolean;override;
    end;

    tDLLScannerWin32=class(tDLLScanner)
    private
      cstring : array[0..127]of char;
      function DOSstubOK(var x:cardinal):boolean;
      function FindDLL(const s:string;var founddll:string):boolean;
      function ExtractDllName(Const Name : string) : string;
    public
      function isSuitableFileType(x:cardinal):longbool;override;
      function GetEdata(HeaderEntry:cardinal):longbool;override;
      function Scan(const binname:string):longbool;override;
    end;

implementation

  uses
    cpuinfo,cgutils;


{*****************************************************************************
                             TIMPORTLIBWIN32
*****************************************************************************}

    procedure timportlibwin32.preparelib(const s : string);
      begin
         if not(assigned(importssection)) then
           importssection:=TAAsmoutput.create;
      end;


    procedure timportlibwin32.win32importproc(aprocdef:tprocdef;const func,module : string;index : longint;const name : string);
      var
         hp1 : timportlist;
         hp2 : twin32imported_item;
         hs  : string;
      begin
         { procdef or funcname must be give, not both }
         if assigned(aprocdef) and (func<>'') then
           internalerror(200411161);
         { append extension if required }
         hs:=AddExtension(module,target_info.sharedlibext);
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
         if assigned(aprocdef) then
           begin
             hp2:=twin32imported_item(hp1.imported_items.first);
             while assigned(hp2) do
              begin
                if (hp2.procdef=aprocdef) then
                  break;
                hp2:=twin32imported_item(hp2.next);
              end;
           end
         else
           begin
             hp2:=twin32imported_item(hp1.imported_items.first);
             while assigned(hp2) do
              begin
                if (hp2.func^=func) then
                  break;
                hp2:=twin32imported_item(hp2.next);
              end;
           end;
         if not assigned(hp2) then
          begin
            hp2:=twin32imported_item.create(func,name,index);
            hp2.procdef:=aprocdef;
            hp1.imported_items.concat(hp2);
          end;
      end;


    procedure timportlibwin32.importprocedure(aprocdef:tprocdef;const module : string;index : longint;const name : string);
      begin
        win32importproc(aprocdef,'',module,index,name);
      end;


    procedure timportlibwin32.importprocedure_str(const func,module : string;index : longint;const name : string);
      begin
        win32importproc(nil,func,module,index,name);
      end;


    procedure timportlibwin32.importvariable(vs:tglobalvarsym;const name,module:string);
      begin
        importvariable_str(vs.mangledname,name,module);
      end;


    procedure timportlibwin32.importvariable_str(const s:string;const name,module:string);
      var
         hp1 : timportlist;
         hp2 : twin32imported_item;
         hs  : string;
      begin
         hs:=AddExtension(module,target_info.sharedlibext);
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
         hp2:=twin32imported_item.create_var(s,name);
         hp2.procdef:=nil;
         hp1.imported_items.concat(hp2);
      end;

    procedure timportlibwin32.generatenasmlib;
      var
         hp1 : timportlist;
         hp2 : twin32imported_item;
         p : pchar;
      begin
         new_section(importssection,sec_code,'',0);
         hp1:=timportlist(current_module.imports.first);
         while assigned(hp1) do
           begin
             hp2:=twin32imported_item(hp1.imported_items.first);
             while assigned(hp2) do
               begin
                 if (aktoutputformat in [as_i386_tasm,as_i386_masm]) then
                   p:=strpnew(#9+'EXTRN '+hp2.func^)
                 else
                   p:=strpnew(#9+'EXTERN '+hp2.func^);
                 importssection.concat(tai_direct.create(p));
                 p:=strpnew(#9+'import '+hp2.func^+' '+hp1.dllname^+' '+hp2.name^);
                 importssection.concat(tai_direct.create(p));
                 hp2:=twin32imported_item(hp2.next);
               end;
             hp1:=timportlist(hp1.next);
           end;
      end;


    procedure timportlibwin32.generatesmartlib;
      var
         hp1 : timportlist;
         mangledstring : string;
{$ifdef GDB}
         importname : string;
         suffix : integer;
{$endif GDB}
         hp2 : twin32imported_item;
         lhead,lname,lcode,
         lidata4,lidata5 : tasmlabel;
         href : treference;
      begin
         if (aktoutputformat in [as_i386_masm,as_i386_tasm,as_i386_nasmwin32]) then
          begin
            generatenasmlib;
            exit;
          end;
         hp1:=timportlist(current_module.imports.first);
         while assigned(hp1) do
           begin
           { Get labels for the sections }
             objectlibrary.getdatalabel(lhead);
             objectlibrary.getdatalabel(lname);
             objectlibrary.getaddrlabel(lidata4);
             objectlibrary.getaddrlabel(lidata5);
           { create header for this importmodule }
             importsSection.concat(Tai_cutobject.Create_begin);
             new_section(importsSection,sec_idata2,'',0);
             importsSection.concat(Tai_label.Create(lhead));
             { pointer to procedure names }
             importsSection.concat(Tai_const.Create_rva_sym(lidata4));
             { two empty entries follow }
             importsSection.concat(Tai_const.Create_32bit(0));
             importsSection.concat(Tai_const.Create_32bit(0));
             { pointer to dll name }
             importsSection.concat(Tai_const.Create_rva_sym(lname));
             { pointer to fixups }
             importsSection.concat(Tai_const.Create_rva_sym(lidata5));
             { first write the name references }
             new_section(importsSection,sec_idata4,'',0);
             importsSection.concat(Tai_const.Create_32bit(0));
             importsSection.concat(Tai_label.Create(lidata4));
             { then the addresses and create also the indirect jump }
             new_section(importsSection,sec_idata5,'',0);
             importsSection.concat(Tai_const.Create_32bit(0));
             importsSection.concat(Tai_label.Create(lidata5));

             { create procedures }
             hp2:=twin32imported_item(hp1.imported_items.first);
             while assigned(hp2) do
               begin
                 { insert cuts }
                 importsSection.concat(Tai_cutobject.Create);
                 { create indirect jump }
                 if not hp2.is_var then
                  begin
                    objectlibrary.getlabel(lcode);
                    reference_reset_symbol(href,lcode,0);
                    { place jump in codesegment, insert a code section in the
                      imporTSection to reduce the amount of .s files (PFV) }
                    new_section(importsSection,sec_code,'',0);
{$IfDef GDB}
                    if (cs_debuginfo in aktmoduleswitches) then
                     importsSection.concat(Tai_stab_function_name.Create(nil));
{$EndIf GDB}
                    if assigned(hp2.procdef) then
                      mangledstring:=hp2.procdef.mangledname
                    else
                      mangledstring:=hp2.func^;
                    importsSection.concat(Tai_symbol.Createname_global(mangledstring,AT_FUNCTION,0));
                    importsSection.concat(Taicpu.Op_ref(A_JMP,S_NO,href));
                    importsSection.concat(Tai_align.Create_op(4,$90));
{$IfDef GDB}
                    if (cs_debuginfo in aktmoduleswitches) and assigned(hp2.procdef) then
                       hp2.procdef.concatstabto(importssection);
{$EndIf GDB}
                  end;
                 { create head link }
                 new_section(importsSection,sec_idata7,'',0);
                 importsSection.concat(Tai_const.Create_rva_sym(lhead));
                 { fixup }
                 objectlibrary.getlabel(tasmlabel(hp2.lab));
                 new_section(importsSection,sec_idata4,'',0);
                 importsSection.concat(Tai_const.Create_rva_sym(hp2.lab));
                 { add jump field to imporTSection }
                 new_section(importsSection,sec_idata5,'',0);
                 if hp2.is_var then
                  importsSection.concat(Tai_symbol.Createname_global(hp2.func^,AT_FUNCTION,0))
                 else
                  importsSection.concat(Tai_label.Create(lcode));
{$ifdef GDB}
                 if (cs_debuginfo in aktmoduleswitches) then
                  begin
                    if assigned(hp2.name) then
                      begin
                        importname:='__imp_'+hp2.name^;
                        suffix:=0;
                        while assigned(objectlibrary.getasmsymbol(importname)) do
                         begin
                           inc(suffix);
                           importname:='__imp_'+hp2.name^+'_'+tostr(suffix);
                         end;
                        importssection.concat(tai_symbol.createname(importname,AT_FUNCTION,4));
                      end
                    else
                      begin
                        importname:='__imp_by_ordinal'+tostr(hp2.ordnr);
                        suffix:=0;
                        while assigned(objectlibrary.getasmsymbol(importname)) do
                         begin
                           inc(suffix);
                           importname:='__imp_by_ordinal'+tostr(hp2.ordnr)+'_'+tostr(suffix);
                         end;
                        importssection.concat(tai_symbol.createname(importname,AT_FUNCTION,4));
                      end;
                  end;
{$endif GDB}
                 if hp2.name^<>'' then
                  importsSection.concat(Tai_const.Create_rva_sym(hp2.lab))
                 else
                  importsSection.concat(Tai_const.Create_32bit(longint($80000000) or longint(hp2.ordnr)));
                 { finally the import information }
                 new_section(importsSection,sec_idata6,'',0);
                 importsSection.concat(Tai_label.Create(hp2.lab));
                 importsSection.concat(Tai_const.Create_16bit(hp2.ordnr));
                 importsSection.concat(Tai_string.Create(hp2.name^+#0));
                 importsSection.concat(Tai_align.Create_op(2,0));
                 hp2:=twin32imported_item(hp2.next);
               end;

              { write final section }
              importsSection.concat(Tai_cutobject.Create_end);
              { end of name references }
              new_section(importsSection,sec_idata4,'',0);
              importsSection.concat(Tai_const.Create_32bit(0));
              { end if addresses }
              new_section(importsSection,sec_idata5,'',0);
              importsSection.concat(Tai_const.Create_32bit(0));
              { dllname }
              new_section(importsSection,sec_idata7,'',0);
              importsSection.concat(Tai_label.Create(lname));
              importsSection.concat(Tai_string.Create(hp1.dllname^+#0));

              hp1:=timportlist(hp1.next);
           end;
       end;


    procedure timportlibwin32.generatelib;
      var
         hp1 : timportlist;
         hp2 : twin32imported_item;
         l1,l2,l3,l4 : tasmlabel;
         mangledstring : string;
{$ifdef GDB}
         importname : string;
         suffix : integer;
{$endif GDB}
         href : treference;
      begin
         if (aktoutputformat in [as_i386_masm,as_i386_tasm,as_i386_nasmwin32]) then
          begin
            generatenasmlib;
            exit;
          end;
         hp1:=timportlist(current_module.imports.first);
         while assigned(hp1) do
           begin
              { align codesegment for the jumps }
              new_section(importsSection,sec_code,'',sizeof(aint));
              { Get labels for the sections }
              objectlibrary.getlabel(l1);
              objectlibrary.getlabel(l2);
              objectlibrary.getlabel(l3);
              new_section(importsSection,sec_idata2,'',0);
              { pointer to procedure names }
              importsSection.concat(Tai_const.Create_rva_sym(l2));
              { two empty entries follow }
              importsSection.concat(Tai_const.Create_32bit(0));
              importsSection.concat(Tai_const.Create_32bit(0));
              { pointer to dll name }
              importsSection.concat(Tai_const.Create_rva_sym(l1));
              { pointer to fixups }
              importsSection.concat(Tai_const.Create_rva_sym(l3));

              { only create one section for each else it will
                create a lot of idata* }

              { first write the name references }
              new_section(importsSection,sec_idata4,'',0);
              importsSection.concat(Tai_label.Create(l2));

              hp2:=twin32imported_item(hp1.imported_items.first);
              while assigned(hp2) do
                begin
                   objectlibrary.getlabel(tasmlabel(hp2.lab));
                   if hp2.name^<>'' then
                     importsSection.concat(Tai_const.Create_rva_sym(hp2.lab))
                   else
                     importsSection.concat(Tai_const.Create_32bit(longint($80000000) or hp2.ordnr));
                   hp2:=twin32imported_item(hp2.next);
                end;
              { finalize the names ... }
              importsSection.concat(Tai_const.Create_32bit(0));

              { then the addresses and create also the indirect jump }
              new_section(importsSection,sec_idata5,'',0);
              importsSection.concat(Tai_label.Create(l3));
              hp2:=twin32imported_item(hp1.imported_items.first);
              while assigned(hp2) do
                begin
                   if not hp2.is_var then
                    begin
                      objectlibrary.getlabel(l4);
                      { create indirect jump }
                      reference_reset_symbol(href,l4,0);
                      { place jump in codesegment }
                      new_section(importsSection,sec_code,'',0);
{$IfDef GDB}
                      if (cs_debuginfo in aktmoduleswitches) then
                        importssection.concat(tai_stab_function_name.create(nil));
{$EndIf GDB}
                      if assigned(hp2.procdef) then
                        mangledstring:=hp2.procdef.mangledname
                      else
                        mangledstring:=hp2.func^;
                      importsSection.concat(Tai_symbol.Createname_global(mangledstring,AT_FUNCTION,0));
                      importsSection.concat(Taicpu.Op_ref(A_JMP,S_NO,href));
                      importsSection.concat(Tai_align.Create_op(4,$90));
{$IfDef GDB}
                      if (cs_debuginfo in aktmoduleswitches) and assigned(hp2.procdef) then
                        hp2.procdef.concatstabto(importssection);
{$EndIf GDB}
                      { add jump field to imporTSection }
                      new_section(importsSection,sec_idata5,'',0);
{$ifdef GDB}
                      if (cs_debuginfo in aktmoduleswitches) then
                       begin
                         if assigned(hp2.name) then
                          begin
                            importname:='__imp_'+hp2.name^;
                            suffix:=0;
                            while assigned(objectlibrary.getasmsymbol(importname)) do
                             begin
                               inc(suffix);
                               importname:='__imp_'+hp2.name^+'_'+tostr(suffix);
                             end;
                            importssection.concat(tai_symbol.createname(importname,AT_FUNCTION,4));
                          end
                         else
                          begin
                            importname:='__imp_by_ordinal'+tostr(hp2.ordnr);
                            suffix:=0;
                            while assigned(objectlibrary.getasmsymbol(importname)) do
                             begin
                               inc(suffix);
                               importname:='__imp_by_ordinal'+tostr(hp2.ordnr)+'_'+tostr(suffix);
                             end;
                            importssection.concat(tai_symbol.createname(importname,AT_FUNCTION,4));
                          end;
                       end;
{$endif GDB}
                      importsSection.concat(Tai_label.Create(l4));
                    end
                   else
                    begin
                      importsSection.concat(Tai_symbol.Createname_global(hp2.func^,AT_DATA,0));
                    end;
                   importsSection.concat(Tai_const.Create_rva_sym(hp2.lab));
                   hp2:=twin32imported_item(hp2.next);
                end;
              { finalize the addresses }
              importsSection.concat(Tai_const.Create_32bit(0));

              { finally the import information }
              new_section(importsSection,sec_idata6,'',0);
              hp2:=twin32imported_item(hp1.imported_items.first);
              while assigned(hp2) do
                begin
                   importsSection.concat(Tai_label.Create(hp2.lab));
                   { the ordinal number }
                   importsSection.concat(Tai_const.Create_16bit(hp2.ordnr));
                   importsSection.concat(Tai_string.Create(hp2.name^+#0));
                   importsSection.concat(Tai_align.Create_op(2,0));
                   hp2:=twin32imported_item(hp2.next);
                end;
              { create import dll name }
              new_section(importsSection,sec_idata7,'',0);
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
         EList_indexed:=tList.Create;
         EList_nonindexed:=tList.Create;
         objectlibrary.getdatalabel(edatalabel);
      end;



    procedure texportlibwin32.exportvar(hp : texported_item);
      begin
         { same code used !! PM }
         exportprocedure(hp);
      end;

    var
      Gl_DoubleIndex:boolean;
      Gl_DoubleIndexValue:longint;

    function IdxCompare(Item1, Item2: Pointer): Integer;
      var
        I1:texported_item absolute Item1;
        I2:texported_item absolute Item2;
      begin
        Result:=I1.index-I2.index;
        if(Result=0)and(Item1<>Item2)then
         begin
          Gl_DoubleIndex:=true;
          Gl_DoubleIndexValue:=I1.index;
         end;
      end;


    procedure texportlibwin32.exportprocedure(hp : texported_item);
      begin
        if ((hp.options and eo_index)<>0)and((hp.index<=0) or (hp.index>$ffff)) then
          begin
           message1(parser_e_export_invalid_index,tostr(hp.index));
           exit;
          end;
        if hp.options and eo_index=eo_index then
          EList_indexed.Add(hp)
        else
          EList_nonindexed.Add(hp);
      end;


    procedure texportlibwin32.exportfromlist(hp : texported_item);
      //formerly texportlibwin32.exportprocedure
      { must be ordered at least for win32 !! }
      var
        hp2 : texported_item;
      begin
        hp2:=texported_item(current_module._exports.first);
        while assigned(hp2) and
           (hp.name^>hp2.name^) do
          hp2:=texported_item(hp2.next);
        { insert hp there !! }
        if hp2=nil then
          current_module._exports.concat(hp)
        else
          begin
            if hp2.name^=hp.name^ then
              begin
                { this is not allowed !! }
                message1(parser_e_export_name_double,hp.name^);
                exit;
              end;
            current_module._exports.insertbefore(hp,hp2);
          end;
      end;


    procedure texportlibwin32.generatelib;
      var
         ordinal_base,ordinal_max,ordinal_min : longint;
         current_index : longint;
         entries,named_entries : longint;
         name_label,dll_name_label,export_address_table : tasmlabel;
         export_name_table_pointers,export_ordinal_table : tasmlabel;
         hp,hp2 : texported_item;
         temtexport : TLinkedList;
         address_table,name_table_pointers,
         name_table,ordinal_table : TAAsmoutput;
         i,autoindex,ni_high : longint;
         hole : boolean;

      begin
         Gl_DoubleIndex:=false;
         ELIst_indexed.Sort(@IdxCompare);

         if Gl_DoubleIndex then
           begin
             message1(parser_e_export_ordinal_double,tostr(Gl_DoubleIndexValue));
             EList_indexed.Free;
             EList_nonindexed.Free;
             exit;
           end;

         autoindex:=1;
         while EList_nonindexed.Count>0 do
          begin
           hole:=(EList_indexed.Count>0)and(texported_item(EList_indexed.Items[0]).index>1);
           if not hole then
            for i:=autoindex to pred(EList_indexed.Count)do
             if texported_item(EList_indexed.Items[i]).index-texported_item(EList_indexed.Items[pred(i)]).index>1 then
              begin
               autoindex:=succ(texported_item(EList_indexed.Items[pred(i)]).index);
               hole:=true;
               break;
              end;
           ni_high:=pred(EList_nonindexed.Count);
           if not hole then
            begin
             autoindex:=succ(EList_indexed.Count);
             EList_indexed.Add(EList_nonindexed.Items[ni_high]);
            end
           else
            EList_indexed.Insert(pred(AutoIndex),EList_nonindexed.Items[ni_high]);
           EList_nonindexed.Delete(ni_high);
           texported_item(EList_indexed.Items[pred(AutoIndex)]).index:=autoindex;
          end;
         EList_nonindexed.Free;
         for i:=0 to pred(EList_indexed.Count)do
          exportfromlist(texported_item(EList_indexed.Items[i]));
         EList_indexed.Free;

         if (aktoutputformat in [as_i386_masm,as_i386_tasm,as_i386_nasmwin32]) then
          begin
            generatenasmlib;
            exit;
          end;

         hp:=texported_item(current_module._exports.first);
         if not assigned(hp) then
           exit;

         ordinal_max:=0;
         ordinal_min:=$7FFFFFFF;
         entries:=0;
         named_entries:=0;
         objectlibrary.getlabel(dll_name_label);
         objectlibrary.getlabel(export_address_table);
         objectlibrary.getlabel(export_name_table_pointers);
         objectlibrary.getlabel(export_ordinal_table);

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

         new_section(exportsSection,sec_edata,'',0);
         { create label to reference from main so smartlink will include
           the .edata section }
         exportsSection.concat(Tai_symbol.Create_global(edatalabel,0));
         { export flags }
         exportsSection.concat(Tai_const.Create_32bit(0));
         { date/time stamp }
         exportsSection.concat(Tai_const.Create_32bit(0));
         { major version }
         exportsSection.concat(Tai_const.Create_16bit(0));
         { minor version }
         exportsSection.concat(Tai_const.Create_16bit(0));
         { pointer to dll name }
         exportsSection.concat(Tai_const.Create_rva_sym(dll_name_label));
         { ordinal base normally set to 1 }
         exportsSection.concat(Tai_const.Create_32bit(ordinal_base));
         { number of entries }
         exportsSection.concat(Tai_const.Create_32bit(entries));
         { number of named entries }
         exportsSection.concat(Tai_const.Create_32bit(named_entries));
         { address of export address table }
         exportsSection.concat(Tai_const.Create_rva_sym(export_address_table));
         { address of name pointer pointers }
         exportsSection.concat(Tai_const.Create_rva_sym(export_name_table_pointers));
         { address of ordinal number pointers }
         exportsSection.concat(Tai_const.Create_rva_sym(export_ordinal_table));
         { the name }
         exportsSection.concat(Tai_label.Create(dll_name_label));
         if st='' then
           exportsSection.concat(Tai_string.Create(current_module.modulename^+target_info.sharedlibext+#0))
         else
           exportsSection.concat(Tai_string.Create(st+target_info.sharedlibext+#0));

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
                   objectlibrary.getlabel(name_label);
                   name_table_pointers.concat(Tai_const.Create_rva_sym(name_label));
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
                hp2:=texported_item(hp2.next);
              if hp2=nil then
                temtexport.concat(hp)
              else
                temtexport.insertbefore(hp,hp2);
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
              case hp.sym.typ of
                globalvarsym :
                  address_table.concat(Tai_const.Createname_rva(tglobalvarsym(hp.sym).mangledname));
                typedconstsym :
                  address_table.concat(Tai_const.Createname_rva(ttypedconstsym(hp.sym).mangledname));
                procsym :
                  address_table.concat(Tai_const.Createname_rva(tprocsym(hp.sym).first_procdef.mangledname));
              end;
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

    procedure texportlibwin32.generatenasmlib;
      var
         hp : texported_item;
         p  : pchar;
         s  : string;
      begin
         new_section(exportssection,sec_code,'',0);
         hp:=texported_item(current_module._exports.first);
         while assigned(hp) do
           begin
             case hp.sym.typ of
               globalvarsym :
                 s:=tglobalvarsym(hp.sym).mangledname;
               typedconstsym :
                 s:=ttypedconstsym(hp.sym).mangledname;
               procsym :
                 s:=tprocsym(hp.sym).first_procdef.mangledname;
               else
                 s:='';
             end;
             p:=strpnew(#9+'export '+s+' '+hp.name^+' '+tostr(hp.index));
             exportssection.concat(tai_direct.create(p));
             hp:=texported_item(hp.next);
           end;
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
end;


Procedure TLinkerWin32.SetDefaultInfo;
begin
  with Info do
   begin
     ExeCmd[1]:='ld -b pe-i386 -m i386pe $OPT $STRIP $APPTYPE $IMAGEBASE $RELOC -o $EXE $RES';
     DllCmd[1]:='ld -b pe-i386 -m i386pe $OPT $STRIP --dll $APPTYPE $IMAGEBASE $RELOC -o $EXE $RES';
     { ExeCmd[2]:='dlltool --as $ASBIN --dllname $EXE --output-exp exp.$$$ $RELOC $DEF';
       use short forms to avoid 128 char limitation problem }
     ExeCmd[2]:='dlltool -S $ASBIN -D $EXE -e exp.$$$ $RELOC $DEF';
     ExeCmd[3]:='ld -b pe-i386 -m i386pe $OPT $STRIP $APPTYPE $IMAGEBASE -o $EXE $RES exp.$$$';
     { DllCmd[2]:='dlltool --as $ASBIN --dllname $EXE --output-exp exp.$$$ $RELOC $DEF'; }
     DllCmd[2]:='dlltool -S $ASBIN -D $EXE -e exp.$$$ $RELOC $DEF';
     DllCmd[3]:='ld -b pe-i386 -m i386pe $OPT $STRIP --dll $APPTYPE $IMAGEBASE -o $EXE $RES exp.$$$';
   end;
end;



Function TLinkerWin32.WriteResponseFile(isdll:boolean) : Boolean;
Var
  linkres : TLinkRes;
  HPath   : TStringListItem;
  s,s2    : string;
  i       : integer;
  linklibcygwin : boolean;
begin
  WriteResponseFile:=False;
  linklibcygwin:=(SharedLibFiles.Find('cygwin')<>nil);

  { Open link.res file }
  LinkRes:=TLinkRes.Create(outputexedir+Info.ResName);

  { Write path to search libraries }
  HPath:=TStringListItem(current_module.locallibrarysearchpath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+MaybeQuoted(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;
  HPath:=TStringListItem(LibrarySearchPath.First);
  while assigned(HPath) do
   begin
     LinkRes.Add('SEARCH_DIR('+MaybeQuoted(HPath.Str)+')');
     HPath:=TStringListItem(HPath.Next);
   end;

  { add objectfiles, start with prt0 always                  }
  { profiling of shared libraries is currently not supported }
  LinkRes.Add('INPUT(');
  if isdll then
   LinkRes.AddFileName(MaybeQuoted(FindObjectFile('wdllprt0','',false)))
  else
  if (cs_profile in aktmoduleswitches) then
   LinkRes.AddFileName(MaybeQuoted(FindObjectFile('gprt0','',false)))
  else
   begin
     if linklibcygwin then
      LinkRes.AddFileName(MaybeQuoted(FindObjectFile('wcygprt0','',false)))
     else
      LinkRes.AddFileName(MaybeQuoted(FindObjectFile('wprt0','',false)));
   end;

  while not ObjectFiles.Empty do
   begin
     s:=ObjectFiles.GetFirst;
     if s<>'' then
      LinkRes.AddFileName(MaybeQuoted(s));
   end;
  LinkRes.Add(')');


  { Write staticlibraries }
  if (not StaticLibFiles.Empty) or (cs_profile in aktmoduleswitches) then
   begin
     LinkRes.Add('GROUP(');
     if (cs_profile in aktmoduleswitches) then
       begin
         LinkRes.Add('-lgcc');
         LinkRes.Add('-lmoldname');
         LinkRes.Add('-lmsvcrt');
         LinkRes.Add('-lgmon');
         LinkRes.Add('-lkernel32');
       end;
     While not StaticLibFiles.Empty do
      begin
        S:=StaticLibFiles.GetFirst;
        LinkRes.AddFileName(MaybeQuoted(s));
      end;
     LinkRes.Add(')');
   end;

  { Write sharedlibraries }
  if not SharedLibFiles.Empty then
   begin
     LinkRes.Add('INPUT(') ;
     While not SharedLibFiles.Empty do
      begin
        S:=SharedLibFiles.GetFirst;
        if FindLibraryFile(s,target_info.staticClibprefix,target_info.staticClibext,s2) then
          begin
            LinkRes.Add(MaybeQuoted(s2));
            continue;
          end;
        if pos(target_info.sharedlibprefix,s)=1 then
          s:=copy(s,length(target_info.sharedlibprefix)+1,255);
        i:=Pos(target_info.sharedlibext,S);
        if i>0 then
         Delete(S,i,255);
        LinkRes.Add('-l'+s);
      end;
     LinkRes.Add(')');
   end;

{ Write and Close response }
  linkres.writetodisk;
  LinkRes.Free;

  WriteResponseFile:=True;
end;


function TLinkerWin32.MakeExecutable:boolean;
var
  binstr : String;
  cmdstr  : TCmdStr;
  success : boolean;
  cmds,i       : longint;
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
  AsBinStr:=FindUtil(utilsprefix+'as');
  if RelocSection then
   RelocStr:='--base-file base.$$$';
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
  if RelocSection or (not Deffile.empty) then
    cmds:=3
  else
    cmds:=1;
  for i:=1 to cmds do
   begin
     SplitBinCmd(Info.ExeCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        Replace(cmdstr,'$EXE',maybequoted(current_module.exefilename^));
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
        Replace(cmdstr,'$APPTYPE',AppTypeStr);
        Replace(cmdstr,'$ASBIN',AsbinStr);
        Replace(cmdstr,'$RELOC',RelocStr);
        Replace(cmdstr,'$IMAGEBASE',ImageBaseStr);
        Replace(cmdstr,'$STRIP',StripStr);
        if not DefFile.Empty then
          begin
            DefFile.WriteFile;
            Replace(cmdstr,'$DEF','-d '+maybequoted(deffile.fname));
          end
        else
          Replace(cmdstr,'$DEF','');
        success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,(i=1),false);
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
  binstr : String;
  cmdstr  : TCmdStr;
  success : boolean;
  cmds,
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
  AsBinStr:=FindUtil(utilsprefix+'as');
  if RelocSection then
   RelocStr:='--base-file base.$$$';
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
  if RelocSection or (not Deffile.empty) then
    cmds:=3
  else
    cmds:=1;
  for i:=1 to cmds do
   begin
     SplitBinCmd(Info.DllCmd[i],binstr,cmdstr);
     if binstr<>'' then
      begin
        Replace(cmdstr,'$EXE',maybequoted(current_module.sharedlibfilename^));
        Replace(cmdstr,'$OPT',Info.ExtraOptions);
        Replace(cmdstr,'$RES',maybequoted(outputexedir+Info.ResName));
        Replace(cmdstr,'$APPTYPE',AppTypeStr);
        Replace(cmdstr,'$ASBIN',AsbinStr);
        Replace(cmdstr,'$RELOC',RelocStr);
        Replace(cmdstr,'$IMAGEBASE',ImageBaseStr);
        Replace(cmdstr,'$STRIP',StripStr);
        if not DefFile.Empty then
          begin
            DefFile.WriteFile;
            Replace(cmdstr,'$DEF','-d '+maybequoted(deffile.fname));
          end
        else
          Replace(cmdstr,'$DEF','');
        success:=DoExec(FindUtil(utilsprefix+binstr),cmdstr,(i=1),false);
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
     RemoveFile('deffile.$$$');
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
  psecfill=^TSecfill;
  TSecfill=record
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
     cmdstr:=cmdstr+' --input '+maybequoted(fn);
     cmdstr:=cmdstr+' --stack '+tostr(stacksize);
     DoExec(FindUtil(utilsprefix+'postw32'),cmdstr,false,false);
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


{****************************************************************************
                            TDLLScannerWin32
****************************************************************************}

    function tDLLScannerWin32.DOSstubOK(var x:cardinal):boolean;
      begin
        blockread(f,TheWord,2,loaded);
        if loaded<>2 then
         DOSstubOK:=false
        else
         begin
           DOSstubOK:=(TheWord='MZ');
           seek(f,$3C);
           blockread(f,x,4,loaded);
           if(loaded<>4)or(longint(x)>filesize(f))then
            DOSstubOK:=false;
         end;
      end;

    function TDLLScannerWin32.FindDLL(const s:string;var founddll:string):boolean;
      var
        sysdir : string;
        Found : boolean;
      begin
        Found:=false;
        { Look for DLL in:
          1. Current dir
          2. Library Path
          3. windir,windir/system,windir/system32 }
        Found:=FindFile(s,'.'+source_info.DirSep,founddll);
        if (not found) then
         Found:=librarysearchpath.FindFile(s,founddll);
        if (not found) then
         begin
           sysdir:=FixPath(GetEnv('windir'),false);
           Found:=FindFile(s,sysdir+';'+sysdir+'system'+source_info.DirSep+';'+sysdir+'system32'+source_info.DirSep,founddll);
         end;
        if (not found) then
         begin
           message1(exec_w_libfile_not_found,s);
           FoundDll:=s;
         end;
        FindDll:=Found;
      end;


    function tDLLScannerWin32.ExtractDllName(Const Name : string) : string;
      var n : string;
      begin
         n:=Upper(SplitExtension(Name));
         if (n='.DLL') or (n='.DRV') or (n='.EXE') then
           ExtractDllName:=Name
         else
           ExtractDllName:=Name+target_info.sharedlibext;
      end;



function tDLLScannerWin32.isSuitableFileType(x:cardinal):longbool;
 begin
  seek(f,x);
  blockread(f,TheWord,2,loaded);
  isSuitableFileType:=(loaded=2)and(TheWord='PE');
 end;


function tDLLScannerWin32.GetEdata(HeaderEntry:cardinal):longbool;
 type
  TObjInfo=packed record
   ObjName:array[0..7]of char;
   VirtSize,
   VirtAddr,
   RawSize,
   RawOffset,
   Reloc,
   LineNum:cardinal;
   RelCount,
   LineCount:word;
   flags:cardinal;
  end;
 var
  i:cardinal;
  ObjOfs:cardinal;
  Obj:TObjInfo;
  APE_obj,APE_Optsize:word;
  ExportRVA:cardinal;
  delta:cardinal;
 const
  IMAGE_SCN_CNT_CODE=$00000020;
 var
 _d:dirstr;
 _n:namestr;
 _e:extstr;
 function isUsedFunction(name:pchar):longbool;
  var
   hp:tExternalsItem;
  begin
   isUsedFunction:=false;
   hp:=tExternalsItem(current_module.Externals.first);
   while assigned(hp)do
    begin
     if(assigned(hp.data))and(not hp.found)then
      if hp.data^=StrPas(name)then
       begin
        isUsedFunction:=true;
        hp.found:=true;
        exit;
       end;
     hp:=tExternalsItem(hp.next);
    end;
  end;

 procedure Store(index:cardinal;name:pchar;isData:longbool);
  begin
   if not isUsedFunction(name)then
    exit;
   if not(current_module.uses_imports) then
    begin
     current_module.uses_imports:=true;
     importlib.preparelib(current_module.modulename^);
    end;
   if IsData then
    timportlibwin32(importlib).importvariable_str(name,_n,name)
   else
    timportlibwin32(importlib).importprocedure_str(name,_n,index,name);
  end;

 procedure ProcessEdata;
  type
   a8=array[0..7]of char;
  function GetSectionName(rva:cardinal;var Flags:cardinal):a8;
   var
    i:cardinal;
    LocObjOfs:cardinal;
    LocObj:TObjInfo;
   begin
    GetSectionName:='';
    Flags:=0;
    LocObjOfs:=APE_OptSize+HeaderOffset+24;
    for i:=1 to APE_obj do
     begin
      seek(f,LocObjOfs);
      blockread(f,LocObj,sizeof(LocObj));
      if(rva>=LocObj.VirtAddr)and(rva<=LocObj.VirtAddr+LocObj.RawSize)then
       begin
        GetSectionName:=a8(LocObj.ObjName);
        Flags:=LocObj.flags;
       end;
     end;
   end;
  var
   j,Fl:cardinal;
   ulongval,procEntry:cardinal;
   Ordinal:word;
   isData:longbool;
   ExpDir:packed record
    flag,
    stamp:cardinal;
    Major,
    Minor:word;
    Name,
    Base,
    NumFuncs,
    NumNames,
    AddrFuncs,
    AddrNames,
    AddrOrds:cardinal;
   end;
  begin
   with Obj do
    begin
     seek(f,RawOffset+delta);
     blockread(f,ExpDir,sizeof(ExpDir));
     fsplit(impname,_d,_n,_e);
     for j:=0 to pred(ExpDir.NumNames)do
      begin
{ Don't know why but this gives serious problems with overflow checking on }
{$IFOPT Q+}
{$DEFINE OVERFLOW_CHECK_WAS_ON}
{$ENDIF}
{$Q-}
       seek(f,RawOffset-VirtAddr+ExpDir.AddrOrds+j*2);
       blockread(f,Ordinal,2);
       seek(f,RawOffset-VirtAddr+ExpDir.AddrFuncs+cardinal(Ordinal)*4);
       blockread(f,ProcEntry,4);
       seek(f,RawOffset-VirtAddr+ExpDir.AddrNames+j*4);
       blockread(f,ulongval,4);
       seek(f,RawOffset-VirtAddr+ulongval);
       blockread(f,cstring,sizeof(cstring));
       isData:=GetSectionName(procentry,Fl)='';
{$IFDEF OVERFLOW_CHECK_WAS_ON}
{$Q+}
{$ENDIF}
       if not isData then
        isData:=Fl and IMAGE_SCN_CNT_CODE<>IMAGE_SCN_CNT_CODE;
       Store(succ(Ordinal),cstring,isData);
      end;
   end;
  end;
 begin
  GetEdata:=false;
  seek(f,HeaderEntry+120);
  blockread(f,ExportRVA,4);
  seek(f,HeaderEntry+6);
  blockread(f,APE_Obj,2);
  seek(f,HeaderEntry+20);
  blockread(f,APE_OptSize,2);
  ObjOfs:=APE_OptSize+HeaderOffset+24;
  for i:=1 to APE_obj do
   begin
    seek(f,ObjOfs);
    blockread(f,Obj,sizeof(Obj));
    inc(ObjOfs,sizeof(Obj));
    with Obj do
     if(VirtAddr<=ExportRva)and(ExportRva<VirtAddr+VirtSize)then
      begin
       delta:=ExportRva-VirtAddr;
       ProcessEdata;
       GetEdata:=true;
      end;
   end;
 end;

function tDLLScannerWin32.scan(const binname:string):longbool;
 var
  OldFileMode:longint;
  hs,
  foundimp : string;
 begin
   Scan:=false;
  { is there already an import library the we will use that one }
  if FindLibraryFile(binname,target_info.staticClibprefix,target_info.staticClibext,foundimp) then
   exit;
  { check if we can find the dll }
  hs:=AddExtension(binname,target_info.sharedlibext);
  if not FindDll(hs,impname) then
   exit;
  { read the dll file }
  assign(f,impname);
  OldFileMode:=filemode;
  filemode:=0;
  reset(f,1);
  filemode:=OldFileMode;
  if not DOSstubOK(HeaderOffset)then
   scan:=false
  else if not isSuitableFileType(HeaderOffset)then
   scan:=false
  else
   scan:=GetEdata(HeaderOffset);
  close(f);
 end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

initialization
{$ifdef i386}
  RegisterExternalLinker(system_i386_win32_info,TLinkerWin32);
  RegisterImport(system_i386_win32,TImportLibWin32);
  RegisterExport(system_i386_win32,TExportLibWin32);
  RegisterDLLScanner(system_i386_win32,TDLLScannerWin32);
  RegisterRes(res_gnu_windres_info);
  RegisterTarget(system_i386_win32_info);
{$endif i386}
{$ifdef x86_64}
  RegisterExternalLinker(system_x64_win64_info,TLinkerWin32);
  RegisterImport(system_x86_64_win64,TImportLibWin32);
  RegisterExport(system_x86_64_win64,TExportLibWin32);
  RegisterDLLScanner(system_x86_64_win64,TDLLScannerWin32);
  RegisterRes(res_gnu_windres_info);
  RegisterTarget(system_x64_win64_info);
{$endif x86_64}
end.
