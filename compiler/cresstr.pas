{
    Copyright (c) 1998-2002 by Michael van Canneyt

    Handles resourcestrings

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
unit cresstr;

{$i fpcdefs.inc}

interface

    Procedure GenerateResourceStrings;


implementation

uses
   SysUtils,
{$if FPC_FULLVERSION<20700}
   ccharset,
{$endif}
   cclasses,widestr,
   cutils,globtype,globals,systems,
   symbase,symconst,symtype,symdef,symsym,symtable,
   verbose,fmodule,ppu,
   aasmtai,aasmdata,aasmcnst,
   aasmcpu;

    Type
      { These are used to form a singly-linked list, ordered by hash value }
      TResourceStringItem = class(TLinkedListItem)
        Sym   : TConstSym;
        Name  : String;
        Value : Pchar;
        Len   : Longint;
        hash  : Cardinal;
        constructor Create(asym:TConstsym);
        destructor  Destroy;override;
        procedure CalcHash;
      end;

      Tresourcestrings=class
      private
        List : TLinkedList;
        procedure ConstSym_Register(p:TObject;arg:pointer);
      public
        constructor Create;
        destructor  Destroy;override;
        procedure CreateResourceStringData;
        procedure WriteRSJFile;
        procedure RegisterResourceStrings;
      end;



{ ---------------------------------------------------------------------
                          TRESOURCESTRING_ITEM
  ---------------------------------------------------------------------}

    constructor TResourceStringItem.Create(asym:TConstsym);
      begin
        inherited Create;
        Sym:=Asym;
        Name:=lower(asym.owner.name^+'.'+asym.Name);
        Len:=asym.value.len;
        GetMem(Value,Len);
        Move(asym.value.valueptr^,Value^,Len);
        CalcHash;
      end;


    destructor TResourceStringItem.Destroy;
      begin
        FreeMem(Value);
      end;


    procedure TResourceStringItem.CalcHash;
      Var
        g : Cardinal;
        I : longint;
      begin
        hash:=0;
        For I:=0 to Len-1 do { 0 terminated }
         begin
           hash:=hash shl 4;
           inc(Hash,Ord(Value[i]));
           g:=hash and ($f shl 28);
           if g<>0 then
            begin
              hash:=hash xor (g shr 24);
              hash:=hash xor g;
            end;
         end;
        If Hash=0 then
          Hash:=$ffffffff;
      end;


{ ---------------------------------------------------------------------
                          Tresourcestrings
  ---------------------------------------------------------------------}

    Constructor Tresourcestrings.Create;
      begin
        List:=TLinkedList.Create;
      end;


    Destructor Tresourcestrings.Destroy;
      begin
        List.Free;
      end;


    procedure Tresourcestrings.CreateResourceStringData;
      Var
        namelab,
        valuelab : tasmlabofs;
        R : TResourceStringItem;
        resstrdef: tdef;
        tcb : ttai_typedconstbuilder;
      begin
        resstrdef:=search_system_type('TRESOURCESTRINGRECORD').typedef;

        { Put resourcestrings in a new objectfile. Putting it in multiple files
          makes the linking too dependent on the linker script requiring a SORT(*) for
          the data sections }
        tcb:=ctai_typedconstbuilder.create([tcalo_make_dead_strippable,tcalo_new_section,tcalo_vectorized_dead_strip_start,tcalo_data_force_indirect,tcalo_is_public_asm]);
        { Write unitname entry }
        tcb.maybe_begin_aggregate(resstrdef);
        namelab:=tcb.emit_ansistring_const(current_asmdata.asmlists[al_const],@current_module.localsymtable.name^[1],length(current_module.localsymtable.name^),getansistringcodepage);
        tcb.emit_string_offset(namelab,length(current_module.localsymtable.name^),st_ansistring,false,charpointertype);
        tcb.emit_tai(tai_const.create_nil_dataptr,cansistringtype);
        tcb.emit_tai(tai_const.create_nil_dataptr,cansistringtype);
        tcb.emit_ord_const(0,u32inttype);
        tcb.maybe_end_aggregate(resstrdef);
        current_asmdata.asmlists[al_resourcestrings].concatList(
          tcb.get_final_asmlist_vectorized_dead_strip(
            resstrdef,'RESSTR','',current_module.localsymtable,sizeof(pint)
          )
        );
        tcb.free;

        { Add entries }
        R:=TResourceStringItem(List.First);
        while assigned(R) do
          begin
            tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_vectorized_dead_strip_item,tcalo_data_force_indirect]);
            if assigned(R.value) and (R.len<>0) then
              valuelab:=tcb.emit_ansistring_const(current_asmdata.asmlists[al_const],R.Value,R.Len,getansistringcodepage)
            else
              begin
                valuelab.lab:=nil;
                valuelab.ofs:=0;
              end;
            current_asmdata.asmlists[al_const].concat(cai_align.Create(sizeof(pint)));
            namelab:=tcb.emit_ansistring_const(current_asmdata.asmlists[al_const],@R.Name[1],length(R.name),getansistringcodepage);
            {
              Resourcestring index:
                  TResourceStringRecord = Packed Record
                     Name,
                     CurrentValue,
                     DefaultValue : AnsiString;
                     HashValue    : LongWord;
                   end;
            }
            tcb.maybe_begin_aggregate(resstrdef);
            tcb.emit_string_offset(namelab,length(current_module.localsymtable.name^),st_ansistring,false,charpointertype);
            tcb.emit_string_offset(valuelab,R.Len,st_ansistring,false,charpointertype);
            tcb.emit_string_offset(valuelab,R.Len,st_ansistring,false,charpointertype);
            tcb.emit_ord_const(R.hash,u32inttype);
            tcb.maybe_end_aggregate(resstrdef);
            current_asmdata.asmlists[al_resourcestrings].concatList(
              tcb.get_final_asmlist_vectorized_dead_strip(
                resstrdef,'RESSTR',R.Sym.Name,R.Sym.Owner,sizeof(pint))
            );
            R:=TResourceStringItem(R.Next);
            tcb.free;
          end;
        tcb:=ctai_typedconstbuilder.create([tcalo_new_section,tcalo_vectorized_dead_strip_end,tcalo_data_force_indirect,tcalo_is_public_asm]);
        tcb.begin_anonymous_record(internaltypeprefixName[itp_emptyrec],
          default_settings.packrecords,sizeof(pint),
          targetinfos[target_info.system]^.alignment.recordalignmin,
          targetinfos[target_info.system]^.alignment.maxCrecordalign);
        current_asmdata.AsmLists[al_resourcestrings].concatList(
          tcb.get_final_asmlist_vectorized_dead_strip(
            tcb.end_anonymous_record,'RESSTR','',current_module.localsymtable,sizeof(pint)
          )
        );
        tcb.free;
      end;

    procedure Tresourcestrings.WriteRSJFile;
      Var
        F: Text;
        R: TResourceStringItem;
        ResFileName: string;
        I: Integer;
        C: tcompilerwidechar;
        W: pcompilerwidestring;
      begin
        ResFileName:=ChangeFileExt(current_module.ppufilename,'.rsj');
        message1 (general_i_writingresourcefile,ExtractFileName(ResFileName));
        Assign(F,ResFileName);
        {$push}{$i-}
        Rewrite(f);
        {$pop}
        if IOresult<>0 then
          begin
            message1(general_e_errorwritingresourcefile,ResFileName);
            exit;
          end;
        { write the data in two formats:
           a) backward compatible: the plain bytes from the source file
           b) portable: converted to utf-16
        }
        writeln(f,'{"version":1,"strings":[');
        R:=TResourceStringItem(List.First);
        while assigned(R) do
          begin
            write(f, '{"hash":',R.Hash,',"name":"',R.Name,'","sourcebytes":[');
            for i:=0 to R.Len-1 do
              begin
                write(f,ord(R.Value[i]));
                if i<>R.Len-1 then
                  write(f,',');
              end;
            write(f,'],"value":"');
            initwidestring(W);
            ascii2unicode(R.Value,R.Len,current_settings.sourcecodepage,W);
            for I := 0 to W^.len - 1 do
              begin
                C := W^.Data[I];
                case C of
                  Ord('"'), Ord('\'), Ord('/'):
                    write(f, '\', Chr(C));
                  8:
                    write(f, '\b');
                  9:
                    write(f, '\t');
                  10:
                    write(f, '\n');
                  13:
                    write(f, '\r');
                  12:
                    write(f, '\f');
                  else
                  if (C < 32) or (C > 127) then
                    write(f,'\u',hexStr(Longint(C), 4))
                  else
                    write(f,Chr(C));
                end;
              end;
            donewidestring(W);
            write(f,'"}');
            R:=TResourceStringItem(R.Next);
            if assigned(R) then
              writeln(f,',')
            else
              writeln(f);
          end;
        writeln(f,']}');
        close(f);
      end;

    procedure Tresourcestrings.ConstSym_Register(p:TObject;arg:pointer);
      begin
        if (tsym(p).typ=constsym) and
           (tconstsym(p).consttyp=constresourcestring) then
          List.Concat(TResourceStringItem.Create(TConstsym(p)));
      end;


    procedure Tresourcestrings.RegisterResourceStrings;
      begin
        if assigned(current_module.globalsymtable) then
          current_module.globalsymtable.SymList.ForEachCall(@ConstSym_Register,nil);
        current_module.localsymtable.SymList.ForEachCall(@ConstSym_Register,nil);
      end;


    Procedure GenerateResourceStrings;
      var
        resstrs : Tresourcestrings;
      begin
        { needed for the typed constant defs that get generated/looked up }
        if assigned(current_module.globalsymtable) then
          symtablestack.push(current_module.globalsymtable);
        symtablestack.push(current_module.localsymtable);
        resstrs:=Tresourcestrings.Create;
        resstrs.RegisterResourceStrings;
        if not resstrs.List.Empty then
          begin
            current_module.flags:=current_module.flags or uf_has_resourcestrings;
            resstrs.CreateResourceStringData;
            resstrs.WriteRSJFile;
          end;
        resstrs.Free;
        symtablestack.pop(current_module.localsymtable);
        if assigned(current_module.globalsymtable) then
          symtablestack.pop(current_module.globalsymtable);
      end;

end.
