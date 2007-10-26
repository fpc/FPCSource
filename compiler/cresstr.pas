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
   cclasses,
   cutils,globtype,globals,systems,
   symconst,symtype,symdef,symsym,
   verbose,fmodule,ppu,
   aasmbase,aasmtai,aasmdata,
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
        Procedure WriteResourceFile;
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

        function WriteValueString(p:pchar;len:longint):TasmLabel;
        var
          s : pchar;
          referencelab: TAsmLabel;
        begin
          if (target_info.system in systems_darwin) then
            begin
              current_asmdata.getdatalabel(referencelab);
              current_asmdata.asmlists[al_const].concat(tai_label.create(referencelab));
            end;
          current_asmdata.getdatalabel(result);
          current_asmdata.asmlists[al_const].concat(tai_align.create(const_align(sizeof(aint))));
          current_asmdata.asmlists[al_const].concat(tai_const.create_aint(-1));
          current_asmdata.asmlists[al_const].concat(tai_const.create_aint(len));
          current_asmdata.asmlists[al_const].concat(tai_label.create(result));
          if (target_info.system in systems_darwin) then
             current_asmdata.asmlists[al_const].concat(tai_directive.create(asd_reference,referencelab.name));
          getmem(s,len+1);
          move(p^,s^,len);
          s[len]:=#0;
          current_asmdata.asmlists[al_const].concat(tai_string.create_pchar(s,len));
          current_asmdata.asmlists[al_const].concat(tai_const.create_8bit(0));
        end;

      Var
        namelab,
        valuelab : tasmlabel;
        resstrlab : tasmsymbol;
        R : TResourceStringItem;
      begin
        { Put resourcestrings in a new objectfile. Putting it in multiple files
	  makes the linking too dependent on the linker script requiring a SORT(*) for
	  the data sections }
        maybe_new_object_file(current_asmdata.asmlists[al_const]);
        maybe_new_object_file(current_asmdata.asmlists[al_resourcestrings]);
        new_section(current_asmdata.asmlists[al_resourcestrings],sec_data,make_mangledname('RESSTR',current_module.localsymtable,'1_START'),sizeof(aint));
        current_asmdata.AsmLists[al_resourcestrings].concat(tai_symbol.createname_global(
          make_mangledname('RESSTR',current_module.localsymtable,'START'),AT_DATA,0));

        { Write unitname entry }
        namelab:=WriteValueString(@current_module.localsymtable.name^[1],length(current_module.localsymtable.name^));
        current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_sym(namelab));
        current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_sym(nil));
        current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_sym(nil));
        current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_32bit(0));
{$ifdef cpu64bit}
        current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_32bit(0));
{$endif cpu64bit}

        { Add entries }
        R:=TResourceStringItem(List.First);
        while assigned(R) do
          begin
            new_section(current_asmdata.asmlists[al_const],sec_rodata,make_mangledname('RESSTR',current_module.localsymtable,'d_'+r.name),sizeof(aint));
            { Write default value }
            if assigned(R.value) and (R.len<>0) then
              valuelab:=WriteValueString(R.Value,R.Len)
            else
              valuelab:=nil;
            { Append the name as a ansistring. }
            namelab:=WriteValueString(@R.Name[1],length(R.name));

            {
              Resourcestring index:
                  TResourceStringRecord = Packed Record
                     Name,
                     CurrentValue,
                     DefaultValue : AnsiString;
                     HashValue    : LongWord;
                   end;
            }
            new_section(current_asmdata.asmlists[al_resourcestrings],sec_data,make_mangledname('RESSTR',current_module.localsymtable,'2_'+r.name),sizeof(aint));
            resstrlab:=current_asmdata.DefineAsmSymbol(make_mangledname('RESSTR',R.Sym.owner,R.Sym.name),AB_GLOBAL,AT_DATA);
            current_asmdata.asmlists[al_resourcestrings].concat(tai_symbol.Create_global(resstrlab,0));
            current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_sym(namelab));
            current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_sym(valuelab));
            current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_sym(valuelab));
            current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_32bit(longint(R.Hash)));
{$ifdef cpu64bit}
            current_asmdata.asmlists[al_resourcestrings].concat(tai_const.create_32bit(0));
{$endif cpu64bit}
            current_asmdata.asmlists[al_resourcestrings].concat(tai_symbol_end.create(resstrlab));
            R:=TResourceStringItem(R.Next);
          end;
        new_section(current_asmdata.asmlists[al_resourcestrings],sec_data,make_mangledname('RESSTR',current_module.localsymtable,'3_END'),sizeof(aint));
        current_asmdata.AsmLists[al_resourcestrings].concat(tai_symbol.createname_global(
          make_mangledname('RESSTR',current_module.localsymtable,'END'),AT_DATA,0));
        { The darwin/ppc64 assembler or linker seems to have trouble       }
        { if a section ends with a global label without any data after it. }
        { So for safety, just put a dummy value here.                      }
        { Further, the regular linker also kills this symbol when turning  }
        { on smart linking in case no value appears after it, so put the   }
        { dummy byte there always                                          }
        if (target_info.system in systems_darwin) then   
          current_asmdata.asmlists[al_resourcestrings].concat(Tai_const.create_8bit(0));
      end;


    Procedure Tresourcestrings.WriteResourceFile;
      Type
        TMode = (quoted,unquoted);
      Var
        F : Text;
        Mode : TMode;
        R : TResourceStringItem;
        C : char;
        Col,i : longint;
        ResFileName : string;

        Procedure Add(Const S : String);
        begin
          Write(F,S);
          inc(Col,length(s));
        end;

      begin
        ResFileName:=ChangeFileExt(current_module.ppufilename^,'.rst');
        message1 (general_i_writingresourcefile,ExtractFileName(ResFileName));
        Assign(F,ResFileName);
        {$i-}
        Rewrite(f);
        {$i+}
        If IOresult<>0 then
          begin
            message1(general_e_errorwritingresourcefile,ResFileName);
            exit;
          end;
        R:=TResourceStringItem(List.First);
        while assigned(R) do
          begin
            writeln(f);
            Writeln(f,'# hash value = ',R.Hash);
            col:=0;
            Add(R.Name+'=');
            Mode:=unquoted;
            For I:=0 to R.Len-1 do
             begin
               C:=R.Value[i];
               If (ord(C)>31) and (Ord(c)<=128) and (c<>'''') then
                begin
                  If mode=Quoted then
                   Add(c)
                  else
                   begin
                     Add(''''+c);
                     mode:=quoted
                   end;
                end
               else
                begin
                  If Mode=quoted then
                   begin
                     Add('''');
                     mode:=unquoted;
                   end;
                  Add('#'+tostr(ord(c)));
                end;
               If Col>72 then
                begin
                  if mode=quoted then
                   Write (F,'''');
                  Writeln(F,'+');
                  Col:=0;
                  Mode:=unQuoted;
                end;
             end;
            if mode=quoted then
             writeln (f,'''');
            Writeln(f);
            R:=TResourceStringItem(R.Next);
          end;
        close(f);
      end;


    procedure Tresourcestrings.ConstSym_Register(p:TObject;arg:pointer);
      begin
        if (tsym(p).typ=constsym) and
           (tconstsym(p).consttyp=constresourcestring) then
          List.Concat(tResourceStringItem.Create(TConstsym(p)));
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
        resstrs:=Tresourcestrings.Create;
        resstrs.RegisterResourceStrings;
        if not resstrs.List.Empty then
          begin
            current_module.flags:=current_module.flags or uf_has_resourcestrings;
            resstrs.CreateResourceStringData;
            resstrs.WriteResourceFile;
          end;
        resstrs.Free;
      end;

end.
