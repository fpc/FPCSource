{
    Copyright (c) 1998-2011 by Florian Klaempfl and Jonas Maebe

    Generate assembler for constant nodes for the JVM

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
unit njvmcon;

{$i fpcdefs.inc}

interface

    uses
       symtype,
       node,ncon,ncgcon;

    type
       tjvmordconstnode = class(tcgordconstnode)
          { normally, we convert the enum constant into a load of the
            appropriate enum class field in pass_1. In some cases (array index),
            we want to keep it as an enum constant however }
          enumconstok: boolean;
          function pass_1: tnode; override;
          function docompare(p: tnode): boolean; override;
          function dogetcopy: tnode; override;
       end;

       tjvmrealconstnode = class(tcgrealconstnode)
          procedure pass_generate_code;override;
       end;

       tjvmstringconstnode = class(tstringconstnode)
          function pass_1: tnode; override;
          procedure pass_generate_code;override;
       end;


implementation

    uses
      globtype,cutils,widestr,verbose,constexp,
      symdef,symsym,symtable,symconst,
      aasmdata,aasmcpu,defutil,
      ncal,nld,
      cgbase,hlcgobj,hlcgcpu,cgutils,cpubase
      ;


{*****************************************************************************
                           TJVMORDCONSTNODE
*****************************************************************************}

    function tjvmordconstnode.pass_1: tnode;
      var
        basedef: tenumdef;
        sym: tenumsym;
        classfield: tsym;
        i: longint;
      begin
        if (resultdef.typ<>enumdef) or
           enumconstok then
          begin
            result:=inherited pass_1;
            exit;
          end;
        { convert into JVM class instance }
        { a) find the enumsym corresponding to the value (may not exist in case
             of an explicit typecast of an integer -> error) }
        sym:=nil;
        basedef:=tenumdef(resultdef).getbasedef;
        for i:=0 to tenumdef(resultdef).symtable.symlist.count-1 do
          begin
            sym:=tenumsym(basedef.symtable.symlist[i]);
            if sym.value=value then
              break;
            sym:=nil;
          end;
        if not assigned(sym) then
          begin
            Message(parser_e_range_check_error);
            exit;
          end;
        { b) find the corresponding class field }
        classfield:=search_struct_member(basedef.classdef,sym.name);
        if not assigned(classfield) or
           (classfield.typ<>staticvarsym) then
          internalerror(2011062606);
        { c) create loadnode of the field }
        result:=cloadnode.create(classfield,classfield.owner);
      end;


    function tjvmordconstnode.docompare(p: tnode): boolean;
      begin
        result:=inherited docompare(p);
        if result then
          result:=(enumconstok=tjvmordconstnode(p).enumconstok);
      end;


    function tjvmordconstnode.dogetcopy: tnode;
      begin
        result:=inherited dogetcopy;
        tjvmordconstnode(result).enumconstok:=enumconstok;
      end;


{*****************************************************************************
                           TJVMREALCONSTNODE
*****************************************************************************}

    procedure tjvmrealconstnode.pass_generate_code;
      begin
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);
        thlcgjvm(hlcg).a_loadfpu_const_stack(current_asmdata.CurrAsmList,resultdef,value_real);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


    { tcgstringconstnode }

    function tjvmstringconstnode.pass_1: tnode;
      var
        strclass: tobjectdef;
        psym: tsym;
        pw: pcompilerwidestring;
      begin
        { all Java strings are utf-16. However, there is no way to
          declare a constant array of bytes (or any other type), those
          have to be constructed by declaring a final field and then
          initialising them in the class constructor element per
          element. We therefore put the straight ASCII values into
          the UTF-16 string, and then at run time extract those and
          store them in an Ansistring/AnsiChar array }
        result:=inherited pass_1;
        if assigned(result) or
           (cst_type in [cst_unicodestring,cst_widestring]) then
          exit;
        { convert the constant into a widestring representation without any
          code page conversion }
        initwidestring(pw);
        ascii2unicode(value_str,len,pw,false);
        ansistringdispose(value_str,len);
        pcompilerwidestring(value_str):=pw;
        { and now add a node to convert the data into ansistring format at
          run time }
        case cst_type of
          cst_ansistring:
            strclass:=tobjectdef(search_system_type('ANSISTRINGCLASS').typedef);
          cst_shortstring:
            strclass:=tobjectdef(search_system_type('SHORTSTRINGCLASS').typedef);
          cst_conststring:
            { used for array of char }
            strclass:=tobjectdef(search_system_type('ANSICHARARRAYCLASS').typedef);
          else
           internalerror(2011052401);
        end;
        cst_type:=cst_unicodestring;
        psym:=search_struct_member(strclass,'CREATEFROMLITERALSTRINGBYTES');
        if not assigned(psym) or
           (psym.typ<>procsym) then
          internalerror(2011052001);
        { since self will be freed, have to make a copy }
        result:=ccallnode.create(ccallparanode.create(self.getcopy,nil),
          tprocsym(psym),psym.owner,nil,[]);
      end;


    procedure tjvmstringconstnode.pass_generate_code;
      begin
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,resultdef);
        case cst_type of
          cst_ansistring:
            begin
              current_asmdata.CurrAsmList.concat(taicpu.op_string(a_ldc,len,value_str));
            end;
          cst_shortstring,
          cst_conststring:
            current_asmdata.CurrAsmList.concat(taicpu.op_string(a_ldc,len,value_str));
          cst_unicodestring,
          cst_widestring:
            current_asmdata.CurrAsmList.concat(taicpu.op_wstring(a_ldc,pcompilerwidestring(value_str)));
        end;
        thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,1);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;



begin
   cordconstnode:=tjvmordconstnode;
   crealconstnode:=tjvmrealconstnode;
   cstringconstnode:=tjvmstringconstnode;
end.
