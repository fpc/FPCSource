{
    $Id$
    Copyright (c) 1998-2001 by Florian Klaempfl

    Generates nodes for routines that need compiler support

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
unit pinline;

{$i defines.inc}

interface

    uses
      symtype,
      node,
      globals,
      cpuinfo;

    function new_dispose_statement(is_new:boolean) : tnode;
    function new_function : tnode;

    function inline_setlength : tnode;
    function inline_finalize : tnode;


implementation

    uses
{$ifdef delphi}
       SysUtils,
{$endif}
       { common }
       cutils,
       { global }
       globtype,tokens,verbose,
       systems,widestr,
       { symtable }
       symconst,symbase,symdef,symsym,symtable,types,
       { pass 1 }
       pass_1,htypechk,
       nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,nbas,
       { parser }
       scanner,
       pbase,pexpr,
       { codegen }
       cgbase
       ;


    function new_dispose_statement(is_new:boolean) : tnode;
      var
        newstatement : tstatementnode;
        temp         : ttempcreatenode;
        para         : tcallparanode;
        p,p2     : tnode;
        again    : boolean; { dummy for do_proc_call }
        destructorname : stringid;
        sym      : tsym;
        classh   : tobjectdef;
        destructorpos,
        storepos : tfileposinfo;
      begin
        consume(_LKLAMMER);
        p:=comp_expr(true);
        { calc return type }
        set_varstate(p,(not is_new));
        { constructor,destructor specified }
        if try_to_consume(_COMMA) then
          begin
            { extended syntax of new and dispose }
            { function styled new is handled in factor }
            { destructors have no parameters }
            destructorname:=pattern;
            destructorpos:=akttokenpos;
            consume(_ID);

            if (p.resulttype.def.deftype<>pointerdef) then
              begin
                 Message1(type_e_pointer_type_expected,p.resulttype.def.typename);
                 p.free;
                 p:=factor(false);
                 p.free;
                 consume(_RKLAMMER);
                 new_dispose_statement:=cerrornode.create;
                 exit;
              end;
            { first parameter must be an object or class }
            if tpointerdef(p.resulttype.def).pointertype.def.deftype<>objectdef then
              begin
                 Message(parser_e_pointer_to_class_expected);
                 p.free;
                 new_dispose_statement:=factor(false);
                 consume_all_until(_RKLAMMER);
                 consume(_RKLAMMER);
                 exit;
              end;
            { check, if the first parameter is a pointer to a _class_ }
            classh:=tobjectdef(tpointerdef(p.resulttype.def).pointertype.def);
            if is_class(classh) then
              begin
                 Message(parser_e_no_new_or_dispose_for_classes);
                 new_dispose_statement:=factor(false);
                 consume_all_until(_RKLAMMER);
                 consume(_RKLAMMER);
                 exit;
              end;
            { search cons-/destructor, also in parent classes }
            storepos:=akttokenpos;
            akttokenpos:=destructorpos;
            sym:=search_class_member(classh,destructorname);
            akttokenpos:=storepos;

            { the second parameter of new/dispose must be a call }
            { to a cons-/destructor                              }
            if (not assigned(sym)) or (sym.typ<>procsym) then
              begin
                 if is_new then
                  Message(parser_e_expr_have_to_be_constructor_call)
                 else
                  Message(parser_e_expr_have_to_be_destructor_call);
                 p.free;
                 new_dispose_statement:=cerrornode.create;
              end
            else
              begin
                if is_new then
                 p2:=chnewnode.create(tpointerdef(p.resulttype.def).pointertype)
                else
                 p2:=chdisposenode.create(p);
                do_resulttypepass(p2);
                if is_new then
                  do_member_read(false,sym,p2,again)
                else
                  begin
                    if not(m_fpc in aktmodeswitches) then
                      do_member_read(false,sym,p2,again)
                    else
                      begin
                        p2:=ccallnode.create(nil,tprocsym(sym),sym.owner,p2);
                        { support dispose(p,done()); }
                        if try_to_consume(_LKLAMMER) then
                          begin
                            if not try_to_consume(_RKLAMMER) then
                              begin
                                Message(parser_e_no_paras_for_destructor);
                                consume_all_until(_RKLAMMER);
                                consume(_RKLAMMER);
                              end;
                          end;
                      end;
                  end;

                { we need the real called method }
                { rg.cleartempgen;}
                do_resulttypepass(p2);
                if not codegenerror then
                 begin
                   if is_new then
                    begin
                      if (tcallnode(p2).procdefinition.proctypeoption<>potype_constructor) then
                        Message(parser_e_expr_have_to_be_constructor_call);
                      p2.resulttype:=p.resulttype;
                      p2:=cassignmentnode.create(p,p2);
                    end
                   else
                    begin
                      if (tcallnode(p2).procdefinition.proctypeoption<>potype_destructor) then
                        Message(parser_e_expr_have_to_be_destructor_call);
                    end;
                 end;
                new_dispose_statement:=p2;
              end;
          end
        else
          begin
             if (p.resulttype.def.deftype<>pointerdef) then
               Begin
                  Message1(type_e_pointer_type_expected,p.resulttype.def.typename);
                  new_dispose_statement:=cerrornode.create;
               end
             else
               begin
                  if (tpointerdef(p.resulttype.def).pointertype.def.deftype=objectdef) and
                     (oo_has_vmt in tobjectdef(tpointerdef(p.resulttype.def).pointertype.def).objectoptions) then
                    Message(parser_w_use_extended_syntax_for_objects);
                  if (tpointerdef(p.resulttype.def).pointertype.def.deftype=orddef) and
                     (torddef(tpointerdef(p.resulttype.def).pointertype.def).typ=uvoid) then
                    begin
                      if (m_tp7 in aktmodeswitches) or
                         (m_delphi in aktmodeswitches) then
                       Message(parser_w_no_new_dispose_on_void_pointers)
                      else
                       Message(parser_e_no_new_dispose_on_void_pointers);
                    end;

                  { create statements with call to getmem+initialize or
                    finalize+freemem }
                  new_dispose_statement:=internalstatements(newstatement);

                  if is_new then
                   begin
                     { create temp for result }
                     temp := ctempcreatenode.create(p.resulttype,p.resulttype.def.size,true);
                     addstatement(newstatement,temp);

                     { create call to fpc_getmem }
                     para := ccallparanode.create(cordconstnode.create
                         (tpointerdef(p.resulttype.def).pointertype.def.size,s32bittype),nil);
                     addstatement(newstatement,cassignmentnode.create(
                         ctemprefnode.create(temp),
                         ccallnode.createintern('fpc_getmem',para)));

                     { create call to fpc_initialize }
                     if tpointerdef(p.resulttype.def).pointertype.def.needs_inittable then
                      begin
                        para := ccallparanode.create(caddrnode.create(crttinode.create(
                                   tstoreddef(tpointerdef(p.resulttype.def).pointertype.def),initrtti)),
                                ccallparanode.create(ctemprefnode.create
                                   (temp),nil));
                        addstatement(newstatement,ccallnode.createintern('fpc_initialize',para));
                      end;

                     { copy the temp to the destination }
                     addstatement(newstatement,cassignmentnode.create(
                         p,
                         ctemprefnode.create(temp)));

                     { release temp }
                     addstatement(newstatement,ctempdeletenode.create(temp));
                   end
                  else
                   begin
                     { create call to fpc_finalize }
                     if tpointerdef(p.resulttype.def).pointertype.def.needs_inittable then
                      begin
                        { we need to use a copy of p here }
                        para := ccallparanode.create(caddrnode.create(crttinode.create
                                   (tstoreddef(tpointerdef(p.resulttype.def).pointertype.def),initrtti)),
                                ccallparanode.create(p.getcopy,nil));
                        addstatement(newstatement,ccallnode.createintern('fpc_finalize',para));
                      end;

                     { create call to fpc_freemem }
                     para := ccallparanode.create(p,nil);
                     addstatement(newstatement,ccallnode.createintern('fpc_freemem',para));
                   end;
               end;
          end;
        consume(_RKLAMMER);
      end;


    function new_function : tnode;
      var
        newstatement : tstatementnode;
        newblock     : tblocknode;
        temp         : ttempcreatenode;
        para         : tcallparanode;
        p1,p2  : tnode;
        classh : tobjectdef;
        sym    : tsym;
        again  : boolean; { dummy for do_proc_call }
      begin
        consume(_LKLAMMER);
        p1:=factor(false);
        if p1.nodetype<>typen then
         begin
           Message(type_e_type_id_expected);
           p1.destroy;
           p1:=cerrornode.create;
           do_resulttypepass(p1);
         end;

        if (p1.resulttype.def.deftype<>pointerdef) then
          Message1(type_e_pointer_type_expected,p1.resulttype.def.typename)
        else
         if token=_RKLAMMER then
          begin
            if (tpointerdef(p1.resulttype.def).pointertype.def.deftype=objectdef) and
               (oo_has_vmt in tobjectdef(tpointerdef(p1.resulttype.def).pointertype.def).objectoptions)  then
              Message(parser_w_use_extended_syntax_for_objects);

            { create statements with call to getmem+initialize }
            newblock:=internalstatements(newstatement);

            { create temp for result }
            temp := ctempcreatenode.create(p1.resulttype,p1.resulttype.def.size,true);
            addstatement(newstatement,temp);

            { create call to fpc_getmem }
            para := ccallparanode.create(cordconstnode.create
                (tpointerdef(p1.resulttype.def).pointertype.def.size,s32bittype),nil);
            addstatement(newstatement,cassignmentnode.create(
                ctemprefnode.create(temp),
                ccallnode.createintern('fpc_getmem',para)));

            { create call to fpc_initialize }
            if tpointerdef(p1.resulttype.def).pointertype.def.needs_inittable then
             begin
               para := ccallparanode.create(caddrnode.create(crttinode.create
                          (tstoreddef(tpointerdef(p1.resulttype.def).pointertype.def),initrtti)),
                       ccallparanode.create(ctemprefnode.create
                          (temp),nil));
               addstatement(newstatement,ccallnode.createintern('fpc_initialize',para));
             end;

            { the last statement should return the value as
              location and type, this is done be referencing the
              temp and converting it first from a persistent temp to
              normal temp }
            addstatement(newstatement,ctempdeletenode.create_normal_temp(temp));
            addstatement(newstatement,ctemprefnode.create(temp));

            p1.destroy;
            p1:=newblock;
            consume(_RKLAMMER);
          end
        else
          begin
            p2:=chnewnode.create(tpointerdef(p1.resulttype.def).pointertype);
            do_resulttypepass(p2);
            consume(_COMMA);
            afterassignment:=false;
            { determines the current object defintion }
            classh:=tobjectdef(p2.resulttype.def);
            if classh.deftype=objectdef then
             begin
               { check for an abstract class }
               if (oo_has_abstract in classh.objectoptions) then
                Message(sym_e_no_instance_of_abstract_object);
               { search the constructor also in the symbol tables of
                 the parents }
               sym:=searchsym_in_class(classh,pattern);
               consume(_ID);
               do_member_read(false,sym,p2,again);
               { we need to know which procedure is called }
               do_resulttypepass(p2);
               if (p2.nodetype<>calln) or
                  (assigned(tcallnode(p2).procdefinition) and
                   (tcallnode(p2).procdefinition.proctypeoption<>potype_constructor)) then
                Message(parser_e_expr_have_to_be_constructor_call);
             end
            else
             Message(parser_e_pointer_to_class_expected);
            { constructors return boolean, update resulttype to return
              the pointer to the object }
            p2.resulttype:=p1.resulttype;
            p1.destroy;
            p1:=p2;
            consume(_RKLAMMER);
          end;
        new_function:=p1;
      end;


    function inline_setlength : tnode;
      var
        paras   : tnode;
        npara,
        ppn     : tcallparanode;
        counter : integer;
        isarray : boolean;
        def     : tdef;
        destppn : tnode;
        newstatement : tstatementnode;
        temp    : ttempcreatenode;
        newblock : tnode;
      begin
        { for easy exiting if something goes wrong }
        result := cerrornode.create;

        consume(_LKLAMMER);
        paras:=parse_paras(false,false);
        consume(_RKLAMMER);
        if not assigned(paras) then
         begin
           CGMessage(parser_e_wrong_parameter_size);
           exit;
         end;

        counter:=0;
        if assigned(paras) then
         begin
           { check type of lengths }
           ppn:=tcallparanode(paras);
           while assigned(ppn.right) do
            begin
              set_varstate(ppn.left,true);
              inserttypeconv(ppn.left,s32bittype);
              inc(counter);
              ppn:=tcallparanode(ppn.right);
            end;
         end;
        if counter=0 then
         begin
           CGMessage(parser_e_wrong_parameter_size);
           paras.free;
           exit;
         end;
        { last param must be var }
        destppn:=ppn.left;
        inc(parsing_para_level);
        valid_for_var(destppn);
        set_varstate(destppn,false);
        dec(parsing_para_level);
        { first param must be a string or dynamic array ...}
        isarray:=is_dynamic_array(destppn.resulttype.def);
        if not((destppn.resulttype.def.deftype=stringdef) or
               isarray) then
         begin
           CGMessage(type_e_mismatch);
           paras.free;
           exit;
         end;

        { only dynamic arrays accept more dimensions }
        if (counter>1) then
         begin
           if (not isarray) then
            CGMessage(type_e_mismatch)
           else
            begin
              { check if the amount of dimensions is valid }
              def := tarraydef(destppn.resulttype.def).elementtype.def;
              while counter > 1 do
                begin
                  if not(is_dynamic_array(def)) then
                    begin
                      CGMessage(parser_e_wrong_parameter_size);
                      break;
                    end;
                  dec(counter);
                  def := tarraydef(def).elementtype.def;
                end;
            end;
         end;

        if isarray then
         begin
            { create statements with call initialize the arguments and
              call fpc_dynarr_setlength }
            newblock:=internalstatements(newstatement);

            { get temp for array of lengths }
            temp := ctempcreatenode.create(s32bittype,counter*s32bittype.def.size,true);
            addstatement(newstatement,temp);

            { load array of lengths }
            ppn:=tcallparanode(paras);
            counter:=0;
            while assigned(ppn.right) do
             begin
               addstatement(newstatement,cassignmentnode.create(
                   ctemprefnode.create_offset(temp,counter*s32bittype.def.size),
                   ppn.left));
               ppn.left:=nil;
               inc(counter);
               ppn:=tcallparanode(ppn.right);
             end;
            { destppn is also reused }
            ppn.left:=nil;

            { create call to fpc_dynarr_setlength }
            npara:=ccallparanode.create(caddrnode.create
                      (ctemprefnode.create(temp)),
                   ccallparanode.create(cordconstnode.create
                      (counter,s32bittype),
                   ccallparanode.create(caddrnode.create
                      (crttinode.create(tstoreddef(destppn.resulttype.def),initrtti)),
                   ccallparanode.create(ctypeconvnode.create_explicit(destppn,voidpointertype),nil))));
            addstatement(newstatement,ccallnode.createintern('fpc_dynarray_setlength',npara));
            addstatement(newstatement,ctempdeletenode.create(temp));

            { we don't need original the callparanodes tree }
            paras.free;
         end
        else
         begin
            { we can reuse the supplied parameters }
            newblock:=ccallnode.createintern(
               'fpc_'+tstringdef(destppn.resulttype.def).stringtypname+'_setlength',paras);
         end;

        result.free;
        result:=newblock;
      end;


    function inline_finalize : tnode;
      var
        newblock,
        paras   : tnode;
        npara,
        destppn,
        ppn     : tcallparanode;
      begin
        { for easy exiting if something goes wrong }
        result := cerrornode.create;

        consume(_LKLAMMER);
        paras:=parse_paras(false,false);
        consume(_RKLAMMER);
        if not assigned(paras) then
         begin
           CGMessage(parser_e_wrong_parameter_size);
           exit;
         end;

        ppn:=tcallparanode(paras);
        { 2 arguments? }
        if assigned(ppn.right) then
         begin
           destppn:=tcallparanode(ppn.right);
           { 3 arguments is invalid }
           if assigned(destppn.right) then
            begin
              CGMessage(parser_e_wrong_parameter_size);
              paras.free;
              exit;
            end;
           { create call to fpc_finalize_array }
           npara:=ccallparanode.create(cordconstnode.create
                     (destppn.left.resulttype.def.size,s32bittype),
                  ccallparanode.create(ctypeconvnode.create
                     (ppn.left,s32bittype),
                  ccallparanode.create(caddrnode.create
                     (crttinode.create(tstoreddef(destppn.left.resulttype.def),initrtti)),
                  ccallparanode.create(caddrnode.create
                     (destppn.left),nil))));
           newblock:=ccallnode.createintern('fpc_finalize_array',npara);
           destppn.left:=nil;
           ppn.left:=nil;
         end
        else
         begin
           { create call to fpc_finalize }
           npara:=ccallparanode.create(caddrnode.create
                     (crttinode.create(tstoreddef(ppn.left.resulttype.def),initrtti)),
                  ccallparanode.create(caddrnode.create
                     (ppn.left),nil));
           newblock:=ccallnode.createintern('fpc_finalize',npara);
           ppn.left:=nil;
         end;
        paras.free;
        result.free;
        result:=newblock;
      end;

end.
{
  $Log$
  Revision 1.1  2002-04-23 19:16:35  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

}
