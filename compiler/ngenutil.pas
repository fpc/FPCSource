{
    Copyright (c) 1998-20011 by Florian Klaempfl

    Generic version of some node tree helper routines that can be overridden
    by cpu-specific versions

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
unit ngenutil;

{$i fpcdefs.inc}

interface

  uses
    node;


  type
    tnodeutils = class
      class function call_fail_node:tnode; virtual;
      class function initialize_data_node(p:tnode):tnode; virtual;
      class function finalize_data_node(p:tnode):tnode; virtual;
    end;
    tnodeutilsclass = class of tnodeutils;

  const
    cnodeutils: tnodeutilsclass = tnodeutils;


implementation

    uses
      verbose,constexp,
      symconst,symtype,symdef,symsym,symbase,symtable,defutil,
      nadd,nbas,ncal,ncnv,ncon,nflw,nld,nmem,nobj,nutils,
      pass_1;

  class function tnodeutils.call_fail_node:tnode;
    var
      para : tcallparanode;
      newstatement : tstatementnode;
      srsym : tsym;
    begin
      result:=internalstatements(newstatement);

      { call fail helper and exit normal }
      if is_class(current_structdef) then
        begin
          srsym:=search_struct_member(current_structdef,'FREEINSTANCE');
          if assigned(srsym) and
             (srsym.typ=procsym) then
            begin
              { if self<>0 and vmt<>0 then freeinstance }
              addstatement(newstatement,cifnode.create(
                  caddnode.create(andn,
                      caddnode.create(unequaln,
                          load_self_pointer_node,
                          cnilnode.create),
                      caddnode.create(unequaln,
                          load_vmt_pointer_node,
                          cnilnode.create)),
                  ccallnode.create(nil,tprocsym(srsym),srsym.owner,load_self_node,[]),
                  nil));
            end
          else
            internalerror(200305108);
        end
      else
        if is_object(current_structdef) then
          begin
            { parameter 3 : vmt_offset }
            { parameter 2 : pointer to vmt }
            { parameter 1 : self pointer }
            para:=ccallparanode.create(
                      cordconstnode.create(tobjectdef(current_structdef).vmt_offset,s32inttype,false),
                  ccallparanode.create(
                      ctypeconvnode.create_internal(
                          load_vmt_pointer_node,
                          voidpointertype),
                  ccallparanode.create(
                      ctypeconvnode.create_internal(
                          load_self_pointer_node,
                          voidpointertype),
                  nil)));
            addstatement(newstatement,
                ccallnode.createintern('fpc_help_fail',para));
          end
      else
        internalerror(200305132);
      { self:=nil }
      addstatement(newstatement,cassignmentnode.create(
          load_self_pointer_node,
          cnilnode.create));
      { exit }
      addstatement(newstatement,cexitnode.create(nil));
    end;


  class function tnodeutils.initialize_data_node(p:tnode):tnode;
    begin
      if not assigned(p.resultdef) then
        typecheckpass(p);
      if is_ansistring(p.resultdef) or
         is_wide_or_unicode_string(p.resultdef) or
         is_interfacecom_or_dispinterface(p.resultdef) or
         is_dynamic_array(p.resultdef) then
        begin
          result:=cassignmentnode.create(
             ctypeconvnode.create_internal(p,voidpointertype),
             cnilnode.create
             );
        end
      else
        begin
          result:=ccallnode.createintern('fpc_initialize',
                ccallparanode.create(
                    caddrnode.create_internal(
                        crttinode.create(
                            tstoreddef(p.resultdef),initrtti,rdt_normal)),
                ccallparanode.create(
                    caddrnode.create_internal(p),
                nil)));
        end;
    end;


  class function tnodeutils.finalize_data_node(p:tnode):tnode;
    var
      newstatement : tstatementnode;
    begin
      if not assigned(p.resultdef) then
        typecheckpass(p);
      if is_ansistring(p.resultdef) then
        begin
          result:=internalstatements(newstatement);
          addstatement(newstatement,ccallnode.createintern('fpc_ansistr_decr_ref',
                ccallparanode.create(
                  ctypeconvnode.create_internal(p,voidpointertype),
                nil)));
          addstatement(newstatement,cassignmentnode.create(
             ctypeconvnode.create_internal(p.getcopy,voidpointertype),
             cnilnode.create
             ));
        end
      else if is_widestring(p.resultdef) then
        begin
          result:=internalstatements(newstatement);
          addstatement(newstatement,ccallnode.createintern('fpc_widestr_decr_ref',
                ccallparanode.create(
                  ctypeconvnode.create_internal(p,voidpointertype),
                nil)));
          addstatement(newstatement,cassignmentnode.create(
             ctypeconvnode.create_internal(p.getcopy,voidpointertype),
             cnilnode.create
             ));
        end
      else if is_unicodestring(p.resultdef) then
        begin
          result:=internalstatements(newstatement);
          addstatement(newstatement,ccallnode.createintern('fpc_unicodestr_decr_ref',
                ccallparanode.create(
                  ctypeconvnode.create_internal(p,voidpointertype),
                nil)));
          addstatement(newstatement,cassignmentnode.create(
             ctypeconvnode.create_internal(p.getcopy,voidpointertype),
             cnilnode.create
             ));
        end
      else if is_interfacecom_or_dispinterface(p.resultdef) then
        begin
          result:=internalstatements(newstatement);
          addstatement(newstatement,ccallnode.createintern('fpc_intf_decr_ref',
                ccallparanode.create(
                  ctypeconvnode.create_internal(p,voidpointertype),
                nil)));
          addstatement(newstatement,cassignmentnode.create(
             ctypeconvnode.create_internal(p.getcopy,voidpointertype),
             cnilnode.create
             ));
        end
      else
        result:=ccallnode.createintern('fpc_finalize',
              ccallparanode.create(
                  caddrnode.create_internal(
                      crttinode.create(
                          tstoreddef(p.resultdef),initrtti,rdt_normal)),
              ccallparanode.create(
                  caddrnode.create_internal(p),
              nil)));
    end;


end.
