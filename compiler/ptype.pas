{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Does parsing types for Free Pascal

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
unit ptype;

{$i defines.inc}

interface

    uses
       globtype,symtype;

    const
       { forward types should only be possible inside a TYPE statement }
       typecanbeforward : boolean = false;

    var
       { hack, which allows to use the current parsed }
       { object type as function argument type  }
       testcurobject : byte;
       curobjectname : stringid;

    { reads a string, file type or a type id and returns a name and }
    { pdef }
    procedure single_type(var tt:ttype;var s : string;isforwarddef:boolean);

    procedure read_type(var tt:ttype;const name : stringid);

    { reads a type definition }
    { to a appropriating pdef, s gets the name of   }
    { the type to allow name mangling          }
    procedure id_type(var tt : ttype;var s : string;isforwarddef:boolean);


implementation

    uses
       { common }
       cutils,cobjects,
       { global }
       globals,tokens,verbose,
       systems,cpuinfo,
       { aasm }
       aasm,
       { symtable }
       symconst,symbase,symdef,symsym,symtable,types,
{$ifdef GDB}
       gdb,
{$endif}
       { pass 1 }
       node,pass_1,
       nmat,nadd,ncal,nmem,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,pdecl,pdecsub,pdecvar,pdecobj,
       { codegen }
{$ifdef newcg}
       cgbase,
{$else}
       hcodegen,
{$endif}
       hcgdata
       ;


    procedure id_type(var tt : ttype;var s : string;isforwarddef:boolean);
    { reads a type definition }
    { to a appropriating pdef, s gets the name of   }
    { the type to allow name mangling          }
      var
        is_unit_specific : boolean;
        pos : tfileposinfo;
      begin
         s:=pattern;
         pos:=akttokenpos;
         { classes can be used also in classes }
         if (curobjectname=pattern) and is_class_or_interface(aktobjectdef) then
           begin
              tt.setdef(aktobjectdef);
              consume(_ID);
              exit;
           end;
         { objects can be parameters }
         if (testcurobject=2) and (curobjectname=pattern) then
           begin
              tt.setdef(aktobjectdef);
              consume(_ID);
              exit;
           end;
         { try to load the symbol to see if it's a unitsym }
         is_unit_specific:=false;
         getsym(s,false);
         consume(_ID);
         if assigned(srsym) and
            (srsym^.typ=unitsym) then
           begin
              consume(_POINT);
              getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
              pos:=akttokenpos;
              s:=pattern;
              consume(_ID);
              is_unit_specific:=true;
           end;
         { are we parsing a possible forward def ? }
         if isforwarddef and
            not(is_unit_specific) then
          begin
            tt.setdef(new(pforwarddef,init(s,pos)));
            exit;
          end;
         { unknown sym ? }
         if not assigned(srsym) then
          begin
            Message1(sym_e_id_not_found,s);
            tt.setdef(generrordef);
            exit;
          end;
         { type sym ? }
         if (srsym^.typ<>typesym) then
          begin
            Message(type_e_type_id_expected);
            tt.setdef(generrordef);
            exit;
          end;
         { Types are first defined with an error def before assigning
           the real type so check if it's an errordef. if so then
           give an error }
         if (ptypesym(srsym)^.restype.def=generrordef) then
          begin
            Message(sym_e_error_in_type_def);
            tt.setdef(generrordef);
            exit;
          end;
         { Only use the definitions for system/current unit, becuase
           they can be refered from the parameters and symbols are not
           loaded at that time. A symbol reference to an other unit
           is still possible, because it's already loaded (PFV)
           can't use in [] here, becuase unitid can be > 255 }
         if (ptypesym(srsym)^.owner^.unitid=0) or
            (ptypesym(srsym)^.owner^.unitid=1) then
          tt.setdef(ptypesym(srsym)^.restype.def)
         else
          tt.setsym(srsym);
      end;


    procedure single_type(var tt:ttype;var s : string;isforwarddef:boolean);
    { reads a string, file type or a type id and returns a name and }
    { pdef                                                        }
       var
          hs : string;
          t2 : ttype;
       begin
          case token of
            _STRING:
                begin
                   tt.setdef(string_dec);
                   s:='STRING';
                end;
            _FILE:
                begin
                   consume(_FILE);
                   if token=_OF then
                     begin
                        consume(_OF);
                        single_type(t2,hs,false);
                        tt.setdef(new(pfiledef,inittyped(t2)));
                        s:='FILE$OF$'+hs;
                     end
                   else
                     begin
                        tt.setdef(cfiledef);
                        s:='FILE';
                     end;
                end;
            else
              begin
                id_type(tt,s,isforwarddef);
              end;
         end;
      end;

    { reads a record declaration }
    function record_dec : pdef;

      var
         symtable : psymtable;
         storetypecanbeforward : boolean;

      begin
         { create recdef }
         symtable:=new(pstoredsymtable,init(recordsymtable));
         record_dec:=new(precorddef,init(symtable));
         { update symtable stack }
         symtable^.next:=symtablestack;
         symtablestack:=symtable;
         { parse record }
         consume(_RECORD);
         storetypecanbeforward:=typecanbeforward;
         { for tp mode don't allow forward types }
         if m_tp in aktmodeswitches then
           typecanbeforward:=false;
         read_var_decs(true,false,false);
         consume(_END);
         typecanbeforward:=storetypecanbeforward;
         { may be scale record size to a size of n*4 ? }
         symtablestack^.datasize:=align(symtablestack^.datasize,symtablestack^.dataalignment);
         { restore symtable stack }
         symtablestack:=symtable^.next;
      end;


    { reads a type definition and returns a pointer to it }
    procedure read_type(var tt : ttype;const name : stringid);
      var
        pt : tnode;
        tt2 : ttype;
        aktenumdef : penumdef;
        ap : parraydef;
        s : stringid;
        l,v : longint;
        oldaktpackrecords : tpackrecords;
        hs : string;
        defpos,storepos : tfileposinfo;

        procedure expr_type;
        var
           pt1,pt2 : tnode;
        begin
           { use of current parsed object ? }
           if (token=_ID) and (testcurobject=2) and (curobjectname=pattern) then
             begin
                consume(_ID);
                tt.setdef(aktobjectdef);
                exit;
             end;
           { classes can be used also in classes }
           if (curobjectname=pattern) and is_class_or_interface(aktobjectdef) then
             begin
                tt.setdef(aktobjectdef);
                consume(_ID);
                exit;
             end;
           { we can't accept a equal in type }
           pt1:=comp_expr(not(ignore_equal));
           do_firstpass(pt1);
           if (token=_POINTPOINT) then
             begin
               consume(_POINTPOINT);
               { get high value of range }
               pt2:=comp_expr(not(ignore_equal));
               do_firstpass(pt2);
               { both must be evaluated to constants now }
               if (pt1.nodetype=ordconstn) and
                  (pt2.nodetype=ordconstn) then
                 begin
                 { check types }
                   if CheckTypes(pt1.resulttype,pt2.resulttype) then
                     begin
                     { Check bounds }
                       if tordconstnode(pt2).value<tordconstnode(pt1).value then
                         Message(cg_e_upper_lower_than_lower)
                       else
                        begin
                        { All checks passed, create the new def }
                          case pt1.resulttype^.deftype of
                            enumdef :
                              tt.setdef(new(penumdef,init_subrange(penumdef(pt1.resulttype),tordconstnode(pt1).value,tordconstnode(pt2).value)));
                            orddef :
                              begin
                                if is_char(pt1.resulttype) then
                                  tt.setdef(new(porddef,init(uchar,tordconstnode(pt1).value,tordconstnode(pt2).value)))
                                else
                                  if is_boolean(pt1.resulttype) then
                                    tt.setdef(new(porddef,init(bool8bit,tordconstnode(pt1).value,tordconstnode(pt2).value)))
                                  else
                                    tt.setdef(new(porddef,init(uauto,tordconstnode(pt1).value,tordconstnode(pt2).value)));
                              end;
                          end;
                        end;
                     end;
                 end
               else
                 Message(sym_e_error_in_type_def);
               pt2.free;
             end
           else
             begin
               { a simple type renaming }
               if (pt1.nodetype=typen) then
                 begin
                   if assigned(ttypenode(pt1).typenodesym) then
                     tt.setsym(ttypenode(pt1).typenodesym)
                   else
                     tt.setdef(pt1.resulttype);
                 end
               else
                 Message(sym_e_error_in_type_def);
             end;
           pt1.free;
        end;

        procedure array_dec;
        var
          lowval,
          highval   : longint;
          arraytype : pdef;
          ht        : ttype;

          procedure setdefdecl(p:pdef);
          begin
            case p^.deftype of
              enumdef :
                begin
                  lowval:=penumdef(p)^.min;
                  highval:=penumdef(p)^.max;
                  arraytype:=p;
                end;
              orddef :
                begin
                  if porddef(p)^.typ in [uchar,
                    u8bit,u16bit,
                    s8bit,s16bit,s32bit,
                    bool8bit,bool16bit,bool32bit,
                    uwidechar] then
                    begin
                       lowval:=porddef(p)^.low;
                       highval:=porddef(p)^.high;
                       arraytype:=p;
                    end
                  else
                    Message1(parser_e_type_cant_be_used_in_array_index,p^.gettypename);
                end;
              else
                Message(sym_e_error_in_type_def);
            end;
          end;

        begin
           consume(_ARRAY);
           { open array? }
           if token=_LECKKLAMMER then
             begin
                consume(_LECKKLAMMER);
                { defaults }
                arraytype:=generrordef;
                lowval:=$80000000;
                highval:=$7fffffff;
                tt.reset;
                repeat
                  { read the expression and check it, check apart if the
                    declaration is an enum declaration because that needs to
                    be parsed by readtype (PFV) }
                  if token=_LKLAMMER then
                   begin
                     read_type(ht,'');
                     setdefdecl(ht.def);
                   end
                  else
                   begin
                     pt:=expr;
                     if pt.nodetype=typen then
                      setdefdecl(pt.resulttype)
                     else
                       begin
                          do_firstpass(pt);
                          if (pt.nodetype=rangen) then
                           begin
                             if (trangenode(pt).left.nodetype=ordconstn) and
                                (trangenode(pt).right.nodetype=ordconstn) then
                              begin
                                lowval:=tordconstnode(trangenode(pt).left).value;
                                highval:=tordconstnode(trangenode(pt).right).value;
                                if highval<lowval then
                                 begin
                                   Message(parser_e_array_lower_less_than_upper_bound);
                                   highval:=lowval;
                                 end;
                                arraytype:=trangenode(pt).right.resulttype;
                              end
                             else
                              Message(type_e_cant_eval_constant_expr);
                           end
                          else
                           Message(sym_e_error_in_type_def)
                       end;
                     pt.free;
                   end;

                { create arraydef }
                  if not assigned(tt.def) then
                   begin
                     ap:=new(parraydef,init(lowval,highval,arraytype));
                     tt.setdef(ap);
                   end
                  else
                   begin
                     ap^.elementtype.setdef(new(parraydef,init(lowval,highval,arraytype)));
                     ap:=parraydef(ap^.elementtype.def);
                   end;

                  if token=_COMMA then
                    consume(_COMMA)
                  else
                    break;
                until false;
                consume(_RECKKLAMMER);
             end
           else
             begin
                ap:=new(parraydef,init(0,-1,s32bitdef));
                ap^.IsDynamicArray:=true;
                tt.setdef(ap);
             end;
           consume(_OF);
           read_type(tt2,'');
           { if no error, set element type }
           if assigned(ap) then
             ap^.elementtype:=tt2;
        end;

      begin
         tt.reset;
         case token of
            _STRING,_FILE:
              begin
                single_type(tt,hs,false);
              end;
           _LKLAMMER:
              begin
                consume(_LKLAMMER);
                { allow negativ value_str }
                l:=-1;
                aktenumdef:=new(penumdef,init);
                repeat
                  s:=orgpattern;
                  defpos:=akttokenpos;
                  consume(_ID);
                  { only allow assigning of specific numbers under fpc mode }
                  if (m_fpc in aktmodeswitches) and
                     (token=_ASSIGNMENT) then
                    begin
                       consume(_ASSIGNMENT);
                       v:=get_intconst;
                       { please leave that a note, allows type save }
                       { declarations in the win32 units !       }
                       if v<=l then
                        Message(parser_n_duplicate_enum);
                       l:=v;
                    end
                  else
                    inc(l);
                  storepos:=akttokenpos;
                  akttokenpos:=defpos;
                  constsymtable^.insert(new(penumsym,init(s,aktenumdef,l)));
                  akttokenpos:=storepos;
                until not try_to_consume(_COMMA);
                tt.setdef(aktenumdef);
                consume(_RKLAMMER);
              end;
            _ARRAY:
              begin
                array_dec;
              end;
            _SET:
              begin
                consume(_SET);
                consume(_OF);
                read_type(tt2,'');
                if assigned(tt2.def) then
                 begin
                   case tt2.def^.deftype of
                     { don't forget that min can be negativ  PM }
                     enumdef :
                       if penumdef(tt2.def)^.min>=0 then
                        tt.setdef(new(psetdef,init(tt2.def,penumdef(tt2.def)^.max)))
                       else
                        Message(sym_e_ill_type_decl_set);
                     orddef :
                       begin
                         case porddef(tt2.def)^.typ of
                           uchar :
                             tt.setdef(new(psetdef,init(tt2.def,255)));
                           u8bit,u16bit,u32bit,
                           s8bit,s16bit,s32bit :
                             begin
                               if (porddef(tt2.def)^.low>=0) then
                                tt.setdef(new(psetdef,init(tt2.def,porddef(tt2.def)^.high)))
                               else
                                Message(sym_e_ill_type_decl_set);
                             end;
                           else
                             Message(sym_e_ill_type_decl_set);
                         end;
                       end;
                     else
                       Message(sym_e_ill_type_decl_set);
                   end;
                 end
                else
                 tt.setdef(generrordef);
              end;
           _CARET:
              begin
                consume(_CARET);
                single_type(tt2,hs,typecanbeforward);
                tt.setdef(new(ppointerdef,init(tt2)));
              end;
            _RECORD:
              begin
                tt.setdef(record_dec);
              end;
            _PACKED:
              begin
                consume(_PACKED);
                if token=_ARRAY then
                  array_dec
                else
                  begin
                    oldaktpackrecords:=aktpackrecords;
                    aktpackrecords:=packrecord_1;
                    if token in [_CLASS,_OBJECT] then
                      tt.setdef(object_dec(name,nil))
                    else
                      tt.setdef(record_dec);
                    aktpackrecords:=oldaktpackrecords;
                  end;
              end;
            _CLASS,
            _CPPCLASS,
            _INTERFACE,
            _OBJECT:
              begin
                tt.setdef(object_dec(name,nil));
              end;
            _PROCEDURE:
              begin
                consume(_PROCEDURE);
                tt.setdef(new(pprocvardef,init));
                if token=_LKLAMMER then
                 parameter_dec(pprocvardef(tt.def));
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
                    include(pprocvardef(tt.def)^.procoptions,po_methodpointer);
                  end;
              end;
            _FUNCTION:
              begin
                consume(_FUNCTION);
                tt.def:=new(pprocvardef,init);
                if token=_LKLAMMER then
                 parameter_dec(pprocvardef(tt.def));
                consume(_COLON);
                single_type(pprocvardef(tt.def)^.rettype,hs,false);
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
                    include(pprocvardef(tt.def)^.procoptions,po_methodpointer);
                  end;
              end;
            else
              expr_type;
         end;
         if tt.def=nil then
          tt.setdef(generrordef);
      end;

end.
{
  $Log$
  Revision 1.15  2000-11-14 23:43:38  florian
    * fixed 1238

  Revision 1.14  2000/11/04 14:25:21  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.13  2000/10/31 22:02:51  peter
    * symtable splitted, no real code changes

  Revision 1.12  2000/10/26 21:54:03  peter
    * fixed crash with error in child definition (merged)

  Revision 1.11  2000/10/21 18:16:12  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.10  2000/10/14 10:14:52  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.9  2000/09/24 15:06:25  peter
    * use defines.inc

  Revision 1.8  2000/08/27 20:19:39  peter
    * store strings with case in ppu, when an internal symbol is created
      a '$' is prefixed so it's not automatic uppercased

  Revision 1.7  2000/08/27 16:11:52  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.6  2000/08/16 18:33:54  peter
    * splitted namedobjectitem.next into indexnext and listnext so it
      can be used in both lists
    * don't allow "word = word" type definitions (merged)

  Revision 1.5  2000/08/06 14:17:15  peter
    * overload fixes (merged)

  Revision 1.4  2000/07/30 17:04:43  peter
    * merged fixes

  Revision 1.3  2000/07/13 12:08:27  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:47  michael
  + removed logs

}