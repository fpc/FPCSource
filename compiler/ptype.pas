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
    { tdef }
    procedure single_type(var tt:ttype;var s : string;isforwarddef:boolean);

    procedure read_type(var tt:ttype;const name : stringid);

    { reads a type definition }
    { to a appropriating tdef, s gets the name of   }
    { the type to allow name mangling          }
    procedure id_type(var tt : ttype;var s : string;isforwarddef:boolean);


implementation

    uses
       { common }
       cutils,cpuinfo,
       { global }
       globals,tokens,verbose,
       systems,
       { aasm }
       aasm,
       { symtable }
       symconst,symbase,symdef,symsym,symtable,types,
       { pass 1 }
       node,pass_1,
       nmat,nadd,ncal,nset,ncnv,ninl,ncon,nld,nflw,
       { parser }
       scanner,
       pbase,pexpr,pdecsub,pdecvar,pdecobj;


    procedure id_type(var tt : ttype;var s : string;isforwarddef:boolean);
    { reads a type definition }
    { to a appropriating tdef, s gets the name of   }
    { the type to allow name mangling          }
      var
        is_unit_specific : boolean;
        pos : tfileposinfo;
        srsym : tsym;
        srsymtable : tsymtable;
        sorg : stringid;
      begin
         s:=pattern;
         sorg:=orgpattern;
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
         searchsym(s,srsym,srsymtable);
         consume(_ID);
         if assigned(srsym) and
            (srsym.typ=unitsym) then
           begin
              is_unit_specific:=true;
              consume(_POINT);
              if srsym.owner.unitid=0 then
               begin
                 srsym:=searchsymonlyin(tunitsym(srsym).unitsymtable,pattern);
                 pos:=akttokenpos;
                 s:=pattern;
               end
              else
               srsym:=nil;
              consume(_ID);
           end;
         { are we parsing a possible forward def ? }
         if isforwarddef and
            not(is_unit_specific) then
          begin
            tt.setdef(tforwarddef.create(s,pos));
            exit;
          end;
         { unknown sym ? }
         if not assigned(srsym) then
          begin
            Message1(sym_e_id_not_found,sorg);
            tt:=generrortype;
            exit;
          end;
         { type sym ? }
         if (srsym.typ<>typesym) then
          begin
            Message(type_e_type_id_expected);
            tt:=generrortype;
            exit;
          end;
         { Types are first defined with an error def before assigning
           the real type so check if it's an errordef. if so then
           give an error }
         if (ttypesym(srsym).restype.def.deftype=errordef) then
          begin
            Message(sym_e_error_in_type_def);
            tt:=generrortype;
            exit;
          end;
         { Only use the definitions for system/current unit, becuase
           they can be refered from the parameters and symbols are not
           loaded at that time. A symbol reference to an other unit
           is still possible, because it's already loaded (PFV)
           can't use in [] here, becuase unitid can be > 255 }
         if (ttypesym(srsym).owner.unitid=0) or
            (ttypesym(srsym).owner.unitid=1) then
          tt.setdef(ttypesym(srsym).restype.def)
         else
          tt.setsym(srsym);
      end;


    procedure single_type(var tt:ttype;var s : string;isforwarddef:boolean);
    { reads a string, file type or a type id and returns a name and }
    { tdef                                                        }
       var
          hs : string;
          t2 : ttype;
       begin
          case token of
            _STRING:
                begin
                   string_dec(tt);
                   s:='STRING';
                end;
            _FILE:
                begin
                   consume(_FILE);
                   if token=_OF then
                     begin
                        consume(_OF);
                        single_type(t2,hs,false);
                        tt.setdef(tfiledef.createtyped(t2));
                        s:='FILE$OF$'+hs;
                     end
                   else
                     begin
                        tt:=cfiletype;
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
    function record_dec : tdef;

      var
         symtable : tsymtable;
         storetypecanbeforward : boolean;
         old_object_option : tsymoptions;
      begin
         { create recdef }
         symtable:=trecordsymtable.create;
         record_dec:=trecorddef.create(symtable);
         { update symtable stack }
         symtable.next:=symtablestack;
         symtablestack:=symtable;
         { parse record }
         consume(_RECORD);
         old_object_option:=current_object_option;
         current_object_option:=[sp_public];
         storetypecanbeforward:=typecanbeforward;
         { for tp mode don't allow forward types }
         if m_tp in aktmodeswitches then
           typecanbeforward:=false;
         read_var_decs(true,false,false);
         consume(_END);
         typecanbeforward:=storetypecanbeforward;
         current_object_option:=old_object_option;
         { may be scale record size to a size of n*4 ? }
         symtablestack.datasize:=align(symtablestack.datasize,symtablestack.dataalignment);
         { restore symtable stack }
         symtablestack:=symtable.next;
      end;


    { reads a type definition and returns a pointer to it }
    procedure read_type(var tt : ttype;const name : stringid);
      var
        pt : tnode;
        tt2 : ttype;
        aktenumdef : tenumdef;
        ap : tarraydef;
        s : stringid;
        l,v : TConstExprInt;
        oldaktpackrecords : longint;
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
           if (token=_POINTPOINT) then
             begin
               consume(_POINTPOINT);
               { get high value of range }
               pt2:=comp_expr(not(ignore_equal));
               { make both the same type }
               inserttypeconv(pt1,pt2.resulttype);
               { both must be evaluated to constants now }
               if (pt1.nodetype=ordconstn) and
                  (pt2.nodetype=ordconstn) then
                 begin
                   { Check bounds }
                   if tordconstnode(pt2).value<tordconstnode(pt1).value then
                     Message(cg_e_upper_lower_than_lower)
                   else
                     begin
                       { All checks passed, create the new def }
                       case pt1.resulttype.def.deftype of
                         enumdef :
                           tt.setdef(tenumdef.create_subrange(tenumdef(pt1.resulttype.def),tordconstnode(pt1).value,tordconstnode(pt2).value));
                         orddef :
                           begin
                             if is_char(pt1.resulttype.def) then
                               tt.setdef(torddef.create(uchar,tordconstnode(pt1).value,tordconstnode(pt2).value))
                             else
                               if is_boolean(pt1.resulttype.def) then
                                 tt.setdef(torddef.create(bool8bit,tordconstnode(pt1).value,tordconstnode(pt2).value))
                               else
                                 tt.setdef(torddef.create(uauto,tordconstnode(pt1).value,tordconstnode(pt2).value));
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
                 tt:=ttypenode(pt1).resulttype
               else
                 Message(sym_e_error_in_type_def);
             end;
           pt1.free;
        end;

        procedure array_dec;
        var
          lowval,
          highval   : longint;
          arraytype : ttype;
          ht        : ttype;

          procedure setdefdecl(const t:ttype);
          begin
            case t.def.deftype of
              enumdef :
                begin
                  lowval:=tenumdef(t.def).min;
                  highval:=tenumdef(t.def).max;
                  arraytype:=t;
                end;
              orddef :
                begin
                  if torddef(t.def).typ in [uchar,
                    u8bit,u16bit,
                    s8bit,s16bit,s32bit,
                    bool8bit,bool16bit,bool32bit,
                    uwidechar] then
                    begin
                       lowval:=torddef(t.def).low;
                       highval:=torddef(t.def).high;
                       arraytype:=t;
                    end
                  else
                    Message1(parser_e_type_cant_be_used_in_array_index,t.def.gettypename);
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
                arraytype:=generrortype;
                lowval:=longint($80000000);
                highval:=$7fffffff;
                tt.reset;
                repeat
                  { read the expression and check it, check apart if the
                    declaration is an enum declaration because that needs to
                    be parsed by readtype (PFV) }
                  if token=_LKLAMMER then
                   begin
                     read_type(ht,'');
                     setdefdecl(ht);
                   end
                  else
                   begin
                     pt:=expr;
                     if pt.nodetype=typen then
                      setdefdecl(pt.resulttype)
                     else
                       begin
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
                     ap:=tarraydef.create(lowval,highval,arraytype);
                     tt.setdef(ap);
                   end
                  else
                   begin
                     ap.elementtype.setdef(tarraydef.create(lowval,highval,arraytype));
                     ap:=tarraydef(ap.elementtype.def);
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
                ap:=tarraydef.create(0,-1,s32bittype);
                ap.IsDynamicArray:=true;
                tt.setdef(ap);
             end;
           consume(_OF);
           read_type(tt2,'');
           { if no error, set element type }
           if assigned(ap) then
             ap.elementtype:=tt2;
        end;

      var
        p : tnode;
        enumdupmsg : boolean;
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
                enumdupmsg:=false;
                aktenumdef:=tenumdef.create;
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
                       { declarations in the win32 units ! }
                       if (v<=l) and (not enumdupmsg) then
                        begin
                          Message(parser_n_duplicate_enum);
                          enumdupmsg:=true;
                        end;
                       l:=v;
                    end
                  else if (m_delphi in aktmodeswitches) and
                     (token=_EQUAL) then
                    begin
                       consume(_EQUAL);
                       p:=comp_expr(true);
                       if (p.nodetype=ordconstn) then
                        begin
                          { we expect an integer or an enum of the
                            same type }
                          if is_integer(p.resulttype.def) or
                             is_equal(p.resulttype.def,aktenumdef) then
                           l:=tordconstnode(p).value
                          else
                           Message2(type_e_incompatible_types,p.resulttype.def.typename,s32bittype.def.typename);
                        end
                       else
                        Message(cg_e_illegal_expression);
                       p.free;
                    end
                  else
                    inc(l);
                  storepos:=akttokenpos;
                  akttokenpos:=defpos;
                  constsymtable.insert(tenumsym.create(s,aktenumdef,l));
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
                   case tt2.def.deftype of
                     { don't forget that min can be negativ  PM }
                     enumdef :
                       if tenumdef(tt2.def).min>=0 then
                        tt.setdef(tsetdef.create(tt2,tenumdef(tt2.def).max))
                       else
                        Message(sym_e_ill_type_decl_set);
                     orddef :
                       begin
                         case torddef(tt2.def).typ of
                           uchar :
                             tt.setdef(tsetdef.create(tt2,255));
                           u8bit,u16bit,u32bit,
                           s8bit,s16bit,s32bit :
                             begin
                               if (torddef(tt2.def).low>=0) then
                                tt.setdef(tsetdef.create(tt2,torddef(tt2.def).high))
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
                 tt:=generrortype;
              end;
           _CARET:
              begin
                consume(_CARET);
                single_type(tt2,hs,typecanbeforward);
                tt.setdef(tpointerdef.create(tt2));
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
                    oldaktpackrecords:=aktalignment.recordalignmax;
                    aktalignment.recordalignmax:=1;
                    if token in [_CLASS,_OBJECT] then
                      tt.setdef(object_dec(name,nil))
                    else
                      tt.setdef(record_dec);
                    aktalignment.recordalignmax:=oldaktpackrecords;
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
                tt.setdef(tprocvardef.create);
                if token=_LKLAMMER then
                 parameter_dec(tprocvardef(tt.def));
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
                    include(tprocvardef(tt.def).procoptions,po_methodpointer);
                  end;
              end;
            _FUNCTION:
              begin
                consume(_FUNCTION);
                tt.def:=tprocvardef.create;
                if token=_LKLAMMER then
                 parameter_dec(tprocvardef(tt.def));
                consume(_COLON);
                single_type(tprocvardef(tt.def).rettype,hs,false);
                if token=_OF then
                  begin
                    consume(_OF);
                    consume(_OBJECT);
                    include(tprocvardef(tt.def).procoptions,po_methodpointer);
                  end;
              end;
            else
              expr_type;
         end;
         if tt.def=nil then
          tt:=generrortype;
      end;

end.
{
  $Log$
  Revision 1.31  2001-12-31 16:59:43  peter
    * protected/private symbols parsing fixed

  Revision 1.30  2001/08/30 20:13:53  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.29  2001/08/12 22:10:16  peter
    * write name in original case when type not found

  Revision 1.28  2001/07/09 21:15:41  peter
    * Length made internal
    * Add array support for Length

  Revision 1.27  2001/07/01 20:16:16  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.26  2001/06/04 18:06:38  peter
    * fix for enum with assignment

  Revision 1.25  2001/06/04 11:51:59  peter
    * enum type declarations assignments can also be of the same enum
      type

  Revision 1.24  2001/06/03 20:16:19  peter
    * allow int64 in range declaration for new types

  Revision 1.23  2001/04/13 01:22:13  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.22  2001/04/04 22:43:53  peter
    * remove unnecessary calls to firstpass

  Revision 1.21  2001/04/02 21:20:34  peter
    * resulttype rewrite

  Revision 1.20  2001/03/22 22:35:42  florian
    + support for type a = (a=1); in Delphi mode added
    + procedure p(); in Delphi mode supported
    + on isn't keyword anymore, it can be used as
      id etc. now

  Revision 1.19  2001/03/12 12:49:01  michael
  + Patches from peter

  Revision 1.18  2001/03/11 22:58:50  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.17  2000/12/07 17:19:43  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.16  2000/11/29 00:30:38  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.15  2000/11/14 23:43:38  florian
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
