{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    Reads typed constants

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
unit ptconst;

  interface

   uses symtable;

    { this procedure reads typed constants }
    { sym is only needed for ansi strings  }
    { the assembler label is in the middle (PM) }
    procedure readtypedconst(def : pdef;sym : ptypedconstsym);

  implementation

    uses
       cobjects,globals,scanner,aasm,tree,pass_1,
       hcodegen,types,verbose
       { parser specific stuff }
       ,pbase,pexpr
       { processor specific stuff }
{$ifdef i386}
       ,i386
{$endif}
{$ifdef m68k}
       ,m68k
{$endif}
       ;

    { this procedure reads typed constants }
    procedure readtypedconst(def : pdef;sym : ptypedconstsym);

      var
         p : ptree;
         i,l,strlength : longint;
         ll : plabel;
         s : string;
         ca : pchar;
         aktpos : longint;
         pd : pprocdef;
         hp1,hp2 : pdefcoll;

         value : bestreal;
         {problem with fldt !!
         anyway .valued is not extended !!
         value : double; }

      procedure check_range;

        begin
           if ((p^.value>porddef(def)^.high) or
               (p^.value<porddef(def)^.low)) then
             Message(parser_e_range_check_error);
        end;

{$R-}  {Range check creates problem with init_8bit(-1) !!}
      begin
         case def^.deftype of
            orddef:
              begin
                 p:=comp_expr(true);
                 do_firstpass(p);
                 case porddef(def)^.typ of
                    s8bit,
                    u8bit : begin
                               if not is_constintnode(p) then
                               { is't an int expected }
                                 Message(cg_e_illegal_expression)
                               else
                                 begin
                                    datasegment^.concat(new(pai_const,init_8bit(p^.value)));
                                    check_range;
                                 end;
                            end;
                    s32bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression)
                                else
                                  begin
                                     datasegment^.concat(new(pai_const,init_32bit(p^.value)));
                                     check_range;
                                  end;
                            end;
                    u32bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression)
                                else
                                   datasegment^.concat(new(pai_const,init_32bit(p^.value)));
                             end;
                    bool8bit : begin
                                  if not is_constboolnode(p) then
                                    Message(cg_e_illegal_expression);
                                  datasegment^.concat(new(pai_const,init_8bit(p^.value)));
                               end;
                    uchar : begin
                                if not is_constcharnode(p) then
                                  Message(cg_e_illegal_expression);
                                datasegment^.concat(new(pai_const,init_8bit(p^.value)));
                            end;
                    u16bit,
                    s16bit : begin
                                if not is_constintnode(p) then
                                  Message(cg_e_illegal_expression);
                                datasegment^.concat(new(pai_const,init_16bit(p^.value)));
                                check_range;
                            end;
                 end;
                 disposetree(p);
              end;
         floatdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if is_constrealnode(p) then
                value:=p^.valued
              else if is_constintnode(p) then
                value:=p^.value
              else
                Message(cg_e_illegal_expression);

              case pfloatdef(def)^.typ of
                 s64real : datasegment^.concat(new(pai_double,init(value)));
                 s32real : datasegment^.concat(new(pai_single,init(value)));
                 s80real : datasegment^.concat(new(pai_extended,init(value)));
                 s64bit  : datasegment^.concat(new(pai_comp,init(value)));
                 f32bit : datasegment^.concat(new(pai_const,init_32bit(trunc(value*65536))));
              else internalerror(18);
              end;
              disposetree(p);
           end;
         pointerdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              { nil pointer ? }
              if p^.treetype=niln then
                datasegment^.concat(new(pai_const,init_32bit(0)))
              { maybe pchar ? }
              else if (ppointerdef(def)^.definition^.deftype=orddef) and
                   (porddef(ppointerdef(def)^.definition)^.typ=uchar) then
                begin
                   getlabel(ll);
                   { insert string at the begin }
                   if p^.treetype=stringconstn then
                     generate_ascii_insert((p^.values^)+#0)
                   else if is_constcharnode(p) then
                     datasegment^.insert(new(pai_string,init(char(byte(p^.value))+#0)))
                   else Message(cg_e_illegal_expression);
                   datasegment^.insert(new(pai_label,init(ll)));
                   { insert label }
                   datasegment^.concat(new(pai_const,init_symbol(strpnew(lab2str(ll)))));
                end
              else if p^.treetype=addrn then
                begin
                   if (is_equal(ppointerdef(p^.resulttype)^.definition,ppointerdef(def)^.definition) or
                      (is_equal(ppointerdef(p^.resulttype)^.definition,voiddef)) or
                      (is_equal(ppointerdef(def)^.definition,voiddef))) and
                      (p^.left^.treetype = loadn) then
                     begin
                        datasegment^.concat(new(pai_const,init_symbol(
                          strpnew(p^.left^.symtableentry^.mangledname))));
                        maybe_concat_external(p^.left^.symtableentry^.owner,
                          p^.left^.symtableentry^.mangledname);
                     end
                   else
                     Message(cg_e_illegal_expression);
                end
              else
              { allow typeof(Object type)}
                if (p^.treetype=inlinen) and
                   (p^.inlinenumber=in_typeof_x) then
                  if (p^.left^.treetype=typen) then
                    begin
                       datasegment^.concat(new(pai_const,init_symbol(
                         strpnew(pobjectdef(p^.left^.resulttype)^.vmt_mangledname))));
                       if pobjectdef(p^.left^.resulttype)^.owner^.symtabletype=unitsymtable then
                          concat_external(pobjectdef(p^.left^.resulttype)^.vmt_mangledname,EXT_NEAR);
                    end
                  else
                    begin
                       Message(cg_e_illegal_expression);
                    end
                else
                  Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         setdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              if p^.treetype=setconstrn then
                begin
                   { we only allow const sets }
                   if assigned(p^.left) then
                     Message(cg_e_illegal_expression)
                   else
                     begin
                        for l:=0 to def^.savesize-1 do
                          datasegment^.concat(
                        new(pai_const,init_8bit(p^.constset^[l])));
                     end;
                end
              else
                Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         enumdef:
       begin
              p:=comp_expr(true);
              do_firstpass(p);
              if p^.treetype=ordconstn then
                begin
                   if is_equal(p^.resulttype,def) then
                     begin
                        datasegment^.concat(new(pai_const,init_32bit(p^.value)));
                     end
                   else
                     Message(cg_e_illegal_expression);
                end
              else
                Message(cg_e_illegal_expression);
              disposetree(p);
           end;
         stringdef:
           begin
              p:=comp_expr(true);
              do_firstpass(p);
              { first take care of prefixes for long and ansi strings }
{$ifdef UseLongString}
              if pstringdef(def)^.string_typ=longstring then
                begin
                  { first write the maximum size }
                  datasegment^.concat(new(pai_const,init_32bit(p^.length)))));
                end else
{$endif UseLongString}
{$ifdef UseAnsiString}
              if pstringdef(def)^.string_typ=ansistring then
                begin
{$ifdef debug}
                  datasegment^.concat(new(pai_asm_comment,init('Header of ansistring')));
{$endif debug}
                  { first write the maximum size }
                  datasegment^.concat(new(pai_const,init_32bit(pstringdef(def)^.len)));
                  { second write the real length }
                  datasegment^.concat(new(pai_const,init_32bit(p^.length)));
                  { redondent with maxlength but who knows ... (PM) }
                  { third write use count (set to -1 for safety ) }
                  datasegment^.concat(new(pai_const,init_32bit(-1)));
                  if assigned(sym) then
                    sym^.really_insert_in_data;
                end;
{$endif UseAnsiString}
              { the actual write is independent of the string_typ }
              if p^.treetype=stringconstn then
                begin
{$ifdef UseAnsiString}
                   if p^.length>=def^.size then
                     strlength:=def^.size-1
                   else
                     strlength:=p^.length;
                   { this can also handle longer strings }
                   generate_pascii(p^.values,strlength);
{$else UseAnsiString}
                   if length(p^.values^)>=def^.size then
                     strlength:=def^.size-1
                   else
                     strlength:=length(p^.values^);
                   generate_ascii(char(strlength)+p^.values^);
{$endif UseAnsiString}
                end
              else if is_constcharnode(p) then
                begin
                   datasegment^.concat(new(pai_string,init(#1+char(byte(p^.value)))));
                   strlength:=1;
                end
              else Message(cg_e_illegal_expression);
              if def^.size>strlength then
                begin
                   getmem(ca,def^.size-strlength);
                   fillchar(ca[0],def^.size-strlength-1,' ');
                   ca[def^.size-strlength-1]:=#0;
{$ifdef UseAnsiString}
                   { this can also handle longer strings }
                   generate_pascii(ca,def^.size-strlength);
{$else UseAnsiString}
                   datasegment^.concat(new(pai_string,init_pchar(ca)));
{$endif UseAnsiString}
                   disposetree(p);
                end;
           end;
         arraydef:
           begin
              if token=LKLAMMER then
                begin
                    consume(LKLAMMER);
                    for l:=parraydef(def)^.lowrange to parraydef(def)^.highrange-1 do
                      begin
                         readtypedconst(parraydef(def)^.definition,nil);
                         consume(COMMA);
                      end;
                    readtypedconst(parraydef(def)^.definition,nil);
                    consume(RKLAMMER);
                 end
              else
                begin
                   p:=comp_expr(true);
                   do_firstpass(p);
                   if p^.treetype=stringconstn then
                     s:=p^.values^
                   else if is_constcharnode(p) then
                     s:=char(byte(p^.value))
                   else Message(cg_e_illegal_expression);
                   l:=length(s);
                   for i:=Parraydef(def)^.lowrange to Parraydef(def)^.highrange do
                     begin
                        if i+1-Parraydef(def)^.lowrange<=l then
                          begin
                             datasegment^.concat(new(pai_const,init_8bit(byte(s[1]))));
                             delete(s,1,1);
                          end
                        else
                          {Fill the remaining positions with #0.}
                          datasegment^.concat(new(pai_const,init_8bit(0)));
                     end;
                   if length(s)>0 then
                     Message(parser_e_string_too_long);
                 end;
           end;
         procvardef:
           begin
              { Procvars and pointers are no longer compatible.  }
              { under tp:  =nil or =var under fpc: =nil or =@var }
              if token=_NIL then
                begin
                   datasegment^.concat(new(pai_const,init_32bit(0)));
                   consume(_NIL);
                   exit;
                end
              else
              if not(cs_tp_compatible in aktswitches) then
                if token=KLAMMERAFFE then
                  consume(KLAMMERAFFE);
              getsym(pattern,true);
              consume(ID);
              if srsym^.typ=unitsym then
                      begin
                         consume(POINT);
                         getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                         consume(ID);
                      end;
                    if srsym^.typ<>procsym then
                      Message(cg_e_illegal_expression)
                    else
                      begin
                         pd:=pprocsym(srsym)^.definition;
                         if assigned(pd^.nextoverloaded) then
                           Message(parser_e_no_overloaded_procvars);
                         if not((pprocvardef(def)^.options=pd^.options)) or
                           not(is_equal(pprocvardef(def)^.retdef,pd^.retdef)) then
                           Message(sym_e_type_mismatch)
                           else
                              begin
                                 hp1:=pprocvardef(def)^.para1;
                                 hp2:=pd^.para1;
                                 while assigned(hp1) and assigned(hp2) do
                                   begin
                                      if not(is_equal(hp1^.data,hp2^.data)) or
                                         not(hp1^.paratyp=hp2^.paratyp) then
                                        begin
                                           Message(sym_e_type_mismatch);
                                           break;
                                        end;
                                      hp1:=hp1^.next;
                                      hp2:=hp2^.next;
                                   end;
                                 if not((hp1=nil) and (hp2=nil)) then
                                   Message(sym_e_type_mismatch);
                              end;
                         datasegment^.concat(new(pai_const,init_symbol(strpnew(pd^.mangledname))));
                         if pd^.owner^.symtabletype=unitsymtable then
                           concat_external(pd^.mangledname,EXT_NEAR);
                      end;
           end;
         { reads a typed constant record }
         recorddef:
           begin
              consume(LKLAMMER);
              aktpos:=0;
              while token<>RKLAMMER do
                begin
                   s:=pattern;
                   consume(ID);
                   consume(COLON);
                   srsym:=precdef(def)^.symtable^.search(s);
                   if srsym=nil then
                     begin
                        Message1(sym_e_id_not_found,s);
                        consume_all_until(SEMICOLON);
                     end
                   else
                     begin
                        { check position }
                        if pvarsym(srsym)^.address<aktpos then
                          Message(parser_e_invalid_record_const);

                        { if needed fill }
                        if pvarsym(srsym)^.address>aktpos then
                          for i:=1 to pvarsym(srsym)^.address-aktpos do
                            datasegment^.concat(new(pai_const,init_8bit(0)));

                        { new position }
                        aktpos:=pvarsym(srsym)^.address+pvarsym(srsym)^.definition^.size;

                        { read the data }
                        readtypedconst(pvarsym(srsym)^.definition,nil);

                        if token=SEMICOLON then
                          consume(SEMICOLON)
                        else break;
                     end;
                end;
              for i:=1 to def^.size-aktpos do
                datasegment^.concat(new(pai_const,init_8bit(0)));
              consume(RKLAMMER);
           end;
         else Message(parser_e_type_const_not_possible);
         end;
      end;

end.
{
  $Log$
  Revision 1.5  1998-06-03 22:49:01  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.4  1998/05/05 12:05:42  florian
    * problems with properties fixed
    * crash fixed:  i:=l when i and l are undefined, was a problem with
      implementation of private/protected

  Revision 1.3  1998/04/29 10:34:00  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.2  1998/04/07 13:19:48  pierre
    * bugfixes for reset_gdb_info
      in MEM parsing for go32v2
      better external symbol creation
      support for rhgdb.exe (lowercase file names)

  Revision 1.1.1.1  1998/03/25 11:18:15  root
  * Restored version

  Revision 1.13  1998/03/20 23:31:35  florian
    * bug0113 fixed
    * problem with interdepened units fixed ("options.pas problem")
    * two small extensions for future AMD 3D support

  Revision 1.12  1998/03/18 22:50:11  florian
    + fstp/fld optimization
    * routines which contains asm aren't longer optimzed
    * wrong ifdef TEST_FUNCRET corrected
    * wrong data generation for array[0..n] of char = '01234'; fixed
    * bug0097 is fixed partial
    * bug0116 fixed (-Og doesn't use enter of the stack frame is greater than
      65535)

  Revision 1.11  1998/03/13 22:45:59  florian
    * small bug fixes applied

  Revision 1.10  1998/03/11 11:23:57  florian
    * bug0081 and bug0109 fixed

  Revision 1.9  1998/03/10 01:17:25  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.8  1998/03/06 00:52:50  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.7  1998/03/02 01:49:10  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.6  1998/02/13 10:35:33  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.5  1998/02/12 11:50:32  daniel
  Yes! Finally! After three retries, my patch!

  Changes:

  Complete rewrite of psub.pas.
  Added support for DLL's.
  Compiler requires less memory.
  Platform units for each platform.

  Revision 1.4  1998/01/24 23:08:19  carl
    + compile time range checking should logically always be on!

  Revision 1.3  1998/01/23 17:12:20  pierre
    * added some improvements for as and ld :
      - doserror and dosexitcode treated separately
      - PATH searched if doserror=2
    + start of long and ansi string (far from complete)
      in conditionnal UseLongString and UseAnsiString
    * options.pas cleaned (some variables shifted to globals)gl

  Revision 1.2  1998/01/09 09:10:03  michael
  + Initial implementation, second try

}
