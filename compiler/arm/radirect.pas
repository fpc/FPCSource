{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Reads inline Powerpc assembler and writes the lines direct to the output

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
{
  This unit reads ARM inline assembler and writes the lines direct to the output file.
}
unit radirect;

{$i fpcdefs.inc}

interface

    uses
      node;

     function assemble : tnode;

  implementation

    uses
       { common }
       cutils,
       { global }
       globals,verbose,
       systems,
       { aasm }
       aasmbase,aasmtai,aasmcpu,
       { symtable }
       symconst,symbase,symtype,symsym,symtable,defutil,
       { pass 1 }
       nbas,
       { parser }
       scanner,
       { codegen }
       cginfo,
       cgbase,
       { constants }
       itarmgas,
       cpubase
       ;

    function is_register(const s:string):boolean;
      begin
        is_register:=false;
        if gas_regnum_search(lower(s))<>NR_NO then
          is_register:=true;
      end;


    function assemble : tnode;
      var
         retstr,s,hs : string;
         c : char;
         ende : boolean;
         srsym,sym : tsym;
         srsymtable : tsymtable;
         code : TAAsmoutput;
         framereg : tregister;
         i,l : longint;

       procedure writeasmline;
         var
           i : longint;
         begin
           i:=length(s);
           while (i>0) and (s[i] in [' ',#9]) do
            dec(i);
           s[0]:=chr(i);
           if s<>'' then
            code.concat(Tai_direct.Create(strpnew(s)));
            { consider it set function set if the offset was loaded }
           if assigned(current_procinfo.procdef.funcretsym) and
              (pos(retstr,upper(s))>0) then
             tvarsym(current_procinfo.procdef.funcretsym).varstate:=vs_assigned;
           s:='';
         end;

     begin
       ende:=false;
       framereg:=NR_FRAME_POINTER_REG;
       s:='';
       if assigned(current_procinfo.procdef.funcretsym) and
          is_fpu(current_procinfo.procdef.rettype.def) then
         tvarsym(current_procinfo.procdef.funcretsym).varstate:=vs_assigned;
       { !!!!!
       if (not is_void(current_procinfo.procdef.rettype.def)) then
         retstr:=upper(tostr(tvarsym(current_procinfo.procdef.funcretsym).adjusted_address)+'('+gas_reg2str[procinfo^.framepointer]+')')
       else
       }
         retstr:='';

       c:=current_scanner.asmgetchar;
       code:=TAAsmoutput.Create;
       while not(ende) do
         begin
            { wrong placement
            current_scanner.gettokenpos; }
            case c of
              'A'..'Z','a'..'z','_':
                begin
                   current_scanner.gettokenpos;
                   i:=0;
                   hs:='';
                   while ((ord(c)>=ord('A')) and (ord(c)<=ord('Z')))
                      or ((ord(c)>=ord('a')) and (ord(c)<=ord('z')))
                      or ((ord(c)>=ord('0')) and (ord(c)<=ord('9')))
                      or (c='_') do
                     begin
                        inc(i);
                        hs[i]:=c;
                        c:=current_scanner.asmgetchar;
                     end;
                   hs[0]:=chr(i);
                   if upper(hs)='END' then
                      ende:=true
                   else
                      begin
                         if c=':' then
                           begin
                             searchsym(upper(hs),srsym,srsymtable);
                             if srsym<>nil then
                               if (srsym.typ = labelsym) then
                                 Begin
                                    hs:=tlabelsym(srsym).lab.name;
                                    tlabelsym(srsym).lab.is_set:=true;
                                 end
                               else
                                 Message(asmr_w_using_defined_as_local);
                           end
                         else
                           { access to local variables }
                           if assigned(current_procinfo.procdef) then
                             begin
                                { I don't know yet, what the ppc port requires }
                                { we'll see how things settle down             }

                                { is the last written character an special }
                                { char ?                                   }
                                { !!!
                                if (s[length(s)]='%') and
                                   ret_in_acc(current_procinfo.procdef.rettype.def) and
                                   ((pos('AX',upper(hs))>0) or
                                   (pos('AL',upper(hs))>0)) then
                                  tfuncretsym(current_procinfo.procdef.funcretsym).funcretstate:=vs_assigned;
                                }
                                if ((s[length(s)]<>'0') or (hs[1]<>'x')) and not(is_register(hs)) then
                                  begin
                                     if assigned(current_procinfo.procdef.localst) and
                                        (current_procinfo.procdef.localst.symtablelevel >= normal_function_level) then
                                       sym:=tsym(current_procinfo.procdef.localst.search(upper(hs)))
                                     else
                                       sym:=nil;
                                     if assigned(sym) then
                                       begin
                                          if (sym.typ=labelsym) then
                                            Begin
                                               hs:=tlabelsym(sym).lab.name;
                                            end
                                          else if sym.typ=varsym then
                                            begin
                                               if (vo_is_external in tvarsym(sym).varoptions) then
                                                 hs:=tvarsym(sym).mangledname
                                               else
                                                 begin
                                                    if (tvarsym(sym).reg<>NR_NO) then
                                                      hs:=std_regname(framereg)
                                                    else
                                                      hs:=tostr(tvarsym(sym).address)+
                                                        '('+std_regname(framereg)+')';
                                                 end;
                                            end
                                          else
                                          { call to local function }
                                          if (sym.typ=procsym) and (pos('BL',upper(s))>0) then
                                            hs:=tprocsym(sym).first_procdef.mangledname;
                                       end
                                     else
                                       begin
                                          if assigned(current_procinfo.procdef.parast) then
                                            sym:=tsym(current_procinfo.procdef.parast.search(upper(hs)))
                                          else
                                            sym:=nil;
                                          if assigned(sym) then
                                            begin
                                               if sym.typ=varsym then
                                                 begin
                                                    l:=tvarsym(sym).address;
                                                    { set offset }
                                                    inc(l,current_procinfo.procdef.parast.address_fixup);
//                                                    hs:=tostr(l)+'('+gas_reg2str[procinfo.framepointer.enum]+')';
                                                    hs:=tostr(l)+'('+std_regname(framereg)+')';
                                                    if pos(',',s) > 0 then
                                                      tvarsym(sym).varstate:=vs_used;
                                                 end;
                                            end
                                          { I added that but it creates a problem in line.ppi
                                          because there is a local label wbuffer and
                                          a static variable WBUFFER ...
                                          what would you decide, florian ?}
                                          else
                                            begin
                                               searchsym(upper(hs),sym,srsymtable);
                                               if assigned(sym) and (sym.owner.symtabletype in [globalsymtable,staticsymtable]) then
                                                 begin
                                                   case sym.typ of
                                                     constsym :
                                                       begin
                                                         inc(tconstsym(sym).refs);
                                                         case tconstsym(sym).consttyp of
                                                           constint,constchar,constbool :
                                                             hs:=tostr(tconstsym(sym).value.valueord);
                                                           constpointer :
                                                             hs:=tostr(tconstsym(sym).value.valueordptr);
                                                           else
                                                             Message(asmr_e_wrong_sym_type);
                                                         end;
                                                       end;
                                                     varsym :
                                                       begin
                                                         Message2(asmr_h_direct_global_to_mangled,hs,tvarsym(sym).mangledname);
                                                         hs:=tvarsym(sym).mangledname;
                                                         inc(tvarsym(sym).refs);
                                                       end;
                                                     typedconstsym :
                                                       begin
                                                         Message2(asmr_h_direct_global_to_mangled,hs,ttypedconstsym(sym).mangledname);
                                                         hs:=ttypedconstsym(sym).mangledname;
                                                       end;
                                                     procsym :
                                                       begin
                                                         { procs can be called or the address can be loaded }
                                                         if (pos('BL',upper(s))>0) {or (pos('LEA',upper(s))>0))}  then
                                                          begin
                                                            if Tprocsym(sym).procdef_count>1 then
                                                              Message1(asmr_w_direct_global_is_overloaded_func,hs);
                                                            Message2(asmr_h_direct_global_to_mangled,hs,tprocsym(sym).first_procdef.mangledname);
                                                            hs:=tprocsym(sym).first_procdef.mangledname;
                                                          end;
                                                       end;
                                                     else
                                                       Message(asmr_e_wrong_sym_type);
                                                   end;
                                                 end
{$ifdef dummy}
                                               else if upper(hs)='__SELF' then
                                                 begin
                                                    if assigned(procinfo^._class) then
                                                      hs:=tostr(procinfo^.selfpointer_offset)+
                                                          '('+gas_reg2str[procinfo^.framepointer]+')'
                                                    else
                                                     Message(asmr_e_cannot_use_SELF_outside_a_method);
                                                 end
                                               else if upper(hs)='__RESULT' then
                                                 begin
                                                    if (not is_void(current_procinfo.procdef.rettype.def)) then
                                                      hs:=retstr
                                                    else
                                                      Message(asmr_e_void_function);
                                                 end
                                               { implement old stack/frame pointer access for nested procedures }
                                               {!!!!
                                               else if upper(hs)='__OLDSP' then
                                                 begin
                                                    { complicate to check there }
                                                    { we do it: }
                                                    if lexlevel>normal_function_level then
                                                      hs:=tostr(procinfo^.framepointer_offset)+
                                                        '('+gas_reg2str[procinfo^.framepointer]+')'
                                                    else
                                                      Message(asmr_e_cannot_use_OLDEBP_outside_nested_procedure);
                                                 end;
                                               }
                                               end;
{$endif dummy}
                                            end;
                                       end;
                                  end;
                             end;
                         s:=s+hs;
                      end;
                end;
              ';',#10,#13:
                begin
                   if pos(retstr,s) > 0 then
                     tvarsym(current_procinfo.procdef.funcretsym).varstate:=vs_assigned;
                   writeasmline;
                   c:=current_scanner.asmgetchar;
                end;
              #26:
                Message(scan_f_end_of_file);
              else
                begin
                  current_scanner.gettokenpos;
                  inc(byte(s[0]));
                  s[length(s)]:=c;
                  c:=current_scanner.asmgetchar;
                end;
            end;
         end;
       writeasmline;
       assemble:=casmnode.create(code);
     end;

{*****************************************************************************
                                     Initialize
*****************************************************************************}

const
  asmmode_arm_direct_info : tasmmodeinfo =
          (
            id    : asmmode_direct;
            idtxt : 'DIRECT'
          );

initialization
  RegisterAsmMode(asmmode_arm_direct_info);
end.
{
  $Log$
  Revision 1.4  2003-09-04 00:15:29  florian
    * first bunch of adaptions of arm compiler for new register type

  Revision 1.3  2003/09/01 15:11:17  florian
    * fixed reference handling
    * fixed operand postfix for floating point instructions
    * fixed wrong shifter constant handling

  Revision 1.2  2003/08/16 13:23:01  florian
    * several arm related stuff fixed

  Revision 1.1  2003/07/21 16:35:30  florian
    * very basic stuff for the arm
}
