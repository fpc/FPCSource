{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Reads inline Alpha assembler and writes the lines direct to the output

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
  This unit reads Alpha inline assembler and writes the lines direct to the output file.
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
       aasmbase,aasmtai,aasmdata,aasmcpu,
       { symtable }
       symconst,symbase,symtype,symsym,symtable,defbase,
       { pass 1 }
       nbas,
       { parser }
       scanner,
       { codegen }
       cgbase,
       { constants }
       agaxpgas,
       cpubase
       ;

    function assemble : tnode;

      var
         retstr,s,hs : string;
         c : char;
         ende : boolean;
         srsym,sym : tsym;
         srsymtable : tsymtable;
         code : TAsmList;
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
           if assigned(aktprocdef.funcretsym) and
              (pos(retstr,upper(s))>0) then
             tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_initialised;
           s:='';
         end;

     begin
       ende:=false;
       s:='';
       if assigned(aktprocdef.funcretsym) and
          is_fpu(aktprocdef.rettype.def) then
         tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_initialised;
       { !!!!!
       if (not is_void(aktprocdef.rettype.def)) then
         retstr:=upper(tostr(procinfo^.return_offset)+'('+gas_reg2str[procinfo^.framepointer]+')')
       else
       }
         retstr:='';

       c:=current_scanner.asmgetchar;
       code:=TAsmList.Create;
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
                           if assigned(aktprocdef) then
                             begin
                                { I don't know yet, what the ppc port requires }
                                { we'll see how things settle down             }

                                { is the last written character an special }
                                { char ?                                   }
                                { !!!
                                if (s[length(s)]='%') and
                                   ret_in_acc(aktprocdef.rettype.def) and
                                   ((pos('AX',upper(hs))>0) or
                                   (pos('AL',upper(hs))>0)) then
                                  tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_initialised;
                                }
                                if ((s[length(s)]<>'0') or (hs[1]<>'x')) then
                                  begin
                                     if assigned(aktprocdef.localst) and
                                        (lexlevel >= normal_function_level) then
                                       sym:=tsym(aktprocdef.localst.search(upper(hs)))
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
                                                    if (tvarsym(sym).reg<>R_NO) then
                                                      hs:=gas_reg2str[procinfo.framepointer]
                                                    else
                                                      hs:=tostr(tvarsym(sym).address)+
                                                        '('+gas_reg2str[procinfo.framepointer]+')';
                                                 end;
                                            end
                                          else
                                          { call to local function }
                                          if (sym.typ=procsym) and (pos('BL',upper(s))>0) then
                                            hs:=tprocsym(sym).first_procdef.mangledname;
                                       end
                                     else
                                       begin
                                          if assigned(aktprocdef.parast) then
                                            sym:=tsym(aktprocdef.parast.search(upper(hs)))
                                          else
                                            sym:=nil;
                                          if assigned(sym) then
                                            begin
                                               if sym.typ=varsym then
                                                 begin
                                                    l:=tvarsym(sym).address;
                                                    { set offset }
                                                    inc(l,aktprocdef.parast.address_fixup);
                                                    hs:=tostr(l)+'('+gas_reg2str[procinfo.framepointer]+')';
                                                    if pos(',',s) > 0 then
                                                      tvarsym(sym).varstate:=vs_readwritten;
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
                                                    if (not is_void(aktprocdef.rettype.def)) then
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
              '{',';',#10,#13:
                begin
                   if pos(retstr,s) > 0 then
                     tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_initialised;
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
  asmmode_ppc_direct_info : tasmmodeinfo =
          (
            id    : asmmode_direct;
            idtxt : 'DIRECT'
          );

initialization
  RegisterAsmMode(asmmode_ppc_direct_info);

end.
