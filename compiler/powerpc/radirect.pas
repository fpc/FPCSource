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
  This unit reads PowerPC inline assembler and writes the lines direct to the output file.
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
       cgbase,
       { constants }
       agppcgas,
       cpubase
       ;

    { checks if a string identifies a register }
    { it should be optimized                   }
    function is_register(const s : string) : boolean;
      var
         r : toldregister;
      begin
         is_register:=false;
         if length(s)>5 then
           exit;
         for r:=low(gas_reg2str) to high(gas_reg2str) do
           if gas_reg2str[r]=s then
              begin
                 is_register:=true;
                 exit;
              end;
      end;

    function assemble : tnode;

      var
         retstr,s,hs : string;
         c : char;
         ende : boolean;
         srsym,sym : tsym;
         srsymtable : tsymtable;
         code : TAAsmoutput;
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
             tvarsym(aktprocdef.funcretsym).varstate:=vs_assigned;
           s:='';
         end;

     begin
       ende:=false;
       s:='';
       if assigned(aktprocdef.funcretsym) and
          is_fpu(aktprocdef.rettype.def) then
         tvarsym(aktprocdef.funcretsym).varstate:=vs_assigned;
       { !!!!!
       if (not is_void(aktprocdef.rettype.def)) then
         retstr:=upper(tostr(procinfo^.return_offset)+'('+gas_reg2str[procinfo^.framepointer]+')')
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
                                  tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_assigned;
                                }
                                if ((s[length(s)]<>'0') or (hs[1]<>'x')) and not(is_register(hs)) then
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
                                                    if (tvarsym(sym).reg.enum<>R_NO) then
// until new regallocator stuff settles down
//                                                      hs:=gas_reg2str[procinfo.framepointer.enum]
                                                      hs:=gas_reg2str[STACK_POINTER_REG]
                                                    else
                                                      hs:=tostr(tvarsym(sym).address)+
//                                                        '('+gas_reg2str[procinfo.framepointer.enum]+')';
                                                        '('+gas_reg2str[STACK_POINTER_REG]+')';
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
//                                                    hs:=tostr(l)+'('+gas_reg2str[procinfo.framepointer.enum]+')';
                                                    hs:=tostr(l)+'('+gas_reg2str[STACK_POINTER_REG]+')';
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
                     tvarsym(aktprocdef.funcretsym).varstate:=vs_assigned;
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
{
  $Log$
  Revision 1.11  2003-04-25 21:05:22  florian
    * fixed tfuncretsym stuff in powerpc specific part

  Revision 1.10  2003/04/24 12:05:53  florian
    * symbols which are register identifiers aren't resolved anymore

  Revision 1.9  2003/04/23 22:18:01  peter
    * fixes to get rtl compiled

  Revision 1.8  2003/03/22 18:00:27  jonas
    * fixes for new regallocator

  Revision 1.7  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.6  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.5  2002/09/03 19:04:18  daniel
    * Fixed PowerPC & M68000 compilation

  Revision 1.4  2002/09/03 16:26:28  daniel
    * Make Tprocdef.defs protected

  Revision 1.3  2002/08/31 15:59:31  florian
    + HEAP* stuff must be generated for Linux/PPC as well
    + direct assembler reader searches now global and static symtables as well

  Revision 1.2  2002/08/18 21:36:42  florian
    + handling of local variables in direct reader implemented

  Revision 1.1  2002/08/10 14:52:52  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.2  2002/07/28 20:45:23  florian
    + added direct assembler reader for PowerPC

  Revision 1.1  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling
}
