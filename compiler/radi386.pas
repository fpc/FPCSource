{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    Reads inline assembler and writes the lines direct to the output

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
unit radi386;

  interface

    uses
      tree;

     function assemble : ptree;

  implementation

     uses
        files,i386,hcodegen,globals,scanner,aasm,
        cobjects,symtable,types,verbose,asmutils;

    function assemble : ptree;

      var
         retstr,s,hs : string;
         c : char;
         ende : boolean;
         sym : psym;
         code : paasmoutput;
         l : longint;

       procedure writeasmline;
         var
           i : longint;
         begin
           i:=length(s);
           while (i>0) and (s[i] in [' ',#9]) do
            dec(i);
           s[0]:=chr(i);
           if s<>'' then
            code^.concat(new(pai_direct,init(strpnew(s))));
            { consider it set function set if the offset was loaded }
           if assigned(procinfo.retdef) and
              (pos(retstr,upper(s))>0) then
              procinfo.funcret_is_valid:=true;
           s:='';
         end;

     begin
       ende:=false;
       s:='';
       if assigned(procinfo.retdef) and
          is_fpu(procinfo.retdef) then
         procinfo.funcret_is_valid:=true;
       if assigned(procinfo.retdef) and
          (procinfo.retdef<>pdef(voiddef)) then
         retstr:=upper(tostr(procinfo.retoffset)+'('+att_reg2str[procinfo.framepointer]+')')
       else
         retstr:='';
         c:=asmgetchar;
         code:=new(paasmoutput,init);
         while not(ende) do
           begin
              tokenpos.line:=current_module^.current_inputfile^.line_no;
              tokenpos.column:=get_file_col;
              tokenpos.fileindex:=current_module^.current_index;
              case c of
                 'A'..'Z','a'..'z','_' : begin
                      hs:='';
                      while ((ord(c)>=ord('A')) and (ord(c)<=ord('Z')))
                         or ((ord(c)>=ord('a')) and (ord(c)<=ord('z')))
                         or ((ord(c)>=ord('0')) and (ord(c)<=ord('9')))
                         or (c='_') do
                        begin
                           inc(byte(hs[0]));
                           hs[length(hs)]:=c;
                           c:=asmgetchar;
                        end;
                      if upper(hs)='END' then
                         ende:=true
                      else
                         begin
                            if c=':' then
                              begin
                                getsym(upper(hs),false);
                                if srsym<>nil then
                                  Message(assem_w_using_defined_as_local);
                              end;
                            if upper(hs)='FWAIT' then
                             FwaitWarning
                            else
                            { access to local variables }
                            if assigned(aktprocsym) then
                              begin
                                 { is the last written character an special }
                                 { char ?                                   }
                                 if (s[length(s)]='%') and
                                    ret_in_acc(procinfo.retdef) and
                                    ((pos('AX',upper(hs))>0) or
                                    (pos('AL',upper(hs))>0)) then
                                   procinfo.funcret_is_valid:=true;
                                 if (s[length(s)]<>'%') and
                                   (s[length(s)]<>'$') and
                                   ((s[length(s)]<>'0') or (hs[1]<>'x')) then
                                   begin
                                      if assigned(aktprocsym^.definition^.localst) then
                                        sym:=aktprocsym^.definition^.localst^.search(upper(hs))
                                      else
                                        sym:=nil;
                                      if assigned(sym) then
                                        begin
                                           if sym^.typ=varsym then
                                             begin
                                             {variables set are after a comma }
                                             {like in movl %eax,I }
                                             if pos(',',s) > 0 then
                                               pvarsym(sym)^.is_valid:=1
                                             else
                                             if (pos('MOV',upper(s)) > 0) and (pvarsym(sym)^.is_valid=0) then
                                              Message1(sym_n_local_var_not_init_yet,hs);
                                             hs:='-'+tostr(pvarsym(sym)^.address)+'('+att_reg2str[procinfo.framepointer]+')';
                                             end
                                           else
                                           { call to local function }
                                           if (sym^.typ=procsym) and (pos('CALL',upper(s))>0) then
                                             begin
                                                hs:=pprocsym(sym)^.definition^.mangledname;
                                             end;
                                        end
                                      else
                                        begin
                                           if assigned(aktprocsym^.definition^.parast) then
                                             sym:=aktprocsym^.definition^.parast^.search(upper(hs))
                                           else
                                             sym:=nil;
                                           if assigned(sym) then
                                             begin
                                                if sym^.typ=varsym then
                                                  begin
                                                     l:=pvarsym(sym)^.address;
                                                     { set offset }
                                                     inc(l,aktprocsym^.definition^.parast^.call_offset);
                                                     hs:=tostr(l)+'('+att_reg2str[procinfo.framepointer]+')';
                                                     if pos(',',s) > 0 then
                                                       pvarsym(sym)^.is_valid:=1;
                                                  end;
                                             end
                                      { I added that but it creates a problem in line.ppi
                                      because there is a local label wbuffer and
                                      a static variable WBUFFER ...
                                      what would you decide, florian ?
                                      else

                                        begin
                                           getsym(upper(hs),false);
                                           sym:=srsym;
                                           if assigned(sym) and (sym^.typ = varsym)
                                              or (sym^.typ = typedconstsym) then
                                             hs:=sym^.mangledname;
                                           if (sym^.typ=procsym) and (pos('CALL',upper(s))>0) then
                                             begin
                                                if assigned(pprocsym(sym)^.definition^.nextoverloaded) then
                                                  begin
                                                     exterror:=strpnew(' calling an overloaded procedure in asm');
                                                     warning(user_defined);
                                                  end;
                                                hs:=pprocsym(sym)^.definition^.mangledname;
                                             end;
                                        end   }
                                           else if upper(hs)='__SELF' then
                                             begin
                                                if assigned(procinfo._class) then
                                                  hs:=tostr(procinfo.ESI_offset)+'('+att_reg2str[procinfo.framepointer]+')'
                                                else
                                                 Message(assem_e_cannot_use_SELF_outside_a_method);
                                             end
                                           else if upper(hs)='__RESULT' then
                                             begin
                                                if assigned(procinfo.retdef) and
                                                  (procinfo.retdef<>pdef(voiddef)) then
                                                  begin
                                                  hs:=retstr;
                                                  end
                                                else
                                                 Message(assem_w_void_function);
                                             end
                                           else if upper(hs)='__OLDEBP' then
                                             begin
                                                            { complicate to check there }
                                                            { we do it: }
                                                if lexlevel>2 then
                                                  hs:=tostr(procinfo.framepointer_offset)
                                                                +'('+att_reg2str[procinfo.framepointer]+')'
                                                else
                                                  Message(assem_e_cannot_use___OLDEBP_outside_nested_procedure);
                                                end;
                                           end;
                                       { end;}
                                   end;
                              end;
                            s:=s+hs;
                         end;
                   end;
 '{',';',#10,#13 : begin
                      if pos(retstr,s) > 0 then
                        procinfo.funcret_is_valid:=true;
                     writeasmline;
                     c:=asmgetchar;
                   end;
             #26 : Message(scan_f_end_of_file);
             else
               begin
                 inc(byte(s[0]));
                 s[length(s)]:=c;
                 c:=asmgetchar;
               end;
           end;
         end;
       writeasmline;
       assemble:=genasmnode(code);
     end;

end.
{
  $Log$
  Revision 1.5  1998-06-12 10:32:32  pierre
    * column problem hopefully solved
    + C vars declaration changed

  Revision 1.4  1998/06/04 23:51:58  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

}
