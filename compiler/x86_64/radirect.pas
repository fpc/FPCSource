{
    $Id$
    Copyright (c) 1998-2001 by Florian Klaempfl

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
unit Radirect;

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
       cpubase,aasmtai,
       { symtable }
       symconst,symbase,symtype,symsym,symtable,defbase,paramgr,
       { pass 1 }
       nbas,
       { parser }
       scanner,
       rax86_64,
       agx64att,
       { codegen }
       cgbase
       ;

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
             tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_assigned;
           s:='';
         end;

     begin
       ende:=false;
       s:='';
       if assigned(aktprocdef.funcretsym) and
          is_fpu(aktprocdef.rettype.def) then
         tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_assigned;
       if (not is_void(aktprocdef.rettype.def)) then
         retstr:=upper(tostr(procinfo^.return_offset)+'('+att_reg2str[procinfo^.framepointer]+')')
       else
         retstr:='';
         c:=current_scanner.asmgetchar;
         code:=TAAsmoutput.Create;
         while not(ende) do
           begin
              { wrong placement
              current_scanner.gettokenpos; }
              case c of
                 'A'..'Z','a'..'z','_' : begin
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
                            else if upper(hs)='FWAIT' then
                             FwaitWarning
                            else
                            { access to local variables }
                            if assigned(aktprocdef) then
                              begin
                                 { is the last written character an special }
                                 { char ?                                   }
                                 if (s[length(s)]='%') and
                                    paramanager.ret_in_acc(aktprocdef.rettype.def) and
                                    ((pos('AX',upper(hs))>0) or
                                    (pos('AL',upper(hs))>0)) then
                                   tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_assigned;
                                 if (s[length(s)]<>'%') and
                                   (s[length(s)]<>'$') and
                                   ((s[length(s)]<>'0') or (hs[1]<>'x')) then
                                   begin
                                      if assigned(aktprocdef.localst) and
                                         (lexlevel >= normal_function_level) then
                                        sym:=tsym(aktprocdef.localst.search(upper(hs)))
                                      else
                                        sym:=nil;
                                      if assigned(sym) then
                                        begin
                                           if (sym.typ = labelsym) then
                                             Begin
                                                hs:=tlabelsym(sym).lab.name;
                                             end
                                           else if sym.typ=varsym then
                                             begin
                                             {variables set are after a comma }
                                             {like in movl %eax,I }
                                             if pos(',',s) > 0 then
                                               tvarsym(sym).varstate:=vs_used
                                             else
                                             if (pos('MOV',upper(s)) > 0) and (tvarsym(sym).varstate=vs_declared) then
                                              Message1(sym_n_uninitialized_local_variable,hs);
                                             if (vo_is_external in tvarsym(sym).varoptions) then
                                               hs:=tvarsym(sym).mangledname
                                             else
                                               hs:='-'+tostr(tvarsym(sym).address)+
                                                   '('+att_reg2str[procinfo^.framepointer]+')';
                                             end
                                           else
                                           { call to local function }
                                           if (sym.typ=procsym) and ((pos('CALL',upper(s))>0) or
                                              (pos('LEA',upper(s))>0)) then
                                             begin
                                                hs:=tprocsym(sym).defs^.def.mangledname;
                                             end;
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
                                                     hs:=tostr(l)+'('+att_reg2str[procinfo^.framepointer]+')';
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
                                                     if ((pos('CALL',upper(s))>0) or (pos('LEA',upper(s))>0)) then
                                                      begin
                                                        if assigned(tprocsym(sym).defs^.def) then
                                                          Message1(asmr_w_direct_global_is_overloaded_func,hs);
                                                        Message2(asmr_h_direct_global_to_mangled,hs,tprocsym(sym).defs^.def.mangledname);
                                                        hs:=tprocsym(sym).defs^.def.mangledname;
                                                      end;
                                                   end;
                                                 else
                                                   Message(asmr_e_wrong_sym_type);
                                               end;
                                             end
                                           else if upper(hs)='__SELF' then
                                             begin
                                                if assigned(procinfo^._class) then
                                                  hs:=tostr(procinfo^.selfpointer_offset)+
                                                      '('+att_reg2str[procinfo^.framepointer]+')'
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
                                           else if upper(hs)='__OLDEBP' then
                                             begin
                                                { complicate to check there }
                                                { we do it: }
                                                if lexlevel>normal_function_level then
                                                  hs:=tostr(procinfo^.framepointer_offset)+
                                                    '('+att_reg2str[procinfo^.framepointer]+')'
                                                else
                                                  Message(asmr_e_cannot_use_OLDEBP_outside_nested_procedure);
                                             end;
                                           end;
                                        end;
                                   end;
                              end;
                            s:=s+hs;
                         end;
                   end;
 '{',';',#10,#13 : begin
                      if pos(retstr,s) > 0 then
                        tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_assigned;
                     writeasmline;
                     c:=current_scanner.asmgetchar;
                   end;
             #26 : Message(scan_f_end_of_file);
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
  asmmode_i386_direct_info : tasmmodeinfo =
          (
            id    : asmmode_direct;
            idtxt : 'DIRECT'
          );

initialization
  RegisterAsmMode(asmmode_i386_direct_info);

end.
{
  $Log$
  Revision 1.1  2002-08-10 14:53:38  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.2  2002/07/25 22:55:34  florian
    * several fixes, small test units can be compiled

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code

}
