{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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
unit Ra386dir;

{$i defines.inc}

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
       cpubase,aasm,
       { symtable }
       symconst,symbase,symtype,symsym,symtable,types,
       { pass 1 }
       nbas,
       { parser }
       scanner,
       ra386,
       { codegen }
       cgbase,
       { constants }
       ag386att
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
         retstr:=upper(tostr(procinfo^.return_offset)+'('+gas_reg2str[procinfo^.framepointer]+')')
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
                                    ret_in_acc(aktprocdef.rettype.def) and
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
                                                   '('+gas_reg2str[procinfo^.framepointer]+')';
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
                                                     hs:=tostr(l)+'('+gas_reg2str[procinfo^.framepointer]+')';
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
                                           else if upper(hs)='__OLDEBP' then
                                             begin
                                                { complicate to check there }
                                                { we do it: }
                                                if lexlevel>normal_function_level then
                                                  hs:=tostr(procinfo^.framepointer_offset)+
                                                    '('+gas_reg2str[procinfo^.framepointer]+')'
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
            id    : asmmode_i386_direct;
            idtxt : 'DIRECT'
          );

initialization
  RegisterAsmMode(asmmode_i386_direct_info);

end.
{
  $Log$
  Revision 1.14  2002-04-15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.13  2002/04/14 17:01:52  carl
  + att_reg2str -> gas_reg2str

  Revision 1.12  2001/11/02 22:58:11  peter
    * procsym definition rewrite

  Revision 1.11  2001/08/26 13:37:02  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.10  2001/08/06 21:40:51  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.9  2001/04/18 22:02:03  peter
    * registration of targets and assemblers

  Revision 1.8  2001/04/13 18:20:21  peter
    * scanner object to class

  Revision 1.7  2001/04/13 01:22:21  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.6  2001/04/02 21:20:40  peter
    * resulttype rewrite

  Revision 1.5  2001/03/11 22:58:52  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.4  2000/12/25 00:07:34  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.3  2000/11/29 00:30:50  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.2  2000/10/31 22:02:57  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:47:43  peter
    * moved to i386/

  Revision 1.5  2000/10/14 10:14:52  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.4  2000/09/24 15:06:26  peter
    * use defines.inc

  Revision 1.3  2000/08/27 16:11:52  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.2  2000/07/13 11:32:48  michael
  + removed logs

}
