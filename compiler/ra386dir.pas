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
unit Ra386dir;

  interface

    uses
      tree;

     function assemble : ptree;

  implementation

     uses
        comphook,files,hcodegen,globals,scanner,aasm
{$ifdef Ag386Bin}
        ,i386base,i386asm
{$else}
        ,i386
{$endif}
        ,cobjects,symtable,types,verbose,rautils;

    function assemble : ptree;

      var
         retstr,s,hs : string;
         c : char;
         ende : boolean;
         sym : psym;
         code : paasmoutput;
         i,l : longint;

       procedure writeasmline;
         var
           i : longint;
         begin
           i:=length(s);
           while (i>0) and (s[i] in [' ',#9]) do
            dec(i);
           {$ifndef TP}
             {$ifopt H+}
               setlength(s,i);
             {$else}
               s[0]:=chr(i);
             {$endif}
           {$else}
             s[0]:=chr(i);
           {$endif}
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
         c:=current_scanner^.asmgetchar;
         code:=new(paasmoutput,init);
         while not(ende) do
           begin
              { wrong placement
              current_scanner^.gettokenpos; }
              case c of
                 'A'..'Z','a'..'z','_' : begin
                      current_scanner^.gettokenpos;
                      i:=0;
                      hs:='';
                      while ((ord(c)>=ord('A')) and (ord(c)<=ord('Z')))
                         or ((ord(c)>=ord('a')) and (ord(c)<=ord('z')))
                         or ((ord(c)>=ord('0')) and (ord(c)<=ord('9')))
                         or (c='_') do
                        begin
                           inc(i);
                           hs[i]:=c;
                           c:=current_scanner^.asmgetchar;
                        end;
                      {$ifndef TP}
                        {$ifopt H+}
                          setlength(hs,i);
                        {$else}
                          hs[0]:=chr(i);
                        {$endif}
                      {$else}
                         hs[0]:=chr(i);
                      {$endif}
                      if upper(hs)='END' then
                         ende:=true
                      else
                         begin
                            if c=':' then
                              begin
                                getsym(upper(hs),false);
                                if srsym<>nil then
                                  if (srsym^.typ = labelsym) then
                                    Begin
                                       hs:=lab2str(plabelsym(srsym)^.number);
                                       {label is set !! }
                                       plabelsym(srsym)^.number^.is_set:=true;
                                    end
                                  else
                                    Message(assem_w_using_defined_as_local);
                              end
                            else if upper(hs)='FWAIT' then
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
                                      if assigned(aktprocsym^.definition^.localst) and
                                         (lexlevel >= normal_function_level) then
                                        sym:=aktprocsym^.definition^.localst^.search(upper(hs))
                                      else
                                        sym:=nil;
                                      if assigned(sym) then
                                        begin
                                           if (sym^.typ = labelsym) then
                                             Begin
                                                hs:=lab2str(plabelsym(sym)^.number);
                                             end
                                           else if sym^.typ=varsym then
                                             begin
                                             {variables set are after a comma }
                                             {like in movl %eax,I }
                                             if pos(',',s) > 0 then
                                               pvarsym(sym)^.is_valid:=1
                                             else
                                             if (pos('MOV',upper(s)) > 0) and (pvarsym(sym)^.is_valid=0) then
                                              Message1(sym_n_uninitialized_local_variable,hs);
                                             hs:='-'+tostr(pvarsym(sym)^.address)+'('+att_reg2str[procinfo.framepointer]+')';
                                             end
                                           else
                                           { call to local function }
                                           if (sym^.typ=procsym) and ((pos('CALL',upper(s))>0) or
                                              (pos('LEA',upper(s))>0)) then
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
                                      what would you decide, florian ?}
                                      else

                                        begin
{$ifndef IGNOREGLOBALVAR}
                                           getsym(upper(hs),false);
                                           sym:=srsym;
                                           if assigned(sym) and (sym^.owner^.symtabletype in [unitsymtable,
                                             globalsymtable,staticsymtable]) then
                                             begin
                                                if (sym^.typ = varsym) or (sym^.typ = typedconstsym) then
                                                  begin
                                                     Do_comment(V_Warning,hs+' translated to '+sym^.mangledname);
                                                     hs:=sym^.mangledname;
                                                     if sym^.typ=varsym then
                                                       inc(pvarsym(sym)^.refs);
                                                  end;
                                                { procs can be called or the address can be loaded }
                                                if (sym^.typ=procsym) and ((pos('CALL',upper(s))>0) or
                                                   (pos('LEA',upper(s))>0)) then
                                                  begin
                                                     if assigned(pprocsym(sym)^.definition^.nextoverloaded) then
                                                       begin
                                                          Do_comment(V_Warning,hs+' is associated to an overloaded function');
                                                       end;
                                                     Do_comment(V_Warning,hs+' translated to '+sym^.mangledname);
                                                     hs:=sym^.mangledname;
                                                  end;
                                             end
                                           else
{$endif TESTGLOBALVAR}
                                           if upper(hs)='__SELF' then
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
                                                if lexlevel>normal_function_level then
                                                  hs:=tostr(procinfo.framepointer_offset)
                                                    +'('+att_reg2str[procinfo.framepointer]+')'
                                                else
                                                  Message(assem_e_cannot_use___OLDEBP_outside_nested_procedure);
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
                        procinfo.funcret_is_valid:=true;
                     writeasmline;
                     c:=current_scanner^.asmgetchar;
                   end;
             #26 : Message(scan_f_end_of_file);
             else
               begin
                 current_scanner^.gettokenpos;
                 {$ifndef TP}
                   {$ifopt H+}
                     setlength(s,length(s)+1);
                   {$else}
                     inc(byte(s[0]));
                   {$endif}
                 {$else}
                    inc(byte(s[0]));
                 {$endif}
                 s[length(s)]:=c;
                 c:=current_scanner^.asmgetchar;
               end;
           end;
         end;
       writeasmline;
       assemble:=genasmnode(code);
     end;

end.
{
  $Log$
  Revision 1.15  1999-03-01 13:22:26  pierre
   * varsym refs incremented

  Revision 1.14  1999/02/22 02:15:36  peter
    * updates for ag386bin

  Revision 1.13  1999/01/27 13:04:12  pierre
   * bug with static vars in assembler readers

  Revision 1.12  1999/01/10 15:37:57  peter
    * moved some tables from ra386*.pas -> i386.pas
    + start of coff writer
    * renamed asmutils unit to rautils

  Revision 1.11  1998/11/17 00:26:12  peter
    * fixed for $H+

  Revision 1.10  1998/11/13 15:40:28  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.9  1998/10/20 08:06:57  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.8  1998/09/04 08:42:08  peter
    * updated some error messages

  Revision 1.7  1998/09/03 17:39:05  florian
    + better code for type conversation longint/dword to real type

  Revision 1.6  1998/09/03 17:08:47  pierre
    * better lines for stabs
      (no scroll back to if before else part
      no return to case line at jump outside case)
    + source lines also if not in order

  Revision 1.5  1998/08/21 08:45:51  pierre
    * better line info for asm statements

  Revision 1.4  1998/07/14 14:46:59  peter
    * released NEWINPUT

  Revision 1.3  1998/07/07 11:20:08  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.2  1998/06/24 14:06:37  peter
    * fixed the name changes

  Revision 1.1  1998/06/23 14:00:18  peter
    * renamed RA* units

  Revision 1.5  1998/06/12 10:32:32  pierre
    * column problem hopefully solved
    + C vars declaration changed

  Revision 1.4  1998/06/04 23:51:58  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

}
