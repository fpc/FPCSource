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
        i386,hcodegen,globals,scanner,aasm,
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
            { if function return is param }
            { consider it set if the offset was loaded }
           if assigned(procinfo.retdef) and
              ret_in_param(procinfo.retdef) and
              (pos(retstr,upper(s))>0) then
              procinfo.funcret_is_valid:=true;
           s:='';
         end;

     begin
       ende:=false;
       s:='';
       if assigned(procinfo.retdef) and
          (procinfo.retdef<>pdef(voiddef)) then
         retstr:=upper(tostr(procinfo.retoffset)+'('+att_reg2str[procinfo.framepointer]+')')
       else
         retstr:='';
       c:=asmgetchar;
         code:=new(paasmoutput,init);
         while not(ende) do
           begin
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
                                 if (s[length(s)]<>'%') and
                                   (s[length(s)]<>'$') then
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
                                                  if pos(',',s) > 0 then
                                                    procinfo.funcret_is_valid:=true;
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
  Revision 1.1  1998-03-25 11:18:15  root
  Initial revision

  Revision 1.13  1998/03/24 21:48:33  florian
    * just a couple of fixes applied:
         - problem with fixed16 solved
         - internalerror 10005 problem fixed
         - patch for assembler reading
         - small optimizer fix
         - mem is now supported

  Revision 1.12  1998/03/10 16:27:43  pierre
    * better line info in stabs debug
    * symtabletype and lexlevel separated into two fields of tsymtable
    + ifdef MAKELIB for direct library output, not complete
    + ifdef CHAINPROCSYMS for overloaded seach across units, not fully
      working
    + ifdef TESTFUNCRET for setting func result in underfunction, not
      working

  Revision 1.11  1998/03/10 01:17:26  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.10  1998/03/09 12:58:12  peter
    * FWait warning is only showed for Go32V2 and $E+
    * opcode tables moved to i386.pas/m68k.pas to reduce circular uses (and
      for m68k the same tables are removed)
    + $E for i386

  Revision 1.9  1998/03/06 00:52:51  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.8  1998/03/03 16:45:23  peter
    + message support for assembler parsers

  Revision 1.7  1998/03/02 01:49:14  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.6  1998/02/13 10:35:35  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.5  1998/02/07 18:01:27  carl
    + fwait warning for emulation

  Revision 1.3  1997/11/30 18:12:17  carl
  * bugfix of line numbering.

  Revision 1.2  1997/11/28 18:14:44  pierre
   working version with several bug fixes

  Revision 1.1.1.1  1997/11/27 08:33:00  michael
  FPC Compiler CVS start


  Pre-CVS log:

  History:
      19th october 1996:
         + created from old asmbl.pas
      13th october 1996:
         + renamed to radi386
}
