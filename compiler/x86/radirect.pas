{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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
       symconst,symbase,symtype,symsym,symtable,defutil,paramgr,
       { pass 1 }
       nbas,
       { parser }
       scanner,
       rax86,
       { codegen }
       cginfo,cgbase,
       { constants }
       itx86att,
       cpubase
       ;

    function assemble : tnode;

      var
         uhs,
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
       s:='';
       if assigned(current_procinfo.procdef.funcretsym) and
          is_fpu(current_procinfo.procdef.rettype.def) then
         tvarsym(current_procinfo.procdef.funcretsym).varstate:=vs_assigned;
       framereg:=current_procinfo.framepointer;
       if (not is_void(current_procinfo.procdef.rettype.def)) then
         retstr:=upper(tostr(tvarsym(current_procinfo.procdef.funcretsym).adjusted_address)+'('+gas_regname(framereg)+')')
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
                          if assigned(current_procinfo.procdef) then
                            begin
                               { is the last written character an special }
                               { char ?                                   }
                               if (s[length(s)]='%') and
                                  (not paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption)) and
                                  ((pos('AX',upper(hs))>0) or
                                  (pos('AL',upper(hs))>0)) then
                                 tvarsym(current_procinfo.procdef.funcretsym).varstate:=vs_assigned;
                               if (s[length(s)]<>'%') and
                                 (s[length(s)]<>'$') and
                                 (s[length(s)]<>'.') and
                                 ((s[length(s)]<>'0') or (hs[1]<>'x')) then
                                 begin
                                    if assigned(current_procinfo.procdef.localst) and
                                       (current_procinfo.procdef.localst.symtablelevel>=normal_function_level) then
                                      sym:=tsym(current_procinfo.procdef.localst.search(upper(hs)))
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
                                                 '('+gas_regname(framereg)+')';
                                           end
                                         else
                                         { call to local function }
                                         if (sym.typ=procsym) and ((pos('CALL',upper(s))>0) or
                                            (pos('LEA',upper(s))>0)) then
                                           begin
                                              hs:=tprocsym(sym).first_procdef.mangledname;
                                           end;
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
                                                   hs:=tostr(l)+'('+gas_regname(framereg)+')';
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
                                         uhs:=upper(hs);
                                         if (uhs='__SELF') then
                                           begin
                                             if assigned(current_procinfo.procdef._class) then
                                              uhs:='self'
                                             else
                                              begin
                                                Message(asmr_e_cannot_use_SELF_outside_a_method);
                                                uhs:='';
                                              end;
                                           end
                                         else
                                          if (uhs='__OLDEBP') then
                                           begin
                                             if current_procinfo.procdef.parast.symtablelevel>normal_function_level then
                                              uhs:='parentframe'
                                             else
                                              begin
                                                Message(asmr_e_cannot_use_OLDEBP_outside_nested_procedure);
                                                uhs:='';
                                              end;
                                           end
                                         else
                                          if uhs='__RESULT' then
                                           begin
                                             if (not is_void(current_procinfo.procdef.rettype.def)) then
                                              uhs:='result'
                                             else
                                              begin
                                                Message(asmr_e_void_function);
                                                uhs:='';
                                              end;
                                           end;

                                         if uhs<>'' then
                                           searchsym(uhs,sym,srsymtable)
                                         else
                                           sym:=nil;
                                         if assigned(sym) then
                                          begin
                                            case sym.owner.symtabletype of
                                              globalsymtable,
                                              staticsymtable :
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
                                                           if tprocsym(sym).procdef_count>1 then
                                                             Message1(asmr_w_direct_global_is_overloaded_func,hs);
                                                           Message2(asmr_h_direct_global_to_mangled,hs,tprocsym(sym).first_procdef.mangledname);
                                                           hs:=tprocsym(sym).first_procdef.mangledname;
                                                         end;
                                                      end;
                                                    else
                                                      Message(asmr_e_wrong_sym_type);
                                                  end;
                                                end;
                                              parasymtable,
                                              localsymtable :
                                                begin
                                                  case sym.typ of
                                                    varsym :
                                                      begin
                                                        hs:=tostr(tvarsym(sym).adjusted_address)+
                                                            '('+gas_regname(framereg)+')';
                                                        inc(tvarsym(sym).refs);
                                                      end;
                                                    typedconstsym :
                                                      begin
                                                        Message2(asmr_h_direct_global_to_mangled,hs,ttypedconstsym(sym).mangledname);
                                                        hs:=ttypedconstsym(sym).mangledname;
                                                      end;
                                                    else
                                                      Message(asmr_e_wrong_sym_type);
                                                  end;
                                                end;
                                             end;
                                           end
                                         end;
                                      end;
                                 end;
                            end;
                          s:=s+hs;
                       end;
                end;
              '{',';',#10,#13 :
                begin
                  if pos(retstr,s) > 0 then
                    tvarsym(current_procinfo.procdef.funcretsym).varstate:=vs_assigned;
                  writeasmline;
                  c:=current_scanner.asmgetchar;
                end;
              #26 :
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
{$ifdef x86_64}
  asmmode_x86_64_direct_info : tasmmodeinfo =
          (
            id    : asmmode_direct;
            idtxt : 'DIRECT'
          );
{$else x86_64}
  asmmode_i386_direct_info : tasmmodeinfo =
          (
            id    : asmmode_direct;
            idtxt : 'DIRECT'
          );
{$endif x86_64}

initialization
{$ifdef x86_64}
  RegisterAsmMode(asmmode_x86_64_direct_info);
{$else x86_64}
  RegisterAsmMode(asmmode_i386_direct_info);
{$endif x86_64}
end.
{
  $Log$
  Revision 1.8  2003-09-03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.7.2.1  2003/08/27 21:06:34  peter
    * more updates

  Revision 1.7  2003/06/13 21:19:33  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.6  2003/06/02 21:42:05  jonas
    * function results can now also be regvars
    - removed tprocinfo.return_offset, never use it again since it's invalid
      if the result is a regvar

  Revision 1.5  2003/05/22 21:33:31  peter
    * removed some unit dependencies

  Revision 1.4  2003/05/15 18:58:54  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.3  2003/05/13 19:15:28  peter
    * removed radirect

  Revision 1.2  2003/05/01 07:59:43  florian
    * introduced defaultordconsttype to decribe the default size of ordinal constants
      on 64 bit CPUs it's equal to cs64bitdef while on 32 bit CPUs it's equal to s32bitdef
    + added defines CPU32 and CPU64 for 32 bit and 64 bit CPUs
    * int64s/qwords are allowed as for loop counter on 64 bit CPUs

  Revision 1.1  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.11  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.10  2003/04/27 07:29:52  peter
    * current_procinfo.procdef cleanup, current_procinfo.procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.9  2003/04/25 20:59:35  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.8  2003/04/25 12:04:31  florian
    * merged agx64att and ag386att to x86/agx86att

  Revision 1.7  2003/04/21 20:05:10  peter
    * removed some ie checks

  Revision 1.6  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.5  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.4  2002/11/18 17:32:00  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.3  2002/09/03 16:26:28  daniel
    * Make Tprocdef.defs protected

  Revision 1.2  2002/08/17 09:23:47  florian
    * first part of procinfo rewrite

  Revision 1.1  2002/08/10 14:47:50  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.21  2002/07/20 11:58:05  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.20  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.19  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.18  2002/05/18 13:34:26  peter
    * readded missing revisions

  Revision 1.17  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.15  2002/05/12 16:53:18  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.14  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.13  2002/04/14 17:01:52  carl
  + att_reg2str -> gas_reg2str

}
