{*****************************************************************************}
{ File                   : radirect.pas                                       }
{ Author                 : Mazen NEIFER                                       }
{ Project                : Free Pascal Compiler (FPC)                         }
{ Creation date          : 2002\08\22                                         }
{ Last modification date : 2002\08\22                                         }
{ Licence                : GPL                                                }
{ Bug report             : mazen.neifer.01@supaero.org                        }
{*****************************************************************************}
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

 ****************************************************************************}
unit radirect;
{$MACRO ON}{$i fpcdefs.inc}
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
       rautils,
       { codegen }
       cgbase,
       { constants }
       aggas,cpubase,globtype
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
           if assigned(current_procdef.funcretsym) and
              (pos(retstr,upper(s))>0) then
             tvarsym(current_procdef.funcretsym).varstate:=vs_assigned;
           s:='';
         end;

     begin
       ende:=false;
       s:='';
       if assigned(current_procdef.funcretsym) and
          is_fpu(current_procdef.rettype.def) then
         tvarsym(current_procdef.funcretsym).varstate:=vs_assigned;
       framereg:=current_procinfo.framepointer;
       convert_register_to_enum(framereg);
       if (not is_void(current_procdef.rettype.def)) then
         retstr:=upper(tostr(current_procinfo.return_offset)+'('+std_reg2str[framereg.enum]+')')
       else
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
                                 begin
                                    hs:=tlabelsym(srsym).lab.name;
                                    tlabelsym(srsym).lab.is_set:=true;
                                 end
                               else
                                 Message(asmr_w_using_defined_as_local);
                           end
                         else
                         { access to local variables }
                         if assigned(current_procdef) then
                           begin
                              { is the last written character an special }
                              { char ?                                   }
                              if (s[length(s)]='%') and
                                 (not paramanager.ret_in_param(current_procdef.rettype.def,current_procdef.proccalloption)) and
                                 ((pos('AX',upper(hs))>0) or
                                 (pos('AL',upper(hs))>0)) then
                                tvarsym(current_procdef.funcretsym).varstate:=vs_assigned;
                              if (s[length(s)]<>'%') and
                                (s[length(s)]<>'$') and
                                ((s[length(s)]<>'0') or (hs[1]<>'x')) then
                                begin
                                   if assigned(current_procdef.localst) and
                                      (current_procdef.localst.symtablelevel >= normal_function_level) then
                                     sym:=tsym(current_procdef.localst.search(upper(hs)))
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
                                                '('+std_reg2str[current_procinfo.framepointer.enum]+')';
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
                                        if assigned(current_procdef.parast) then
                                          sym:=tsym(current_procdef.parast.search(upper(hs)))
                                        else
                                          sym:=nil;
                                        if assigned(sym) then
                                          begin
                                             if sym.typ=varsym then
                                               begin
                                                  l:=tvarsym(sym).address;
                                                  { set offset }
                                                  inc(l,current_procdef.parast.address_fixup);
                                                  hs:=tostr(l)+'('+std_reg2str[current_procinfo.framepointer.enum]+')';
                                                  if pos(',',s) > 0 then
                                                    tvarsym(sym).varstate:=vs_used;
                                               end;
                                          end
                                     end
                                end
                              else
                                begin
                                   uhs:=upper(hs);
                                   if (uhs='__SELF') then
                                     begin
                                       if assigned(current_procdef._class) then
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
                                        if current_procdef.parast.symtablelevel>normal_function_level then
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
                                          if (not is_void(current_procdef.rettype.def)) then
                                           uhs:='result'
                                          else
                                           begin
                                             Message(asmr_e_void_function);
                                             uhs:='';
                                           end;
                                        end;

                                    if uhs<>'' then
                                      searchsym(upper(hs),sym,srsymtable)
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
                                                        '('+std_reg2str[framereg.enum]+')';
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
                                      end;
                                end;
                           end;
                         s:=s+hs;
                      end;
                 end;
               '{',';',#10,#13:
                 begin
                    if pos(retstr,s) > 0 then
                      tvarsym(current_procdef.funcretsym).varstate:=vs_assigned;
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
  asmmode_sparc_direct_info : tasmmodeinfo =
          (
            id    : asmmode_direct;
            idtxt : 'DIRECT'
          );

initialization
  RegisterAsmMode(asmmode_sparc_direct_info);
end.
{
  $Log$
  Revision 1.10  2003-05-23 22:33:48  florian
    * fix some small flaws which prevent sparc linux system unit from compiling
    * some reformatting done

  Revision 1.9  2003/05/23 21:10:50  florian
    * fixed sparc compiler compilation

  Revision 1.8  2003/05/22 16:11:22  florian
    * fixed sparc compilation partially

  Revision 1.7  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.6  2003/04/27 07:48:05  peter
    * updated for removed lexlevel

  Revision 1.5  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.4  2002/11/25 17:43:29  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.3  2002/11/18 17:32:01  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.2  2002/09/19 20:24:47  mazen
  + call support

  Revision 1.1  2002/08/23 10:08:28  mazen
  *** empty log message ***

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
  + att_Reg2str -> std_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.13  2002/04/14 17:01:52  carl
  + att_reg2str -> std_reg2str

}
