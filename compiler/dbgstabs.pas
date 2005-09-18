{
    Copyright (c) 2003-2004 by Peter Vreman and Florian Klaempfl

    This units contains support for STABS debug info generation

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
unit dbgstabs;

{$i fpcdefs.inc}

interface

    uses
      DbgBase,
      aasmtai;

    type
      TDebugInfoStabs=class(TDebugInfo)
        procedure insertmodulestart(list:taasmoutput);override;
        procedure insertmoduleend(list:taasmoutput);override;
        procedure insertlineinfo(list:taasmoutput);override;
      end;

implementation

    uses
      cutils,
      systems,globals,
      aasmbase,
      finput,fmodule;

    const
      N_GSYM = $20;
      N_STSYM = 38;     { initialized const }
      N_LCSYM = 40;     { non initialized variable}
      N_Function = $24; { function or const }
      N_TextLine = $44;
      N_DataLine = $46;
      N_BssLine = $48;
      N_RSYM = $40;     { register variable }
      N_LSYM = $80;
      N_tsym = 160;
      N_SourceFile = $64;
      N_IncludeFile = $84;
      N_BINCL = $82;
      N_EINCL = $A2;
      N_EXCL  = $C2;

    procedure tdebuginfostabs.insertlineinfo(list:taasmoutput);
      var
        currfileinfo,
        lastfileinfo : tfileposinfo;
        currfuncname : pstring;
        currsectype  : tasmsectiontype;
        hlabel       : tasmlabel;
        hp : tai;
        infile : tinputfile;
      begin
        FillChar(lastfileinfo,sizeof(lastfileinfo),0);
        currfuncname:=nil;
        currsectype:=sec_code;
        hp:=Tai(list.first);
        while assigned(hp) do
          begin
            case hp.typ of
              ait_section :
                currsectype:=tai_section(hp).sectype;
              ait_function_name :
                currfuncname:=tai_function_name(hp).funcname;
              ait_force_line :
                lastfileinfo.line:=-1;
            end;

            if (currsectype=sec_code) and
               (hp.typ=ait_instruction) then
              begin
                currfileinfo:=tailineinfo(hp).fileinfo;
                { file changed ? (must be before line info) }
                if (currfileinfo.fileindex<>0) and
                   (lastfileinfo.fileindex<>currfileinfo.fileindex) then
                  begin
                    infile:=current_module.sourcefiles.get_file(currfileinfo.fileindex);
                    if assigned(infile) then
                      begin
                        objectlibrary.getlabel(hlabel,alt_dbgfile);
                        { emit stabs }
                        if (infile.path^<>'') then
                          list.insertbefore(Tai_stab.Create_str(stab_stabs,'"'+BsToSlash(FixPath(infile.path^,false))+'",'+tostr(n_includefile)+
                                            ',0,0,'+hlabel.name),hp);
                        list.insertbefore(Tai_stab.Create_str(stab_stabs,'"'+FixFileName(infile.name^)+'",'+tostr(n_includefile)+
                                          ',0,0,'+hlabel.name),hp);
                        list.insertbefore(tai_label.create(hlabel),hp);
                        { force new line info }
                        lastfileinfo.line:=-1;
                      end;
                  end;

                { line changed ? }
                if (lastfileinfo.line<>currfileinfo.line) and (currfileinfo.line<>0) then
                  begin
                     if assigned(currfuncname) and
                        (target_info.use_function_relative_addresses) then
                      begin
                        objectlibrary.getlabel(hlabel,alt_dbgline);
                        list.insertbefore(Tai_stab.Create_str(stab_stabn,tostr(n_textline)+',0,'+tostr(currfileinfo.line)+','+
                                          hlabel.name+' - '+currfuncname^),hp);
                        list.insertbefore(tai_label.create(hlabel),hp);
                      end
                     else
                      list.insertbefore(Tai_stab.Create_str(stab_stabd,tostr(n_textline)+',0,'+tostr(currfileinfo.line)),hp);
                  end;
                lastfileinfo:=currfileinfo;
              end;

            hp:=tai(hp.next);
          end;
      end;


    procedure tdebuginfostabs.insertmodulestart(list:taasmoutput);
      var
        hlabel : tasmlabel;
        infile : tinputfile;
        templist : taasmoutput;
      begin
        { emit main source n_sourcefile }
        objectlibrary.getlabel(hlabel,alt_dbgfile);
        infile:=current_module.sourcefiles.get_file(1);
        templist:=taasmoutput.create;
        new_section(templist,sec_code,'',0);
        if (infile.path^<>'') then
          templist.concat(Tai_stab.Create_str(stab_stabs,'"'+BsToSlash(FixPath(infile.path^,false))+'",'+tostr(n_sourcefile)+
                      ',0,0,'+hlabel.name));
        templist.concat(Tai_stab.Create_str(stab_stabs,'"'+FixFileName(infile.name^)+'",'+tostr(n_sourcefile)+
                    ',0,0,'+hlabel.name));
        templist.concat(tai_label.create(hlabel));
        list.insertlist(templist);
        templist.free;
      end;


    procedure tdebuginfostabs.insertmoduleend(list:taasmoutput);
      var
        hlabel : tasmlabel;
        templist : taasmoutput;
      begin
        { emit empty n_sourcefile }
        objectlibrary.getlabel(hlabel,alt_dbgfile);
        templist:=taasmoutput.create;
        new_section(templist,sec_code,'',0);
        templist.concat(Tai_stab.Create_str(stab_stabs,'"",'+tostr(n_sourcefile)+',0,0,'+hlabel.name));
        templist.concat(tai_label.create(hlabel));
        list.insertlist(templist);
        templist.free;
      end;


    const
      dbg_stabs_info : tdbginfo =
         (
           id     : dbg_stabs;
           idtxt  : 'STABS';
         );

initialization
  RegisterDebugInfo(dbg_stabs_info,TDebugInfoStabs);
end.
