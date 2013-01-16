{
    Copyright (c) 2012 by the FPC development team

    Contains functionality to save/restore the global compiler state when
    switching between the compilation of different units.

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
unit globstat;

{$i fpcdefs.inc}

interface

uses
  globtype,tokens,globals,
  aasmdata,
  dbgbase,
  symbase,symsym,
  fmodule,
  scanner,scandir,
  procinfo;


type
  pglobalstate=^tglobalstate;
  tglobalstate=record
  { scanner }
    oldidtoken,
    oldtoken       : ttoken;
    oldtokenpos    : tfileposinfo;
    oldc           : char;
    oldpattern,
    oldorgpattern  : string;
    old_block_type : tblock_type;
  { symtable }
    oldsymtablestack,
    oldmacrosymtablestack : TSymtablestack;
    oldaktprocsym    : tprocsym;
  { cg }
    oldparse_only  : boolean;
  { akt.. things }
    oldcurrent_filepos      : tfileposinfo;
    old_current_module : tmodule;
    oldcurrent_procinfo : tprocinfo;
    old_settings : tsettings;
    old_switchesstatestack : tswitchesstatestack;
    old_switchesstatestackpos : Integer;

  { only saved/restored if "full" is true }
    old_asmdata : tasmdata;
    old_debuginfo : tdebuginfo;
    old_scanner : tscannerfile;
    old_parser_file : string;
  end;

procedure save_global_state(out state:tglobalstate;full:boolean);
procedure restore_global_state(const state:tglobalstate;full:boolean);

implementation

uses
  pbase;

  procedure save_global_state(out state:tglobalstate;full:boolean);
    begin
      with state do
        begin
          old_current_module:=current_module;

          { save symtable state }
          oldsymtablestack:=symtablestack;
          oldmacrosymtablestack:=macrosymtablestack;
          oldcurrent_procinfo:=current_procinfo;

          { save scanner state }
          oldc:=c;
          oldpattern:=pattern;
          oldorgpattern:=orgpattern;
          oldtoken:=token;
          oldidtoken:=idtoken;
          old_block_type:=block_type;
          oldtokenpos:=current_tokenpos;
          old_switchesstatestack:=switchesstatestack;
          old_switchesstatestackpos:=switchesstatestackpos;

          { save cg }
          oldparse_only:=parse_only;

          { save akt... state }
          { handle the postponed case first }
          //flushpendingswitchesstate;
          oldcurrent_filepos:=current_filepos;
          old_settings:=current_settings;

          if full then
            begin
              old_asmdata:=current_asmdata;
              old_debuginfo:=current_debuginfo;
              old_parser_file:=parser_current_file;
              old_scanner:=current_scanner;
            end;
        end;
    end;


  procedure restore_global_state(const state:tglobalstate;full:boolean);
    begin
      with state do
        begin
          { restore scanner }
          c:=oldc;
          pattern:=oldpattern;
          orgpattern:=oldorgpattern;
          token:=oldtoken;
          idtoken:=oldidtoken;
          current_tokenpos:=oldtokenpos;
          block_type:=old_block_type;
          switchesstatestack:=old_switchesstatestack;
          switchesstatestackpos:=old_switchesstatestackpos;

          { restore cg }
          parse_only:=oldparse_only;

          { restore symtable state }
          symtablestack:=oldsymtablestack;
          macrosymtablestack:=oldmacrosymtablestack;
          current_procinfo:=oldcurrent_procinfo;
          current_filepos:=oldcurrent_filepos;
          current_settings:=old_settings;

          if full then
            begin
              current_module:=old_current_module; {!}
              current_asmdata:=old_asmdata;
              current_debuginfo:=old_debuginfo;
              current_scanner:=old_scanner;
              parser_current_file:=old_parser_file;
            end;
        end;
    end;

end.

