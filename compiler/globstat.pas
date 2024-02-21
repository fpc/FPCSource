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

  tglobalstate = class
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
    old_verbosity : longint;

  { only saved/restored if "full" is true }
    old_asmdata : tasmdata;
    old_debuginfo : tdebuginfo;
    old_scanner : tscannerfile;
    old_parser_file : string;
    constructor create(savefull : boolean);
    destructor destroy; override;
    procedure clearscanner;
    class procedure remove_scanner_from_states(scanner : tscannerfile); static;
    procedure save(full : boolean);
    procedure restore(full : boolean);
  end;

procedure save_global_state(state:tglobalstate;full:boolean);
procedure restore_global_state(state:tglobalstate;full:boolean);

implementation

uses
  switches, verbose, pbase,comphook;

var
  states : array of tglobalstate;
  statecount : integer = 0;



  class procedure tglobalstate.remove_scanner_from_states(scanner : tscannerfile);

  var
    i : integer;

  begin
    for I:=0 to statecount-1 do
      if (states[i].old_scanner=scanner) then
        states[i].clearscanner;
  end;

  procedure addstate(astate : tglobalstate);

  var
    l : integer;

  begin
    l:=length(states);
    if l=statecount then
      setlength(states,l+10);
    states[statecount]:=astate;
    inc(statecount);
  end;

  procedure removestate(astate : tglobalstate);

  var
    l : integer;

  begin
    l:=statecount-1;
    While (l>=0) and (states[l]<>astate) do
      dec(l);
    if l<0 then
      exit;
    if l<>statecount-1 then
      states[l]:=states[statecount-1];
    states[statecount-1]:=Nil;
    Dec(Statecount);
  end;

  procedure save_global_state(state:tglobalstate;full:boolean);
    begin
      state.save(full);
    end;

  procedure restore_global_state(state:tglobalstate;full:boolean);

  begin
    state.restore(full);
  end;

  procedure tglobalstate.save(full: boolean);

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
      {
        consuming the semicolon after a uses clause can add to the
        pending state if the first directives change warning state.
        So we must flush before context switch. See for example:
        ppcgen/cgppc.pas
        line 144 has a WARN 6018 OFF...
      }
      flushpendingswitchesstate;
      old_switchesstatestack:=switchesstatestack;
      old_switchesstatestackpos:=switchesstatestackpos;

      { save cg }
      oldparse_only:=parse_only;

      { save akt... state }
      { handle the postponed case first }
      oldcurrent_filepos:=current_filepos;
      old_settings:=current_settings;
      old_verbosity:=status.verbosity;

      if full then
        begin
          old_asmdata:=current_asmdata;
          old_debuginfo:=current_debuginfo;
          old_parser_file:=parser_current_file;
          old_scanner:=current_scanner;
        end;
    end;

  procedure tglobalstate.restore(full: boolean);

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
      status.verbosity:=old_verbosity;
      { restore message settings which were recorded prior to unit switch }

      RestoreLocalVerbosity(current_settings.pmessage);

      if full then
        begin
          set_current_module(old_current_module);
          // These can be different
          current_asmdata:=old_asmdata;
          current_debuginfo:=old_debuginfo;
        end;
    end;

    constructor tglobalstate.create(savefull: boolean);

    begin
      addstate(self);
      save(savefull);
    end;

  destructor tglobalstate.destroy;

    begin
      removestate(self);
      inherited destroy;
    end;

  procedure tglobalstate.clearscanner;

  begin
    old_scanner:=nil;
    oldidtoken:=NOTOKEN;
    oldtoken:=NOTOKEN;
    oldtokenpos:=Default(tfileposinfo);
    oldc:=#0;
    oldpattern:='';
    oldorgpattern:='';
    old_block_type:=bt_none;
  end;

initialization
  onfreescanner:=@tglobalstate.remove_scanner_from_states;
finalization
  onfreescanner:=Nil;
end.

