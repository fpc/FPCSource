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
  globtype,globals,compilerbase,
  aasmdata,
  dbgbase,
  symbase,symsym,
  fmodule,
  scanner,scandir,
  procinfo;


type
  TSymTableStackKind = (
    stsk_global,
    stsk_macro
    );

  { tglobalstate }
  { Note: this is only needed for pas modules, ppu modules simply skip save and restore
    Especially a ppu does not create its own symtablestack }

  tglobalstate = class
    compiler : TCompilerBase;
    reload: boolean;
  { scanner }
    oldtokenpos    : tfileposinfo;
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
    constructor create(for_module_switch: boolean;acompiler: TCompilerBase);
    destructor destroy; override;
    procedure clearscanner;
    class procedure remove_scanner_from_states(scanner : tscannerfile); static;
    procedure save(for_module_switch: boolean = false);
    procedure save_symtable_stack(stack: TSymtablestack; kind: TSymTableStackKind);
    procedure restore;
    procedure reload_symtable_stack(stack: TSymtablestack; kind: TSymTableStackKind);
    class procedure clear_state(_compiler: TCompilerBase);
  end;

procedure save_global_state(state:tglobalstate);
procedure restore_global_state(state:tglobalstate);

implementation

uses
  switches, verbose, pbase,comphook,compiler;

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

  procedure save_global_state(state:tglobalstate);
    begin
      state.save;
    end;

  procedure restore_global_state(state:tglobalstate);

    begin
      state.restore;
    end;

  procedure tglobalstate.save(for_module_switch: boolean);

    begin
      reload:=for_module_switch;
      old_current_module:=compiler.current_module;

      { save symtable state }
      if compiler.current_module.fromppu then
        begin
          { a ppu does not use the symbolstacks
            they might have been set by a pas module loading this ppu
            => do not store them in the the ppu state }
          oldsymtablestack:=nil;
          oldmacrosymtablestack:=nil;
        end
      else
        begin
          { a pas module has symbolstacks, which contain references to other modules
            when switching between modules, the references must be updated as the
            used module might have been recompiled }
          oldsymtablestack:=compiler.symtablestack;
          if for_module_switch then
            save_symtable_stack(oldsymtablestack,stsk_global);
          oldmacrosymtablestack:=compiler.macrosymtablestack;
          if for_module_switch then
            save_symtable_stack(oldmacrosymtablestack,stsk_macro);
        end;
      oldcurrent_procinfo:=current_procinfo;

      { save scanner state }
      old_block_type:=compiler.globals.block_type;
      oldtokenpos:=compiler.globals.current_tokenpos;
      {
        consuming the semicolon after a uses clause can add to the
        pending state if the first directives change warning state.
        So we must flush before context switch. See for example:
        ppcgen/cgppc.pas
        line 144 has a WARN 6018 OFF...
      }
      flushpendingswitchesstate; { flushpendingswitchesstate before storing compiler.globals.current_settings.pmessage, switchesstatestack etc }
      old_switchesstatestack:=switchesstatestack;
      old_switchesstatestackpos:=switchesstatestackpos;

      { save cg }
      oldparse_only:=compiler.parser.pbase.parse_only;

      { save akt... state }
      { handle the postponed case first }
      oldcurrent_filepos:=compiler.globals.current_filepos;
      old_settings:=compiler.globals.current_settings.ToRecord;
      old_verbosity:=status.verbosity;

      old_asmdata:=current_asmdata;
      old_debuginfo:=current_debuginfo;
      old_parser_file:=compiler.globals.parser_current_file;
      old_scanner:=current_scanner;
    end;

  procedure tglobalstate.save_symtable_stack(stack: TSymtablestack; kind: TSymTableStackKind);
    var
      item: psymtablestackitem;
      m: tmodule;
      id: LongInt;
    begin
      {$IFDEF DEBUG_SAVESYMSTACK}
      writeln('DEBUG_SAVESYMSTACK: tglobalstate.save_symtable_stack ',compiler.current_module.modulename^,' ',compiler.current_module.statestr,' ',kind,' Stack=',hexstr(ptruint(stack),16),' self=',hexstr(ptruint(self),16));
      {$ENDIF}
      if stack=nil then exit;

      item:=stack.stack;
      while item<>nil do
        begin
          id:=item^.symtable.moduleid;
          item^.saved_moduleid:=id;
          if (id<>compiler.current_module.moduleid) and (id>0) then
            begin
              m:=get_module(id);
              {$IFDEF DEBUG_SAVESYMSTACK}
              writeln('   ',m.modulename^,' ',m.statestr);
              {$ENDIF}
              if m=nil then
                begin
                  writeln('tglobalstate.save_symtable_stack ',compiler.current_module.modulename^,' ',compiler.current_module.statestr,' ',kind,' unknown moduleid: ',id);
                  Internalerror(2026030103);
                end;
              case kind of
                stsk_global:
                  if m.globalsymtable<>item^.symtable then
                    begin
                      writeln('tglobalstate.save_symtable_stack ',compiler.current_module.modulename^,' ',compiler.current_module.statestr,' globalsymstack: item is not globalsymtable of ', m.modulename^,' ',m.statestr);
                      Internalerror(2026030101);
                    end;
                stsk_macro:
                  if m.globalmacrosymtable<>item^.symtable then
                    begin
                      writeln('tglobalstate.save_symtable_stack ',compiler.current_module.modulename^,' ',compiler.current_module.statestr,' globalmacrosymstack: item is not globalmacrosymtable of ', m.modulename^,' ',m.statestr);
                      Internalerror(2026030102);
                    end;
              end;
            end;
          item:=item^.next;
        end;
      {$IFDEF DEBUG_SAVESYMSTACK}
      writeln('DEBUG_SAVESYMSTACK: END tglobalstate.save_symtable_stack ',compiler.current_module.modulename^,' ',compiler.current_module.statestr,' ',kind,' Stack=',hexstr(ptruint(stack),16),' self=',hexstr(ptruint(self),16));
      {$ENDIF}
    end;

  procedure tglobalstate.restore;

    begin
      set_current_module(old_current_module);

      { restore scanner }
      compiler.globals.current_tokenpos:=oldtokenpos;
      compiler.globals.block_type:=old_block_type;
      switchesstatestack:=old_switchesstatestack;
      switchesstatestackpos:=old_switchesstatestackpos;

      { restore cg }
      compiler.parser.pbase.parse_only:=oldparse_only;

      { restore symtable state }
      tcompiler(compiler).symtablestack:=oldsymtablestack;
      if reload then
        reload_symtable_stack(compiler.symtablestack,stsk_global);
      tcompiler(compiler).macrosymtablestack:=oldmacrosymtablestack;
      if reload then
        reload_symtable_stack(compiler.macrosymtablestack,stsk_macro);
      current_procinfo:=oldcurrent_procinfo;
      compiler.globals.current_filepos:=oldcurrent_filepos;
      compiler.globals.current_settings.CreateFromRecord(old_settings);
      status.verbosity:=old_verbosity;

      { restore message settings which were recorded prior to unit switch }
      compiler.verbose.RestoreLocalVerbosity(compiler.globals.current_settings.pmessage);

      // These can be different
      current_asmdata:=old_asmdata;
      current_debuginfo:=old_debuginfo;
    end;

  procedure tglobalstate.reload_symtable_stack(stack: TSymtablestack; kind: TSymTableStackKind);
    var
      item: psymtablestackitem;
      m: tmodule;
      id: LongInt;
    begin
      {$IFDEF DEBUG_SAVESYMSTACK}
      writeln('DEBUG_SAVESYMSTACK: tglobalstate.reload_symtable_stack ',old_current_module.modulename^,' ',old_current_module.statestr,' ',kind,' Stack=',hexstr(ptruint(stack),16),' self=',hexstr(ptruint(self),16));
      {$ENDIF}
      if stack=nil then exit;
      if old_current_module.fromppu then
        begin
          { ppu does not have its own symtablestack }
          writeln('tglobalstate.reload_symtable_stack ',old_current_module.modulename^,' ',old_current_module.statestr);
          Internalerror(2026030106);
        end;
      item:=stack.stack;
      while item<>nil do
        begin
          id:=item^.saved_moduleid;
          if (id<>old_current_module.moduleid) and (id>0) then
            begin
              m:=get_module(id);
              {$IFDEF DEBUG_SAVESYMSTACK}
              writeln('  ',m.modulename^,' ',m.statestr,' HasGlobalSymTable=',m.globalsymtable<>nil);
              {$ENDIF}
              case kind of
                stsk_global: item^.symtable:=m.globalsymtable;
                stsk_macro:  item^.symtable:=m.globalmacrosymtable;
              end;
            end;
          item:=item^.next;
        end;
    end;

  class procedure tglobalstate.clear_state(_compiler: TCompilerBase);
    begin
      tcompiler(_compiler).symtablestack:=nil;
      tcompiler(_compiler).macrosymtablestack:=nil;
      current_procinfo:=nil;

      _compiler.globals.block_type:=bt_none;
      flushpendingswitchesstate;
      switchesstatestack:=default(tswitchesstatestack);
      switchesstatestackpos:=0;

      // keep "compiler.globals.current_settings"

      _compiler.parser.pbase.parse_only:=false;
      current_asmdata:=nil;
      current_debuginfo:=nil;

      _compiler.globals.parser_current_file:='';
      set_current_scanner(nil);
    end;

  constructor tglobalstate.create(for_module_switch: boolean;acompiler: TCompilerBase);

    begin
      compiler:=acompiler;
      addstate(self);
      save(for_module_switch);
    end;

  destructor tglobalstate.destroy;

    begin
      {$IFDEF DEBUG_SAVESYMSTACK}
      writeln('DEBUG_SAVESYMSTACK: tglobalstate.destroy Stack=',hexstr(ptruint(symtablestack),16),' self=',hexstr(ptruint(self),16),' reload=',reload);
      {$ENDIF}
      removestate(self);
      inherited destroy;
    end;

  procedure tglobalstate.clearscanner;

  begin
    old_scanner:=nil;
    oldtokenpos:=Default(tfileposinfo);
    old_block_type:=bt_none;
  end;

initialization
  onfreescanner:=@tglobalstate.remove_scanner_from_states;
finalization
  onfreescanner:=Nil;
end.

