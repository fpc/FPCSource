{
    Copyright (c) 2024- by Michael Van Canneyt

    This unit handles the compiler tasks.

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

unit ctask;

{$mode ObjFPC}

{ $DEFINE DEBUG_CTASK}

interface

uses
  finput, fmodule, cclasses, globals, globstat;

type
  { ttask

    About state:
      Contains scanner/parser position needed for compiling pascal sources,
      irrelevant for loading ppu(s).
      It is restored before continuing and saved afterwards (if unfinished).
      Loading ppu files works recursively and stops when a unit requires (re)compile,
      A recompile discards the saved state the adds the module to ctask.
      When the recursion steps back, leaving the current unit unfinished the state is saved,
      so ctask can continue with another unit.
  }

  ttask = class
    module : tmodule;
    state : tglobalstate;
    prev, next: ttask;
    constructor create(m : tmodule);
    destructor destroy; override;
    procedure SaveState;
    procedure RestoreState;
    procedure DiscardState;
  end;

  { ttask_handler }

  ttask_handler = class
  private
    taskcount: integer;
    firsttask: ttask;
    function createtask(m: tmodule): ttask;
    procedure freetask(t: ttask);
    procedure finishmodule(m: tmodule);
    procedure queuemodule(m: tmodule);
    function restore_state(m: tmodule): ttask;
    function reload_module(m: tmodule): ttask;
    function recompile_module(m: tmodule): ttask;
    procedure update_circular_unit_groups;
    procedure search_finished_scc(m: tmodule{$IFDEF DEBUG_PPU_CYCLES}; const Indent: string = ''{$ENDIF});
    function find_unfinished_leaf_scc(m: tmodule): tmodule;
    function is_uses_waiting(m: tmodule; uu: tused_unit): boolean;
    function check_do_reload_cycle(scc_root: tmodule): boolean;
    function check_crc_mismatches(scc_root: tmodule): boolean;
    function check_cycle_wait_for_pas(scc_root: tmodule): boolean;
    function recompile_pending(scc_root: tmodule): boolean;
    procedure recompile_scc(scc_root: tmodule);
  public
    constructor create;
    destructor destroy; override;
    { Find the task for module m }
    function findtask(m : tmodule; autocreate: boolean = false) : ttask;
    // Can we continue processing this module ? If not, firstwaiting contains first module that m is waiting for.
    function cancontinue(m : tmodule; out firstwaiting: tmodule): boolean;
    { Overload of cancontinue, based on task. }
    function cancontinue(t: ttask; out firstwaiting: tmodule): boolean; inline;
    { Continue processing this module. Return true if the module is done and can be removed. }
    function continue_module(m : tmodule): Boolean;
    { process the queue. Note that while processing the queue, elements will be added. }
    procedure processqueue;
    { add a module to the queue. If a module is already in the queue, we do not add it again. }
    procedure addmodule(m : tmodule);
    { write current scc and what is waiting for what }
    procedure write_scc;
    { get number of modules in a scc }
    function get_scc_count(scc_root: tmodule): integer;
    function has_scc_another_unfinished_module(m: tmodule): boolean;
  end;


var
  task_handler : TTask_handler;

procedure InitTaskHandler;
procedure DoneTaskHandler;

implementation

uses
  verbose, fppu, sysutils,
  scanner, parser, pmodules, symbase;

procedure InitTaskHandler;
begin
  task_handler:=ttask_handler.create;
end;

procedure DoneTaskHandler;
begin
  freeandnil(task_handler);
end;

{ ttask }

constructor ttask.create(m: tmodule);
begin
  inherited create;
  module:=m;
  m.task:=self;
  state:=nil;
end;

destructor ttask.destroy;
begin
  { the module might already be freed! }
  DiscardState;
  Inherited;
end;

procedure ttask.DiscardState;
begin
  FreeAndNil(state);
end;

procedure ttask.SaveState;
begin
  if module.fromppu then exit;
  set_current_module(module);
  if State=Nil then
    State:=tglobalstate.Create(true)
  else
    State.save(true);
end;

procedure ttask.RestoreState;
begin
  if module.fromppu then exit;
  if module.is_reset then
    begin
      writeln('ttask.RestoreState is_reset ',module.modulename^,' ',module.statestr);
      Internalerror(2026030105);
    end;
  if state=nil then
    begin
      writeln('ttask.RestoreState state=nil ',module.modulename^,' ',module.statestr);
      Internalerror(2026030106);
    end;
  state.restore;
  if assigned(current_scanner) and assigned(current_scanner.inputfile) then
    if current_scanner.inputfile.closed then
      begin
        current_scanner.tempopeninputfile;
        current_scanner.gettokenpos;
      end;
end;

{ ttask_handler }

constructor ttask_handler.create;
begin
  tmodule.finish_module:=@finishmodule;
end;

destructor ttask_handler.destroy;
begin
  while firsttask<>nil do
    freetask(firsttask);
  inherited destroy;
end;

function ttask_handler.findtask(m: tmodule; autocreate: boolean): ttask;
begin
  Result:=ttask(m.task);
  if (Result=nil) and autocreate then
    begin
      addmodule(m);
      Result:=ttask(m.task);
    end;
end;

function ttask_handler.createtask(m: tmodule): ttask;
begin
  Result:=ttask.create(m);
  if firsttask<>nil then
    begin
      firsttask.next:=Result;
      Result.prev:=firsttask;
    end;
  firsttask:=Result;
  inc(taskcount);
end;

procedure ttask_handler.freetask(t: ttask);
begin
  { the module might already be freed! }
  if firsttask=t then
    firsttask:=t.next;
  if t.next<>nil then
    t.next.prev:=t.prev;
  if t.prev<>nil then
    t.prev.next:=t.next;
  t.free;
  dec(taskcount);
end;

procedure ttask_handler.finishmodule(m: tmodule);
{ finish loading special units added by the compiler itself, e.g. variants.ppu
  No reloads, no recompiles }

  function find_cancontinue(root: tmodule): tmodule;
  var
    uu: tused_unit;
    firstwaiting: tmodule;
  begin
    Result:=nil;
    if root.cycle_search_stamp=tmodule.cycle_stamp then exit;
    root.cycle_search_stamp:=tmodule.cycle_stamp;

    if root.do_recompile then
      begin
        // e.g. system.ppu not found
        tppumodule(root).check_sources_for_recompile;
        writeln('ttask_handler.finishmodule ',root.modulename^,' ',root.statestr);
        Internalerror(2026032615);
      end
    else if not (root.state in [ms_registered,ms_load,ms_compiled,ms_processed]) then
      begin
        writeln('ttask_handler.finishmodule ',root.modulename^,' ',root.statestr);
        Internalerror(2026030401);
      end;

    { finish used units first }
    uu:=tused_unit(root.used_units.First);
    while assigned(uu) do
      begin
        Result:=find_cancontinue(uu.u);
        if assigned(Result) then exit;
        uu:=tused_unit(uu.Next);
      end;

    if not (root.state in [ms_compiled,ms_processed])
        and cancontinue(root,firstwaiting) then
      exit(root);
  end;

  procedure find_unfinished(root: tmodule);
  { find unfinished unit(s) and write message }
  var
    uu: tused_unit;
    um: tmodule;
  begin
    if root.cycle_search_stamp=tmodule.cycle_stamp then exit;
    root.cycle_search_stamp:=tmodule.cycle_stamp;

    if root.state in [ms_compiled,ms_processed] then
      begin
        uu:=tused_unit(root.used_units.First);
        while assigned(uu) do
          begin
            find_unfinished(uu.u);
            uu:=tused_unit(uu.Next);
          end;
        exit;
      end;

    { check for invalid unit cycle }
    uu:=tused_unit(root.used_units.First);
    while assigned(uu) and (uu.u.state in [ms_compiled,ms_processed]) do
      uu:=tused_unit(uu.Next);
    if uu<>nil then
      Message2(unit_f_circular_unit_reference,root.realmodulename^,uu.u.realmodulename^);

    if root.waitingforunit<>nil then
      begin
        um:=tmodule(root.waitingforunit[0]);
        Message2(unit_f_circular_unit_reference,root.realmodulename^,um.realmodulename^);
      end;

    writeln('CTASK: cannot finish module ',root.realmodulename^);
    Internalerror(2026030301);
  end;

var
  next: tmodule;
  state: tglobalstate;
begin
  if m.scc_finished then exit;

  state:=nil;
  repeat
    tmodule.increase_cycle_stamp;
    next:=find_cancontinue(m);
    if next=nil then break;
    if state=nil then
      state:=tglobalstate.create(true);
    {$IF defined(DEBUG_CTASK) or defined(Debug_FreeParseMem)}Writeln('CTASK-finish: continuing ',next.ToString,' state=',next.statestr,' total-units=',loaded_units.Count,' tasks=',taskcount);{$ENDIF}
    continue_module(next);
  until false;

  tmodule.increase_cycle_stamp;
  find_unfinished(m);

  if state<>nil then
    begin
      state.restore;
      state.free;
    end;
end;

procedure ttask_handler.queuemodule(m: tmodule);
var
  t: ttask;
begin
  addmodule(m);
  t:=findtask(m);
  t.SaveState;
end;

function ttask_handler.cancontinue(m: tmodule; out firstwaiting: tmodule): boolean;
begin
  firstwaiting:=nil;

  { We do not need to consider the program as long as there are units that need to be treated. }
  if m.is_initial and has_scc_another_unfinished_module(m) then
    begin
      if not m.is_unit then
        exit(false);
      if m.state=ms_compiled then
        exit(false); { the initial unit must wait for ms_processed til all others are processed }
    end;

  if m.do_reload then
    cancontinue:=tppumodule(m).canreload(firstwaiting,false)
  else
  begin
    case m.state of
      ms_unknown : cancontinue:=true;
      ms_registered : cancontinue:=true;
      ms_load: cancontinue:=tppumodule(m).ppuloadcancontinue(firstwaiting);
      ms_compile : cancontinue:=true;
      ms_compiling_wait : cancontinue:=m.usedunitsloaded(true,firstwaiting);
      ms_compiling_waitintf : cancontinue:=m.usedunitsloaded(true,firstwaiting);
      ms_compiling_waitimpl : cancontinue:=m.usedunitsloaded(false,firstwaiting);
      ms_compiling_waitfinish : cancontinue:=m.nowaitingforunits(firstwaiting);
      ms_compiled_waitcrc : cancontinue:=m.usedunitsfinalcrc(firstwaiting);
      ms_compiled,
      ms_processed,
      ms_moduleerror : cancontinue:=true;
    end;
  end;

  {$IFDEF DEBUG_CTASK_VERBOSE}
  Write('CTASK: ',m.ToString,' state: ',m.state,', can continue: ',Result);
  if result then
    Writeln
  else
    begin
    Write(' (First waiting: ');
    If Assigned(FirstWaiting) then
      Writeln(FirstWaiting.ToString,' )')
    else
      Writeln('<none>)');
    end;
  {$ENDIF}
end;

function ttask_handler.cancontinue(t: ttask; out firstwaiting: tmodule): boolean;

begin
  Result:=cancontinue(t.module,firstwaiting);
end;

function ttask_handler.continue_module(m: tmodule): Boolean;

var
  t : ttask;

begin
  set_current_module(m);
  t:=findtask(m,true);
  if Assigned(t.state) then
    t.RestoreState;

  tmodule.ctask_fast_backtrack:=false;
  if m.do_reload then
  begin
    tppumodule(m).reload;
    t.SaveState;
    tglobalstate.clear_state;
    exit(false);
  end;
  case m.state of
    ms_registered :
      tppumodule(m).loadppu(tppumodule(m).loadedfrommodule);
    ms_load: (m as tppumodule).continueloadppu;
    ms_compile :
      begin
        if (m=main_module) and (macrosymtablestack<>nil) then
          begin
            macrosymtablestack.clear;
            FreeAndNil(macrosymtablestack);
          end;
        parser.compile_module(m);
      end;
    ms_compiled : ;
    ms_compiling_wait : pmodules.proc_program_declarations(m,m.islibrary);
    ms_compiling_waitintf : pmodules.parse_unit_interface_declarations(m);
    ms_compiling_waitimpl : pmodules.proc_unit_implementation(m);
    ms_compiling_waitfinish : pmodules.finish_compile_unit(m);
    ms_compiled_waitcrc : pmodules.finish_unit(m);
    ms_processed : ;
  else
    InternalError(2024011801);
  end;

  if m.state = ms_moduleerror then
    exit(false);

  if m.is_initial and has_scc_another_unfinished_module(m) then
    begin
      { main module must wait for all units to finish }
      if (not m.is_unit) and (m.state in [ms_compiled,ms_processed]) then
        { program, library, package frees modules, there should be no tasked left }
        Internalerror(2026030302);
      { Note: when main module is a unit, it can finish before used units finish }
    end
  else if m.state=ms_compiled then
    begin
    if not m.fromppu then
      parsing_done(m);
    if (m.state=ms_compiled) and m.is_unit then
      tppumodule(m).post_load_or_compile(m);
    m.state:=ms_processed;
    end;
  Result:=m.state=ms_processed;
  {$IFDEF DEBUG_CTASK}
  Write('CTASK: ',m.ToString,' done: ',Result);
  if Result then
    Writeln
  else
    Writeln(', state is now: ',m.statestr);
  {$ENDIF}
  if result then
    begin
      {$IFDEF DEBUG_CTASK}Writeln('CTASK: ',m.ToString,' is finished, removing from task list');{$ENDIF}
      m.task:=nil;
      freetask(t);
    end
  else
    { Not done, save state }
    t.SaveState;
  tglobalstate.clear_state;
end;

procedure ttask_handler.search_finished_scc(m: tmodule{$IFDEF DEBUG_PPU_CYCLES}; const Indent: string{$ENDIF});
{ called after all circular_unit_groups have beeen computed.
  depth-first-search all modules and computes scc_tree_unfinished, other_scc_unfinished
  and scc_tree_crc_wait.
  marks finished scc with scc_finished:=true.
  returns an unfinished scc root, which sub sccs are all finished }
var
  uu: tused_unit;
  um: tmodule;
begin
  if m.cycle_search_stamp=tmodule.cycle_stamp then
    exit;
  m.cycle_search_stamp:=tmodule.cycle_stamp;

  {$IFDEF DEBUG_PPU_CYCLES}
  writeln(Indent,'ttask_handler.search_finished_scc ',m.modulename^,' ',m.statestr);
  {$ENDIF}

  m.scc_tree_unfinished:=m.do_reload or (m.state<>ms_processed);
  m.other_scc_unfinished:=false;
  m.scc_tree_crc_wait:=nil;

  uu:=tused_unit(m.used_units.First);
  while assigned(uu) do
    begin
      um:=uu.u;
      if not um.scc_finished then
        begin
          search_finished_scc(um{$IFDEF DEBUG_PPU_CYCLES},Indent+'  '{$ENDIF});
          if um.scc_tree_unfinished then
            begin
              m.scc_tree_unfinished:=true;
              if um.other_scc_unfinished
                  or ((m.scc_lowindex<>um.scc_lowindex) and um.scc_tree_unfinished) then
                m.other_scc_unfinished:=true;
            end
          else if um.other_scc_unfinished then
            Internalerror(2026022201);
          if m.scc_tree_crc_wait=nil then
            m.scc_tree_crc_wait:=um.scc_tree_crc_wait;
        end;
      uu:=tused_unit(uu.Next);
    end;

  if (m.scc_tree_crc_wait=nil)
      and (m.do_reload or not m.crc_final or not m.interface_compiled) then
    m.scc_tree_crc_wait:=m;

  if m.scc_root=m then
    begin
      um:=m;
      while assigned(um) do
        begin
          if (not m.scc_tree_unfinished) then
            begin
              { scc finished }
              {$IFDEF DEBUG_CTASK}
              writeln('CTASK finished scc: ',um.modulename^,' ',um.statestr);
              {$ENDIF}
              um.scc_finished:=true;
              um.scc_lowindex:=0;
              um.scc_index:=0;
            end;
          if um.scc_tree_crc_wait=nil then
            um.scc_tree_crc_wait:=m.scc_tree_crc_wait;
          um:=um.scc_next;
        end;
    end;

  {$IFDEF DEBUG_PPU_CYCLES}
  if m=m.scc_root then
    writeln(Indent,'SCC: ',m.modulename^,' ',m.statestr,' size=',get_scc_count(m),' other_scc=',m.other_scc_unfinished,' tree_crc_wait=',m.scc_tree_crc_wait<>nil);
  {$ENDIF}
end;

function ttask_handler.find_unfinished_leaf_scc(m: tmodule): tmodule;
var
  uu: tused_unit;
  um: tmodule;
begin
  if m.cycle_search_stamp=tmodule.cycle_stamp then
    exit(nil);
  m.cycle_search_stamp:=tmodule.cycle_stamp;

  if (m=m.scc_root)                  { m is a scc root }
      and not m.other_scc_unfinished { scc has no unfinished sub scc }
      and m.scc_tree_unfinished      { scc has at least one unfinished module }
      then
    exit(m);

  uu:=tused_unit(m.used_units.First);
  while assigned(uu) do
    begin
      um:=uu.u;
      if not um.scc_finished then
        begin
          Result:=find_unfinished_leaf_scc(um);
          if Result<>nil then exit;
        end;
      uu:=tused_unit(uu.Next);
    end;
  Result:=nil;
end;

function ttask_handler.is_uses_waiting(m: tmodule; uu: tused_unit): boolean;
var
  um: tmodule;
  check_impl_uses, check_crc: boolean;
begin
  um:=uu.u;
  tppumodule(m).get_check_uses(check_impl_uses, check_crc);

  if (uu.in_interface or check_impl_uses)
      and not um.interface_compiled then
    exit(true); { waiting for interface_compiled }

  if check_crc and not um.crc_final then
    exit(true); { waiting for crc }

  Result:=false;
end;

function ttask_handler.check_do_reload_cycle(scc_root: tmodule): boolean;
{ return true if something changed }
var
  m, firstwaiting: tmodule;
  HasDoReload: Boolean;
begin
  Result:=false;
  HasDoReload:=false;
  m:=scc_root;
  while assigned(m) do
    begin
      if m.do_reload then
        begin
          HasDoReload:=true;
          if not tppumodule(m).canreload(firstwaiting,true) then
            exit;
        end;
      m:=m.scc_next;
    end;

  if not HasDoReload then
    exit;

  { reload all do_reloads }
  Result:=true;
  m:=scc_root;
  while assigned(m) do
    begin
      if m.do_reload then
        begin
          {$IFDEF DEBUG_CTASK}
          writeln('PPUALGO ttask_handler.check_do_reload_cycle reloading ',m.modulename^,' ',m.statestr,' ...');
          {$ENDIF}
          reload_module(m);
        end;
      m:=m.scc_next;
    end;
end;

function ttask_handler.check_crc_mismatches(scc_root: tmodule): boolean;
{ recompile all scc modules whose crc of used units mismatch }
var
  m, next_m: tmodule;
  pu: tused_unit;
  check_impl_uses, check_crc: Boolean;
begin
  Result:=false;
  m:=scc_root;
  while assigned(m) do
    begin
      next_m:=m.scc_next;
      if tppumodule(m).get_check_uses(check_impl_uses, check_crc) or check_crc then
        begin
          pu:=tused_unit(m.used_units.First);
          while assigned(pu) do
            begin
              if pu.in_interface or check_crc then
                begin
                  if (pu.u.interface_compiled
                      and ((pu.u.interface_crc<>pu.interface_checksum)
                        or (pu.u.indirect_crc<>pu.indirect_checksum)))
                      or (pu.u.crc_final and check_crc and (pu.u.crc<>pu.checksum) ) then
                    begin
                      {$IFDEF DEBUG_CTASK}
                      writeln('PPUALGO ttask_handler.check_crc_mismatches recompile ',m.modulename^,' ',m.statestr,' ',BoolToStr(pu.in_interface,'interface','implementation'),' uses ',pu.u.modulename^,' ...');
                      {$ENDIF}
                      {$ifdef DEBUG_UNIT_CRC_CHANGES}
                      if (pu.u.interface_crc<>pu.interface_checksum) then
                        writeln('  intfcrc change: '+hexstr(pu.u.interface_crc,8)+' for '+pu.u.modulename^+' <> '+hexstr(pu.interface_checksum,8)+' in unit '+m.modulename^)
                      else if (pu.u.indirect_crc<>pu.indirect_checksum) then
                        writeln(V_Normal,'  indcrc change: '+hexstr(pu.u.indirect_crc,8)+' for '+pu.u.modulename^+' <> '+hexstr(pu.indirect_checksum,8)+' in unit '+m.modulename^)
                      else
                        writeln(V_Normal,'  implcrc change: '+hexstr(pu.u.crc,8)+' for '+pu.u.modulename^+' <> '+hexstr(pu.checksum,8)+' in unit '+m.modulename^);
                      {$endif DEBUG_UNIT_CRC_CHANGES}
                      recompile_module(m);
                      Result:=true;
                      break;
                    end;
                end;
              pu:=tused_unit(pu.Next);
            end;
        end;
      m:=next_m;
    end;
end;

function ttask_handler.check_cycle_wait_for_pas(scc_root: tmodule): boolean;
{ find a waiting pas module A (non ppu),
  dfs through the scc to find all modules waiting for A,
  choose a ppu module to recompile }
var
  pas_mod, best: tmodule;

  function search(m: tmodule): boolean;
  var
    uu: tused_unit;
    um: tmodule;
    i: Integer;
  begin
    Result:=false;
    if m.cycle_search_stamp=tmodule.cycle_stamp then exit;
    m.cycle_search_stamp:=tmodule.cycle_stamp;

    uu:=tused_unit(m.used_units.First);
    while assigned(uu) do
      begin
        um:=uu.u;
        if um=pas_mod then
          begin
            { m waits for pas_mod }
            Result:=true;
            if m.fromppu then
              begin
                best:=m;
                exit;
              end;
          end;
        if (um.scc_root=m.scc_root)
            and is_uses_waiting(m,uu) then
          begin
            if search(um) then
              begin
                Result:=true;
                if m.fromppu then
                  begin
                    best:=m;
                    exit;
                  end;
              end;
          end;
        uu:=tused_unit(uu.Next);
      end;

    if (m.state=ms_compiling_waitfinish) and assigned(m.waitingforunit) then
      begin
        for i:=0 to m.waitingforunit.Count do
          begin
            um:=tmodule(m.waitingforunit[i]);
            if um=pas_mod then
              begin
                { m waits for pas_mod }
                Result:=true;
                if m.fromppu then
                  begin
                    best:=m;
                    exit;
                  end;
              end;
            if search(um) then
              begin
                Result:=true;
                if m.fromppu then
                  begin
                    best:=m;
                    exit;
                  end;
              end;
          end;
      end;
  end;

begin
  Result:=false;

  { find one pas module (non ppu) }
  best:=nil;
  tmodule.increase_cycle_stamp;
  pas_mod:=scc_root;
  while assigned(pas_mod) do
    begin
      if pas_mod.fromppu then
        begin
          { dfs through the scc to find a ppu waiting for pas_mod and vice versus
            Note: already searched pas are skipped, resulting in linear time }
          search(pas_mod);

          if best<>nil then
            begin
              { waiting ppu found -> recompile }
              {$IFDEF DEBUG_CTASK}
              writeln('PPUALGO ttask_handler.check_cycle_wait_for_pas breaking cycle, recompiling ',best.modulename^,' ',best.statestr,' ...');
              {$ENDIF}
              recompile_module(best);
              exit(true);
            end;
        end;
      pas_mod:=pas_mod.scc_next;
    end;
end;

function ttask_handler.recompile_pending(scc_root: tmodule): boolean;
var
  m, next: tmodule;
begin
  Result:=false;
  m:=scc_root;
  while assigned(m) do
    begin
      next:=m.scc_next;
      if m.do_recompile then
        begin
          {$IFDEF DEBUG_CTASK}
          writeln('PPUALGO ttask_handler.recompile_pending recompiling ',m.modulename^,' ',m.statestr,' ...');
          {$ENDIF}
          Result:=true;
          recompile_module(m);
        end;
      m:=next;
    end;
end;

procedure ttask_handler.recompile_scc(scc_root: tmodule);
var
  m, next: tmodule;
  changed: Boolean;
begin
  repeat
    changed:=false;
    m:=scc_root;
    while assigned(m) do
      begin
        next:=m.scc_next;
        if m.do_reload or m.fromppu then
          begin
            {$IFDEF DEBUG_CTASK}
            writeln('PPUALGO ttask_handler.recompile_scc breaking cycle, recompiling ',m.modulename^,' ',m.statestr,' ...');
            {$ENDIF}
            changed:=true;
            recompile_module(m);
          end;
        m:=next;
      end;
  until not changed;
end;

function ttask_handler.restore_state(m: tmodule): ttask;
begin
  Result:=findtask(m);
  if Result=nil then
    begin
      addmodule(m);
      Result:=findtask(m);
    end
  else if Result.state<>nil then
    Result.RestoreState;
end;

function ttask_handler.reload_module(m: tmodule): ttask;
begin
  if m.state in [ms_compiled,ms_processed] then
    begin
      writeln('ttask_handler.reload_module ',m.modulename^,' ',m.statestr);
      Internalerror(2026022410);
    end;

  Result:=restore_state(m);
  tppumodule(m).reload;
  Result.SaveState;
  tglobalstate.clear_state;
end;

function ttask_handler.recompile_module(m: tmodule): ttask;
begin
  if m.state in [ms_compiled,ms_processed] then
    begin
      writeln('ttask_handler.recompile_module ',m.modulename^,' ',m.statestr);
      Internalerror(2026022411);
    end;
  {$IFDEF DEBUG_UR_RECOMPILE}
  if (mf_release in m.moduleflags) and not m.is_initial then begin
    writeln('ttask_handler.recompile_module UR ',m.modulename^,' ',m.statestr);
    Internalerror(2026030915);
  end;
  {$ENDIF}

  Result:=restore_state(m);
  if m.recompile_reason=rr_unknown then
    m.recompile_reason:=rr_buildcycle;
  tppumodule(m).recompile_from_sources;
  addmodule(m);
  tglobalstate.clear_state;
end;

procedure ttask_handler.update_circular_unit_groups;
var
  {$IFDEF DEBUG_PPU_CYCLES}
  grp_cnt: integer;
  {$ENDIF}
  cnt: Integer;
  cur_index: integer;
  stack: array of tmodule;
  stackindex: integer;

  procedure scc_traverse(m: tmodule);
    var
      uu: tused_unit;
      um, prev: tmodule;
    begin
      inc(cur_index);
      m.scc_index:=cur_index;
      m.scc_lowindex:=cur_index;
      //writeln('scc_traverse ',m.modulename^,' cur_index=',cur_index,' ',length(stack),' stackindex=',stackindex);
      stack[stackindex]:=m;
      inc(stackindex);
      m.scc_onstack:=true;

      uu:=tused_unit(m.used_units.first);
      while assigned(uu) do
        begin
          um:=uu.u;
          if not um.scc_finished then
            begin
              if um.scc_index=0 then
                begin
                  scc_traverse(um);
                  if m.scc_lowindex > um.scc_lowindex then
                    m.scc_lowindex:=um.scc_lowindex;
                end
              else if um.scc_onstack then
                begin
                  { um is in same scc }
                  if m.scc_lowindex > um.scc_index then
                    m.scc_lowindex:=um.scc_index;
                end;
            end;
          uu:=tused_unit(uu.next);
        end;

      if m.scc_lowindex = m.scc_index then
        begin
          { new scc, pop from stack }
          {$IFDEF DEBUG_PPU_CYCLES}
          writeln('scc_traverse scc_root=',m.modulename^,' ',m.statestr);
          inc(grp_cnt);
          {$ENDIF}
          prev:=nil;
          repeat
            dec(stackindex);
            um:=stack[stackindex];
            {$IFDEF DEBUG_PPU_CYCLES}
            if m<>um then writeln('  scc module: ',um.modulename^,' ',um.statestr);
            {$ENDIF}
            um.scc_onstack:=false;
            um.scc_root:=m;
            um.scc_next:=prev;
            um.scc_lowindex:=m.scc_lowindex;
            prev:=um;
          until m=um;
        end;
    end;

  procedure scc_clear(m: tmodule);
    var
      uu: tused_unit;
    begin
      if m.scc_finished then exit;

      if m.cycle_search_stamp=tmodule.cycle_stamp then
        exit; { already visited }
      m.cycle_search_stamp:=tmodule.cycle_stamp;

      inc(cnt);
      m.scc_root:=nil;
      m.scc_next:=nil;
      m.scc_index:=0;
      m.scc_lowindex:=0;
      m.scc_onstack:=false;
      uu:=tused_unit(m.used_units.First);
      while assigned(uu) do
        begin
          if not uu.u.scc_finished then
            scc_clear(uu.u);
          uu:=tused_unit(uu.Next);
        end;
    end;

begin
  cnt:=0;
  tmodule.increase_cycle_stamp;
  scc_clear(main_module);
  stack:=[];
  SetLength(stack,cnt);
  stackindex:=0;

  {$IFDEF DEBUG_PPU_CYCLES}
  grp_cnt:=0;
  {$ENDIF}
  cur_index:=0;
  scc_traverse(main_module);
  {$IFDEF DEBUG_PPU_CYCLES}
  writeln('tmodule.update_circular_unit_groups modulecnt=',cnt,' grp_cnt=',grp_cnt);
  {$ENDIF}
end;

procedure ttask_handler.processqueue;

var
  firstwaiting, m, scc_root, best: tmodule;
  {$IFDEF DEBUG_CTASK}
  loopcnt: integer;
  {$ENDIF}
begin
  { Strategy: goal is to write ppus early, so that mem is freed early and in case of an error
              next compile can load ppus instead of compiling again. }
  {$IFDEF DEBUG_CTASK}
  loopcnt:=0;
  {$ENDIF}
  scc_root:=nil;
  repeat
    {$IFDEF DEBUG_CTASK}
    inc(loopcnt);
    writeln('CTASK: Iteration: ',loopcnt);
    {$ENDIF}

    if main_module.state<ms_compiled then
      recompile_pending(scc_root);

    { compute circular unit groups aka scc (strongly connected components) }
    update_circular_unit_groups;

    { compute scc_tree_unfinished and other_scc_unfinished, and mark finished scc }
    tmodule.increase_cycle_stamp;
    search_finished_scc(main_module);
    { now each scc root module has scc_tree_unfinished and other_scc_unfinished:
        scc_tree_unfinished=true means at least one module is not yet finished (do_reload or not ms_compiled)
        other_scc_unfinished=true means at least one sub scc has one unfinished module }

    if not main_module.scc_tree_unfinished then
      exit;

    { find a scc with at least one unfinished module and all sub scc are finished }
    tmodule.increase_cycle_stamp;
    scc_root:=find_unfinished_leaf_scc(main_module);
    if scc_root=nil then
    begin
      write_scc;
      Internalerror(2026022204);
    end;
    {$IFDEF DEBUG_PPU_CYCLES}
    writeln('ttask_handler.processqueue scc_root: ',scc_root.modulename^,' ',scc_root.statestr);
    {$ENDIF}

    { recompile marked modules }
    if recompile_pending(scc_root) then
      continue;

    { check all scc' modules if crc mismatch }
    if check_crc_mismatches(scc_root) then
      continue;

    { check if any of the scc' modules can continue }
    best:=nil;
    m:=scc_root;
    while assigned(m) do
      begin
        {$IFDEF DEBUG_PPU_CYCLES}
        writeln('ttask_handler.processqueue check scc module: ',m.modulename^,' ',m.statestr);
        {$ENDIF}
        if (m.state<ms_processed) and cancontinue(m,firstwaiting) then
          begin
            { prefer do_reload, then pas, last ppu }
            if (best=nil)
                or m.do_reload
                or (not best.do_reload and best.fromppu and not m.fromppu)
            then
              best:=m;
          end;
        m:=m.scc_next;
      end;

    if best=nil then
      begin
        { checking for a do_reload cycle }
        if check_do_reload_cycle(scc_root) then
          continue;
        { break the cycle by recompiling one ppu module waiting for a pas }
        if check_cycle_wait_for_pas(scc_root) then
          continue;
        { finally: recompile the whole scc }
        recompile_scc(scc_root);
        continue;
      end;

    {$IF defined(DEBUG_CTASK) or defined(Debug_FreeParseMem)}Writeln('CTASK: continuing ',best.ToString,' state=',best.statestr,' total-units=',loaded_units.Count,' tasks=',taskcount);{$ENDIF}
    if continue_module(best) then
      // done
    else if best.state=ms_moduleerror then
      exit;
  until false;
end;

procedure ttask_handler.addmodule(m: tmodule);

var
  t : ttask;

begin
  t:=findtask(m);
  if t=nil then
    begin
    {$IFDEF DEBUG_CTASK}Writeln('CTASK: ',m.ToString,' added to task scheduler. State: ',m.statestr,' moduleid=',m.moduleid);{$ENDIF}
    { Clear reset flag.
      This can happen when during load, reset is done and unit is added to task list. }
    m.is_reset:=false;
    t:=createtask(m);
    end
  else
    begin
    { We have a task, if it was reset, then clear the state. }
    if m.is_reset then
      begin
      {$IFDEF DEBUG_CTASK}Writeln('CTASK: ',m.ToString,' was reset, resetting flag. State: ',m.statestr);{$ENDIF}
      m.is_reset:=false;
      t.DiscardState;
      end;
    end;
  tmodule.ctask_fast_backtrack:=true;
end;

procedure ttask_handler.write_scc;

  function Search(m: tmodule; const Indent: string): tmodule;
  // returns m, if m is an unfinished scc
  var
    firstwaiting, sub_scc, um: tmodule;
    uu, um2: tused_unit;
  begin
    Result:=nil;
    if m.cycle_search_stamp=tmodule.cycle_stamp then exit;
    m.cycle_search_stamp:=tmodule.cycle_stamp;

    sub_scc:=nil;
    uu:=tused_unit(m.used_units.First);
    while assigned(uu) do
      begin
        um:=Search(uu.u,Indent+'  ');
        if sub_scc=nil then sub_scc:=um;
        uu:=tused_unit(uu.Next);
      end;

    firstwaiting:=nil;
    if m=m.scc_root then
      cancontinue(m,firstwaiting);
    write(Indent,' ',m.modulename^,' ',m.statestr);
    if m.scc_finished then
      write('  scc_finished');
    if m=m.scc_root then
      begin
        if not m.scc_finished and m.scc_tree_unfinished then
          Result:=m;
        write(' SCC ',get_scc_count(m),' WaitForSCC=',m.other_scc_unfinished);
        if sub_scc<>nil then
          begin
            // has an unfinished sub scc
            if not m.other_scc_unfinished then
              begin
                writeln;
                writeln('ERROR: has unfinished sub scc: ',sub_scc.modulename^,' ',sub_scc.statestr,' scc_tree_unfinished=',sub_scc.scc_tree_unfinished);
                Internalerror(2026022205);
              end
            else
              write(' unfinished sub scc=',sub_scc.modulename^,' ',sub_scc.statestr);
          end
        else
          begin
            // has no unfinished sub scc
            if m.other_scc_unfinished then
              begin
                writeln;
                writeln('ERROR: this scc has no unfinished sub scc:');
                um:=m;
                while assigned(um) do
                  begin
                    cancontinue(um,firstwaiting);
                    write('  ',um.modulename^,' ',um.statestr,' tree_unfinished=',um.scc_tree_unfinished,' other=',um.other_scc_unfinished);
                    if um.scc_finished then
                      write(' ERROR:scc_finished');
                    if firstwaiting<>nil then
                      write(' WaitsFor=',firstwaiting.modulename^,' ',firstwaiting.statestr)
                    else
                      write(' WaitsFor=nil');
                    if um.scc_root<>m then
                      if um.scc_root<>nil then
                        write(' ERROR:scc_root=',um.scc_root.modulename^)
                      else
                        write(' ERROR:scc_root=nil');
                    writeln;

                    um2:=tused_unit(um.used_units.First);
                    while assigned(um2) do
                      begin
                        writeln('    ',um2.u.modulename^,' ',um2.u.statestr,' scc_finished=',um2.u.scc_finished,' tree_unfinished=',um2.u.scc_tree_unfinished);
                        um2:=tused_unit(um2.Next);
                      end;

                    um:=um.scc_next;
                  end;
                Internalerror(2026022206);
              end;
            if firstwaiting<>nil then
              write(' WaitForMod=',firstwaiting.modulename^,' ',firstwaiting.statestr)
            else
              write(' WaitForMod=nil');
          end;
      end
    else
      begin
        if m.scc_root=nil then
          write(' ERROR: scc_root=nil')
        else begin
          write(' scc_root=',m.scc_root.modulename^);
          cancontinue(m,firstwaiting);
          if firstwaiting<>nil then
            write(' WaitForMod=',firstwaiting.modulename^,' ',firstwaiting.statestr)
          else
            write(' WaitForMod=nil');
        end;
      end;
    writeln;
  end;

begin
  writeln('ttask_handler.write_scc');
  tmodule.increase_cycle_stamp;
  Search(main_module,'');
end;

function ttask_handler.get_scc_count(scc_root: tmodule): integer;
begin
  Result:=0;
  while assigned(scc_root) do
    begin
      inc(Result);
      scc_root:=scc_root.scc_next;
    end;
end;

function ttask_handler.has_scc_another_unfinished_module(m: tmodule): boolean;
var
  scc_root: tmodule;
begin
  Result:=false;
  scc_root:=m.scc_root;
  while assigned(scc_root) do
    begin
      if (m<>scc_root) and (scc_root.state<>ms_processed) then
        exit(true);
      scc_root:=scc_root.scc_next;
    end;
end;

end.

