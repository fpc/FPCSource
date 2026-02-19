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
  finput, fmodule, cclasses, globstat;

type
  { ttask_list

    About state:
      Contains scanner/parser position needed for compiling pascal sources,
      irrelevant for loading ppu(s).
      It is restored before continuing and saved afterwards (if unfinished).
      Loading ppu files works recursively and stops when a unit requires (re)compile,
      A recompile discards the saved state the adds the module to ctask.
      When the recursion steps back, leaving the current unit unfinished the state is saved,
      so ctask can continue with another unit.
  }

  ttask_list = class(tlinkedlistitem)
    module : tmodule;
    state : tglobalstate;
    constructor create(_m : tmodule);
    destructor destroy; override;
    procedure SaveState;
    procedure RestoreState;
    procedure DiscardState;
    function nexttask : ttask_list; inline;
  end;

  ttasklinkedlist = class(tlinkedlist)
    function firsttask : ttask_list; inline;
  end;

  { ttask_handler }

  ttask_handler = class
  private
    list : ttasklinkedlist;
    hash : TFPHashList;
    main : tmodule;
    procedure rebuild_hash;
  public
    constructor create;
    destructor destroy; override;
    // Find the task for module m
    function findtask(m : tmodule) : ttask_list;
    // Can we continue processing this module ? If not, firstwaiting contains first module that m is waiting for.
    function cancontinue(m : tmodule; out firstwaiting: tmodule): boolean;
    // Overload of cancontinue, based on task.
    function cancontinue(t: ttask_list; out firstwaiting: tmodule): boolean; inline;
    // Continue processing this module. Return true if the module is done and can be removed.
    function continue_task(t : ttask_list): Boolean;
    {$IFNDEF DisableCTaskPPU}
    // Check for a circular dependency and fix it
    function check_cycle: boolean;
    {$ENDIF}
    // process the queue. Note that while processing the queue, elements will be added.
    procedure processqueue;
    // add a module to the queue. If a module is already in the queue, we do not add it again.
    procedure addmodule(m : tmodule);
    // write current queue and what is waiting for what
    procedure write_queue;
  end;


var
  task_handler : TTask_handler;

procedure InitTaskHandler;
procedure DoneTaskHandler;

implementation

uses
  verbose, fppu, globtype, sysutils,
  scanner, parser, pmodules, symbase;

procedure InitTaskHandler;
begin
  task_handler:=ttask_handler.create;
end;

procedure DoneTaskHandler;
begin
  freeandnil(task_handler);
end;

{ ttasklinkedlist }

function ttasklinkedlist.firsttask: ttask_list;
begin
  Result:=ttask_list(first);
end;

{ ttask_list }

constructor ttask_list.create(_m: tmodule);
begin
  inherited create;
  module:=_m;
  state:=nil;
end;

destructor ttask_list.destroy;
begin
  DiscardState;
  Inherited;
end;

procedure ttask_list.DiscardState;

begin
  FreeAndNil(state);
end;

function ttask_list.nexttask: ttask_list;
begin
  Result:=ttask_list(next);
end;

procedure ttask_list.SaveState;
begin
  if State=Nil then
    State:=tglobalstate.Create
  else
    State.save;
end;

procedure ttask_list.RestoreState;
begin
  if not module.is_reset then
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
  list:=ttasklinkedlist.Create;
  hash:=TFPHashList.Create;
  {$IFNDEF DisableCTaskPPU}
  tmodule.queue_module:=@addmodule;
  {$ENDIF}
end;

destructor ttask_handler.destroy;
begin
  {$IFNDEF DisableCTaskPPU}
  tmodule.queue_module:=nil;
  {$ENDIF}
  hash.free;
  hash := nil;
  List.Clear;
  FreeAndNil(list);
  inherited destroy;
end;

function ttask_handler.findtask(m: tmodule): ttask_list;

begin
  result:=list.FirstTask;
  while result<>nil do
    begin
    if result.module=m then
      exit;
    result:=result.nexttask;
    end;
  {$IFDEF DEBUG_CTASK_VERBOSE}Writeln('No task found for '+m.ToString);{$ENDIF}
end;

function ttask_handler.cancontinue(m: tmodule; out firstwaiting: tmodule): boolean;
begin
  firstwaiting:=nil;

  // We do not need to consider the program as long as there are units that need to be treated.
  if (m.is_initial and not m.is_unit) and (list.count>1) then
    exit(False);

  {$IFNDEF DisableCTaskPPU}
  if m.do_reload then
    cancontinue:=tppumodule(m).canreload(firstwaiting)
  else
  {$ENDIF}
  begin
    case m.state of
      ms_unknown : cancontinue:=true;
      ms_registered : cancontinue:=true;
      {$IFNDEF DisableCTaskPPU}
      ms_load: cancontinue:=tppumodule(m).ppuloadcancontinue(firstwaiting);
      {$ENDIF}
      ms_compile : cancontinue:=true;
      ms_compiling_wait : cancontinue:=m.usedunitsloaded(true,firstwaiting);
      ms_compiling_waitintf : cancontinue:=m.usedunitsloaded(true,firstwaiting);
      ms_compiling_waitimpl : cancontinue:=m.usedunitsloaded(false,firstwaiting);
      ms_compiling_waitfinish : cancontinue:=m.nowaitingforunits(firstwaiting);
      ms_compiled_waitcrc : cancontinue:=m.usedunitsfinalcrc(firstwaiting);
      ms_compiled : cancontinue:=true;
      ms_processed : cancontinue:=true;
      ms_moduleerror : cancontinue:=true;
    {$IFDEF DisableCTaskPPU}
    else
      InternalError(2024011802);
    {$ENDIF}
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

function ttask_handler.cancontinue(t : ttask_list; out firstwaiting : tmodule): boolean;

begin
  Result:=cancontinue(t.module,firstwaiting);
end;

function ttask_handler.continue_task(t : ttask_list) : Boolean;

var
  m : tmodule;
  orgname : shortstring;

begin
  m:=t.module;
  orgname:=m.modulename^;
  {$IFDEF DEBUG_CTASK}Writeln('CTASK: ',m.ToString,' Continues. State: ',m.state,' do_reload=',m.do_reload);{$ENDIF}
  if Assigned(t.state) then
    t.RestoreState;
  {$IFNDEF DisableCTaskPPU}
  if m.do_reload then
  begin
    tppumodule(m).reload;
    exit(false);
  end;
  {$ENDIF}
  case m.state of
    ms_registered : parser.compile_module(m);
    {$IFNDEF DisableCTaskPPU}
    ms_load: (m as tppumodule).continueloadppu;
    {$ENDIF}
    ms_compile :
      begin
        if m=main then
          begin
            macrosymtablestack.clear;
            FreeAndNil(macrosymtablestack);
          end;
        parser.compile_module(m);
      end;
    ms_compiled : if (not m.is_initial) or m.is_unit then
                    (m as tppumodule).post_load_or_compile(m,m.compilecount>1);
    ms_compiling_wait : pmodules.proc_program_declarations(m,m.islibrary);
    ms_compiling_waitintf : pmodules.parse_unit_interface_declarations(m);
    ms_compiling_waitimpl : pmodules.proc_unit_implementation(m);
    ms_compiling_waitfinish : pmodules.finish_compile_unit(m);
    ms_compiled_waitcrc : pmodules.finish_unit(m);
    ms_processed : ;
  else
    InternalError(2024011801);
  end;

  if (m.is_initial and not m.is_unit) and (list.Count>1) then
    // program must wait for all units to finish
  else if m.state=ms_compiled then
    begin
    parsing_done(m);
    if m.is_initial and not m.is_unit then
      m.state:=ms_processed;
    end;
  Result:=m.state=ms_processed;
  {$IFDEF DEBUG_CTASK}
  Write('CTASK: ',m.ToString,' done: ',Result);
  if Result then
    Writeln
  else
    Writeln(', state is now: ',m.state);
  {$ENDIF}
  if not result then
    // Not done, save state
    t.SaveState;
  {
    the name can change as a result of processing, e.g. PROGRAM -> TB0406
    Normally only for the initial module, but we'll do a generic check.
  }
  if m.modulename^<>orgname then
    rebuild_hash;
end;

{$IFNDEF DisableCTaskPPU}
function ttask_handler.check_cycle: boolean;
// returns true if something changed
var
  last: ttask_list;
  cycle_unit: tppumodule;

  function Search(m: tppumodule): boolean;
  var
    pm: tppumodule;
    firstwaiting: tmodule;
  begin
    Result:=false;

    // mark module as searched
    m.cycle_search_stamp:=m.cycle_stamp;

    cancontinue(m,firstwaiting);
    if firstwaiting=nil then
      Internalerror(2026021913);
    pm:=tppumodule(firstwaiting);
    if pm.cycle_stamp=pm.cycle_search_stamp then
    begin
      // cycle found
      cycle_unit:=pm;
      Result:=true;
    end else if Search(pm) then
    begin
      // m and pm are part of the cycle
      Result:=true;
    end;

    if Result then
    begin
      // m is part of the cycle -> recompile ppu
      if m.fromppu then
      begin
        {$IFDEF DEBUG_CTASK}
        writeln('PPUALGO check_cycle last=',last.module.modulename^,' ',last.module.statestr,', RECOMPILE ',m.modulename^,' ',m.statestr);
        {$ENDIF}
        m.recompile_cycle;
        check_cycle:=true; // something changed
      end;
    end;

    if m=cycle_unit then
      Result:=false; // the cycle started with m, the remaining path is not part of the cycle
  end;

var
  t: ttask_list;
begin
  Result:=false;

  // find highest unit_index in queue
  t:=list.firsttask;
  if t=nil then exit;
  last:=nil;
  while t<>nil do
    begin
    {$IFDEF DEBUG_CTASK}
    writeln('PPUALGO check_cycle queued: ',t.module.modulename^,' ',t.module.statestr);
    {$ENDIF}
    if (last=nil) or (last.module.unit_index<t.module.unit_index) then
      last:=t;
    t:=t.nexttask;
    end;

  if tppumodule.cycle_stamp=high(dword) then
    Internalerror(2026021623);
  inc(tppumodule.cycle_stamp);

  cycle_unit:=nil;
  Search(tppumodule(last.module));
end;
{$ENDIF}

procedure ttask_handler.rebuild_hash;

var
  t : ttask_list;

begin
  Hash.Clear;
  t:=list.firsttask;
  While assigned(t) do
    begin
    Hash.Add(t.module.modulename^,t);
    t:=t.nexttask;
    end;
end;

procedure ttask_handler.processqueue;

var
  t, besttask: ttask_list;
  firstwaiting, m: tmodule;

begin
  // Strategy: goal is to write ppus early, so that mem is freed early and in case of an error
  //           next compile can load ppus instead of compiling again.

  repeat
    {$IFDEF DEBUG_CTASK}writeln('CTASK: ttask_handler.processqueue: task-count=',list.Count);{$ENDIF}
    besttask:=nil;
    if list.firsttask=nil then
      exit; // completed

    // search for any module, that can continue, with furthest state
    t:=list.firsttask;
    while t<>nil do
      begin
      m:=t.module;
      if (besttask<>nil) and (besttask.module.unit_index>m.unit_index) then
        // skip
      else if cancontinue(m,firstwaiting) then
        begin
        {$IFDEF DEBUG_CTASK}
        Writeln('CTASK: ',m.ToString,' state=',m.state,' unit_index=',m.unit_index);
        {$ENDIF}
        // prefer highest unit_index to complete strongly connected components first
        if (besttask=nil)
            or (besttask.module.unit_index<m.unit_index) then
          besttask:=t;
        end;
      t:=t.nexttask;
      end;

    {$IFNDEF DisableCTaskPPU}
    if besttask=nil then
      if check_cycle then continue;
    {$ENDIF}

    if besttask=nil then
      begin
      // no progress possible
      write_queue;
      InternalError(2026012015);
      end;

    {$IF defined(DEBUG_CTASK) or defined(Debug_FreeParseMem)}Writeln('CTASK: continuing ',besttask.module.ToString,' state=',besttask.module.statestr,' total-units=',loaded_units.Count,' tasks=',list.Count);{$ENDIF}
    if continue_task(besttask) then
    begin
      {$IFDEF DEBUG_CTASK}Writeln('CTASK: ',besttask.module.ToString,' is finished, removing from task list');{$ENDIF}
      hash.Remove(besttask.module);
      list.Remove(besttask);
      FreeAndNil(besttask);
    end;
  until false;
end;

procedure ttask_handler.addmodule(m: tmodule);

var
  n : TSymStr;
  e, t : ttask_list;

begin
  n:=m.modulename^;
  //e:=ttask_list(Hash.Find(n));

  e:=findtask(m);
  {$IFDEF DEBUG_CTASK}
  //if findtask(m)<>e then
  //begin
  //  writeln('ttask_handler.addmodule Hash<>findtask ',m.modulename^);
  //  Internalerror(2026021902);
  //end;
  {$ENDIF}

  if e=nil then
    begin
    {$IFDEF DEBUG_CTASK}Writeln('CTASK: ',m.ToString,' added to task scheduler. State: ',m.state,' unit_index=',m.unit_index);{$ENDIF}
    // Clear reset flag.
    // This can happen when during load, reset is done and unit is added to task list.
    m.is_reset:=false;
    t:=ttask_list.create(m);
    list.insert(t);
    hash.Add(n,t);
    if list.count=1 then
      main:=m;
    end
  else
    begin
    // We have a task, if it was reset, then clear the state and move the task to the start.
    if m.is_reset then
      begin
      {$IFDEF DEBUG_CTASK}Writeln('CTASK: ',m.ToString,' was reset, resetting flag. State: ',m.state);{$ENDIF}
      m.is_reset:=false;
      t:=findtask(m);
      if assigned(t) then
        begin
        t.DiscardState;
        list.Remove(t);
        list.insertbefore(t,list.First);
        end;
      end;
    end;
end;

procedure ttask_handler.write_queue;
var
  last: ttask_list;
  cycle_unit: tppumodule;

  function Search(m: tppumodule): boolean;
  var
    pm: tppumodule;
    firstwaiting: tmodule;
  begin
    Result:=false;

    // mark module as searched
    m.cycle_search_stamp:=m.cycle_stamp;

    cancontinue(m,firstwaiting);
    if firstwaiting=nil then
      exit;
    pm:=tppumodule(firstwaiting);
    if pm.cycle_stamp=pm.cycle_search_stamp then
    begin
      // cycle found
      cycle_unit:=pm;
      Result:=true;
      writeln('cycle found: ',pm.modulename^,' ',pm.statestr,' ppu=',pm.fromppu,' used by...');
    end else if Search(pm) then
    begin
      // m and pm are part of the cycle
      Result:=true;
    end;

    if Result then
    begin
      // m is part of the cycle -> recompile ppu
      writeln(' cycle-path: ',m.modulename^,' ',m.statestr,' ppu=',m.fromppu,' used by...');
    end;

    if m=cycle_unit then
      Result:=false; // the cycle started with m, the remaining path is not part of the cycle
  end;

var
  t, wt: ttask_list;
  firstwaiting, m: tmodule;
  cc: Boolean;
  n: TSymStr;
begin
  writeln('ttask_handler.write_queue:');
  t:=list.firsttask;
  while t<>nil do
    begin
    cc:=cancontinue(t,firstwaiting);
    m:=t.module;

    if m.is_unit then
    begin
      n:=m.modulename^;
      wt:=ttask_list(Hash.Find(n));
      if wt<>t then
        writeln('Error: module=',m.modulename^,' ',m.statestr,' wrong hash task');
    end;

    if firstwaiting<>nil then
    begin
      writeln('queue: ',m.modulename^,' ',m.statestr,' cancontinue=',cc,' firstwaiting=',firstwaiting.modulename^,' ',firstwaiting.statestr,' intfcompiled=',firstwaiting.interface_compiled,' crc=',firstwaiting.crc_final);
      wt:=findtask(firstwaiting);
      if wt=nil then
        writeln('Error: waiting for ',firstwaiting.modulename^,', which is not in queue');
    end
    else
      writeln('queue: ',m.modulename^,' ',m.statestr,' cancontinue=',cc,' firstwaiting=nil');
    t:=t.nexttask;
    end;

  // write a cycle:

  // find highest unit_index in queue
  t:=list.firsttask;
  if t=nil then exit;
  last:=nil;
  while t<>nil do
    begin
    if (last=nil) or (last.module.unit_index<t.module.unit_index) then
      last:=t;
    t:=t.nexttask;
    end;
  writeln('last unit_index: ',last.module.modulename^,' ',last.module.unit_index);

  if tppumodule.cycle_stamp=high(dword) then
    Internalerror(2026021624);
  inc(tppumodule.cycle_stamp);

  Search(tppumodule(last.module));
end;

end.

