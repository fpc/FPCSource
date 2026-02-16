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
    function cancontinue(m : tmodule; checksub : boolean; out firstwaiting: tmodule): boolean;
    // Overload of cancontinue, based on task.
    function cancontinue(t: ttask_list; out firstwaiting: tmodule): boolean; inline;
    // Check modules waiting for t, find highest state and count them
    function countwaiting(m : tmodule; out highest_state: tmodulestate; out firsthighestwaiting: tmodule): integer; // EnableCTaskPPU: remove
    // Continue processing this module. Return true if the module is done and can be removed.
    function continue_task(t : ttask_list): Boolean;
    {$IFDEF EnableCTaskPPU}
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
    state.restore(true);
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
  {$IFDEF EnableCTaskPPU}
  tmodule.queue_module:=@addmodule;
  {$ENDIF}
end;

destructor ttask_handler.destroy;
begin
  {$IFDEF EnableCTaskPPU}
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

function ttask_handler.cancontinue(m: tmodule; checksub : boolean; out firstwaiting: tmodule): boolean;

  procedure CheckUsed(out acandidate : tmodule);

  var
    itm : TLinkedListItem;
    iscandidate : boolean;
    m2 : tmodule;

  begin
    acandidate:=nil;
    itm:=m.used_units.First;
    while assigned(itm) do
      begin
      iscandidate:=Not (tused_unit(itm).u.state in [ms_processed,ms_compiled]);
      if iscandidate then
        begin
        acandidate:=tused_unit(itm).u;
        if cancontinue(acandidate,false,m2) then
          break;
        end;
      itm:=itm.Next;
      end;
    acandidate:=nil;
  end;

var
  m2 : tmodule;

begin
  firstwaiting:=nil;

  // We do not need to consider the program as long as there are units that need to be treated.
  if (m.is_initial and not m.is_unit) and (list.count>1) then
    exit(False);

  {$IFDEF EnableCTaskPPU}
  if m.do_reload then
    cancontinue:=tppumodule(m).canreload(firstwaiting)
  else
  {$ENDIF}
  begin
    case m.state of
      ms_unknown : cancontinue:=true;
      ms_registered : cancontinue:=true;
      {$IFDEF EnableCTaskPPU}
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
    else
      InternalError(2024011802);
    end;
  end;

  // EnableCTaskPPU: remove checksub
  if (not cancontinue) and checksub then
    begin
    checkused(m2);
    if m2<>nil then
      firstwaiting:=m2;
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
  Result:=cancontinue(t.module,true,firstwaiting);
end;

function ttask_handler.countwaiting(m: tmodule; out highest_state: tmodulestate; out
  firsthighestwaiting: tmodule): integer;
var
  i: Integer;
  dep_unit: tdependent_unit;
  state: tmodulestate;
  waitfor_unit: tmodule;
begin
  Result:=0;
  highest_state:=ms_registered;
  firsthighestwaiting:=nil;

  if m.is_initial and not m.is_unit then
    // program/library
    exit;

  if m.waitingunits<>nil then
  begin
    for i:=0 to m.waitingunits.Count-1 do
    begin
      waitfor_unit:=tmodule(m.waitingunits[i]);
      state:=waitfor_unit.state;
      if state in [ms_compiled, ms_processed] then
        // not waiting
      else if state<highest_state then
        // worse
      else if state=highest_state then
        // same
        inc(Result)
      else
        begin
        // better
        Result:=1;
        highest_state:=state;
        firsthighestwaiting:=waitfor_unit;
        end;
    end;
  end;

  if m.dependent_units<>nil then
  begin
    dep_unit:=tdependent_unit(m.dependent_units.First);
    while dep_unit<>nil do
      begin
      state:=dep_unit.u.state;
      if state in [ms_compiled, ms_processed] then
        // not waiting
      else if state<highest_state then
        // worse
      else if state=highest_state then
        // same
        inc(Result)
      else
        begin
        // better
        Result:=1;
        highest_state:=state;
        firsthighestwaiting:=dep_unit.u;
        end;
      dep_unit:=tdependent_unit(dep_unit.Next);
      end;
  end;
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
  {$IFDEF EnableCTaskPPU}
  if m.do_reload then
  begin
    writeln('ttask_handler.continue ',m.modulename^,' ',m.state,' reloading...');
    tppumodule(m).reload;
    exit;
  end;
  writeln('ttask_handler.continue ',m.modulename^,' ',m.state,' continue...');
  {$ENDIF}
  case m.state of
    ms_registered : parser.compile_module(m);
    {$IFDEF EnableCTaskPPU}
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
  {$IFDEF EnableCTaskPPU}
  writeln('ttask_handler.continue AFTER ',m.modulename^,' ',m.state,' reload=',m.do_reload);
  {$ENDIF}

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

{$IFDEF EnableCTaskPPU}
function ttask_handler.check_cycle: boolean;
var
  last: ttask_list;

  function Search(m: tppumodule): boolean;
  var
    uu: tused_unit;
    pm: tppumodule;
  begin
    Result:=false;

    // mark module as searched
    m.cycle_search_stamp:=m.cycle_stamp;

    uu:=tused_unit(m.used_units);
    while uu<>nil do
    begin
      pm:=tppumodule(uu.u);
      if pm<>nil then
      begin
        if pm=last.module then
          Result:=true
        else if pm.cycle_stamp=pm.cycle_search_stamp then
          // already searched
        else
          Result:=Result or Search(pm);
      end;
      uu:=tused_unit(uu.Next);
    end;

    if Result then
    begin
      // cycle detected -> recompile ppu
      if m.state=ms_load then
      begin
        {$IFDEF DEBUG_CTASK}
        writeln('PPUALGO check_cycle last=',last.module.modulename^,' ',last.module.state,', RECOMPILE ',m.modulename^,' ',m.state);
        {$ENDIF}
        m.recompile_cycle;
        check_cycle:=true;
      end;
    end;
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
    if (last=nil) or (last.module.unit_index<t.module.unit_index) then
      last:=t;
    t:=t.nexttask;
    end;

  if tppumodule.cycle_stamp=high(dword) then
    tppumodule.cycle_stamp:=0
  else
    inc(tppumodule.cycle_stamp);
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
  firstwaiting, bestmod, m, firsthighestwaiting: tmodule;

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
      else if cancontinue(m,false,firstwaiting) then
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

    {$IFDEF EnableCTaskPPU}
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
  e:=ttask_list(Hash.Find(n));
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
  t: ttask_list;
  firstwaiting, m: tmodule;
  cc: Boolean;
begin
  writeln('ttask_handler.write_queue:');
  t:=list.firsttask;
  while t<>nil do
    begin
    cc:=cancontinue(t,firstwaiting);
    m:=t.module;
    if firstwaiting<>nil then
      writeln('queue: ',m.realmodulename^,' ',m.statestr,' cancontinue=',cc,' firstwaiting=',firstwaiting.realmodulename^,' ',firstwaiting.state)
    else
      writeln('queue: ',m.realmodulename^,' ',m.statestr,' cancontinue=',cc,' firstwaiting=nil');
    t:=t.nexttask;
    end;
end;

end.

