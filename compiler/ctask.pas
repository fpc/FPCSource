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

interface

uses
  fmodule, cclasses, globstat;

type
  { ttask_list }

  ttask_list = class(tlinkedlistitem)
     module : tmodule;
     state : tglobalstate;
     constructor create(_m : tmodule);
     destructor destroy; override;
     procedure SaveState;
     Procedure RestoreState;
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
  public
    constructor create;
    destructor destroy; override;
    function findtask(m : tmodule) : ttask_list;
    // Can we continue processing this module ?
    function cancontinue(t : ttask_list) : boolean;
    // Continue processing this module. Return true if the module is done and can be removed.
    function continue(t : ttask_list): Boolean;
    // process the queue. Note that while processing the queue, elements will be added.
    procedure processqueue;
    // add a module to the queue. If a module is already in the queue, we do not add it again.
    procedure addmodule(m : tmodule);
  end;


var
  task_handler : TTask_handler;

procedure InitTaskHandler;
procedure DoneTaskHandler;

implementation

uses verbose, finput, globtype, sysutils, scanner, parser, pmodules;

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
    State:=tglobalstate.Create(true);
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
      // parser_current_file:=current_scanner.inputfile.name;
      end;

end;

{ ttask_handler }

constructor ttask_handler.create;
begin
  list:=ttasklinkedlist.Create;
  hash:=TFPHashList.Create;
end;

destructor ttask_handler.destroy;
begin
  hash.free;
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
end;

function ttask_handler.cancontinue(t : ttask_list): boolean;

var
  m : tmodule;

begin
  m:=t.module;
  case m.state of
    ms_unknown : cancontinue:=true;
    ms_registered : cancontinue:=true;
    ms_compile : cancontinue:=true;
    ms_compiling_waitimpl : cancontinue:=m.usedunitsloaded(false);
    ms_compiling_waitintf : cancontinue:=m.usedunitsloaded(true);
    ms_compiling_wait : cancontinue:=m.usedunitsloaded(true);
    ms_compiled : cancontinue:=true;
    ms_moduleerror : cancontinue:=true;
  else
    InternalError(2024011802);
  end;
end;

function ttask_handler.continue(t : ttask_list) : Boolean;

var
  m : tmodule;

begin
  m:=t.module;
  if Assigned(t.state) then
    t.RestoreState;
  case m.state of
    ms_registered : parser.compile_module(m);
    ms_compile : parser.compile_module(m);
    ms_compiling_waitintf : pmodules.parse_unit_interface_declarations(m);
    ms_compiling_waitimpl : pmodules.proc_unit_implementation(m);
    ms_compiling_wait : pmodules.proc_program_declarations(m,m.islibrary);
  else
    InternalError(2024011801);
  end;
  Result:=m.state=ms_compiled;
  if not Result then
    // Not done, save state
    t.SaveState;
end;

procedure ttask_handler.processqueue;

var
  t : ttask_list;
  process : boolean;

begin
  t:=list.firsttask;
  While t<>nil do
    begin
    process:=cancontinue(t);
    if process then
      begin
      if continue(t) then
        begin
        hash.Remove(t.module);
        list.Remove(t);
        end;
      // maybe the strategy can be improved.
      t:=list.firsttask;
      end
    else
      t:=t.nexttask;
    end;
end;

procedure ttask_handler.addmodule(m: tmodule);


var
  n : TSymStr;
  e : tmodule;
  t : ttask_list;

begin
  n:=m.modulename^;
  e:=tmodule(Hash.Find(n));
  if e=nil then
    begin
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



end.

