{
    This file is part of the Free Pascal component library.
    Copyright (c) 2026 by Michael Van Canneyt
    member of the Free Pascal development team

    tasks (currently unused)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Opf.Task;

{$mode objfpc}{$H+}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils;
{$ELSE}
  Classes, SysUtils;
{$ENDIF}

Type
  TOPTaskStatus = (tsTodo, tsExecuting, tsExecuted, tsUndoing, tsUndone, tsExecuteFailed, tsUndoFailed);
  TOPTaskStatuses = set of TOPTaskStatus;

  { TOPTask }

  TOPTask = Class(TObject)
  private
    FStatus: TOPTaskStatus;
  Protected
    Procedure DoExecute; virtual; abstract;
    Procedure DoUndo; virtual; abstract;
    Function GetAutoUndo : Boolean; virtual;
  Public
    // Runs the task and updates its status; undoes it on failure when AutoUndo is set
    Procedure Execute;
    // Reverses the work of the task and updates its status
    Procedure Undo;
    // The current state of the task
    Property Status : TOPTaskStatus Read FStatus;
    // True if the task is undone automatically when running it fails
    Property AutoUndo : Boolean Read GetAutoUndo;
  end;
  TOPTaskArray = Array of TOPTask;

  { TOPListTask }

  TOPListTask = Class(TOPTask)
  Private
    FList : TFPList;
    function GetCount: Integer;
    function GetSubTask(aIndex : Integer): TOPTask;
  Protected
    Procedure AddCapacity(aCount : Integer);
    Procedure ClearItems;
    Procedure DoExecute; override;
    Procedure DoUndo; override;
  Public
    // Frees the task list and all of its subtasks
    destructor destroy; override;
    // Creates an empty task list
    Constructor Create;
    // Creates a task list with the tasks of a TFPList
    Constructor Create(aList : TFPList);
    // Creates a task list with the tasks of a TList
    Constructor Create(aList : TList);
    // Creates a task list with the tasks of an array
    Constructor Create(aArray : Array of TOPTask);
    // Adds all tasks of a TFPList; the task list then owns them
    Procedure Add(aList : TFPList); overload;
    // Adds all tasks of a TList; the task list then owns them
    Procedure Add(aList : TList); overload;
    // Adds all tasks of an array; the task list then owns them
    Procedure Add(aArray : Array of TOPTask); overload;
    // Adds a single task; the task list then owns it
    Procedure Add(aTask : TOPTask); overload;
    // The subtasks, by position
    Property SubTasks[aIndex : Integer] : TOPTask Read GetSubTask; default;
    // The number of subtasks
    Property Count : Integer Read GetCount;
  end;

implementation

uses Opf.Logging, Opf.Consts;

{ TOPListTask }

function TOPListTask.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TOPListTask.GetSubTask(aIndex : Integer): TOPTask;
begin
  Result:=TOPTask(FList[aIndex]);
end;

procedure TOPListTask.AddCapacity(aCount: Integer);
begin
  FList.Capacity:=FList.Count+ACount;
end;

procedure TOPListTask.ClearItems;

Var
  I : Integer;

begin
  For I:=0 to FList.Count-1 do
    TObject(FList[i]).Free;
  Flist.Clear;
end;

procedure TOPListTask.DoExecute;

Var
  I : integer;
  T : TOPTask;

begin
  For I:=0 to Count-1 do
    begin
    T:=SubTasks[I];
    if T.Status=tsTodo then
      T.Execute;
    end;
end;

procedure TOPListTask.DoUndo;
Var
  I : integer;
  T : TOPTask;
  UndoStatuses : TOPTaskStatuses;

begin
  // Executed subtasks are undone too: undoing the list undoes all of its work,
  // in reverse order
  UndoStatuses:= [tsExecuted,tsExecuting,tsExecuteFailed];
  For I:=Count-1 downto 0 do
    begin
    T:=SubTasks[I];
    if (T.Status in UndoStatuses) then
      T.Undo;
    end;
end;

destructor TOPListTask.destroy;

begin
  ClearItems;
  FreeAndNil(FList);
  inherited destroy;
end;

constructor TOPListTask.Create;
begin
  inherited Create;
  FList:=TFPList.Create;
end;

constructor TOPListTask.Create(aList: TFPList);
begin
  Create;
  Add(aList);
end;

constructor TOPListTask.Create(aList: TList);
begin
  Create;
  Add(aList);
end;

constructor TOPListTask.Create(aArray: array of TOPTask);
begin
  Create;
  Add(aArray);
end;

procedure TOPListTask.Add(aList: TFPList);

Var
  I : Integer;

begin
  if (aList=Nil) or (aList.Count=0) then exit;
  AddCapacity(aList.Count);
  for I:=0 to aList.Count-1 do
    Add(TOPTask(aList[i]));
end;

procedure TOPListTask.Add(aList: TList);

Var
  I : Integer;

begin
  if (aList=Nil) or (aList.Count=0) then exit;
  AddCapacity(aList.Count);
  for I:=0 to aList.Count-1 do
    Add(TOPTask(aList[i]));
end;

procedure TOPListTask.Add(aArray: array of TOPTask);

Var
  T : TOPTask;

begin
  AddCapacity(Length(aArray));
  For T in aArray do
    Add(T);
end;

procedure TOPListTask.Add(aTask: TOPTask);
begin
  FList.Add(aTask);
end;

{ TOPTask }

function TOPTask.GetAutoUndo: Boolean;
begin
  Result:=False;
end;

procedure TOPTask.Execute;
begin
  FStatus:=tsExecuting;
  try
    DoExecute;
    FStatus:=tsExecuted;
  except
    FStatus:=tsExecuteFailed;
    if AutoUndo then
      try
        Undo;
      except
        // The execute error is the one to report; the status shows the undo failed
        on E: Exception do
          OPLogger.LogError(Self, SErrTaskUndoFailed, [E.Message]);
      end;
    Raise;
  end;
end;

procedure TOPTask.Undo;
begin
  FStatus:=tsUndoing;
  try
    DoUndo;
    FStatus:=tsUndone;
  except
    FStatus:=tsUndoFailed;
    Raise;
  end;
end;

end.

