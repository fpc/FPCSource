{
    Free Pascal port of the Hermes C library.
    Copyright (C) 2001-2003  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C version by Christian Nentwich (c.nentwich@cs.ucl.ac.uk)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

type
  TListFreeCallback = procedure(p: Pointer);
  PHermesListElementStruct = ^THermesListElementStruct;
  THermesListElementStruct = record
    handle: THermesHandle;
    data: Pointer;
    next: PHermesListElementStruct;
  end;
  PHermesListElement = ^THermesListElement;
  THermesListElement = THermesListElementStruct;
  PHermesList = ^THermesList;
  THermesList = record
    first, last: PHermesListElement;
  end;

{function Hermes_ListNew: PHermesList;
procedure Hermes_ListDestroy(list: PHermesList);
function Hermes_ListElementNew(handle: THermesHandle): PHermesListElement;
procedure Hermes_ListAdd(list: PHermesList; element: PHermesListElement);
procedure Hermes_ListAddFront(list: PHermesList; element: PHermesListElement);
function Hermes_ListDeleteElement(list: PHermesList; handle: THermesHandle): Boolean;
function Hermes_ListLookup(list: PHermesList;
                           handle: THermesHandle): PHermesListElement;}

function Hermes_ListNew: PHermesList;
begin
  New(Result);
  Result^.first := nil;
  Result^.last := nil;
end;

procedure Hermes_ListDestroy(list: PHermesList; user_free: TListFreeCallback);
var
  tmp, run: PHermesListElement;
begin
  if list = nil then
    exit;
  run := list^.first;
  while run <> nil do
  begin
    tmp := run;
    run := run^.next;
    if (tmp^.data <> nil) and (user_free <> nil) then
      user_free(tmp^.data);
    Dispose(tmp);
  end;
  Dispose(list);
end;

function Hermes_ListElementNew(handle: THermesHandle): PHermesListElement;
begin
  New(Result);
  Result^.handle := handle;
  Result^.next := nil;
  Result^.data := nil;
end;

procedure Hermes_ListAdd(list: PHermesList; element: PHermesListElement);
begin
  if (list = nil) or (element = nil) then
    exit;
  if list^.first = nil then
  begin
    list^.first := element;
    list^.last := element;
    element^.next := nil;
    exit;
  end;
  list^.last^.next := element;
  list^.last := element;
  element^.next := nil;
end;

procedure Hermes_ListAddFront(list: PHermesList; element: PHermesListElement);
begin
  if (list = nil) or (element = nil) then
    exit;
  if list^.first = nil then
  begin
    list^.first := element;
    list^.last := element;
    element^.next := nil;
    exit;
  end;
  element^.next := list^.first;
  list^.first := element;
end;

function Hermes_ListDeleteElement(list: PHermesList; handle: THermesHandle; user_free: TListFreeCallback): Boolean;
var
  run, previous: PHermesListElement;
begin
  if list = nil then
  begin
    Result := False;
    exit;
  end;
  previous := nil;
  run := list^.first;
  while run <> nil do
  begin
    if run^.handle = handle then
    begin
      if run = list^.first then
        list^.first := run^.next
      else
        previous^.next := run^.next;

      if run = list^.last then
      begin
        list^.last := previous;
        if list^.last <> nil then
          list^.last^.next := nil;
      end;

      if (run^.data <> nil) and (user_free <> nil) then
        user_free(run^.data);

      Dispose(run);
      Result := True;
      exit;
    end;
    previous := run;
    run := run^.next;
  end;
  Result := False;
end;

function Hermes_ListLookup(list: PHermesList;
                           handle: THermesHandle): PHermesListElement;
var
  run: PHermesListElement;
begin
  if list = nil then
  begin
    Result := nil;
    exit;
  end;
  run := list^.first;
  while run <> nil do
  begin
    if run^.handle = handle then
    begin
      Result := run;
      exit;
    end;
    run := run^.next;
  end;
  Result := nil;
end;
