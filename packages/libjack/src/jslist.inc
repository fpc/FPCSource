(*
  Based on gslist.c from glib-1.2.9 (LGPL).

  Adaption to JACK, Copyright (C) 2002 Kai Vehmanen.
    - replaced use of gtypes with normal ANSI C types
    - glib's memory allocation routines replaced with
      malloc/free calls

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation; either version 2.1 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

*)

{$ifndef __jack_jslist_h__}
{$define __jack_jslist_h__}

//#include <stdlib.h>
//#include <jack/systemdeps.h>
//{$I systemdeps.inc}

//#ifdef sun
//#define __inline__
//#endif

type
  JCompareFunc = function(a, b: Pointer): cint;

  PPJSList = ^PJSList;
  PJSList = ^JSList;
  JSList = record
    data: Pointer;
    next: PJSList;
  end;

function jack_slist_alloc: PJSList; inline;
function jack_slist_prepend (list: PJSList; data: Pointer): PJSList; inline;
function jack_slist_next(list: PJSList): PJSList; inline;
function jack_slist_last (list: PJSList): PJSList; inline;
function jack_slist_remove_link (list: PJSList;
                                 link: PJSList): PJSList; inline;
procedure jack_slist_free (list: PJSList); inline;
procedure jack_slist_free_1 (list: PJSList); inline;
function jack_slist_remove (list: PJSList;
                            data: Pointer): PJSList; inline;
function jack_slist_length (list: PJSList): cuint; inline;
function jack_slist_find (list: PJSList;
                          data: Pointer): PJSList; inline;
function jack_slist_copy (list: PJSList): PJSList; inline;
function jack_slist_append (list: PJSList;
                            data: Pointer): PJSList; inline;
function jack_slist_sort_merge (l1: PJSList;
                                l2: PJSList;
                                compare_func: JCompareFunc): PJSList; inline;
function jack_slist_sort (list: PJSList;
                          compare_func: JCompareFunc): PJSList; inline;

implementation

function jack_slist_alloc: PJSList; inline;
var
  new_list: PJSList;
begin
  new_list := New(PJSList);
  if new_list <> nil then
  begin
    new_list^.data := nil;
    new_list^.next := nil;
  end;
  jack_slist_alloc := new_list;
end;

function jack_slist_prepend (list: PJSList; data: Pointer): PJSList; inline;
var
  new_list: PJSList;
begin
  new_list := New(PJSList);
  if new_list <> nil then
  begin
    new_list^.data := data;
    new_list^.next := list;
  end;

  jack_slist_prepend := new_list;
end;

function jack_slist_next(list: PJSList): PJSList; inline;
begin
  if list <> nil then
    jack_slist_next := list^.next
  else
    jack_slist_next := nil;
end;

function jack_slist_last (list: PJSList): PJSList; inline;
begin
  if list <> nil then
  begin
    while list^.next <> nil do
      list := list^.next;
  end;

  jack_slist_last := list;
end;

function jack_slist_remove_link (list: PJSList;
                                 link: PJSList): PJSList; inline;
var
  tmp: PJSList;
  prev: PJSList;
begin
  prev := nil;
  tmp := list;

  while tmp <> nil do
  begin
    if tmp = link then
    begin
      if prev <> nil then
        prev^.next := tmp^.next;
      if list = tmp then
        list := list^.next;

      tmp^.next := nil;
      break;
    end;

    prev := tmp;
    tmp := tmp^.next;
  end;

  jack_slist_remove_link := list;
end;

procedure jack_slist_free (list: PJSList); inline;
var
  next: PJSList;
begin
  while list <> nil do
  begin
    next := list^.next;
    Dispose(list);
    list := next;
  end;
end;

procedure jack_slist_free_1 (list: PJSList); inline;
begin
  if list <> nil then
    Dispose(list);
end;

function jack_slist_remove (list: PJSList;
                            data: Pointer): PJSList; inline;
var
  tmp: PJSList;
  prev: PJSList;
begin
  prev := nil;
  tmp := list;

  while tmp <> nil do
  begin
    if tmp^.data = data then
    begin
      if prev <> nil then
        prev^.next := tmp^.next;
      if list = tmp then
        list := list^.next;

      tmp^.next := nil;
      jack_slist_free (tmp);

      break;
    end;

    prev := tmp;
    tmp := tmp^.next;
  end;

  jack_slist_remove := list;
end;

function jack_slist_length (list: PJSList): cuint; inline;
var
  length: cuint;
begin
  length := 0;
  while list <> nil do
  begin
    Inc(length);
    list := list^.next;
  end;

  jack_slist_length := length;
end;

function jack_slist_find (list: PJSList;
                          data: Pointer): PJSList; inline;
begin
  while list <> nil do
  begin
    if list^.data = data then
      break;
    list := list^.next;
  end;

  jack_slist_find := list;
end;

function jack_slist_copy (list: PJSList): PJSList; inline;
var
  new_list: PJSList = nil;
  last: PJSList;
begin
  if list <> nil then
  begin
    new_list := jack_slist_alloc ();
    new_list^.data := list^.data;
    last := new_list;
    list := list^.next;
    while list <> nil do
    begin
      last^.next := jack_slist_alloc ();
      last := last^.next;
      last^.data := list^.data;
      list := list^.next;
    end;
  end;

  jack_slist_copy := new_list;
end;

function jack_slist_append (list: PJSList;
                            data: Pointer): PJSList; inline;
var
  new_list: PJSList;
  last: PJSList;
begin
  new_list := jack_slist_alloc ();
  new_list^.data := data;

  if list <> nil then
  begin
    last := jack_slist_last (list);
    last^.next := new_list;

    jack_slist_append := list;
  end
  else
    jack_slist_append := new_list;
end;

function jack_slist_sort_merge (l1: PJSList;
                                l2: PJSList;
                                compare_func: JCompareFunc): PJSList; inline;
var
  list: JSList;
  l: PJSList;
begin
  l := @list;

  while (l1 <> nil) and (l2 <> nil) do
  begin
    if compare_func(l1^.data, l2^.data) < 0 then
    begin
      l^.next := l1;
      l := l^.next;
      l1 := l1^.next;
    end
    else
    begin
      l^.next := l2;
      l := l^.next;
      l2 := l2^.next;
    end;
  end;
  if l1 <> nil then
    l^.next := l1
  else
    l^.next := l2;

  jack_slist_sort_merge := list.next;
end;

function jack_slist_sort (list: PJSList;
                          compare_func: JCompareFunc): PJSList; inline;
var
  l1, l2: PJSList;
begin
  if list = nil then
  begin
    jack_slist_sort := nil;
    exit;
  end;
  if list^.next = nil then
  begin
    jack_slist_sort := list;
    exit;
  end;

  l1 := list;
  l2 := list^.next;

  l2 := l2^.next;
  while l2 <> nil do
  begin
    l2 := l2^.next;
    if l2 = nil then
      break;
    l1 := l1^.next;

    l2 := l2^.next;
  end;
  l2 := l1^.next;
  l1^.next := nil;

  jack_slist_sort := jack_slist_sort_merge (jack_slist_sort (list, compare_func),
                                            jack_slist_sort (l2, compare_func),
                                            compare_func);
end;

{$endif __jack_jslist_h__}

