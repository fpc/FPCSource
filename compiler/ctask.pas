{
    This unit implements basic task handling for unit and package handling

    Copyright (c) 2005 by Florian Klaempfl

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

{$i fpcdefs.inc}

  interface

    uses
      cclasses;

    type
      tabstracttask = class;

      ttasklistitem = class(TLinkedListItem);
        task : tabstracttask;
        constructor create(p : ttabstracttask);
      end;

      tabstracttask = class
        lastchecked : aint;
        dependson : tlinkedlist;
        requiredby : tlinkedlist;
        destructor destroy;override;
      end;

      ttaskqueue = class
        run : aint;
        tasks : tlinkedlist;
        destructor destroy;override;

        procedure addtask(p : tabstracttask);
        procedure removetask(p : tabstracttask);
        procedure adddependency(p : tabstracttask;requires : tabstracttask);

        procedure markasdone(p : tabstracttask);
        procedure finished(p : tabstracttask);

        { searches for the next task to execute }
        function searchdoabletask : tabstracttask;
      end;

  implementation

    constructor ttasklistitem.create(p : ttabstracttask);
      begin
        inherited create;
        task:=p;
      end;


    destructor ttaskqueue.destroy;
      begin
        dependson.free;
        requiredby.free;
        inherited destroy;
      end;


    destructor ttaskqueue.destroy;
      begin
        tasks.free;
        inherited destroy;
      end;


    procedure ttaskqueue.addtask(p : tabstracttask);
      begin
        tasks.add(ttasklistitem.create(p));
      end;


    procedure ttaskqueue.tasktoitem(p : tabstracttask) : ttasklistitem;
      var
        hp : ttasklistitem;
      begin
        hp:=ttasklistitem(tasks.getfirst);
        while assigned(hp) do
          begin
            if hp.task=p then
              begin
                result:=hp.task;
                exit;
              end;
            hp:=ttasklistitem(hp.next);
          end;
        internalerror(2005052901);
      end;


    procedure ttaskqueue.removetask(p : tabstracttask);
      begin
        tasks.remove(tasktoitem(p));
      end;


    procedure ttaskqueue.markasdone(p : tabstracttask);
      begin
        { sanity check }
        if not(dependson.empty) then
          internalerror(2005052902);
        { walk through all tasks depending on the current one }
        !!!!
        removetask(p);
      end;


    procedure ttaskqueue.finished(p : tabstracttask);
      begin
        markasdone(p);
        p.free;
      end;


    function ttaskqueue.searchdoabletask : tabstracttask;
      var
        hp : ttasklistitem;
      begin
        inc(run);
        hp:=ttasklistitem(tasks.getfirst);
        while assigned(hp) do
          begin
            if hp.task.dependson.empty then
              begin
                result:=hp.task;
                exit;
              end;
            { did we touch this task already? }
            if hp.task.run=run then
              begin
                result:=nil;
                exit;
              end;
            { tag current task }
            hp.task.run:=run;
            next:=hp.next;

            { move task to the end of the queue }
            tasks.remove(hp);
            tasks.concat(hp);

            hp:=next;
          end;
      end;

end.
