{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1997,98 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{***********************************************************************
  This file includes the system and processor independed
  implementations of some Object Pascal features

  This file is intended to be used with Free Pascal and should be
  included in the system unit.
 ***********************************************************************}

{************************************************************************}
{                          misc. routines                                }
{************************************************************************}

  { the reverse order of the parameters make code generation easier }
  function _is(aclass : tclass;aobject : tobject) : boolean;[public,alias: 'DO_IS'];

    begin
       _is:=aobject.inheritsfrom(aclass);
    end;

  { the reverse order of the parameters make code generation easier }
  procedure _as(aclass : tclass;aobject : tobject);[public,alias: 'DO_AS'];

    begin
       if assigned(aobject) and not(aobject.inheritsfrom(aclass)) then
         { throw an exception }
    end;

  procedure abstracterror;[public,alias: 'ABSTRACTERROR'];

    type
       proc = procedure;

    begin
       if assigned(abstracterrorproc) then
         proc(abstracterrorproc)()
       else
         runerror(210);
    end;

{************************************************************************}
{                               TOBJECT                                  }
{************************************************************************}

    constructor tobject.create;

      begin
      end;

    destructor tobject.destroy;

      begin
      end;

    procedure tobject.free;

      begin
         if self<>nil then
           destroy;
      end;

    class function tobject.instancesize : longint;

      type
         plongint = ^longint;

      begin
         { type of self is class of tobject => it points to the vmt }
         { the size is saved at offset 0                            }
         instancesize:=plongint(self)^;
      end;

    class function tobject.initinstance(instance : pointer) : tobject;

      type
         ppointer = ^pointer;

      begin
         fillchar(instance^,self.instancesize,0);
         { insert VMT pointer into the new created memory area }
         { (in class methods self contains the VMT!)           }
         ppointer(instance)^:=pointer(self);
         initinstance:=tobject(instance);
      end;

    class function tobject.classparent : tclass;

      type
         ptclass = ^tclass;

      begin
         { type of self is class of tobject => it points to the vmt }
         { the parent vmt is saved at offset 8                      } 
         classparent:=(ptclass(self)+8)^;
      end;

    class function tobject.newinstance : tobject;

      var
         p : pointer;

      begin
         getmem(p,instancesize);
         initinstance(p);
         newinstance:=tobject(p);
      end;

    procedure tobject.freeinstance;

      var
         p : pointer;

      begin
         { !!! we should finalize some data }

         { self is a register, so we can't pass it call by reference }
         p:=pointer(self);
         freemem(p,instancesize);
      end;

    function tobject.classtype : tclass;

      begin
         classtype:=tclass(pointer(self)^)
      end;

    class function tobject.methodaddress(const name : shortstring) : pointer;

      begin
      end;

    class function tobject.methodname(address : pointer) : shortstring;

      begin
      end;

    function tobject.fieldaddress(const name : shortstring) : pointer;

      begin
      end;

    function tobject.safecallexception(exceptobject : tobject;
      exceptaddr : pointer) : integer;

      begin
      end;

    class function tobject.classinfo : pointer;

      begin
      end;

    class function tobject.classname : shortstring;

      begin
      end;

    class function tobject.classnameis(const name : string) : boolean;

      begin
      end;

    class function tobject.inheritsfrom(aclass : tclass) : boolean;

      var
         c : tclass;

      begin
         c:=self;
         while assigned(c) do
           begin
              if c=aclass then
                begin
                   inheritsfrom:=true;
                   exit;
                end;
              c:=c.classparent;
           end;
         inheritsfrom:=false;
      end;

    procedure tobject.dispatch(var message);

      begin
      end;

    procedure tobject.defaulthandler(var message);

      begin
      end;

    procedure tobject.cleanupinstance;

      begin
      end;

{
  $Log$
  Revision 1.1  1998-03-25 11:18:43  root
  Initial revision

  Revision 1.9  1998/02/03 22:12:25  florian
    + helper routines for is and as
    * fix of tobject.classparent
    + tobject.inheritsfrom

  Revision 1.8  1998/01/27 22:05:09  florian
    * again small fixes to DOM (Delphi Object Model)

  Revision 1.7  1998/01/26 12:00:18  michael
  + Added log at the end


  
  Working file: rtl/inc/objpas.inc
  description:
  ----------------------------
  revision 1.6
  date: 1998/01/25 22:30:49;  author: florian;  state: Exp;  lines: +3 -3
    * DOM: some fixes to tobject and the con-/destructor help routines
  ----------------------------
  revision 1.5
  date: 1998/01/23 18:08:31;  author: florian;  state: Exp;  lines: +19 -6
    * more bugs in FCL object model removed
  ----------------------------
  revision 1.4
  date: 1998/01/23 10:48:32;  author: florian;  state: Exp;  lines: +63 -7
    * syntax errors fixed
    + implementation of all methods added, at least with empty body
  ----------------------------
  revision 1.3
  date: 1998/01/16 23:10:53;  author: florian;  state: Exp;  lines: +19 -1
    + some tobject stuff
  ----------------------------
  revision 1.2
  date: 1998/01/10 11:08:58;  author: florian;  state: Exp;  lines: +53 -1
    + start of tobject impkentations
  ----------------------------
  revision 1.1
  date: 1998/01/09 16:05:43;  author: florian;  state: Exp;
    + ojbpash.inc and objpas.inc
    * $E- from objects.pp removed to avoid a warning
  =============================================================================
}
