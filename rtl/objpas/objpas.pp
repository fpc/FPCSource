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
{ this unit makes Free Pascal as much as possible Delphi compatible }

unit objpas;

  interface  

    type       
       { first, in object pascal, the types must be redefined }
       smallint = system.integer;
       integer = system.longint;

       { define some more types }
       shortstring = string;

       { some pointer definitions }
       pshortstring = ^shortstring;
       // pansistring = ^ansistring;
       // pwidestring = ^widestring;
       // pstring = pansistring;
       pextended = ^extended;
       

       { now the let's declare the base classes for the class object }
       { model                                                       }
       tobject = class;
       tclass = class of tobject;

       tobject = class
          { please don't change the order of virtual methods, because      }
          { their vmt offsets are used by some assembler code which uses   }
          { hard coded addresses      (FK)                                 }
          constructor create;
          destructor destroy;virtual;
          class function newinstance : tobject;virtual;
          procedure freeinstance;virtual;
          procedure free;
          class function initinstance(instance : pointer) : tobject;
          procedure cleanupinstance;
          function classtype : tclass;
          class function classinfo : pointer;
          class function classname : shortstring;
          class function classnameis(const name : string) : boolean;
          class function classparent : tclass;
          class function instancesize : longint;
          class function inheritsfrom(aclass : tclass) : boolean;

          { message handling routines }
          procedure dispatch(var message);
          procedure defaulthandler(var message);virtual;

          class function methodaddress(const name : shortstring) : pointer;
          class function methodname(address : pointer) : shortstring;
          function fieldaddress(const name : shortstring) : pointer;

          { interface functions, I don't know if we need this }
          {
          function getinterface(const iid : tguid;out obj) : boolean;
          class function getinterfaceentry(const iid : tguid) : pinterfaceentry;
          class function getinterfacetable : pinterfacetable;
          }
          function safecallexception(exceptobject : tobject;
            exceptaddr : pointer) : integer;virtual;
       end;
       
       TExceptProc = Procedure (Obj : TObject; Addr: Pointer);
       
       var
          abstracterrorproc : pointer;
       Const
          ExceptProc : Pointer {TExceptProc} = Nil;
          

  implementation

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
           // the call via self avoids a warning
           if self<>nil then
             self.destroy;  
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
           methodaddress:=nil;
        end;

      class function tobject.methodname(address : pointer) : shortstring;

        begin
           methodname:='';
        end;

      function tobject.fieldaddress(const name : shortstring) : pointer;

        begin
           fieldaddress:=nil;
        end;

      function tobject.safecallexception(exceptobject : tobject;
        exceptaddr : pointer) : integer;

        begin
           safecallexception:=0;
        end;

      class function tobject.classinfo : pointer;

        begin
           classinfo:=nil;
        end;

      class function tobject.classname : shortstring;

        begin
           classname:='';
        end;

      class function tobject.classnameis(const name : string) : boolean;

        begin
           classnameis:=classname=name;
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

{$i except.inc}

begin
  InitExceptions
end.
{
  $Log$
  Revision 1.5  1998-07-30 16:10:11  michael
  + Added support for ExceptProc+

  Revision 1.4  1998/07/29 15:44:33  michael
   included sysutils and math.pp as target. They compile now.

  Revision 1.3  1998/07/29 10:09:28  michael
  + put in exception support

  Revision 1.2  1998/03/25 23:40:24  florian
    + stuff from old objpash.inc and objpas.inc merged in

}
