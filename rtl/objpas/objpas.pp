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

{$ifdef VER0_99_5}
  {$warning objpas can't be compiled with FPC 0.99.5}
  interface
  implementation
  end.
{$else}


  interface

    const
       // vmtSelfPtr           = -36;  { not implemented yet }
       vmtIntfTable         = -32;
       vmtAutoTable         = -28;
       vmtInitTable         = -24;
       vmtTypeInfo          = -20;
       vmtFieldTable        = -16;
       vmtMethodTable       = -12;
       vmtDynamicTable      = -8;
       vmtClassName         = -4;
       vmtInstanceSize      = 0;
       vmtParent            = 8;
       vmtDestroy           = 12;
       vmtNewInstance       = 16;
       vmtFreeInstance      = 20;
       vmtSafeCallException = 24;
       vmtDefaultHandler    = 28;

    type
       { first, in object pascal, the types must be redefined }
       smallint = system.integer;
       integer = system.longint;

       { define some more types }
       shortstring = string;

       { some pointer definitions }
       pshortstring = ^shortstring;
       plongstring = ^longstring;
       pansistring = ^ansistring;
       pwidestring = ^widestring;
       // pstring = pansistring;
       pextended = ^extended;
       ppointer = ^pointer;

       { now the let's declare the base classes for the class object }
       { model                                                       }
       tobject = class;
       tclass = class of tobject;
       pclass = ^tclass;

       tobject = class
          { please don't change the order of virtual methods, because      }
          { their vmt offsets are used by some assembler code which uses   }
          { hard coded addresses      (FK)                                 }
          constructor create;
          { the virtual procedures must be in THAT order }
          destructor destroy;virtual;
          class function newinstance : tobject;virtual;
          procedure freeinstance;virtual;
          function safecallexception(exceptobject : tobject;
            exceptaddr : pointer) : integer;virtual;
          procedure defaulthandler(var message);virtual;

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

          class function methodaddress(const name : shortstring) : pointer;
          class function methodname(address : pointer) : shortstring;
          function fieldaddress(const name : shortstring) : pointer;

          { interface functions, I don't know if we need this }
          {
          function getinterface(const iid : tguid;out obj) : boolean;
          class function getinterfaceentry(const iid : tguid) : pinterfaceentry;
          class function getinterfacetable : pinterfacetable;
          }
       end;

       TExceptProc = Procedure (Obj : TObject; Addr: Pointer);

       var
          abstracterrorproc : pointer;
       Const
          ExceptProc : Pointer {TExceptProc} = Nil;

  implementation

    procedure finalize(data,typeinfo : pointer);external name 'FINALIZE';

    { the reverse order of the parameters make code generation easier }
    function _is(aclass : tclass;aobject : tobject) : boolean;[public,alias: 'DO_IS'];

      begin
         _is:=aobject.inheritsfrom(aclass);
      end;

    { the reverse order of the parameters make code generation easier }
    procedure _as(aclass : tclass;aobject : tobject);[public,alias: 'DO_AS'];

      begin
         if assigned(aobject) and not(aobject.inheritsfrom(aclass)) then
           runerror(219);
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

      constructor TObject.Create;

        begin
        end;

      destructor TObject.Destroy;

        begin
        end;

      procedure TObject.Free;

        begin
           // the call via self avoids a warning
           if self<>nil then
             self.destroy;
        end;

      class function TObject.InstanceSize : LongInt;

        type
           plongint = ^longint;

        begin
           { type of self is class of tobject => it points to the vmt }
           { the size is saved at offset 0                            }
           InstanceSize:=plongint(self)^;
        end;

      class function TObject.InitInstance(instance : pointer) : tobject;

        begin
           fillchar(instance^,self.instancesize,0);
           { insert VMT pointer into the new created memory area }
           { (in class methods self contains the VMT!)           }
           ppointer(instance)^:=pointer(self);
           InitInstance:=TObject(Instance);
        end;

      class function TObject.ClassParent : tclass;

        begin
           { type of self is class of tobject => it points to the vmt }
           { the parent vmt is saved at offset vmtParent              }
           classparent:=(pclass(self)+vmtParent)^;
        end;

      class function TObject.NewInstance : tobject;

        var
           p : pointer;

        begin
           getmem(p,instancesize);
           InitInstance(p);
           NewInstance:=TObject(p);
        end;

      procedure TObject.FreeInstance;

        var
           p : Pointer;

        begin
           CleanupInstance;

           { self is a register, so we can't pass it call by reference }
           p:=Pointer(Self);
           FreeMem(p,InstanceSize);
        end;

      function TObject.ClassType : TClass;

        begin
           ClassType:=TClass(Pointer(Self)^)
        end;

      class function TObject.MethodAddress(const name : shortstring) : pointer;

        begin
           methodaddress:=nil;
        end;

      class function TObject.MethodName(address : pointer) : shortstring;

        begin
           methodname:='';
        end;

      function TObject.FieldAddress(const name : shortstring) : pointer;

        begin
           fieldaddress:=nil;
        end;

      function TObject.safecallexception(exceptobject : tobject;
        exceptaddr : pointer) : integer;

        begin
           safecallexception:=0;
        end;

      class function TObject.ClassInfo : pointer;

        begin
           ClassInfo:=(PPointer(self)+vmtTypeInfo)^;
        end;

      class function TObject.ClassName : ShortString;

        begin
           ClassName:=PShortString((PPointer(Self)+vmtClassName)^)^;
        end;

      class function TObject.ClassNameIs(const name : string) : boolean;

        begin
           ClassNameIs:=ClassName=name;
        end;

      class function TObject.InheritsFrom(aclass : TClass) : Boolean;

        var
           c : tclass;

        begin
           c:=self;
           while assigned(c) do
             begin
                if c=aclass then
                  begin
                     InheritsFrom:=true;
                     exit;
                  end;
                c:=c.ClassParent;
             end;
           InheritsFrom:=false;
        end;

      procedure TObject.Dispatch(var message);

        begin
        end;

      procedure TObject.DefaultHandler(var message);

        begin
        end;

      procedure TObject.CleanupInstance;

        var
           vmt : tclass;

        begin
           vmt:=ClassType;
           while vmt<>nil do
             begin
                if Assigned(Pointer((Pointer(vmt)+vmtInitTable)^)) then
                  Finalize(Pointer(Self),Pointer((Pointer(vmt)+vmtInitTable)^));
                vmt:=vmt.ClassParent;
             end;
        end;

{$i except.inc}

begin
  InitExceptions
end.
{$endif VER0_99_5}
{
  $Log$
  Revision 1.8  1998-09-06 21:27:31  florian
    + method tobject.classinfo added

  Revision 1.7  1998/09/04 08:49:06  peter
    * 0.99.5 doesn't compile a whole objpas anymore to overcome crashes

  Revision 1.6  1998/08/23 20:58:52  florian
    + rtti for objects and classes
    + TObject.GetClassName implemented

  Revision 1.5  1998/07/30 16:10:11  michael
  + Added support for ExceptProc+

  Revision 1.4  1998/07/29 15:44:33  michael
   included sysutils and math.pp as target. They compile now.

  Revision 1.3  1998/07/29 10:09:28  michael
  + put in exception support

  Revision 1.2  1998/03/25 23:40:24  florian
    + stuff from old objpash.inc and objpas.inc merged in

}
