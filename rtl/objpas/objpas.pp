{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998 by the Free Pascal development team

    This unit makes Free Pascal as much as possible Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit objpas;

{$I-,S-}

interface

{*****************************************************************************
                            Basic Types/constants
*****************************************************************************}

    const
       // vmtSelfPtr           = -36;  { not implemented yet }
       vmtIntfTable            = -32;
       vmtAutoTable            = -28;
       vmtInitTable            = -24;
       vmtTypeInfo             = -20;
       vmtFieldTable           = -16;
       vmtMethodTable          = -12;
       vmtDynamicTable         = -8;
       vmtClassName            = -4;
       vmtInstanceSize         = 0;
       vmtParent               = 8;
       vmtDestroy              = 12;
       vmtNewInstance          = 16;
       vmtFreeInstance         = 20;
       vmtSafeCallException    = 24;
       vmtDefaultHandler       = 28;
       vmtAfterConstruction    = 32;
       vmtBeforeDestruction    = 36;

    type
       { first, in object pascal, the types must be redefined }
       smallint = system.integer;
       integer  = system.longint;

       { some pointer definitions }
       pshortstring = ^shortstring;
       plongstring  = ^longstring;
       pansistring  = ^ansistring;
       pwidestring  = ^widestring;
       // pstring   = pansistring;
       pextended    = ^extended;
       ppointer     = ^pointer;

       { now the let's declare the base classes for the class object }
       { model                                                       }
       tobject = class;
       tclass  = class of tobject;
       pclass  = ^tclass;

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

          { new since Delphi 4 }
          procedure AfterConstruction;virtual;
          procedure BeforeDestruction;virtual;

          { interface functions, I don't know if we need this }
          {
          function getinterface(const iid : tguid;out obj) : boolean;
          class function getinterfaceentry(const iid : tguid) : pinterfaceentry;
          class function getinterfacetable : pinterfacetable;
          }
       end;

       TExceptProc = Procedure (Obj : TObject; Addr: Pointer);

       Const
          ExceptProc : Pointer {TExceptProc} = Nil;


{*****************************************************************************
                              Variant Type
*****************************************************************************}

    Const
       varEmpty     = $0000;
       varNull      = $0001;
       varSmallint  = $0002;
       varInteger   = $0003;
       varSingle    = $0004;
       varDouble    = $0005;
       varCurrency  = $0006;
       varDate      = $0007;
       varOleStr    = $0008;
       varDispatch  = $0009;
       varError     = $000A;
       varBoolean   = $000B;
       varVariant   = $000C;
       varUnknown   = $000D;
       varByte      = $0011;
       varString    = $0100;
       varAny       = $0101;
       varTypeMask  = $0FFF;
       varArray     = $2000;
       varByRef     = $4000;

       vtInteger    = 0;
       vtBoolean    = 1;
       vtChar       = 2;
       vtExtended   = 3;
       vtString     = 4;
       vtPointer    = 5;
       vtPChar      = 6;
       vtObject     = 7;
       vtClass      = 8;
       vtWideChar   = 9;
       vtPWideChar  = 10;
       vtAnsiString = 11;
       vtCurrency   = 12;
       vtVariant    = 13;
       vtInterface  = 14;
       vtWideString = 15;
       vtInt64      = 16;

    Type
       PVarRec = ^TVarRec;
       TVarRec = record
         case VType : Longint of
           vtInteger    : (VInteger: Integer);
           vtBoolean    : (VBoolean: Boolean);
           vtChar       : (VChar: Char);
           vtExtended   : (VExtended: PExtended);
           vtString     : (VString: PShortString);
           vtPointer    : (VPointer: Pointer);
           vtPChar      : (VPChar: PChar);
           vtObject     : (VObject: TObject);
           vtClass      : (VClass: TClass);
//           vtWideChar   : (VWideChar: WideChar);
//           vtPWideChar  : (VPWideChar: PWideChar);
           vtAnsiString : (VAnsiString: Pointer);
//           vtCurrency   : (VCurrency: PCurrency);
//           vtVariant    : (VVariant: PVariant);
//           vtInterface  : (VInterface: Pointer);
           vtWideString : (VWideString: Pointer);
//           vtInt64      : (VInt64: PInt64);
       end;


  implementation

{****************************************************************************
                  Internal Routines called from the Compiler
****************************************************************************}

    procedure finalize(data,typeinfo : pointer);external name 'FPC_FINALIZE';


    { the reverse order of the parameters make code generation easier }
    function int_do_is(aclass : tclass;aobject : tobject) : boolean;[public,alias: 'FPC_DO_IS'];
      begin
         int_do_is:=aobject.inheritsfrom(aclass);
      end;


    { the reverse order of the parameters make code generation easier }
    procedure int_do_as(aclass : tclass;aobject : tobject);[public,alias: 'FPC_DO_AS'];
      begin
         if assigned(aobject) and not(aobject.inheritsfrom(aclass)) then
           runerror(219);
      end;


{****************************************************************************
                               TOBJECT
****************************************************************************}

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

      function TObject.SafeCallException(exceptobject : tobject;
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

      procedure TObject.AfterConstruction;

        begin
        end;

      procedure TObject.BeforeDestruction;

        begin
        end;


{****************************************************************************
                             Exception Support
****************************************************************************}

{$i except.inc}


{****************************************************************************
                                Initialize
****************************************************************************}

begin
  InitExceptions;
end.
{
  $Log$
  Revision 1.17  1998-10-05 12:32:53  peter
    + assert() support

  Revision 1.16  1998/10/03 15:07:16  florian
    + TObject.AfterConstruction and TObject.BeforeDestruction of Delphi 4

  Revision 1.15  1998/09/24 16:13:48  michael
  Changes in exception and open array handling

  Revision 1.14  1998/09/23 12:40:43  michael
  Fixed TVarRec again. Should be OK now

  Revision 1.13  1998/09/23 12:18:32  michael
  + added VType in TVArRec

  Revision 1.12  1998/09/23 10:00:47  peter
    * tvarrec should be 8 bytes

  Revision 1.11  1998/09/22 15:30:07  peter
    * array of const update

  Revision 1.9  1998/09/16 13:08:19  michael
  Added AbstractErrorHandler

  Revision 1.8  1998/09/06 21:27:31  florian
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
