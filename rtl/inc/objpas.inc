{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1998,99 by the Free Pascal development team

    This unit makes Free Pascal as much as possible Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                  Internal Routines called from the Compiler
****************************************************************************}

    { the reverse order of the parameters make code generation easier }
    function int_do_is(aclass : tclass;aobject : tobject) : boolean;[public,alias: 'FPC_DO_IS'];
      begin
         int_do_is:=aobject.inheritsfrom(aclass);
      end;


    { the reverse order of the parameters make code generation easier }
    procedure int_do_as(aclass : tclass;aobject : tobject);[public,alias: 'FPC_DO_AS'];
      begin
         if assigned(aobject) and not(aobject.inheritsfrom(aclass)) then
           handleerror(219);
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
        exceptaddr : pointer) : longint;

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

      class function TObject.stringmessagetable : pstringmessagetable;

        type
           pdword = ^dword;

        begin
           stringmessagetable:=pstringmessagetable((pdword(Self)+vmtMsgStrPtr)^);
        end;

      procedure TObject.Dispatch(var message);

        type
           tmsgtable = record
              index : dword;
              method : pointer;
           end;

           pmsgtable = ^tmsgtable;

           pdword = ^dword;

        var
           index : dword;
           count,i : longint;
           msgtable : pmsgtable;
           p : pointer;
           vmt : tclass;

        begin
           index:=dword(message);
           vmt:=ClassType;
           while assigned(vmt) do
             begin
                msgtable:=pmsgtable((pdword(vmt)+vmtDynamicTable)^+4);
                count:=pdword((pdword(vmt)+vmtDynamicTable)^)^;
                { later, we can implement a binary search here }
                for i:=0 to count-1 do
                  begin
                     if index=msgtable[i].index then
                       begin
                          p:=msgtable[i].method;
                          asm
                             pushl message
                             pushl %esi
                             movl p,%edi
                             call *%edi
                          end;
                          exit;
                       end;
                  end;
                vmt:=vmt.ClassParent;
             end;
           DefaultHandler(message);
        end;

      procedure TObject.DispatchStr(var message);

        type
           pdword = ^dword;

        var
           name : shortstring;
           count,i : longint;
           msgstrtable : pmsgstrtable;
           p : pointer;
           vmt : tclass;

        begin
           name:=pshortstring(@message)^;
           vmt:=ClassType;
           while assigned(vmt) do
             begin
                count:=pdword((pdword(vmt)+vmtMsgStrPtr)^)^;
                msgstrtable:=pmsgstrtable((pdword(vmt)+vmtMsgStrPtr)^+4);
                { later, we can implement a binary search here }
                for i:=0 to count-1 do
                  begin
                     if name=msgstrtable[i].name^ then
                       begin
                          p:=msgstrtable[i].method;
                          asm
                             pushl message
                             pushl %esi
                             movl p,%edi
                             call *%edi
                          end;
                          exit;
                       end;
                  end;
                vmt:=vmt.ClassParent;
             end;
           DefaultHandlerStr(message);
        end;

      procedure TObject.DefaultHandler(var message);

        begin
        end;

      procedure TObject.DefaultHandlerStr(var message);

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

{
  $Log$
  Revision 1.5  1999-07-05 20:04:24  peter
    * removed temp defines

  Revision 1.4  1999/05/19 13:20:09  peter
    * fixed dispatchstr

  Revision 1.3  1999/05/17 21:52:37  florian
    * most of the Object Pascal stuff moved to the system unit

}
