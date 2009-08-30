{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    This unit makes Free Pascal as much as possible Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

    procedure DoDispCallByIDError(res : Pointer; const disp : IDispatch;desc : PDispDesc; params : Pointer);
      begin
        handleerrorframe(RuntimeErrorExitCodes[reVarDispatch],get_frame);
      end;


    procedure fpc_dispatch_by_id(Result: Pointer; const Dispatch: pointer;
      DispDesc: Pointer; Params: Pointer); compilerproc;
      type
        TDispProc = procedure(res : Pointer; const disp : IDispatch;desc : PDispDesc; params : Pointer);
      begin
        TDispProc(DispCallByIDProc)(Result,IDispatch(Dispatch),DispDesc,Params);
      end;

{****************************************************************************
                  Internal Routines called from the Compiler
****************************************************************************}

    { the reverse order of the parameters make code generation easier }
    function fpc_do_is(aclass : tclass;aobject : tobject) : boolean;[public,alias: 'FPC_DO_IS']; compilerproc;
      begin
         fpc_do_is:=assigned(aobject) and assigned(aclass) and
           aobject.inheritsfrom(aclass);
      end;


    { the reverse order of the parameters make code generation easier }
    function fpc_do_as(aclass : tclass;aobject : tobject): tobject;[public,alias: 'FPC_DO_AS']; compilerproc;
      begin
         if assigned(aobject) and not(aobject.inheritsfrom(aclass)) then
           handleerrorframe(219,get_frame);
         result := aobject;
      end;

    { interface helpers }
    procedure fpc_intf_decr_ref(var i: pointer);[public,alias: 'FPC_INTF_DECR_REF']; compilerproc;
      begin
        if assigned(i) then
          begin
            IUnknown(i)._Release;
            i:=nil;
          end;
      end;

    { local declaration for intf_decr_ref for local access }
    procedure intf_decr_ref(var i: pointer); [external name 'FPC_INTF_DECR_REF'];


    procedure fpc_intf_incr_ref(i: pointer);[public,alias: 'FPC_INTF_INCR_REF']; compilerproc;
      begin
         if assigned(i) then
           IUnknown(i)._AddRef;
      end;

    { local declaration of intf_incr_ref for local access }
    procedure intf_incr_ref(i: pointer); [external name 'FPC_INTF_INCR_REF'];

    procedure fpc_intf_assign(var D: pointer; const S: pointer);[public,alias: 'FPC_INTF_ASSIGN']; compilerproc;
      begin
         if assigned(S) then
           IUnknown(S)._AddRef;
         if assigned(D) then
           IUnknown(D)._Release;
         D:=S;
      end;

    procedure fpc_intf_assign_by_iid(var D: pointer; const S: pointer; const iid: TGUID);[public,alias: 'FPC_INTF_ASSIGN2']; compilerproc;
      var
        tmp : pointer;
      begin
         if assigned(S) then
           begin
             if IUnknown(S).QueryInterface(iid,tmp)<>S_OK then
               handleerror(219);
             if assigned(D) then
               IUnknown(D)._Release;
             D:=tmp;
           end
         else
           begin
             if assigned(D) then
               IUnknown(D)._Release;
             D:=nil;
           end;
      end;


    function fpc_intf_as(const S: pointer; const iid: TGUID): IInterface;[public,alias: 'FPC_INTF_AS']; compilerproc;
      var
        tmpi: pointer; // _AddRef before _Release
      begin
        if assigned(S) then
          begin
             if IUnknown(S).QueryInterface(iid,tmpi)<>S_OK then
               handleerror(219);
             pointer(fpc_intf_as):=tmpi;
          end
        else
          fpc_intf_as:=nil;
      end;


    function fpc_class_as_intf(const S: pointer; const iid: TGUID): IInterface;[public,alias: 'FPC_CLASS_AS_INTF']; compilerproc;
      var
        tmpi: pointer; // _AddRef before _Release
      begin
        if assigned(S) then
          begin
             if not TObject(S).GetInterface(iid,tmpi) then
               handleerror(219);
             pointer(fpc_class_as_intf):=tmpi;
          end
        else
          fpc_class_as_intf:=nil;
      end;


    function fpc_class_as_corbaintf(const S: pointer; const iid: Shortstring): Pointer;[public,alias: 'FPC_CLASS_AS_CORBAINTF']; compilerproc;
      var
        tmpi: pointer; // _AddRef before _Release
      begin
        if assigned(S) then
          begin
             if not TObject(S).GetInterface(iid,tmpi) then
               handleerror(219);
             fpc_class_as_corbaintf:=tmpi;
          end
        else
          fpc_class_as_corbaintf:=nil;
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

      class function TObject.InstanceSize : SizeInt;

        begin
           InstanceSize := PVmt(Self)^.vInstanceSize;
        end;

      var
        emptyintf: ptruint; public name 'FPC_EMPTYINTF';

      procedure InitInterfacePointers(objclass: tclass;instance : pointer);

        var
          ovmt: PVmt;
          i: longint;
          intftable: pinterfacetable;
          Res: pinterfaceentry;
        begin
          ovmt := PVmt(objclass);
          while assigned(ovmt) and (ovmt^.vIntfTable <> @emptyintf) do
            begin
              intftable:=ovmt^.vIntfTable;
              if assigned(intftable) then
              begin
                i:=intftable^.EntryCount;
                Res:=@intftable^.Entries[0];
                while i>0 do begin
                  if Res^.IType = etStandard then
                    ppointer(@(pbyte(instance)[Res^.IOffset]))^:=
                      pointer(Res^.VTable);
                  inc(Res);
                  dec(i);
                end;
              end;
              ovmt:=ovmt^.vParent;
            end;
        end;

      class function TObject.InitInstance(instance : pointer) : tobject; {$ifdef SYSTEMINLINE} inline; {$ENDIF}

        begin
           { the size is saved at offset 0 }
           fillchar(instance^, InstanceSize, 0);
           { insert VMT pointer into the new created memory area }
           { (in class methods self contains the VMT!)           }
           ppointer(instance)^:=pointer(self);
           if PVmt(self)^.vIntfTable <> @emptyintf then
             InitInterfacePointers(self,instance);
           InitInstance:=TObject(Instance);
        end;

      class function TObject.ClassParent : tclass;

        begin
           { type of self is class of tobject => it points to the vmt }
           { the parent vmt is saved at offset vmtParent              }
           classparent:=tclass(PVmt(Self)^.vParent);
        end;

      class function TObject.NewInstance : tobject;

        var
           p : pointer;

        begin
           getmem(p, InstanceSize);
           if p <> nil then
              InitInstance(p);
           NewInstance:=TObject(p);
        end;

      procedure TObject.FreeInstance;

        begin
           CleanupInstance;
           FreeMem(Pointer(Self));
        end;

      class function TObject.ClassType : TClass;

        begin
           ClassType:=TClass(Pointer(Self))
        end;

      type
         tmethodnamerec = packed record
            name : pshortstring;
            addr : pointer;
         end;

         tmethodnametable = packed record
           count : dword;
           entries : packed array[0..0] of tmethodnamerec;
         end;

         pmethodnametable =  ^tmethodnametable;

      class function TObject.MethodAddress(const name : shortstring) : pointer;

        var
           methodtable : pmethodnametable;
           i : dword;
           ovmt : PVmt;

        begin
           ovmt:=PVmt(self);
           while assigned(ovmt) do
             begin
                methodtable:=pmethodnametable(ovmt^.vMethodTable);
                if assigned(methodtable) then
                  begin
                     for i:=0 to methodtable^.count-1 do
                       if ShortCompareText(methodtable^.entries[i].name^, name)=0 then
                         begin
                            MethodAddress:=methodtable^.entries[i].addr;
                            exit;
                         end;
                  end;
                ovmt := ovmt^.vParent;
             end;
           MethodAddress:=nil;
        end;


      class function TObject.MethodName(address : pointer) : shortstring;
        var
           methodtable : pmethodnametable;
           i : dword;
           ovmt : PVmt;
        begin
           ovmt:=PVmt(self);
           while assigned(ovmt) do
             begin
                methodtable:=pmethodnametable(ovmt^.vMethodTable);
                if assigned(methodtable) then
                  begin
                     for i:=0 to methodtable^.count-1 do
                       if methodtable^.entries[i].addr=address then
                         begin
                            MethodName:=methodtable^.entries[i].name^;
                            exit;
                         end;
                  end;
                ovmt := ovmt^.vParent;
             end;
           MethodName:='';
        end;


      function TObject.FieldAddress(const name : shortstring) : pointer;
        type
           PFieldInfo = ^TFieldInfo;
           TFieldInfo =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
           packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
           record
             FieldOffset: PtrUInt;
             ClassTypeIndex: Word;
             Name: ShortString;
           end;

           PFieldTable = ^TFieldTable;
           TFieldTable =
{$ifndef FPC_REQUIRES_PROPER_ALIGNMENT}
           packed
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
           record
             FieldCount: Word;
             ClassTable: Pointer;
             { should be array[Word] of TFieldInfo;  but
               Elements have variant size! force at least proper alignment }
             Fields: array[0..0] of TFieldInfo
           end;

        var
           ovmt: PVmt;
           FieldTable: PFieldTable;
           FieldInfo: PFieldInfo;
           i: longint;

        begin
           if Length(name) > 0 then
           begin
             ovmt := PVmt(ClassType);
             while ovmt <> nil do
             begin
               FieldTable := PFieldTable(ovmt^.vFieldTable);
               if FieldTable <> nil then
               begin
                 FieldInfo := @FieldTable^.Fields[0];
                 for i := 0 to FieldTable^.FieldCount - 1 do
                 begin
                   if ShortCompareText(FieldInfo^.Name, name) = 0 then
                   begin
                     fieldaddress := Pointer(Self) + FieldInfo^.FieldOffset;
                     exit;
                   end;
                   FieldInfo := PFieldInfo(PByte(@FieldInfo^.Name) + 1 + Length(FieldInfo^.Name));
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
                   { align to largest field of TFieldInfo }
                   FieldInfo := Align(FieldInfo, SizeOf(PtrUInt));
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
                 end;
               end;
               { Try again with the parent class type }
               ovmt:=ovmt^.vParent;
             end;
           end;

           fieldaddress:=nil;
        end;

      function TObject.SafeCallException(exceptobject : tobject;
        exceptaddr : pointer) : longint;

        begin
           safecallexception:=0;
        end;

      class function TObject.ClassInfo : pointer;

        begin
          ClassInfo := PVmt(Self)^.vTypeInfo;
        end;

      class function TObject.ClassName : ShortString;

        begin
          ClassName := PVmt(Self)^.vClassName^;
        end;

      class function TObject.ClassNameIs(const name : string) : boolean;

        begin
        // call to ClassName inlined here, this eliminates stack and string copying.
           ClassNameIs:=ShortCompareText(PVmt(Self)^.vClassName^, name) = 0;
        end;

      class function TObject.InheritsFrom(aclass : TClass) : Boolean;

        var
           vmt: PVmt;

        begin
           vmt:=PVmt(self);
           while assigned(vmt) and (vmt <> PVmt(aclass)) do
             vmt := vmt^.vParent;
           InheritsFrom := (vmt = PVmt(aclass));
        end;

      class function TObject.stringmessagetable : pstringmessagetable;

        begin
          stringmessagetable:=PVmt(Self)^.vMsgStrPtr;
        end;

      type
         tmessagehandler = procedure(var msg) of object;


      procedure TObject.Dispatch(var message);

        type
           tmsgtable = packed record
              index : dword;
              method : pointer;
           end;

           pmsgtable = ^tmsgtable;

        var
           index : dword;
           count,i : longint;
           msgtable : pmsgtable;
           p : pointer;
           ovmt : PVmt;
           msghandler : tmessagehandler;

        begin
           index:=dword(message);
           ovmt := PVmt(ClassType);
           while assigned(ovmt) do
             begin
                // See if we have messages at all in this class.
                p:=ovmt^.vDynamicTable;
                If Assigned(p) then
                  begin
                     msgtable:=pmsgtable(p+4);
                     count:=pdword(p)^;
                  end
                else
                  Count:=0;
                { later, we can implement a binary search here }
                for i:=0 to count-1 do
                  begin
                     if index=msgtable[i].index then
                       begin
                          TMethod(msghandler).Code:=msgtable[i].method;
                          TMethod(msghandler).Data:=self;
                          msghandler(message);
                          exit;
                       end;
                  end;
                ovmt:=ovmt^.vParent;
             end;
           DefaultHandler(message);
        end;

      procedure TObject.DispatchStr(var message);

        type
           PSizeUInt = ^SizeUInt;

        var
           name : shortstring;
           count,i : longint;
           msgstrtable : pmsgstrtable;
           p: pstringmessagetable;
           ovmt : PVmt;
           msghandler : tmessagehandler;

        begin
           name:=pshortstring(@message)^;
           ovmt:=PVmt(ClassType);
           while assigned(ovmt) do
           begin
                p := ovmt^.vMsgStrPtr;
                if (P<>Nil) and (p^.count<>0) then
                  begin
                  count:=p^.count;
                  msgstrtable:=@p^.msgstrtable;
                  end
                else
                  Count:=0;
                { later, we can implement a binary search here }
                for i:=0 to count-1 do
                  begin
                     if name=msgstrtable[i].name^ then
                       begin
                          TMethod(msghandler).Code:=msgstrtable[i].method;
                          TMethod(msghandler).Data:=self;
                          msghandler(message);
                          exit;
                       end;
                  end;
                ovmt:=ovmt^.vParent;
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

        Type
          TRecElem = packed Record
            Info : Pointer;
            Offset : Longint;
          end;

{$ifdef CPU16}
          TRecElemArray = packed array[1..Maxint div sizeof(TRecElem)-1] of TRecElem;
{$else CPU16}
          TRecElemArray = packed array[1..Maxint] of TRecElem;
{$endif CPU16}

          PRecRec = ^TRecRec;
          TRecRec = record
            Size,Count : Longint;
            Elements : TRecElemArray;
          end;

        var
           vmt  : PVmt;
           temp : pbyte;
           count,
           i    : longint;
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
           recelem  : TRecElem;
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
        begin
           vmt := PVmt(ClassType);
           while vmt<>nil do
             begin
               { This need to be included here, because Finalize()
                 has should support for tkClass }
               Temp:= vmt^.vInitTable;
               if Assigned(Temp) then
                 begin
                   inc(Temp);
                   I:=Temp^;
                   inc(temp,I+1);                // skip name string;
                   temp:=aligntoptr(temp);
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
                   move(PRecRec(Temp)^.Count,Count,sizeof(Count));
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
                   Count:=PRecRec(Temp)^.Count;  // get element Count
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
                   For I:=1 to count do
{$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
                     begin
                       move(PRecRec(Temp)^.elements[I],RecElem,sizeof(RecElem));
                       With RecElem do
                         int_Finalize (pointer(self)+Offset,Info);
                     end;
{$else FPC_REQUIRES_PROPER_ALIGNMENT}
                     With PRecRec(Temp)^.elements[I] do
                       int_Finalize (pointer(self)+Offset,Info);
{$endif FPC_REQUIRES_PROPER_ALIGNMENT}
                 end;
               vmt:= vmt^.vParent;
             end;
        end;

      procedure TObject.AfterConstruction;

        begin
        end;

      procedure TObject.BeforeDestruction;

        begin
        end;

      function IsGUIDEqual(const guid1, guid2: tguid): boolean;
        begin
          IsGUIDEqual:=
            (guid1.D1=guid2.D1) and
            (PDWORD(@guid1.D2)^=PDWORD(@guid2.D2)^) and
            (PDWORD(@guid1.D4[0])^=PDWORD(@guid2.D4[0])^) and
            (PDWORD(@guid1.D4[4])^=PDWORD(@guid2.D4[4])^);
        end;

      function getinterfacebyentry(Instance: pointer; IEntry: pinterfaceentry; Corba: Boolean; out obj): boolean;
        var
          Getter: function: IInterface of object;
        begin
          Pointer(Obj) := nil;
          if Assigned(IEntry) and Assigned(Instance) then
          begin
            case IEntry^.IType of
              etStandard:
                begin
                  //writeln('Doing etStandard cast of ', TObject(Instance).classname(), ' with self = ', ptruint(Instance), ' and offset = ', IEntry^.IOffset);
                  Pbyte(Obj):=Pbyte(instance)+IEntry^.IOffset;
                end;
              etFieldValue:
                begin
                  // writeln('Doing etFieldValue cast of ', TObject(Instance).classname(), ' with offset = ', IEntry^.IOffset);
                  Pointer(obj) := PPointer(Pbyte(Instance)+IEntry^.IOffset)^;
                end;
              etVirtualMethodResult:
                begin
                  //writeln('Doing etVirtualMethodResult cast of ', TObject(Instance).classname());
                  TMethod(Getter).data := Instance;
                  TMethod(Getter).code := ppointer(Pbyte(Instance) + IEntry^.IOffset)^;
                  Pointer(obj) := Pointer(Getter());
                end;
              etStaticMethodResult:
                begin
                  //writeln('Doing etStaticMethodResult cast of ', TObject(Instance).classname());
                  TMethod(Getter).data := Instance;
                  TMethod(Getter).code := pointer(IEntry^.IOffset);
                  Pointer(obj) := Pointer(Getter());
                end;
            end;
          end;
          result := assigned(pointer(obj));
          if result and not Corba then
            IInterface(obj)._AddRef;
        end;

      function TObject.getinterface(const iid : tguid;out obj) : boolean;
        begin
          Result := getinterfacebyentry(self, getinterfaceentry(iid), false, obj);
        end;

      function TObject.getinterfacebystr(const iidstr : shortstring;out obj) : boolean;
        begin
          Result := getinterfacebyentry(self, getinterfaceentrybystr(iidstr), true, obj);
        end;

      function TObject.getinterface(const iidstr : shortstring;out obj) : boolean;
        begin
          Result := getinterfacebystr(iidstr,obj);
        end;

      class function TObject.getinterfaceentry(const iid : tguid) : pinterfaceentry;
        var
          i: longint;
          intftable: pinterfacetable;
          ovmt: PVmt;
        begin
          ovmt := PVmt(Self);
          while Assigned(ovmt) and (ovmt^.vIntfTable <> @emptyintf) do
          begin
            intftable:=ovmt^.vIntfTable;
            if assigned(intftable) then
            begin
              for i:=0 to intftable^.EntryCount-1 do
              begin
                result:=@intftable^.Entries[i];
                if assigned(Result^.iid) and IsGUIDEqual(Result^.iid^,iid) then
                  Exit;
              end;
            end;
            ovmt := ovmt^.vParent;
          end;
          result := nil;
        end;

      class function TObject.getinterfaceentrybystr(const iidstr : shortstring) : pinterfaceentry;
        var
          i: longint;
          intftable: pinterfacetable;
          ovmt: PVmt;
        begin
          ovmt := PVmt(Self);
          while Assigned(ovmt) and (ovmt^.vIntfTable <> @emptyintf) do
          begin
            intftable:=ovmt^.vIntfTable;
            if assigned(intftable) then
            begin
              for i:=0 to intftable^.EntryCount-1 do
              begin
                result:=@intftable^.Entries[i];
                if assigned(result^.iidstr) and (result^.iidstr^ = iidstr) then
                  Exit;
              end;
            end;
            ovmt := ovmt^.vParent;
          end;
          result:=nil;
        end;

      class function TObject.getinterfacetable : pinterfacetable;
        begin
          getinterfacetable:=PVmt(Self)^.vIntfTable;
        end;

{****************************************************************************
                               TINTERFACEDOBJECT
****************************************************************************}

    function TInterfacedObject.QueryInterface(
      const iid : tguid;out obj) : longint;stdcall;

      begin
         if getinterface(iid,obj) then
           result:=0
         else
           result:=longint(E_NOINTERFACE);
      end;

    function TInterfacedObject._AddRef : longint;stdcall;

      begin
         _addref:=interlockedincrement(frefcount);
      end;

    function TInterfacedObject._Release : longint;stdcall;

      begin
         _Release:=interlockeddecrement(frefcount);
         if _Release=0 then
           self.destroy;
      end;

    procedure TInterfacedObject.AfterConstruction;

      begin
         { we need to fix the refcount we forced in newinstance }
         { further, it must be done in a thread safe way        }
         declocked(frefcount);
      end;

    procedure TInterfacedObject.BeforeDestruction;

      begin
         if frefcount<>0 then
           HandleError(204);
      end;

    class function TInterfacedObject.NewInstance : TObject;

      begin
         NewInstance:=inherited NewInstance;
         if NewInstance<>nil then
           TInterfacedObject(NewInstance).frefcount:=1;
      end;

{****************************************************************************
                               TAGGREGATEDOBJECT
****************************************************************************}

    constructor TAggregatedObject.Create(const aController: IUnknown);

      begin
        inherited Create;
        { do not keep a counted reference to the controller! }
        fcontroller := Pointer(aController);
      end;

    function TAggregatedObject.QueryInterface(
      const iid : tguid;out obj) : longint;stdcall;

      begin
         Result := IUnknown(fcontroller).QueryInterface(iid, obj);
      end;

    function TAggregatedObject._AddRef : longint;stdcall;

      begin
         Result := IUnknown(fcontroller)._AddRef;
      end;

    function TAggregatedObject._Release : longint;stdcall;

      begin
         Result := IUnknown(fcontroller)._Release;
      end;

    function TAggregatedObject.GetController : IUnknown;

      begin
         Result := IUnknown(fcontroller);
      end;

{****************************************************************************
                               TContainedOBJECT
****************************************************************************}

    function TContainedObject.QueryInterface(
            const iid : tguid;out obj) : longint; stdcall;

    begin
      if getinterface(iid,obj) then
        result:=0
      else
        result:=longint(E_NOINTERFACE);
    end;

{****************************************************************************
                             Exception Support
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_EXCEPTIONS}
{$i except.inc}
{$endif FPC_HAS_FEATURE_EXCEPTIONS}
