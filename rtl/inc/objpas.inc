{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

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
    function fpc_do_is(aclass : tclass;aobject : tobject) : boolean;[public,alias: 'FPC_DO_IS']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
         fpc_do_is:=assigned(aobject) and assigned(aclass) and
           aobject.inheritsfrom(aclass);
      end;


    { the reverse order of the parameters make code generation easier }
    function fpc_do_as(aclass : tclass;aobject : tobject): tobject;[public,alias: 'FPC_DO_AS']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
         if assigned(aobject) and not(aobject.inheritsfrom(aclass)) then
           handleerrorframe(219,get_frame);
         result := aobject;
      end;

{$ifndef HASINTF}
    { dummies for make cycle with 1.0.x }
    procedure fpc_intf_decr_ref(var i: pointer);saveregisters;[public,alias: 'FPC_INTF_DECR_REF']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
      end;

    procedure fpc_intf_incr_ref(i: pointer);saveregisters;[public,alias: 'FPC_INTF_INCR_REF']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
      end;

    procedure fpc_intf_assign(var D: pointer; const S: pointer);[public,alias: 'FPC_INTF_ASSIGN']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
      end;

    function fpc_intf_as(const S: pointer; const iid: TGUID): pointer;[public,alias: 'FPC_INTF_AS']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
      end;

    function fpc_class_as_intf(const S: pointer; const iid: TGUID): pointer;[public,alias: 'FPC_CLASS_AS_INTF']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
      end;

{$else HASINTF}

    { interface helpers }
    procedure fpc_intf_decr_ref(var i: pointer);saveregisters;[public,alias: 'FPC_INTF_DECR_REF']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
        if assigned(i) then
          IUnknown(i)._Release;
        i:=nil;
      end;

    {$ifdef hascompilerproc}
    { local declaration for intf_decr_ref for local access }
    procedure intf_decr_ref(var i: pointer);saveregisters; [external name 'FPC_INTF_DECR_REF'];
    {$endif hascompilerproc}


    procedure fpc_intf_incr_ref(i: pointer);saveregisters;[public,alias: 'FPC_INTF_INCR_REF']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
         if assigned(i) then
           IUnknown(i)._AddRef;
      end;

    {$ifdef hascompilerproc}
    { local declaration of intf_incr_ref for local access }
    procedure intf_incr_ref(i: pointer);saveregisters; [external name 'FPC_INTF_INCR_REF'];
    {$endif hascompilerproc}

    procedure fpc_intf_assign(var D: pointer; const S: pointer);[public,alias: 'FPC_INTF_ASSIGN']; {$ifdef hascompilerproc} compilerproc; {$endif}
      begin
         if assigned(S) then
           IUnknown(S)._AddRef;
         if assigned(D) then
           IUnknown(D)._Release;
         D:=S;
      end;

    function fpc_intf_as(const S: pointer; const iid: TGUID): pointer;[public,alias: 'FPC_INTF_AS']; {$ifdef hascompilerproc} compilerproc; {$endif}
      const
        S_OK = 0;
      var
        tmpi: pointer; // _AddRef before _Release
      begin
        if assigned(S) then
          begin
             if IUnknown(S).QueryInterface(iid,tmpi)<>S_OK then
               handleerror(219);
             fpc_intf_as:=tmpi;
          end
        else
          fpc_intf_as:=nil;
      end;


    function fpc_class_as_intf(const S: pointer; const iid: TGUID): pointer;[public,alias: 'FPC_CLASS_AS_INTF']; {$ifdef hascompilerproc} compilerproc; {$endif}
      const
        S_OK = 0;
      var
        tmpi: pointer; // _AddRef before _Release
      begin
        if assigned(S) then
          begin
             if not TObject(S).GetInterface(iid,tmpi) then
               handleerror(219);
             fpc_class_as_intf:=tmpi;
          end
        else
          fpc_class_as_intf:=nil;
      end;
{$endif HASINTF}


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

        begin
           InstanceSize:=plongint(pointer(self)+vmtInstanceSize)^;
        end;

      procedure InitInterfacePointers(objclass: tclass;instance : pointer);

{$ifdef HASINTF}
        var
           intftable : pinterfacetable;
           i : longint;
        begin
          while assigned(objclass) do
            begin
               intftable:=pinterfacetable((pointer(objclass)+vmtIntfTable)^);
               if assigned(intftable) then
                 for i:=0 to intftable^.EntryCount-1 do
                   ppointer(@(PChar(instance)[intftable^.Entries[i].IOffset]))^:=
                     pointer(intftable^.Entries[i].VTable);
               objclass:=pclass(pointer(objclass)+vmtParent)^;
            end;
        end;
{$else HASINTF}
        begin
        end;
{$endif HASINTF}

      class function TObject.InitInstance(instance : pointer) : tobject;

        begin
           { the size is saved at offset 0 }
           fillchar(instance^,plongint(pointer(self)+vmtInstanceSize)^,0);
           { insert VMT pointer into the new created memory area }
           { (in class methods self contains the VMT!)           }
           ppointer(instance)^:=pointer(self);
{$ifdef HASINTF}
           InitInterfacePointers(self,instance);
{$endif HASINTF}
           InitInstance:=TObject(Instance);
        end;

      class function TObject.ClassParent : tclass;

        begin
           { type of self is class of tobject => it points to the vmt }
           { the parent vmt is saved at offset vmtParent              }
           classparent:=pclass(pointer(self)+vmtParent)^;
        end;

      class function TObject.NewInstance : tobject;

        var
           p : pointer;

        begin
           getmem(p,plongint(pointer(self)+vmtInstanceSize)^);
           if p <> nil then
              InitInstance(p);
           NewInstance:=TObject(p);
        end;

      procedure TObject.FreeInstance;

        begin
           CleanupInstance;
           FreeMem(Pointer(Self));
        end;

      function TObject.ClassType : TClass;

        begin
           ClassType:=TClass(Pointer(Self)^)
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
           UName : ShortString;
           methodtable : pmethodnametable;
           i : dword;
           vmt : tclass;

        begin
           UName := UpCase(name);
           vmt:=self;
           while assigned(vmt) do
             begin
                methodtable:=pmethodnametable((Pointer(vmt)+vmtMethodTable)^);
                if assigned(methodtable) then
                  begin
                     for i:=0 to methodtable^.count-1 do
                       if UpCase(methodtable^.entries[i].name^)=UName then
                         begin
                            MethodAddress:=methodtable^.entries[i].addr;
                            exit;
                         end;
                  end;
                vmt:=pclass(pointer(vmt)+vmtParent)^;
             end;
           MethodAddress:=nil;
        end;


      class function TObject.MethodName(address : pointer) : shortstring;
        var
           methodtable : pmethodnametable;
           i : dword;
           vmt : tclass;
        begin
           vmt:=self;
           while assigned(vmt) do
             begin
                methodtable:=pmethodnametable((Pointer(vmt)+vmtMethodTable)^);
                if assigned(methodtable) then
                  begin
                     for i:=0 to methodtable^.count-1 do
                       if methodtable^.entries[i].addr=address then
                         begin
                            MethodName:=methodtable^.entries[i].name^;
                            exit;
                         end;
                  end;
                vmt:=pclass(pointer(vmt)+vmtParent)^;
             end;
           MethodName:='';
        end;


      function TObject.FieldAddress(const name : shortstring) : pointer;
        type
           PFieldInfo = ^TFieldInfo;
           TFieldInfo = packed record
             FieldOffset: LongWord;
             ClassTypeIndex: Word;
             Name: ShortString;
           end;

           PFieldTable = ^TFieldTable;
           TFieldTable = packed record
             FieldCount: Word;
             ClassTable: Pointer;
             { Fields: array[Word] of TFieldInfo;  Elements have variant size! }
           end;

        var
           UName: ShortString;
           CurClassType: TClass;
           FieldTable: PFieldTable;
           FieldInfo: PFieldInfo;
           i: Integer;

        begin
           if Length(name) > 0 then
           begin
             UName := UpCase(name);
             CurClassType := ClassType;
             while CurClassType <> nil do
             begin
               FieldTable := PFieldTable((Pointer(CurClassType) + vmtFieldTable)^);
               if FieldTable <> nil then
               begin
                 FieldInfo := PFieldInfo(Pointer(FieldTable) + 6);
                 for i := 0 to FieldTable^.FieldCount - 1 do
                 begin
                   if UpCase(FieldInfo^.Name) = UName then
                   begin
                     fieldaddress := Pointer(Self) + FieldInfo^.FieldOffset;
                     exit;
                   end;
                   Inc(Pointer(FieldInfo), 7 + Length(FieldInfo^.Name));
                 end;
               end;
               { Try again with the parent class type }
               CurClassType:=pclass(pointer(CurClassType)+vmtParent)^;
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
           ClassInfo:=ppointer(Pointer(self)+vmtTypeInfo)^;
        end;

      class function TObject.ClassName : ShortString;

        begin
           ClassName:=PShortString((Pointer(Self)+vmtClassName)^)^;
        end;

      class function TObject.ClassNameIs(const name : string) : boolean;

        begin
           ClassNameIs:=Upcase(ClassName)=Upcase(name);
        end;

      class function TObject.InheritsFrom(aclass : TClass) : Boolean;

        var
           vmt : tclass;

        begin
           vmt:=self;
           while assigned(vmt) do
             begin
                if vmt=aclass then
                  begin
                     InheritsFrom:=true;
                     exit;
                  end;
                vmt:=pclass(pointer(vmt)+vmtParent)^;
             end;
           InheritsFrom:=false;
        end;

      class function TObject.stringmessagetable : pstringmessagetable;

        type
           pdword = ^dword;

        begin
           stringmessagetable:=pstringmessagetable((pointer(Self)+vmtMsgStrPtr)^);
        end;

      type
         tmessagehandler = procedure(var msg) of object;
         tmessagehandlerrec = packed record
            proc : pointer;
            obj : pointer;
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
           msghandler : tmessagehandler;

        begin
           index:=dword(message);
           vmt:=ClassType;
           while assigned(vmt) do
             begin
                // See if we have messages at all in this class.
                p:=pointer(vmt)+vmtDynamicTable;
                If Assigned(p) and (Pdword(p)^<>0) then
                  begin
                  msgtable:=pmsgtable(pdword(P)^+4);
                  count:=pdword(pdword(P)^)^;
                  end
                else
                  Count:=0;
                { later, we can implement a binary search here }
                for i:=0 to count-1 do
                  begin
                     if index=msgtable[i].index then
                       begin
                          p:=msgtable[i].method;
                          tmessagehandlerrec(msghandler).proc:=p;
                          tmessagehandlerrec(msghandler).obj:=self;
                          msghandler(message);
                          exit;
                       end;
                  end;
                vmt:=pclass(pointer(vmt)+vmtParent)^;
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
           msghandler : tmessagehandler;

        begin
           name:=pshortstring(@message)^;
           vmt:=ClassType;
           while assigned(vmt) do
             begin
                p:=(pointer(vmt)+vmtMsgStrPtr);
                If (P<>Nil) and (PDWord(P)^<>0) then
                  begin
                  count:=pdword(pdword(p)^)^;
                  msgstrtable:=pmsgstrtable(pdword(P)^+4);
                  end
                else
                  Count:=0;
                { later, we can implement a binary search here }
                for i:=0 to count-1 do
                  begin
                     if name=msgstrtable[i].name^ then
                       begin
                          p:=msgstrtable[i].method;
                          tmessagehandlerrec(msghandler).proc:=p;
                          tmessagehandlerrec(msghandler).obj:=self;
                          msghandler(message);
                          exit;
                       end;
                  end;
                vmt:=pclass(pointer(vmt)+vmtParent)^;
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
                  int_finalize(Pointer(Self),Pointer((Pointer(vmt)+vmtInitTable)^));
                vmt:=pclass(pointer(vmt)+vmtParent)^;
             end;
        end;

      procedure TObject.AfterConstruction;

        begin
        end;

      procedure TObject.BeforeDestruction;

        begin
        end;

{$ifdef HASINTF}
      function IsGUIDEqual(const guid1, guid2: tguid): boolean;
        begin
          IsGUIDEqual:=
            (guid1.D1=guid2.D1) and
            (PDWORD(@guid1.D2)^=PDWORD(@guid2.D2)^) and
            (PDWORD(@guid1.D4[0])^=PDWORD(@guid2.D4[0])^) and
            (PDWORD(@guid1.D4[4])^=PDWORD(@guid2.D4[4])^);
        end;

      function TObject.getinterface(const iid : tguid;out obj) : boolean;
        var
          IEntry: pinterfaceentry;
        begin
          IEntry:=getinterfaceentry(iid);
          if Assigned(IEntry) then begin
            PDWORD(@obj)^:=DWORD(PDWORD(Self))+IEntry^.IOffset;
            intf_incr_ref(pointer(obj)); { it must be an com interface }
            getinterface:=True;
          end
          else begin
            PDWORD(@Obj)^:=0;
            getinterface:=False;
          end;
        end;

      function TObject.getinterfacebystr(const iidstr : string;out obj) : boolean;
        var
          IEntry: pinterfaceentry;
        begin
          IEntry:=getinterfaceentrybystr(iidstr);
          if Assigned(IEntry) then begin
            PDWORD(@obj)^:=DWORD(PDWORD(Self))+IEntry^.IOffset;
            if Assigned(IEntry^.iid) then { for Com interfaces }
              intf_incr_ref(pointer(obj));
            getinterfacebystr:=True;
          end
          else begin
            PDWORD(@Obj)^:=0;
            getinterfacebystr:=False;
          end;
        end;

      class function TObject.getinterfaceentry(const iid : tguid) : pinterfaceentry;
        var
          i: integer;
          intftable: pinterfacetable;
          Res: pinterfaceentry;
        begin
          getinterfaceentry:=nil;
          intftable:=pinterfacetable((pointer(Self)+vmtIntfTable)^);
          if assigned(intftable) then begin
            i:=intftable^.EntryCount;
            Res:=@intftable^.Entries[0];
            while (i>0) and
               not (assigned(Res^.iid) and IsGUIDEqual(Res^.iid^,iid)) do begin
              inc(Res);
              dec(i);
            end;
            if (i>0) then
              getinterfaceentry:=Res;
          end;
        end;

      class function TObject.getinterfaceentrybystr(const iidstr : string) : pinterfaceentry;
        var
          i: integer;
          intftable: pinterfacetable;
          Res: pinterfaceentry;
        begin
          getinterfaceentrybystr:=nil;
          intftable:=getinterfacetable;
          if assigned(intftable) then begin
            i:=intftable^.EntryCount;
            Res:=@intftable^.Entries[0];
            while (i>0) and (Res^.iidstr^<>iidstr) do begin
              inc(Res);
              dec(i);
            end;
            if (i>0) then
              getinterfaceentrybystr:=Res;
          end;
        end;

      class function TObject.getinterfacetable : pinterfacetable;
        begin
          getinterfacetable:=pinterfacetable((pointer(Self)+vmtIntfTable)^);
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
           result:=longint($80004002);
      end;

    function TInterfacedObject._AddRef : longint;stdcall;

      begin
         inclocked(frefcount);
         _addref:=frefcount;
      end;

    function TInterfacedObject._Release : longint;stdcall;

      begin
         if declocked(frefcount) then
           begin
              destroy;
              _Release:=0;
           end
         else
           _Release:=frefcount;
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
         TInterfacedObject(NewInstance).frefcount:=1;
      end;

{$endif HASINTF}

{****************************************************************************
                             Exception Support
****************************************************************************}

{$i except.inc}

{****************************************************************************
                                Initialize
****************************************************************************}

{
  $Log$
  Revision 1.30  2002-10-19 15:53:20  peter
    * 'inlined' some more calls

  Revision 1.29  2002/10/15 19:29:49  peter
    * manual inline classparent calls in the loops

  Revision 1.28  2002/10/11 14:05:21  florian
    * initinterfacepointers improved

  Revision 1.27  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.26  2002/09/07 11:08:58  carl
    - remove logs

  Revision 1.25  2002/08/31 13:11:11  florian
    * several fixes for Linux/PPC compilation

}