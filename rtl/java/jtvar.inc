{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    member of the Free Pascal development team.

    This file implements support routines for threadvarq with FPC/JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

  function FpcImplicitPtrThreadVar.initialValue: JLObject;
    var
      owningClass: JLClass;
    begin
      { look up the clone method if we haven't done this yet }
      if not assigned(fCloneMethod) then
        begin
          owningClass:=fInstanceToClone.getClass;
          repeat
            try
              fCloneMethod:=owningClass.getDeclaredMethod('clone',[]);
            except
              on JLNoSuchMethodException do
                owningClass:=owningClass.getSuperClass;
            end;
          until assigned(fCloneMethod);
          { required to enable calling methods of non-public classes (e.g. a
            record type defined in the implementation of a unit) -- can cause
            security exceptions if the security manager doesn't allow this
            though... }
          if not fCloneMethod.isAccessible then
            fCloneMethod.setAccessible(true);
        end;
      { return a copy of the record/shortstring/set/... }
      result:=fCloneMethod.invoke(fInstanceToClone,[]);
    end;


  constructor FpcImplicitPtrThreadVar.create(initInstanceToClone: JLObject);
    begin
      fInstanceToClone:=initInstanceToClone;
    end;


  function FpcImplicitPtrThreadVar.getReadWriteReference: Pointer;
    begin
      { return the address of the record/shortstring/set/... }
      result:=Pointer(get);
    end;


  function FpcNormalArrayThreadVar.initialValue: JLObject;
    begin
      result:=fpc_dynarray_copy(fInstanceToClone,-1,-1,fArrDim,fArrTyp);
    end;


  constructor FpcNormalArrayThreadVar.create(initInstanceToClone: JLObject; arrdim: longint; arrtyp: widechar);
    begin
      inherited create(initInstanceToClone);
      fArrDim:=arrdim;
      fArrTyp:=arrtyp;
    end;


  function FpcBooleanThreadVar.initialValue: JLObject;
    var
      arr: TJBooleanArray;
    begin
      setlength(arr,1);
      result:=JLObject(arr);
    end;


  function FpcBooleanThreadVar.getReadWriteReference: PBoolean;
    var
      arr: TJBooleanArray;
    begin
      arr:=TJBooleanArray(get);
      result:=@arr[0];
    end;


  function FpcByteThreadVar.initialValue: JLObject;
    var
      arr: TJByteArray;
    begin
      setlength(arr,1);
      result:=JLObject(arr);
    end;


  function FpcByteThreadVar.getReadWriteReference: PShortint;
    var
      arr: TJByteArray;
    begin
      arr:=TJByteArray(get);
      result:=@arr[0];
    end;


  function FpcShortThreadVar.initialValue: JLObject;
    var
      arr: TJShortArray;
    begin
      setlength(arr,1);
      result:=JLObject(arr);
    end;


  function FpcShortThreadVar.getReadWriteReference: PSmallint;
    var
      arr: TJShortArray;
    begin
      arr:=TJShortArray(get);
      result:=@arr[0];
    end;


  function FpcIntThreadVar.initialValue: JLObject;
    var
      arr: TJIntArray;
    begin
      setlength(arr,1);
      result:=JLObject(arr);
    end;


  function FpcIntThreadVar.getReadWriteReference: PLongint;
    var
      arr: TJIntArray;
    begin
      arr:=TJIntArray(get);
      result:=@arr[0];
    end;


  function FpcLongThreadVar.initialValue: JLObject;
    var
      arr: TJLongArray;
    begin
      setlength(arr,1);
      result:=JLObject(arr);
    end;


  function FpcLongThreadVar.getReadWriteReference: PInt64;
    var
      arr: TJLongArray;
    begin
      arr:=TJLongArray(get);
      result:=@arr[0];
    end;


  function FpcCharThreadVar.initialValue: JLObject;
    var
      arr: TJCharArray;
    begin
      setlength(arr,1);
      result:=JLObject(arr);
    end;


  function FpcCharThreadVar.getReadWriteReference: PWideChar;
    var
      arr: TJCharArray;
    begin
      arr:=TJCharArray(get);
      result:=@arr[0];
    end;


  function FpcFloatThreadVar.initialValue: JLObject;
    var
      arr: TJFloatArray;
    begin
      setlength(arr,1);
      result:=JLObject(arr);
    end;


  function FpcFloatThreadVar.getReadWriteReference: PSingle;
    var
      arr: TJFloatArray;
    begin
      arr:=TJFloatArray(get);
      result:=@arr[0];
    end;


  function FpcDoubleThreadVar.initialValue: JLObject;
    var
      arr: TJDoubleArray;
    begin
      setlength(arr,1);
      result:=JLObject(arr);
    end;


  function FpcDoubleThreadVar.getReadWriteReference: PDouble;
    var
      arr: TJDoubleArray;
    begin
      arr:=TJDoubleArray(get);
      result:=@arr[0];
    end;


  function FpcPointerThreadVar.initialValue: JLObject;
    var
      arr: TJObjectArray;
    begin
      setlength(arr,1);
      arr[0]:=fInitVal;
      result:=JLObject(arr);
    end;


  constructor FpcPointerThreadVar.create(initVal: JLObject);
    begin
      fInitVal:=initVal;
    end;


  function FpcPointerThreadVar.getReadWriteReference: PPointer;
    var
      arr: TJObjectArray;
    begin
      arr:=TJObjectArray(get);
      result:=PPointer(@arr[0]);
    end;


