{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    members of the Free Pascal development team.

    This file implements support infrastructure for procvars under the JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


  constructor FpcBaseProcVarType.create(inst: jlobject; const methodName: unicodestring; const argTypes: array of JLClass);
    begin
      method.data:=inst;
      setFpcBaseProcVarTypeBySignature(methodName,argtypes);
    end;


  constructor FpcBaseProcVarType.create(const meth: tmethod);
    begin
      method:=meth;
    end;


  procedure FpcBaseProcVarType.setFpcBaseProcVarTypeBySignature(const methodName: unicodestring; const argTypes: array of JLClass);
    var
      owningClass: JLClass;
    begin
      { class method or instance method }
      if method.data is JLClass then
        owningClass:=JLClass(method.data)
      else
        owningClass:=method.data.getClass;
      method.code:=owningClass.getDeclaredMethod(methodName,argTypes);
      { required to enable calling private methods in one class from another
        class -- can cause security exceptions if the security manager doesn't
        allow this though... }
      if not method.code.isAccessible then
        method.code.setAccessible(true);
    end;


  procedure FpcBaseProcVarType.fpcDeepCopy(result: FpcBaseProcVarType);
    begin
      result.method:=method;
    end;


  function FpcBaseProcVarType.clone: JLObject;
    begin
      result:=inherited;
      FpcBaseProcVarType(result).method:=method;
    end;


  procedure FpcBaseProcVarType.invokeProc(const args: array of jlobject);
    begin
      method.code.invoke(method.data,args);
    end;


  function FpcBaseProcVarType.invokeBooleanFunc(const args: array of jlobject): jboolean;
    begin
      result:=JLBoolean(method.code.invoke(method.data,args)).booleanValue;
    end;


  function FpcBaseProcVarType.invokeCharFunc(const args: array of jlobject): jchar;
    begin
      result:=JLCharacter(method.code.invoke(method.data,args)).charValue;
    end;


  function FpcBaseProcVarType.invokeByteFunc(const args: array of jlobject): jbyte;
    begin
      result:=JLByte(method.code.invoke(method.data,args)).byteValue;
    end;


  function FpcBaseProcVarType.invokeShortFunc(const args: array of jlobject): jshort;
    begin
      result:=JLShort(method.code.invoke(method.data,args)).shortValue;
    end;


  function FpcBaseProcVarType.invokeIntFunc(const args: array of jlobject): jint;
    begin
      result:=JLInteger(method.code.invoke(method.data,args)).intValue;
    end;


  function FpcBaseProcVarType.invokeLongFunc(const args: array of jlobject): jlong;
    begin
      result:=JLLong(method.code.invoke(method.data,args)).longValue;
    end;


  function FpcBaseProcVarType.invokeSingleFunc(const args: array of jlobject): jfloat;
    begin
      result:=JLFloat(method.code.invoke(method.data,args)).floatValue;
    end;


  function FpcBaseProcVarType.invokeDoubleFunc(const args: array of jlobject): jdouble;
    begin
      result:=JLDouble(method.code.invoke(method.data,args)).doubleValue;
    end;


  function FpcBaseProcVarType.invokeObjectFunc(const args: array of jlobject): jlobject;
    begin
      result:=method.code.invoke(method.data,args);
    end;




  function FpcBaseNestedProcVarType.getNestedArgs(const args: array of jlobject): TJLObjectDynArray;
    var
      arglen: longint;
    begin
      { add the parentfp struct pointer as last argument (delphi nested cc
        "calling convention") }
      arglen:=length(args);
      setlength(result,arglen+1);
      JLSystem.ArrayCopy(JLObject(@args),0,JLObject(result),0,arglen);
      result[arglen]:=nestedfpstruct;
    end;


  constructor FpcBaseNestedProcVarType.create(inst, context: jlobject; const methodName: unicodestring; const argTypes: array of JLClass);
    begin
      inherited create(inst,methodName,argTypes);
      nestedfpstruct:=context;
    end;


  procedure FpcBaseNestedProcVarType.fpcDeepCopy(result: FpcBaseProcVarType);
    begin
      inherited fpcDeepCopy(result);
      FpcBaseNestedProcVarType(result).nestedfpstruct:=nestedfpstruct;
    end;


  function FpcBaseNestedProcVarType.clone: JLObject;
    begin
      result:=inherited;
      FpcBaseNestedProcVarType(result).nestedfpstruct:=nestedfpstruct;
    end;


  procedure FpcBaseNestedProcVarType.invokeProc(const args: array of jlobject);
    begin
      inherited invokeProc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeBooleanFunc(const args: array of jlobject): jboolean;
    begin
      result:=inherited invokeBooleanFunc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeCharFunc(const args: array of jlobject): jchar;
    begin
      result:=inherited invokeCharFunc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeByteFunc(const args: array of jlobject): jbyte;
    begin
      result:=inherited invokeByteFunc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeShortFunc(const args: array of jlobject): jshort;
    begin
      result:=inherited invokeShortFunc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeIntFunc(const args: array of jlobject): jint;
    begin
      result:=inherited invokeIntFunc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeLongFunc(const args: array of jlobject): jlong;
    begin
      result:=inherited invokeLongFunc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeSingleFunc(const args: array of jlobject): jfloat;
    begin
      result:=inherited invokeSingleFunc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeDoubleFunc(const args: array of jlobject): jdouble;
    begin
      result:=inherited invokeDoubleFunc(getNestedArgs(args));
    end;


  function FpcBaseNestedProcVarType.invokeObjectFunc(const args: array of jlobject): jlobject;
    begin
      result:=inherited invokeObjectFunc(getNestedArgs(args));
    end;



