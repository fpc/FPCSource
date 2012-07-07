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
      method.code:=nil;
      { getDeclaredMethod does not search superclasses -> manually traverse
        until found. If we don't find it anywhere, we'll traverse up to the
        parent of java.lang.Object = null and throw a NullPointerException }
      repeat
        try
          method.code:=owningClass.getDeclaredMethod(methodName,argTypes);
        except
          on JLNoSuchMethodException do
            owningClass:=owningClass.getSuperClass;
        end;
      until assigned(method.code);

      { required to enable calling private methods in one class from another
        class -- can cause security exceptions if the security manager doesn't
        allow this though... }
      if not method.code.isAccessible then
        method.code.setAccessible(true);
    end;


  function FpcBaseProcVarType.getClassProcArgs(const args: array of jlobject): TJLObjectDynArray;
    var
      arglen: longint;
    begin
      { add the self pointer as first argument (Java class methods don't take an
        implicit self parameters, Pascal ones do) }
      arglen:=length(args);
      setlength(result,arglen+1);
      JLSystem.ArrayCopy(JLObject(@args),0,JLObject(result),1,arglen);
      result[0]:=method.data;
    end;


  procedure FpcBaseProcVarType.fpcDeepCopy(result: FpcBaseProcVarType);
    begin
      result.method:=method;
    end;


  function FpcBaseProcVarType.clone: JLObject;
    var
      field: JLRField;
      newmethodrec: tmethod;
    begin
      result:=inherited;
      { replace the method record pointer (the inherited clone will have copied
        it, and there is no way we can change it using Pascal code since it's
        not a pointer at the Pascal level) }
      newmethodrec:=method;
      field:=getClass.getField('method');
      { doesn't matter that it's a local variable, everything is garbage
        collected }
      field.&set(result,JLObject(@newmethodrec));
    end;


  procedure FpcBaseProcVarType.invokeProc(const args: array of jlobject);
    begin
      { caching the length would be faster, but that would have to be done
        in a synchronised way. Doing it at construction time and in fpcDeepCopy/
        clone is not enough, because the method field can be manipulated
        directly }
      try
        if length(method.code.getParameterTypes)=length(args) then
          method.code.invoke(method.data,args)
        else
          method.code.invoke(method.data,getClassProcArgs(args));
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeBooleanFunc(const args: array of jlobject): jboolean;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=JLBoolean(method.code.invoke(method.data,args)).booleanValue
        else
          result:=JLBoolean(method.code.invoke(method.data,getClassProcArgs(args))).booleanValue
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeCharFunc(const args: array of jlobject): jchar;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=JLCharacter(method.code.invoke(method.data,args)).charValue
        else
          result:=JLCharacter(method.code.invoke(method.data,getClassProcArgs(args))).charValue;
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeByteFunc(const args: array of jlobject): jbyte;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=JLByte(method.code.invoke(method.data,args)).byteValue
        else
          result:=JLByte(method.code.invoke(method.data,getClassProcArgs(args))).byteValue
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeShortFunc(const args: array of jlobject): jshort;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=JLShort(method.code.invoke(method.data,args)).shortValue
        else
          result:=JLShort(method.code.invoke(method.data,getClassProcArgs(args))).shortValue
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeIntFunc(const args: array of jlobject): jint;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=JLInteger(method.code.invoke(method.data,args)).intValue
        else
          result:=JLInteger(method.code.invoke(method.data,getClassProcArgs(args))).intValue
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeLongFunc(const args: array of jlobject): jlong;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=JLLong(method.code.invoke(method.data,args)).longValue
        else
          result:=JLLong(method.code.invoke(method.data,getClassProcArgs(args))).longValue;
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeSingleFunc(const args: array of jlobject): jfloat;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=JLFloat(method.code.invoke(method.data,args)).floatValue
        else
          result:=JLFloat(method.code.invoke(method.data,getClassProcArgs(args))).floatValue
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeDoubleFunc(const args: array of jlobject): jdouble;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=JLDouble(method.code.invoke(method.data,args)).doubleValue
        else
          result:=JLDouble(method.code.invoke(method.data,getClassProcArgs(args))).doubleValue
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
    end;


  function FpcBaseProcVarType.invokeObjectFunc(const args: array of jlobject): jlobject;
    begin
      try
        if length(method.code.getParameterTypes)=length(args) then
          result:=method.code.invoke(method.data,args)
        else
          result:=method.code.invoke(method.data,getClassProcArgs(args))
      except
        on e: JLRInvocationTargetException do
          raise e.getCause
      end;
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



