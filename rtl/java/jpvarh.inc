{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2011 by Jonas Maebe,
    members of the Free Pascal development team.

    This file declares support infrastructure for procvars under the JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  { tmethod record }
  TMethod = record
    code: JLRMethod;
    data: jlobject;
  end;


  { base type for procedure variables }
  FpcBaseProcVarType = class(jlobject,jlcloneable)
    method: TMethod;

    constructor create(inst: jlobject; const methodName: unicodestring; const argTypes: array of JLClass);overload;
    constructor create(const meth: tmethod);overload;

    procedure setFpcBaseProcVarTypeBySignature(const methodName: unicodestring; const argTypes: array of JLClass); virtual;
    procedure fpcDeepCopy(result: FpcBaseProcVarType); virtual;
    function clone: JLObject; override;

   strict protected
    type
      { TJObjectArray isn't declared here yet }
      TJLObjectDynArray = array of JLObject;

    function getClassProcArgs(const args: array of jlobject): TJLObjectDynArray;
    procedure invokeProc(const args: array of jlobject); virtual;
    function invokeBooleanFunc(const args: array of jlobject): jboolean; virtual;
    function invokeCharFunc(const args: array of jlobject): jchar; virtual;
    function invokeByteFunc(const args: array of jlobject): jbyte; virtual;
    function invokeShortFunc(const args: array of jlobject): jshort; virtual;
    function invokeIntFunc(const args: array of jlobject): jint; virtual;
    function invokeLongFunc(const args: array of jlobject): jlong; virtual;
    function invokeSingleFunc(const args: array of jlobject): jfloat; virtual;
    function invokeDoubleFunc(const args: array of jlobject): jdouble; virtual;
    function invokeObjectFunc(const args: array of jlobject): jlobject; virtual;
  end;

  FpcBaseNestedProcVarType = class(FpcBaseProcVarType)
   strict protected
    { add the nestedfpstruct to the list of parameters }
    function getNestedArgs(const args: array of jlobject): TJLObjectDynArray; virtual;
   public
    nestedfpstruct: jlobject;

    constructor create(inst, context: jlobject; const methodName: unicodestring; const argTypes: array of JLClass);
    procedure fpcDeepCopy(result: FpcBaseProcVarType); override;
    function clone: JLObject; override;

   strict protected
    procedure invokeProc(const args: array of jlobject); override;
    function invokeBooleanFunc(const args: array of jlobject): jboolean; override;
    function invokeCharFunc(const args: array of jlobject): jchar; override;
    function invokeByteFunc(const args: array of jlobject): jbyte; override;
    function invokeShortFunc(const args: array of jlobject): jshort; override;
    function invokeIntFunc(const args: array of jlobject): jint; override;
    function invokeLongFunc(const args: array of jlobject): jlong; override;
    function invokeSingleFunc(const args: array of jlobject): jfloat; override;
    function invokeDoubleFunc(const args: array of jlobject): jdouble; override;
    function invokeObjectFunc(const args: array of jlobject): jlobject; override;
  end;

