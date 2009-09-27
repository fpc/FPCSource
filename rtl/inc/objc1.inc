{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by the Free Pascal development team

    This unit provides an interface to the Objective-C 1.0
    run time as defined by Apple

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

interface

{$inline on}

uses
  ctypes
{$ifdef unix}
  ,unixtype
{$endif}
  ;

{$packrecords c}

{$ifdef darwin}
const
  libname = 'objc';
  {$linkframework Foundation}
  {$define targetok}
{$endif}

{$ifndef targetok}
  {$error Add support for the current target to the objc1 unit }
{$endif}

const
  CLS_CLASS	 	      = $1;
  CLS_META		      = $2;

type
  { make all opaque types assignment-incompatible with other typed pointers by
    declaring them as pointers to empty records

    WARNING: do NOT change the names, types or field names/types of these
      types, as many are used internally by the compiler.
  }

  { BOOL is one byte and uses 0/1, just like Pascal }
  BOOL = boolean;

  tobjc_class = record
  end;
  pobjc_class = ^tobjc_class;
  _Class = pobjc_class;

  objc_object = record
    isa: pobjc_class;
  end;
  id = ^objc_object;
  pobjc_object = id;

  _fpc_objc_sel_type = record
  end;
  SEL = ^_fpc_objc_sel_type;

  objc_method = record
  end;
  Pobjc_method = ^objc_method;
  Method = Pobjc_method;
  PMethod = ^Method;

  IMP = function(target: id; msg: SEL): id; varargs; cdecl;

  objc_super = record
    receiver: id;
    _class: pobjc_class;
  end;
  pobjc_super = ^objc_super;

  _fpc_objc_protocol_type = record
  end;
  pobjc_protocol = ^_fpc_objc_protocol_type;
  ppobjc_protocol = ^pobjc_protocol;

  objc_ivar = packed record
  end;
  Pobjc_ivar = ^objc_ivar;
  Ivar = Pobjc_ivar;
  PIvar = ^Ivar;

  { type that certainly will be returned by address }
  tdummyrecbyaddrresult = record
    a: array[0..1000] of shortstring;
  end;

  ptrdiff_t = ptrint;

{ sending messages }
  function  objc_msgSend(self: id; op: SEL): id; cdecl; varargs; external libname;
  function  objc_msgSendSuper(const super: pobjc_super; op: SEL): id; cdecl; varargs; external libname;
  { The following two are declared as procedures with the hidden result pointer
    as their first parameter. This corresponds to the declaration below as far
    as the code generator is concerned (and is easier to handle in the compiler).  }
  function  objc_msgSend_stret(self: id; op: SEL): tdummyrecbyaddrresult; cdecl; varargs; external libname;
  function  objc_msgSendSuper_stret(const super: pobjc_super; op: SEL): tdummyrecbyaddrresult; cdecl; varargs; external libname;
{$ifdef cpui386}
  function  objc_msgSend_fpret (self: id; op: SEL): double; cdecl; varargs; external libname;
{$else cpui386}
  function  objc_msgSend_fpret (self: id; op: SEL): double; cdecl; varargs; external libname name 'objc_msgSend';
{$endif cpui386}

  function sel_getName(sel: SEL): PChar; cdecl; external libname;
  function sel_registerName(str: PChar): SEL; cdecl; external libname;
  function object_getClassName(obj: id): PChar; cdecl; external libname;
  function object_getIndexedIvars(obj: id ): Pointer; cdecl; external libname;

  function sel_getUid(const str: PChar): SEL; cdecl; external libname;

  function object_copy(obj:id; size:size_t):id; cdecl; external libname;
  function object_dispose(obj:id):id; cdecl; external libname;

  function object_getClass(obj:id): pobjc_class; cdecl;
  function object_setClass(obj:id; cls: pobjc_class):pobjc_class; cdecl;

  function object_getIvar(obj:id; _ivar:Ivar):id; cdecl;
  procedure object_setIvar(obj:id; _ivar:Ivar; value:id); cdecl;

  function object_setInstanceVariable(obj:id; name:pchar; value:pointer):Ivar; cdecl; external libname;
  function object_getInstanceVariable(obj:id; name:pchar; var outValue: Pointer):Ivar; cdecl; external libname;

  function objc_getClass(name:pchar):id; cdecl; external libname;
  function objc_getMetaClass(name:pchar):id; cdecl; external libname;
  function objc_lookUpClass(name:pchar):id; cdecl; external libname;
  function objc_getClassList(buffer:pClass; bufferCount:cint):cint; cdecl; external libname;

{$ifdef FPC_HAS_FEATURE_OBJECTIVEC1}
  function objc_getProtocol(name:pchar): pobjc_protocol; cdecl; weakexternal libname;
  function objc_copyProtocolList(outCount:pdword):ppobjc_protocol; cdecl; weakexternal libname;
{$endif}

  function class_getName(cls:pobjc_class):PChar; cdecl; inline;
  function class_isMetaClass(cls:pobjc_class):BOOL; cdecl;
  function class_getSuperclass(cls:pobjc_class):pobjc_class; cdecl; inline;

  function class_getVersion(cls:pobjc_class):longint; cdecl; external libname;
  procedure class_setVersion(cls:pobjc_class; version:longint); cdecl; external libname;

  function class_getInstanceSize(cls:pobjc_class):size_t; cdecl; external libname;

  function class_getInstanceVariable(cls:pobjc_class; name:pchar):Ivar; cdecl; external libname;
  function class_getClassVariable(cls:pobjc_class; name:pchar):Ivar; cdecl; external libname;
  function class_copyIvarList(cls:pobjc_class; outCount:pdword):PIvar; cdecl; external libname;

  function class_getInstanceMethod(cls:pobjc_class; name:SEL):Method; cdecl; external libname;
  function class_getClassMethod(cls:pobjc_class; name:SEL):Method; cdecl; external libname;
  function class_getMethodImplementation(cls:pobjc_class; name:SEL):IMP; cdecl; external libname;
  function class_getMethodImplementation_stret(cls:pobjc_class; name:SEL):IMP; cdecl; external libname;
  function class_respondsToSelector(cls:pobjc_class; sel:SEL):BOOL; cdecl; external libname;
  function class_copyMethodList(cls:pobjc_class; outCount:pdword):PMethod; cdecl; external libname;

  function class_conformsToProtocol(cls:pobjc_class; var protocol: pobjc_protocol):BOOL; cdecl; external libname;
  function class_copyProtocolList(cls:pobjc_class; var outCount: dword):ppobjc_protocol; cdecl; external libname;

  function class_createInstance(cls:pobjc_class; extraBytes:size_t):id; cdecl; external libname;

(*
  function objc_allocateClassPair(superclass:pobjc_class; name:pchar; extraBytes:size_t):pobjc_class; cdecl; external libname;
  procedure objc_registerClassPair(cls:pobjc_class); cdecl; external libname;
  function objc_duplicateClass(original:pobjc_class; name:pchar; extraBytes:size_t):pobjc_class; cdecl; external libname;
  procedure objc_disposeClassPair(cls:pobjc_class); cdecl; external libname;

  function class_addMethod(cls:pobjc_class; name:SEL; imp:IMP; types:pchar):BOOL; cdecl; external libname;
  function class_addIvar(cls:pobjc_class; name:pchar; size:size_t; alignment:uint8_t; types:pchar):BOOL; cdecl; external libname;
  function class_addProtocol(cls:pobjc_class; protocol:pProtocol):BOOL; cdecl; external libname;
*)

  function method_getName(m:Method):SEL; cdecl; inline;
  function method_getImplementation(m:Method):IMP; cdecl; inline;
  function method_getTypeEncoding(m:Method):Pchar; cdecl; inline;

  function method_getNumberOfArguments(m:Method):dword; cdecl; external libname;
(*
  function method_copyReturnType(m:Method):Pchar; cdecl; weakexternal libname;
  function method_copyArgumentType(m:Method; index:dword):Pchar; cdecl; weakexternal libname;
  procedure method_getReturnType(m:Method; dst:pchar; dst_len:size_t); cdecl; external libname;

  function method_setImplementation(m:Method; imp:IMP):IMP; cdecl; external libname;
*)

  function ivar_getName(v:Ivar):Pchar; cdecl; inline;
  function ivar_getTypeEncoding(v:Ivar):Pchar; cdecl; inline;
  function ivar_getOffset(v:Ivar):ptrdiff_t; cdecl; inline;

(*
  function sel_isEqual(lhs:SEL; rhs:SEL):BOOL; cdecl; external libname;
*)

implementation

type
  {* Method Template }
  Pobjc_method1 = ^objc_method1;
  Method1 = Pobjc_method1;

  objc_method1 = packed record
    method_name   : SEL;
    method_types  : PChar;
    method_imp    : IMP;
  end;
  Pobjc_method_list1 = ^objc_method_list1;
  PPobjc_method_list1 = ^Pobjc_method_list1;

  objc_method_list1 = packed record
    obsolete      : Pobjc_method_list1;
    method_count  : cint;
    {$ifdef __alpha__}
    space: cint;
    {$endif}
    method_list1  : array[0..0] of objc_method1;	{ variable length structure }
  end;

  {* Instance Variable Template}
  Pobjc_ivar1 = ^objc_ivar1;
  Ivar1 = Pobjc_ivar1;
  PIvar1 = ^Ivar1;
  objc_ivar1 = packed record
    ivar_name   : PChar;
    ivar_type   : PChar;
    ivar_offset : cint;
  {$ifdef __alpha__}
    space: cint;
  {$endif}
  end;

  Pobjc_ivar_list1 = ^objc_ivar_list1;
  objc_ivar_list1 = packed record
    ivar_count: cint;
    {$ifdef __alpha__}
    space: cint;
    {$endif}
    ivar_list: array[0..0] of objc_ivar1;		{ variable length structure }
  end;

  Pobjc_cache1 = ^objc_cache1;
  objc_cache1 = record
    mask      : cuint;            { total = mask + 1 }
    occupied  : cuint;
    buckets   : array[0..0] of Method1;
  end;

  Protocol1 = objc_object;

  Pobjc_protocol_list1 = ^objc_protocol_list1;
  objc_protocol_list1 = record
    next    : Pobjc_protocol_list1;
    count   : cint;
    list    : array[0..0] of Protocol1;
  end;

  pobjc_class1 = ^objc_class1;
  objc_class1 = packed record
	  isa           : Pobjc_class1;
	  super_class   : Pobjc_class1;
	  name          : PChar;
	  version       : culong;
	  info          : culong;
	  instance_size : culong;
	  ivars         : Pobjc_ivar_list1;
	  methodLists   : PPobjc_method_list1;
	  cache         : Pobjc_cache1;
 	  protocols     : Pobjc_protocol_list1;
  end;

  Pid = ^id;

function object_getClass(obj:id): pobjc_class; cdecl;
  begin
    if obj = nil then
      object_getClass := nil
    else
      begin
        object_getClass := pobjc_class(Pobjc_object(obj)^.isa);
      end;
  end;

function object_setClass(obj:id; cls: pobjc_class): pobjc_class; cdecl;
  begin
    // can this be done in that way?
    object_setClass := pobjc_class(Pobjc_object(obj)^.isa);
    Pobjc_object(obj)^.isa := pobjc_class(cls);
  end;

function object_getIvar(obj:id; _ivar:Ivar):id; cdecl;
  begin
    object_getIvar := nil;
    if not Assigned(obj) or
       not Assigned(_ivar) then
      Exit;
    object_getIvar := Pid(PtrUInt(obj) + ivar_getOffset(_ivar))^;
  end;

procedure object_setIvar(obj:id; _ivar:Ivar; value:id); cdecl;
  begin
    if not Assigned(obj) or
       not Assigned(_ivar) then
      Exit;
    Pid(PtrUInt(obj) + ivar_getOffset(_ivar))^ := value;
  end;

function class_getName(cls:pobjc_class):PChar; cdecl; inline;
  begin
    class_getName := pobjc_class1(cls)^.name;
  end;

function class_getSuperclass(cls:pobjc_class):pobjc_class; cdecl; inline;
  begin
    class_getSuperclass := pobjc_class(pobjc_class1(cls)^.super_class);
  end;

function class_isMetaClass(cls:_Class):BOOL; cdecl;
  begin
    class_isMetaClass := Assigned(cls) and (pobjc_class1(cls)^.Info = CLS_META);
  end;

function method_getName(m:Method):SEL; cdecl; inline;
  begin
    method_getName := Method1(m)^.method_name;
  end;

function method_getImplementation(m:Method):IMP; cdecl; inline;
  begin
    method_getImplementation := IMP(Method1(m)^.method_imp);
  end;

function method_getTypeEncoding(m:Method):Pchar; cdecl; inline;
  begin
    method_getTypeEncoding := Method1(m)^.method_types;
  end;

function ivar_getName(v:Ivar):Pchar; cdecl; inline;
  begin
    ivar_getName := IVar1(v)^.ivar_name;
  end;

function ivar_getTypeEncoding(v:Ivar):Pchar; cdecl; inline;
  begin
    ivar_getTypeEncoding := IVar1(v)^.ivar_type;
  end;

function ivar_getOffset(v:Ivar):ptrdiff_t; cdecl; inline;
  begin
    ivar_getOffset := ptrdiff_t(IVar1(v)^.ivar_offset);
  end;


end.
