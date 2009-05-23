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

unit objc1;

interface

{$ifdef darwin}
const
  libname = 'objc';
  {$linkframework Cocoa}
  {$define targetok}
{$endif}

{$ifndef targetok}
  {$error Add support for the current target to the objc1 unit }
{$endif}

type
  { make all opaque types assignment-incompatible with other typed pointers by
    declaring them as pointers to empty records

    WARNING: do NOT change the names, types or field names/types of these
      types, as many are used internally by the compiler.
  }
  tobjc_class = record
  end;
  pobjc_class = ^tobjc_class;

  objc_object = record
    _class: pobjc_class;
  end;
  id = ^objc_object;

  _fpc_objc_sel_type = record
  end;
  SEL = ^_fpc_objc_sel_type;

  IMP = function(target: id; msg: SEL): id; varargs; cdecl;

  objc_super = record
    receiver: id;
    _class: pobjc_class;
  end;
  pobjc_super = ^objc_super;

  _fpc_objc_protocol_type = record
  end;
  pobjc_protocal = ^_fpc_objc_protocol_type;

  { type that certainly will be returned by address }
  tdummyrecbyaddrresult = record
    a: array[0..1000] of shortstring;
  end;

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
{$endif cpui386}

function class_getSuperclass(cls: pobjc_class): pobjc_class; cdecl; external libname;


implementation

end.
