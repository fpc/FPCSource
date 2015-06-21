{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Florian Klaempfl
    member of the Free Pascal development team.

    System unit for embedded systems

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit System;

{$namespace org.freepascal.rtl}

{*****************************************************************************}
                                    interface
{*****************************************************************************}

{$define FPC_IS_SYSTEM}

{$I-,Q-,H-,R-,V-,P+,T+}
{$implicitexceptions off}
{$mode objfpc}
{$modeswitch advancedrecords}

Type
  { Java primitive types }
  jboolean = boolean;
  pjboolean = ^boolean;
  jbyte = shortint;
  pjbyte = ^jbyte;
  jshort = smallint;
  pjshort = ^jshort;
  jint = longint;
  pjint = ^jint;
  jlong = int64;
  pjlong = ^jlong;
  jchar = widechar;
  pjchar = ^jchar;
  jfloat = single;
  pjfloat = ^jfloat;
  jdouble = double;
  pjdouble = ^jdouble;

  Arr1jboolean = array of jboolean;
  Arr1jbyte = array of jbyte;
  Arr1jshort = array of jshort;
  Arr1jint = array of jint;
  Arr1jlong = array of jlong;
  Arr1jchar = array of jchar;
  Arr1jfloat = array of jfloat;
  Arr1jdouble = array of jdouble;

  Arr2jboolean = array of Arr1jboolean;
  Arr2jbyte = array of Arr1jbyte;
  Arr2jshort = array of Arr1jshort;
  Arr2jint = array of Arr1jint;
  Arr2jlong = array of Arr1jlong;
  Arr2jchar = array of Arr1jchar;
  Arr2jfloat = array of Arr1jfloat;
  Arr2jdouble = array of Arr1jdouble;

  Arr3jboolean = array of Arr2jboolean;
  Arr3jbyte = array of Arr2jbyte;
  Arr3jshort = array of Arr2jshort;
  Arr3jint = array of Arr2jint;
  Arr3jlong = array of Arr2jlong;
  Arr3jchar = array of Arr2jchar;
  Arr3jfloat = array of Arr2jfloat;
  Arr3jdouble = array of Arr2jdouble;

const
  maxExitCode = 255;


{ Java base class type }
{$ifdef java}
{$define GOTJAVASYSINCLUDE}
{$i java_sysh.inc}
{$i java_sys.inc}
{$endif}

{$ifdef android}
{$define GOTJAVASYSINCLUDE}
{$i java_sysh_android.inc}
{$i java_sys_android.inc}
{$endif}

{$ifndef GOTJAVASYSINCLUDE}
{$error Missing include file with base Java classes}
{$endif}

  FpcEnumValueObtainable = interface
    function fpcOrdinal: jint;
    function fpcGenericValueOf(__fpc_int: longint): JLEnum;
  end;

{ generic versions are based on FPC/Delphi-style RTTI }
{$define FPC_STR_ENUM_INTERN}

{$i jrech.inc}
{$i jseth.inc}
{$i jpvarh.inc}

{$i jsystemh_types.inc}

{$i jtvarh.inc}
{$i jsstringh.inc}
{$i jdynarrh.inc}
{$i jastringh.inc}
{$i justringh.inc}

{$i jsystemh.inc}
{$i jtconh.inc}


{*****************************************************************************}
                                 implementation
{*****************************************************************************}

function min(a,b : longint) : longint;
  begin
     if a<=b then
       min:=a
     else
       min:=b;
  end;

{$i jtcon.inc}
{$i jtvar.inc}
{$i jsstrings.inc}
{$i jastrings.inc}
{$i justrings.inc}
{$i jrec.inc}
{$i jset.inc}
{$i jpvar.inc}
{$i jdynarr.inc}
{$i jsystem.inc}


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure System_exit;
  begin
    JLRuntime.getRuntime.exit(ExitCode);
  end;


procedure randomize;
  begin
    randseed:=JUCalendar.getInstance.getTimeInMillis;
  end;


type
  CopyOutVarModifiedException = class(JLException)
  end;

procedure fpc_var_copyout_mismatch(line,column: longint); compilerproc;
  var
    linestr,columnstr: unicodestring;
  begin
    str(line,linestr);
    str(column,columnstr);
    raise CopyOutVarModifiedException.create('Var parameter ending at line '+linestr+' column '+columnstr+' in the previous stack frame has been modified to a different value than the returned copyback value');
  end;

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

begin
 initunicodestringmanager
end.

