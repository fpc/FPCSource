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

Unit system;

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
  jbyte = shortint;
  jshort = smallint;
  jint = longint;
  jlong = int64;
  jchar = widechar;
  jfloat = single;
  jdouble = double;

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
{$i java_sysh.inc}
{$i java_sys.inc}

  FpcEnumValueObtainable = interface
    function fpcOrdinal: jint;
    function fpcGenericValueOf(__fpc_int: longint): JLEnum;
  end;

{$i jrech.inc}
{$i jseth.inc}
{$i jpvarh.inc}
{$i jsystemh_types.inc}
{$i jtvarh.inc}
{$i sstringh.inc}
{$i jdynarrh.inc}
{$i astringh.inc}
{$i jsystemh.inc}
{$i compproc.inc}


{*****************************************************************************}
                                 implementation
{*****************************************************************************}

{$i jtvar.inc}
{$i jdynarr.inc}

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

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

end.

