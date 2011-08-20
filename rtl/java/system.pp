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

{*****************************************************************************}
                                    interface
{*****************************************************************************}

{$define FPC_IS_SYSTEM}

{$I-,Q-,H-,R-,V-}
{$mode objfpc}

Type
  { The compiler has all integer types defined internally. Here
    we define only aliases }
  DWord    = LongWord;
  Cardinal = LongWord;
  Integer  = SmallInt;
  UInt64   = QWord;
  
  ValReal = Double;
  
  { map comp to int64, }
  Comp = Int64;

  HResult = type longint;

  { Java primitive types }
  jboolean = boolean;
  jbyte = shortint;
  jshort = smallint;
  jint = longint;
  jlong = int64;
  jchar = widechar;
  jfloat = single;
  jdouble = double;

const
{ max. values for longint and int}
  maxLongint  = $7fffffff;
  maxSmallint = 32767;

  maxint   = maxsmallint;

type
  { Java base class type }
  TObject = class external 'java.lang' name 'Object'
   protected
    function clone: TObject;
   public
    constructor create;
    function equals(obj: TObject): boolean;
    function hashcode: longint;
  end;

{$i innr.inc}
{$i jmathh.inc}

{*****************************************************************************}
                                 implementation
{*****************************************************************************}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

end.
