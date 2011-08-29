{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005,2011 by Florian Klaempfl and Jonas Maebe,
    members of the Free Pascal development team.

    This file implements support routines for AnsiStrings with FPC/JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$define FPC_ANSISTRING_TYPE_DEFINED}
type
  AnsistringClass = class sealed (JLObject)
   private
    fdata: TAnsiCharArray;
   public
    constructor Create(len: longint);overload;
    constructor Create(const arr: array of ansichar; length: longint);overload;
    constructor Create(const arr: array of unicodechar);overload;
    constructor Create(const u: unicodestring);overload;
    constructor Create(const a: ansistring);overload;
    constructor Create(const s: shortstring);overload;
    constructor Create(ch: ansichar);overload;
    constructor Create(ch: unicodechar);overload;
    class function CreateFromLiteralStringBytes(const u: unicodestring): ansistring; static;
    function charAt(index: jint): ansichar;
    function toUnicodeString: unicodestring;
    function toShortstring(maxlen: byte): shortstring;
    function toString: JLString; override;
//    function concat(const a: ansistring): ansistring;
//    function concatmultiple(const arr: array of ansistring): ansistring;
    function length: jint;
    property internChars: TAnsiCharArray read fdata;
  end;


