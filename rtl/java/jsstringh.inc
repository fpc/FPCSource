{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl,
    member of the Free Pascal development team.

    This file implements support routines for Shortstrings with FPC/JVM

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  TAnsiCharArray = array of ansichar;
  ShortstringClass = class sealed (JLObject,JLCloneable)
   public
    { "length byte" }
    curlen: byte;
    { length is always the maximum length of the string (so that even reads
      past the current length of the shortstring work, just like in regular
      shortstrings }
    fdata: TAnsiCharArray;
   public
    constructor Create(const arr: array of ansichar; maxlen: byte);overload;
    constructor Create(const arr: array of unicodechar; maxlen: byte);overload;
    constructor Create(const u: unicodestring; maxlen: byte);overload;
    constructor Create(const a: ansistring; maxlen: byte);overload;
    constructor Create(const s: shortstring; maxlen: byte);overload;
    constructor Create(ch: ansichar; maxlen: byte);overload;
    constructor Create(ch: unicodechar; maxlen: byte);overload;
    class function CreateEmpty(maxlen: byte): ShortstringClass; static;
    class function CreateFromLiteralStringBytes(const u: unicodestring): shortstring; static;
    procedure FpcDeepCopy(dest: ShortstringClass);
    procedure copyFromAnsiCharArray(const arr: array of ansichar; maxlen: byte);
    procedure setChar(index: jint; char: ansichar);
    function charAt(index: jint): ansichar;
    function toUnicodeString: unicodestring;
    function toAnsistring: ansistring;
    function toString: JLString; override;
    function clone: JLObject; override;
//    function concat(const a: shortstring): shortstring;
//    function concatmultiple(const arr: array of shortstring): shortstring;
    function length: jint;
  end;

  AnsiCharArrayClass = class sealed (JLObject)
   class function CreateFromLiteralStringBytes(const u: unicodestring): TAnsiCharArray; static;
  end;


