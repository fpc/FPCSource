{
    This file is part of the Free Pascal run time library.
    and implements some stuff for protected mode programming
    Copyright (c) 1999-2000 by the Free Pascal development team.

    These files adds support for TP styled port accesses

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
{$ifdef VER3_0}
   tport = object
      procedure writeport(p : word;data : byte);stdcall;
      function  readport(p : word) : byte;stdcall;
      property pp[w : word] : byte read readport write writeport;default;
   end;

   tportw = object
      procedure writeport(p : word;data : word);stdcall;
      function  readport(p : word) : word;stdcall;
      property pp[w : word] : word read readport write writeport;default;
   end;

   tportl = object
      procedure writeport(p : word;data : longint);stdcall;
      function  readport(p : word) : longint;stdcall;
      property pp[w : word] : longint read readport write writeport;default;
   end;
{$else VER3_0}
   tport = object
   private
      procedure writeport(p : word;data : byte);inline;
      function  readport(p : word) : byte;inline;
   public
      property pp[w : word] : byte read readport write writeport;default;
   end;

   tportw = object
   private
      procedure writeport(p : word;data : word);inline;
      function  readport(p : word) : word;inline;
   public
      property pp[w : word] : word read readport write writeport;default;
   end;

   tportl = object
   private
      procedure writeport(p : word;data : longint);inline;
      function  readport(p : word) : longint;inline;
   public
      property pp[w : word] : longint read readport write writeport;default;
   end;
{$endif VER3_0}
var
{ we don't need to initialize port, because neither member
  variables nor virtual methods are accessed }
   port,
   portb : tport;
   portw : tportw;
   portl : tportl;
