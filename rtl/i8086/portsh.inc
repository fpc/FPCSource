{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    These files adds support for TP styled port accesses

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
   tport = object
      procedure writeport(p : word;data : byte);inline;
      function  readport(p : word) : byte;inline;
      property pp[w : word] : byte read readport write writeport;default;
   end;

   tportw = object
      procedure writeport(p : word;data : word);inline;
      function  readport(p : word) : word;inline;
      property pp[w : word] : word read readport write writeport;default;
   end;

   tportl = object
      procedure writeport(p : word;data : longint);
      function  readport(p : word) : longint;
      property pp[w : word] : longint read readport write writeport;default;
   end;

var
{ we don't need to initialize port, because neither member
  variables nor virtual methods are accessed }
   port,
   portb : tport;
   portw : tportw;
   portl : tportl;
