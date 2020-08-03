{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2020 by the Free Pascal development team.

    These files adds support for TP styled port accesses

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
   tport = object
   private
      procedure writeport(p : byte;data : byte);inline;
      function  readport(p : byte) : byte;inline;
   public
      property pp[p : byte] : byte read readport write writeport;default;
   end;

var
{ we don't need to initialize port, because neither member
  variables nor virtual methods are accessed }
   port : tport;
   portb : tport absolute port;
