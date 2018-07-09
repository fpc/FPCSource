{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Implements the
     port[] portw[] and portl[]
  constructs using objects }

type
   tport = object
     private
       procedure writeport(p : longint;data : byte);inline;
       function  readport(p : longint) : byte;inline;
     public
       property pp[w : longint] : byte read readport write writeport;default;
   end;

   tportw = object
     private
       procedure writeport(p : longint;data : word);inline;
       function  readport(p : longint) : word;inline;
     public
       property pp[w : longint] : word read readport write writeport;default;
   end;

   tportl = object
     Private
       procedure writeport(p : longint;data : longint);inline;
       function  readport(p : longint) : longint;inline;
     Public
      property pp[w : Longint] : longint read readport write writeport;default;
   end;


    { Non-Instantiaded vars. As yet, they don't have to be instantiated,
      because there is no need for 'self' etc. }

var
   port,
   portb : tport;
   portw : tportw;
   portl : tportl;
