{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Unit ports;

{$mode objfpc}

{ Implements the
     port[] portw[] and portl[]
  constructs using Delphi classes }

Interface

type
   tport = class
     protected
       procedure writeport(p : longint;data : byte);
       function  readport(p : longint) : byte;
     public
       property pp[w : longint] : byte read readport write writeport;default;
   end;

   tportw = class
     protected
       procedure writeport(p : longint;data : word);
       function  readport(p : longint) : word;
     public
       property pp[w : longint] : word read readport write writeport;default;
   end;

   tportl = class
     Protected
       procedure writeport(p : longint;data : longint);
       function  readport(p : longint) : longint;
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


implementation

uses unix;

{ to give easy port access like tp with port[] }

procedure tport.writeport(p : Longint;data : byte);

begin
  unix.writeport (p,data)
end;

function tport.readport(p : Longint) : byte;

begin
  readport := unix.readportb (p);
end;

procedure tportw.writeport(p : longint;data : word);

begin
  unix.writeport (p,data)
end;

function tportw.readport(p : longint) : word;

begin
  readport := unix.readportw(p);
end;

procedure tportl.writeport(p : longint;data : longint);

begin
  unix.writeport (p,data)
end;

function tportl.readport(p : longint) : longint;

begin
  readPort := Unix.readportl(p);
end;

end.
  $Log$
  Revision 1.4  2002-09-07 16:01:27  peter
    * old logs removed and tabs fixed

}
