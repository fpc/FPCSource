{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    This is unit implements some of the crt functionality
    for the gui win32 graph unit implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wincrt;

  interface

    function readkey : char;
    function keypressed : boolean;
    procedure delay(ms : word);

    { dummy }
    procedure textmode(mode : integer);

    { plays the windows standard sound }
    { hz is ignored (at least on win95 }
    procedure sound(hz : word);

    { dummy }
    procedure nosound;


  var
     directvideo : boolean;

     { dummy }
     lastmode : word;

  implementation

    uses
       windows,graph;

    const
       keybuffersize = 16;

    var
       keyboardhandling : TCriticalSection;
       keybuffer : array[1..keybuffersize] of char;
       nextfree,nexttoread : longint;

    procedure inccyclic(var i : longint);

      begin
         inc(i);
         if i>keybuffersize then
           i:=1;
      end;

    procedure addchar(c : char);

      begin
         EnterCriticalSection(keyboardhandling);
         keybuffer[nextfree]:=c;
         inccyclic(nextfree);
         { skip old chars }
         if nexttoread=nextfree then
           inccyclic(nexttoread);
         LeaveCriticalSection(keyboardhandling);
      end;

    function readkey : char;

      begin
         while true do
           begin
              EnterCriticalSection(keyboardhandling);
              if nexttoread<>nextfree then
                begin
                   readkey:=keybuffer[nexttoread];
                   inccyclic(nexttoread);
                   LeaveCriticalSection(keyboardhandling);
                   exit;
                end;
              LeaveCriticalSection(keyboardhandling);
              { give other threads a chance }
              Windows.Sleep(0);
           end;
      end;

    function keypressed : boolean;

      begin
         EnterCriticalSection(keyboardhandling);
         keypressed:=nexttoread<>nextfree;
         LeaveCriticalSection(keyboardhandling);
      end;

    procedure delay(ms : word);

      begin
         Sleep(ms);
      end;

    procedure textmode(mode : integer);

      begin
      end;

    procedure sound(hz : word);

      begin
         Windows.Beep(hz,500);
      end;

    procedure nosound;

      begin
      end;

    function msghandler(Window: hwnd; AMessage, WParam,
      LParam: Longint): Longint;

      begin
         case amessage of
           WM_CHAR:
             begin
                addchar(chr(wparam));
             end;
           WM_KEYDOWN:
             begin
             end;
         end;
         msghandler:=0;
      end;

    var
       oldexitproc : pointer;

    procedure myexitproc;

      begin
         exitproc:=oldexitproc;
         charmessagehandler:=nil;
         DeleteCriticalSection(keyboardhandling);
      end;

begin
   charmessagehandler:=@msghandler;
   nextfree:=1;
   nexttoread:=1;
   InitializeCriticalSection(keyboardhandling);
   oldexitproc:=exitproc;
   exitproc:=@myexitproc;
   lastmode:=0;
end.
{
  $Log$
  Revision 1.4  2000-03-05 13:07:58  florian
    + some more functions added, also some dummies

  Revision 1.3  2000/01/07 16:41:53  daniel
    * copyright 2000

  Revision 1.2  1999/11/29 22:03:39  florian
    * first implementation of winmouse unit

  Revision 1.1  1999/11/24 22:33:15  florian
    + created from extgraph
}
