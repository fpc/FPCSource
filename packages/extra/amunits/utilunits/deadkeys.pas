
{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998-2002 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{
        DeadKeyConvert

        When you want Intuition to send you keystrokes, you either get just
    the simple key-cap type keys (i.e. no cursor or function keys) or you
    get keycodes, which tell you nothing about what was on the key the
    user pressed.  DeadKeyConvert allows you to receive RAWKEY messages,
    then translate them into their ANSI key sequences.  These are known as
    cooked keys, and a single keystroke might be converted into as many as
    four characters (including the CSI).  See the ROM Kernel Manual and the
    Enhancer manual for details.
        The difference between this and RawKeyConvert is that this also
    handles the deadkeys - for example, pressing ALT-K, releasing it, and
    pressing o gives you an o with an umlaut.
        Also note that some keys will come back with a length of zero - the
    shift and alt keys, for example.  You can probably ignore them.
        Finally, I'll point out that, since this function calls RawKeyConvert,
    you need to call OpenConsoleDevice (defined in ConsoleUtils) before using
    it.
}

{
    History:
    A translation of DeadKeyConvert.p from PCQ Pascal.
    26 Aug 2000.
    nils.sjoholm@mailbox.swipnet.se
}

unit deadkeys;

interface

uses exec,intuition,console,inputevent;

function DeadKeyConvert(msg : pIntuiMessage; buffer : pchar;
                        bufsize : longint; keymap : pointer): longint;

implementation


function DeadKeyConvert(msg : pIntuiMessage; buffer : pchar;
                        bufsize : longint; keymap : pointer): longint;

var
    theevent  : tInputEvent;
    Temp      : ^pointer;
begin
    if msg^.IClass <> IDCMP_RAWKEY then
         DeadKeyConvert := -2;
    with theevent do begin
         ie_NextEvent := nil;
         ie_Class := IECLASS_RAWKEY;
         ie_SubClass := 0;
         ie_Code := msg^.Code;
         ie_Qualifier := msg^.Qualifier;
         Temp := msg^.IAddress;
         ie_position.ie_addr := Temp^;
    end;
    DeadKeyConvert := RawKeyConvert(Addr(theevent),buffer,bufsize,keymap);
end;

var
    my_exit : pointer;
    ConsoleRequest : tIOStdReq;

procedure CloseConsoleDevice;
begin
   CloseDevice(Addr(ConsoleRequest));
end;

begin
   ConsoleDevice := nil;
   OpenDevice(pchar('console.device'#0),-1,Addr(ConsoleRequest),0);
   ConsoleDevice := ConsoleRequest.io_Device;
   my_exit := ExitProc;
   ExitProc := @CloseConsoleDevice;
end.
