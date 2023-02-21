{
    Copyright (c) 1998-2022 by the Free Pascal development team

    Broadcom "mailbox" call interface

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{$IFNDEF FPC_DOTTEDUNITS}
unit mailbox;
{$ENDIF FPC_DOTTEDUNITS}

interface

type
    TMBox = bitpacked Array[0..36] of DWord;

var
    MBox: TMBox;

const
    MBOX_REQUEST    = 0;

    { Channels }
    MBOX_CH_POWER   = 0;
    MBOX_CH_FB      = 1;
    MBOX_CH_VUART   = 2;
    MBOX_CH_VCHIQ   = 3;
    MBOX_CH_LEDS    = 4;
    MBOX_CH_BTNS    = 5;
    MBOX_CH_TOUCH   = 6;
    MBOX_CH_COUNT   = 7;
    MBOX_CH_PROP    = 8;

    { Tags }
    MBOX_TAG_GETSERIAL     = $10004;
    MBOX_TAG_SETCLKRATE    = $38002;
    MBOX_TAG_LAST          = 0;

    { Responses }
    MBOX_RESPONSE   = $80000000;
    MBOX_FULL       = $80000000;
    MBOX_EMPTY      = $40000000;

    { Mailbox offsets from PeripheralBase }
    VIDEOCORE_MBOX  = $0000B880;
    MBOX_READ       = $0;
    MBOX_POLL       = $10;
    MBOX_SENDER     = $14;
    MBOX_STATUS     = $18;
    MBOX_CONFIG     = $1C;
    MBOX_WRITE      = $20;

function MailboxCall(BaseAddr: DWord; Channel: DWord): DWord;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
    EmbeddedApi.mmio;
{$ELSE FPC_DOTTEDUNITS}
uses
    mmio;
{$ENDIF FPC_DOTTEDUNITS}

function MailboxCall(BaseAddr: DWord; Channel: DWord): DWord;
var
    MBoxPtr: Pointer;
    R: PtrUint;
    MboxAddr: DWord;
begin
    MboxAddr := BaseAddr + VIDEOCORE_MBOX;

    MBoxPtr := Addr(MBox);

    while True do
    begin
        DUMMY(1);
        if (GET32(MboxAddr + MBOX_STATUS) and MBOX_FULL) = 0 then break;
    end;

    { Join the address and channel together to identify our message }
    R := (PtrUint(MBoxPtr) and (not $0F)) or (Channel and $0F);

    { Write to the mailbox - put the address of our message on the mailbox }
    PUT32(MboxAddr + MBOX_WRITE, R);

    while True do
    begin
        DUMMY(1);
        { Are there any messages pending in the mailbox? }
        if (GET32(MboxAddr + MBOX_STATUS) and MBOX_EMPTY) > 0 then continue;

        { Is it a response to our message? }
        if (GET32(MboxAddr + MBOX_READ) = R) then
        begin
            Exit(MBox[1]);
        end;
    end;

    MailboxCall := 0;
end;

end.
