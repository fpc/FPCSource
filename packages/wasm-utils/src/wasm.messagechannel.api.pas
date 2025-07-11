{
    This file is part of the Free Component Library

    Webassembly MessageChannel API - low-level interface.
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.messagechannel.api;

{$mode ObjFPC}{$H+}

interface

uses
  wasm.messagechannel.shared;


function __msgchannel_send_message_utf8(
  aID: TWasmMessageChannelID;
  aData: PByte;
  aDataLen: Longint;
  aDeserialize: Longint
  ): TWasmMessageChannelResult; external MsgChannelExportName name MsgChannelFN_SendUTF8;

function __msgchannel_send_message_utf16(
  aID: TWasmMessageChannelID;
  aData: PUnicodeChar;
  aDataCharLen: Longint;
  aDeserialize: Longint
  ): TWasmMessageChannelResult; external MsgChannelExportName name MsgChannelFN_SendUTF16;

function __msgchannel_allocate(
  aID: TWasmMessageChannelID;
  aType: Longint;
  aName: PAnsiChar;
  aNameLen: Longint
  ): TWasmMessageChannelResult; external MsgChannelExportName name MsgChannelFN_Allocate;

function __msgchannel_deallocate(
  aID: TWasmMessageChannelID
  ): TWasmMessageChannelResult; external MsgChannelExportName name MsgChannelFN_DeAllocate;

function __msgchannel_listen(
  aID: TWasmMessageChannelID;
  aUseUTF16 : boolean
  ): TWasmMessageChannelResult; external MsgChannelExportName name MsgChannelFN_Listen;

Type
  TWasmOnUTF8MessageCallBack = procedure(aID : TWasmMessageChannelID; S : AnsiString) of object;
  TWasmOnUTF16MessageCallBack = procedure(aID : TWasmMessageChannelID; S : UnicodeString) of object;

var
  OnMessageUTF8 : TWasmOnUTF8MessageCallBack;
  OnMessageUTF16 : TWasmOnUTF16MessageCallBack;

procedure messagechannel_onmesssage_callback_utf8(
  aID : TWasmMessageChannelID;
  aMsg : PAnsiChar;
  aMsgLen : Longint);

procedure messagechannel_onmesssage_callback_utf16(
  aID : TWasmMessageChannelID;
  aMsg : PUnicodeChar;
  aMsgLen : Longint);


implementation


procedure messagechannel_onmesssage_callback_utf8(aID: TWasmMessageChannelID; aMsg: PAnsiChar; aMsgLen: Longint);

var
  S : UTF8String;

begin
  if assigned(OnMessageUTF8) then
    begin
    SetLength(S,aMsgLen);
    if aMsgLen>0 then
      Move(aMsg^,S[1],aMsgLen);
    OnMessageUTF8(aID,S);
    end;
end;

procedure messagechannel_onmesssage_callback_utf16(aID: TWasmMessageChannelID; aMsg: PUnicodeChar; aMsgLen: Longint);

var
  S : UnicodeString;

begin
  if assigned(OnMessageUTF16) then
    begin
    SetLength(S,aMsgLen);
    if aMsgLen>0 then
      Move(aMsg^,S[1],aMsgLen*2);
    OnMessageUTF16(aID,S);
    end;
end;

exports
  messagechannel_onmesssage_callback_utf8, messagechannel_onmesssage_callback_utf16;

end.

