{
    This file is part of the Free Component Library

    Webassembly MessageChannel API - shared parts with pas2js
    Copyright (c) 2025 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit wasm.messagechannel.shared;

interface
type
  TWasmMessageChannelResult = longint;
  TWasmMessageChannelID = longint;
  TWasmMessageChannelType = (ctWorker,ctBroadcast);


const
  WASMMSGCHANNEL_RESULT_SUCCESS         = 0;
  WASMMSGCHANNEL_RESULT_INVALIDCHANNEL  = -1;
  WASMMSGCHANNEL_RESULT_INVALIDDATALEN  = -2;
  WASMMSGCHANNEL_RESULT_INVALIDTYPE     = -3;
  WASMMSGCHANNEL_RESULT_UNSUPPORTEDTYPE = -4;


const
  MsgChannelExportName  = 'messagechannel';
  MsgChannelFN_Allocate = 'messagechannel_allocate';
  MsgChannelFN_DeAllocate = 'messagechannel_deallocate';
  MsgChannelFN_SendUTF8 = 'messagechannel_send_utf8';
  MsgChannelFN_SendUTF16 = 'messagechannel_send_utf16';
  MsgChannelFN_Listen = 'messagechannel_listen';
  MsgChannelFN_OnMessageUTF8 = 'messagechannel_onmesssage_callback_utf8';
  MsgChannelFN_OnMessageUTF16 = 'messagechannel_onmesssage_callback_utf16';

implementation

end.
