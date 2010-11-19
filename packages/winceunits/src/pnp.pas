unit PnP;

{$mode objfpc}

(*

  (pnp.h)

*)

//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//
//
// Use of this source code is subject to the terms of the Microsoft end-user
// license agreement (EULA) under which you licensed this SOFTWARE PRODUCT.
// If you did not accept the terms of the EULA, you are not authorized to use
// this source code. For a copy of the EULA, please see the LICENSE.RTF on your
// install media.
//
// --------------------------------------------------------------------------

interface

uses
  Windows;

const
  // Maximum size of a device interface name. The choice of value is arbitrary
  // but necessary for componenents that want to browse available interfaces
  // so that they can set up their message queues. This value does not include
  // the (required) terminating zero - that's already counted in DEVDETAIL.
  MAX_DEVCLASS_NAMELEN       = 64;

  // Indicates an ordinary "stream" interface: open/read/write/iocontrol/close.
  // Devices that do not specify anything else and which expose a "ABCN:"
  // type of name automatically generate a notification with this GUID and
  // and a name equal to the device name (e.g., "FOO2:").
  DEVCLASS_STREAM_STRING     = '{f8a6ba98-087a-43ac-a9d8-b7f13c5bae31}';
  DEVCLASS_STREAM_GUID: GUID = '{f8a6ba98-087a-43ac-a9d8-b7f13c5bae31}';

type
  DEVDETAIL = record
    guidDevClass: GUID;          // the device interface id for this notification
    dwReserved: DWORD;           // do not use this
    fAttached: BOOL;             // true if the di is present, false otherwise
    cbName: LongInt;             // byte count of the interface's name
    szName: TCHAR;               // beginning of the interface's name
  end;
  TDEVDETAIL = DEVDETAIL;
  PDEVDETAIL = ^DEVDETAIL;

implementation

end.

