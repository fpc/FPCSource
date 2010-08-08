{%MainUnit ddk.pas}
{
    Driver Development Kit for Native NT
    Basic types used in Kernel Mode

    This file is part of the Free Pascal run time library.
    Copyright (c) 2009 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  _DEVICE_OBJECT = packed record

  end;
  TDeviceObject = _DEVICE_OBJECT;
  PDeviceObject = ^TDeviceObject;

  _FAST_IO_DISPATCH = packed record

  end;
  TFastIODispatch = _FAST_IO_DISPATCH;
  PFastIODispatch = ^TFastIODispatch;

  _DRIVER_EXTENSION = packed record

  end;
  TDriverExtension = _DRIVER_EXTENSION;
  PDriverExtension = ^TDriverExtension;

  _DRIVER_OBJECT = packed record
    _Type: SmallInt;
    Size: SmallInt;
    DeviceObject: PDeviceObject;
    Flags: LongWord;
    DriverStart: Pointer;
    DriverSize: LongWord;
    DriverSection: Pointer;
    DriverExtension: PDriverExtension;
    DriverName: TNtUnicodeString;
    HardwareDatabase: PNtUnicodeString;
    FastIoDispatch: PFastIODispatch;
    DriverInit: PLongInt;
    DriverStartIo: Pointer;
    DriverUnload: Pointer;
    MajorFunction: array[0..27] of PLongInt;
  end;
  TDriverObject = _Driver_Object;
  PDriverObject = ^TDriverObject;

  POOL_TYPE = (
    NonPagedPool,
    PagedPool,
    NonPagedPoolMustSucceed,
    DontUseThisType,
    NonPagedPoolCacheAligned,
    PagedPoolCacheAligned,
    NonPagedPoolCacheAlignedMustS,
    MaxPoolType,
    NonPagedPoolSession = 32,
    PagedPoolSession,
    NonPagedPoolMustSucceedSession,
    DontUseThisTypeSession,
    NonPagedPoolCacheAlignedSession,
    PagedPoolCacheAlignedSession,
    NonPagedPoolCacheAlignedMustSSession
  );
  TPoolType = POOL_TYPE;

