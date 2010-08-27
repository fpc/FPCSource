unit StoreMgr;

{$PACKSET 1}
{$mode objfpc}

(*

  Store Manager API
  (storemgr.h)

*)

//
// Copyright (c) Microsoft Corporation.  All rights reserved.
//
//
// Use of this sample source code is subject to the terms of the Microsoft
// license agreement under which you licensed this sample source code. If
// you did not accept the terms of the license agreement, you are not
// authorized to use this sample source code. For the terms of the license,
// please see the license agreement between you and Microsoft or, if applicable,
// see the LICENSE.RTF on your install media or the root of your tools installation.
// THE SAMPLE SOURCE CODE IS PROVIDED "AS IS", WITH NO WARRANTIES.
//

interface

uses
  Windows;

const
  BLOCK_DRIVER_GUID: GUID                    = '{A4E7EDDA-E575-4252-9D6B-4195D48BB865}';
  STORE_MOUNT_GUID: GUID                     = '{C1115848-46FD-4976-BDE9-D79448457004}';
  FATFS_MOUNT_GUID: GUID                     = '{169E1941-04CE-4690-97AC-776187EB67CC}';
  CDFS_MOUNT_GUID: GUID                      = '{72D75746-D54A-4487-B7A1-940C9A3F259A}';
  UDFS_MOUNT_GUID: GUID                      = '{462FEDA9-D478-4b00-86BB-51A8E3D10890}';
  CDDA_MOUNT_GUID: GUID                      = '{BA6B1343-7980-4d0c-9290-762D527B33AB}';
  FSD_MOUNT_GUID: GUID                       = '{8C77EDE8-47B9-45ae-8BC9-86E7B8D00EDD}';
  BOOTFS_MOUNT_GUID: GUID                    = '{9A1E75E9-B24A-4838-B448-C026CA01323C}';
  ROOTFS_MOUNT_GUID: GUID                    = '{0473DD50-D4CA-4ae2-BF5C-F09FD611C1CF}';
  ROMFS_MOUNT_GUID: GUID                     = '{945E7231-17C0-4753-AD4E-BEFCA2FA6AE2}';
  STORAGE_MEDIA_GUID: GUID                   = '{54CA35ED-CAF3-4b34-BC6B-6DFC7F22E6A9}';

  BLOCK_DRIVER_GUID_STRING                   = '{A4E7EDDA-E575-4252-9D6B-4195D48BB865}';
  STORE_MOUNT_GUID_STRING                    = '{C1115848-46FD-4976-BDE9-D79448457004}';
  FATFS_MOUNT_GUID_STRING                    = '{169E1941-04CE-4690-97AC-776187EB67CC}';
  CDFS_MOUNT_GUID_STRING                     = '{72D75746-D54A-4487-B7A1-940C9A3F259A}';
  UDFS_MOUNT_GUID_STRING                     = '{462FEDA9-D478-4b00-86BB-51A8E3D10890}';
  CDDA_MOUNT_GUID_STRING                     = '{BA6B1343-7980-4d0c-9290-762D527B33AB}';
  FSD_MOUNT_GUID_STRING                      = '{8C77EDE8-47B9-45ae-8BC9-86E7B8D00EDD}';
  BOOTFS_MOUNT_GUID_STRING                   = '{9A1E75E9-B24A-4838-B448-C026CA01323C}';
  ROOTFS_MOUNT_GUID_STRING                   = '{0473DD50-D4CA-4ae2-BF5C-F09FD611C1CF}';
  ROMFS_MOUNT_GUID_STRING                    = '{945E7231-17C0-4753-AD4E-BEFCA2FA6AE2}';

  STOREMGR_DRIVER_GUID: GUID                 = '{14ACDEE6-5CF3-496b-B39D-8954C96DF002}';
  STOREMGR_DRIVER_GUID_STRING                = '{14ACDEE6-5CF3-496b-B39D-8954C96DF002}';

  //*****************************************************************************/
  //* attributes of a store */
  STORE_ATTRIBUTE_READONLY                   = $00000001;
  STORE_ATTRIBUTE_REMOVABLE                  = $00000002;
  STORE_ATTRIBUTE_UNFORMATTED                = $00000004;
  STORE_ATTRIBUTE_AUTOFORMAT                 = $00000008;
  STORE_ATTRIBUTE_AUTOPART                   = $00000010;
  STORE_ATTRIBUTE_AUTOMOUNT                  = $00000020;

  //* attributes for a partition */
  PARTITION_ATTRIBUTE_EXPENDABLE             = $00000001;  // partition may be trashed
  PARTITION_ATTRIBUTE_READONLY               = $00000002;  // partition is read-only
  PARTITION_ATTRIBUTE_AUTOFORMAT             = $00000004;
  PARTITION_ATTRIBUTE_ACTIVE                 = $00000008;
  PARTITION_ATTRIBUTE_BOOT                   = $00000008;  // Active(DOS) == Boot(CE)
  PARTITION_ATTRIBUTE_MOUNTED                = $00000010;

  //* some basic constants */
  DEVICENAMESIZE                             = 8;
  STORENAMESIZE                              = 32;
  FILESYSNAMESIZE                            = 32;
  FORMATNAMESIZE                             = 32;
  PARTITIONNAMESIZE                          = 32;
  PROFILENAMESIZE                            = 32;
  FOLDERNAMESIZE                             = 32;
  VOLUMENAMESIZE                             = 64;
  FSDDESCSIZE                                = 32;

  STORAGE_DEVICE_CLASS_BLOCK                 = $1;
  STORAGE_DEVICE_CLASS_MULTIMEDIA            = $2;

  STORAGE_DEVICE_TYPE_PCIIDE                 = (1 shl 0);
  STORAGE_DEVICE_TYPE_FLASH                  = (1 shl 1);
  STORAGE_DEVICE_TYPE_ATA                    = (1 shl 2);
  STORAGE_DEVICE_TYPE_ATAPI                  = (1 shl 4);
  STORAGE_DEVICE_TYPE_PCCARD                 = (1 shl 5);
  STORAGE_DEVICE_TYPE_CFCARD                 = (1 shl 6);
  STORAGE_DEVICE_TYPE_SRAM                   = (1 shl 7);
  STORAGE_DEVICE_TYPE_DVD                    = (1 shl 8);
  STORAGE_DEVICE_TYPE_CDROM                  = (1 shl 9);
  STORAGE_DEVICE_TYPE_USB                    = (1 shl 10);
  STORAGE_DEVICE_TYPE_1394                   = (1 shl 11);
  STORAGE_DEVICE_TYPE_DOC                    = (1 shl 12);
  STORAGE_DEVICE_TYPE_UNKNOWN                = (1 shl 29);
  STORAGE_DEVICE_TYPE_REMOVABLE_DRIVE        = (1 shl 30); // Drive itself is removable
  STORAGE_DEVICE_TYPE_REMOVABLE_MEDIA        = (1 shl 31); // Just the media is removable ex. CDROM, FLOPPY

  STORAGE_DEVICE_FLAG_READWRITE              = (1 shl 0);
  STORAGE_DEVICE_FLAG_READONLY               = (1 shl 1);
  STORAGE_DEVICE_FLAG_TRANSACTED             = (1 shl 2);
  STORAGE_DEVICE_FLAG_MEDIASENSE             = (1 shl 3);  // Device requires media sense calls
  STORAGE_DEVICE_FLAG_XIP                    = (1 shl 4);

  CE_VOLUME_ATTRIBUTE_READONLY               = $1;
  CE_VOLUME_ATTRIBUTE_HIDDEN                 = $2;
  CE_VOLUME_ATTRIBUTE_REMOVABLE              = $4;
  CE_VOLUME_ATTRIBUTE_SYSTEM                 = $8;
  CE_VOLUME_ATTRIBUTE_BOOT                   = $10;

  CE_VOLUME_TRANSACTION_SAFE                 = $1;     // Performs transaction safe operations
  CE_VOLUME_FLAG_TRANSACT_WRITE              = $2;
  CE_VOLUME_FLAG_WFSC_SUPPORTED              = $4;
  CE_VOLUME_FLAG_LOCKFILE_SUPPORTED          = $8;
  CE_VOLUME_FLAG_NETWORK                     = $10;
  CE_VOLUME_FLAG_STORE                       = $20;
  CE_VOLUME_FLAG_RAMFS                       = $40;
  CE_VOLUME_FLAG_FILE_SECURITY_SUPPORTED     = $80;    // Persistent file and directory security descriptors
  CE_VOLUME_FLAG_64BIT_FILES_SUPPORTED       = $100;   // 64-bit file sizes and offsets

  STOREAPIDLL                                = 'coredll';

type
  SECTORNUM = ULONGLONG;

  //****************************************************************************

  STORAGEDEVICEINFO = record
      cbSize: DWORD;
      szProfile: array[0..PROFILENAMESIZE-1] of TCHAR;
      dwDeviceClass: DWORD;
      dwDeviceType: DWORD;
      dwDeviceFlags: DWORD;
  end;
  tagSTORAGEDEVICEINFO = STORAGEDEVICEINFO;
  TSTORAGEDEVICEINFO = STORAGEDEVICEINFO;
  PSTORAGEDEVICEINFO = ^STORAGEDEVICEINFO;

  //*****************************************************************************/
  // information about a store */
  STOREINFO = record
      cbSize: DWORD;                                       // sizeof(PD_STOREINFO)
      szDeviceName: array[0..DEVICENAMESIZE-1] of TCHAR;
      szStoreName: array[0..STORENAMESIZE-1] of TCHAR;
      dwDeviceClass: DWORD;
      dwDeviceType: DWORD;
      sdi: STORAGEDEVICEINFO;
      dwDeviceFlags: DWORD;
      snNumSectors: SECTORNUM;                             // number of sectors on store
      dwBytesPerSector: DWORD;                             // number of bytes per sector
      snFreeSectors: SECTORNUM;                            // number of unallocated sectors
      snBiggestPartCreatable: SECTORNUM;                   // biggest partition currently creatable
      ftCreated: FILETIME;                                 // last time store was formatted
      ftLastModified: FILETIME;                            // last time partition table was modified
      dwAttributes: DWORD;                                 // store attributes, see below
      dwPartitionCount: DWORD;                             // Number of Partitions
      dwMountCount: DWORD;                                 // Number of partitions that have been mounted
  end;
  tagSTOREINFO = STOREINFO;
  TSTOREINFO = STOREINFO;
  PSTOREINFO = ^STOREINFO;

  //***************************************************************************
  // information about a partition
  PARTINFO = record
    cbSize: DWORD;                                        // sizeof(PD_PARTINFO)
    szPartition: array[0..PARTITIONNAMESIZE-1] of TCHAR;  // name of partition
    szFileSys: array[0..FILESYSNAMESIZE-1] of TCHAR;
    szVolumeName: array[0..VOLUMENAMESIZE-1] of TCHAR;
    snNumSectors: SECTORNUM;                              // number of sectors in partition
    ftCreated: FILETIME;                                  // creation time of partition
    ftLastModified: FILETIME;                             // last time partition was modified
    dwAttributes: DWORD;                                  // partition attributes, see below
    bPartType: BYTE;
  end;
  tagPARTINFO = PARTINFO;
  TPARTINFO = PARTINFO;
  PPARTINFO = ^PARTINFO;

  //****************************************************************************/

  STORAGECONTEXT = record
      cbSize: DWORD;
      StoreInfo: STOREINFO;
      PartInfo: PARTINFO;
      dwFlags: DWORD;
  end;
  tagSTORAGECONTEXT = STORAGECONTEXT;
  TSTORAGECONTEXT = STORAGECONTEXT;
  PSTORAGECONTEXT = ^STORAGECONTEXT;

  CE_VOLUME_INFO = record
      cbSize: DWORD;
      dwAttributes: DWORD;
      dwFlags: DWORD;
      dwBlockSize: DWORD;
      szStoreName: array[0..STORENAMESIZE-1] of TCHAR;
      szPartitionName: array[0..PARTITIONNAMESIZE-1] of TCHAR;
  end;
  _CE_VOLUME_INFO = CE_VOLUME_INFO;
  PCE_VOLUME_INFO = ^CE_VOLUME_INFO;
  LPCE_VOLUME_INFO = ^CE_VOLUME_INFO;

  CE_VOLUME_INFO_LEVEL = (CeVolumeInfoLevelStandard);
  _CE_VOLUME_INFO_LEVEL = CE_VOLUME_INFO_LEVEL;

function  IsStorageManagerRunning: BOOL; cdecl; external STOREAPIDLL name 'IsStorageManagerRunning';
// Storage Management API's
function  OpenStore(szDeviceName: LPCTSTR): HANDLE; cdecl; external STOREAPIDLL name 'OpenStore';
function  DismountStore(hStore: HANDLE): BOOL; cdecl; external STOREAPIDLL name 'DismountStore';
function  FormatStore(hStore: HANDLE): BOOL; cdecl; external STOREAPIDLL name 'FormatStore';
function  FindFirstStore(pStoreInfo: PSTOREINFO): HANDLE; cdecl; external STOREAPIDLL name 'FindFirstStore';
function  FindNextStore(hSearch: HANDLE; pStoreInfo: PSTOREINFO): BOOL; cdecl; external STOREAPIDLL name 'FindNextStore';
function  FindCloseStore(hSearch: HANDLE): BOOL; cdecl; external STOREAPIDLL name 'FindCloseStore';
function  GetStoreInfo(hStore: HANDLE; pStoreInfo: PSTOREINFO): BOOL; cdecl; external STOREAPIDLL name 'GetStoreInfo';
// Partition Management API's
function  CreatePartition(hStore: HANDLE; szPartitionName: LPCTSTR; snNumSectors: SECTORNUM): BOOL; cdecl; external STOREAPIDLL name 'CreatePartition';
function  CreatePartitionEx(hStore: HANDLE; szPartitionName: LPCTSTR; bPartType: BYTE; snNumSectors: SECTORNUM): BOOL; cdecl; external STOREAPIDLL name 'CreatePartitionEx';
function  DeletePartition(hStore: HANDLE; szPartitionName: LPCTSTR): BOOL; cdecl; external STOREAPIDLL name 'DeletePartition';
function  OpenPartition(hStore: HANDLE; szPartitionName: LPCTSTR): HANDLE; cdecl; external STOREAPIDLL name 'OpenPartition';
function  MountPartition(hPartition: HANDLE): BOOL; cdecl; external STOREAPIDLL name 'MountPartition';
function  DismountPartition(hPartition: HANDLE): BOOL; cdecl; external STOREAPIDLL name 'DismountPartition';
function  RenamePartition(hPartition: HANDLE; szNewName: LPCTSTR): BOOL; cdecl; external STOREAPIDLL name 'RenamePartition';
function  SetPartitionAttributes(hPartition: HANDLE; dwAttrs: DWORD): BOOL; cdecl; external STOREAPIDLL name 'SetPartitionAttributes';
function  GetPartitionInfo(hPartition: HANDLE; pPartInfo: PPARTINFO): BOOL; cdecl; external STOREAPIDLL name 'GetPartitionInfo';
function  FormatPartition(hPartition: HANDLE): BOOL; cdecl; external STOREAPIDLL name 'FormatPartition';
function  FormatPartitionEx(hPartition: HANDLE; bPartType: BYTE; bAuto: BOOL): BOOL; cdecl; external STOREAPIDLL name 'FormatPartitionEx';
function  FindFirstPartition(hStore: HANDLE; pPartInfo: PPARTINFO): HANDLE; cdecl; external STOREAPIDLL name 'FindFirstPartition';
function  FindNextPartition(hSearch: HANDLE; pPartInfo: PPARTINFO): BOOL; cdecl; external STOREAPIDLL name 'FindNextPartition';
function  FindClosePartition(hSearch: HANDLE): BOOL; cdecl; external STOREAPIDLL name 'FindClosePartition';

function  CeGetVolumeInfoW(pszRootPath: LPCWSTR; InfoLevel: CE_VOLUME_INFO_LEVEL;
  lpVolumeInfo: LPCE_VOLUME_INFO): BOOL; cdecl; external STOREAPIDLL name 'CeGetVolumeInfoW';
function  CeGetVolumeInfo(pszRootPath: LPCWSTR; InfoLevel: CE_VOLUME_INFO_LEVEL;
  lpVolumeInfo: LPCE_VOLUME_INFO): BOOL; cdecl; external STOREAPIDLL name 'CeGetVolumeInfoW';

implementation

end.

