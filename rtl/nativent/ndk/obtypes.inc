{%MainUnit ndk.pas}
{
    Native Development Kit for Native NT

    This file is part of the Free Pascal run time library.
    This unit contains types for use with the Object Manager.
    Copyright (c) 2012 by Sven Barth

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

const
  //
  // Directory Object Access Rights
  //
  DIRECTORY_QUERY               = $0001;
  DIRECTORY_TRAVERSE            = $0002;
  DIRECTORY_CREATE_OBJECT       = $0004;
  DIRECTORY_CREATE_SUBDIRECTORY = $0008;
  { TODO }
  //DIRECTORY_ALL_ACCESS          = (STANDARD_RIGHTS_REQUIRED or $F);

type
  //
  // Object Information Classes for NtQueryInformationObject
  //
  _OBJECT_INFORMATION_CLASS = (
      ObjectBasicInformation,
      ObjectNameInformation,
      ObjectTypeInformation,
      ObjectTypesInformation,
      ObjectHandleFlagInformation,
      ObjectSessionInformation,
      MaxObjectInfoClass
  );
  OBJECT_INFORMATION_CLASS = _OBJECT_INFORMATION_CLASS;

  //
  // Object Information Types for NtQueryInformationObject
  //
  _OBJECT_NAME_INFORMATION = record
    Name: UNICODE_STRING;
  end;
  OBJECT_NAME_INFORMATION = _OBJECT_NAME_INFORMATION;
  POBJECT_NAME_INFORMATION = ^OBJECT_NAME_INFORMATION;

  _OBJECT_HANDLE_ATTRIBUTE_INFORMATION = record
    Inherit: NT_BOOLEAN;
    ProtectFromClose: NT_BOOLEAN;
  end;
  OBJECT_HANDLE_ATTRIBUTE_INFORMATION = _OBJECT_HANDLE_ATTRIBUTE_INFORMATION;
  POBJECT_HANDLE_ATTRIBUTE_INFORMATION = ^OBJECT_HANDLE_ATTRIBUTE_INFORMATION;

  _OBJECT_DIRECTORY_INFORMATION = record
    Name: UNICODE_STRING;
    TypeName: UNICODE_STRING;
  end;
  OBJECT_DIRECTORY_INFORMATION = _OBJECT_DIRECTORY_INFORMATION;
  POBJECT_DIRECTORY_INFORMATION = ^OBJECT_DIRECTORY_INFORMATION;

  _OBJECT_BASIC_INFORMATION = record
    Attributes: ULONG;
    GrantedAccess: ACCESS_MASK;
    HandleCount: ULONG;
    PointerCount: ULONG;
    PagedPoolUsage: ULONG;
    NonPagedPoolUsage: ULONG;
    Reserved: array[0..2] of ULONG;
    NameInformationLength: ULONG;
    TypeInformationLength: ULONG;
    SecurityDescriptorLength: ULONG;
    CreateTime: LARGE_INTEGER;
  end;
  OBJECT_BASIC_INFORMATION = _OBJECT_BASIC_INFORMATION;
  POBJECT_BASIC_INFORMATION = ^OBJECT_BASIC_INFORMATION;

