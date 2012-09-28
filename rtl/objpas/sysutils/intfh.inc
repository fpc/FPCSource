{
    *********************************************************************
    Copyright (C) 2002 Peter Vreman,
    member of the Free Pascal Development Team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
    *********************************************************************
}

const
  GUID_NULL: TGuid = '{00000000-0000-0000-0000-000000000000}';

function Supports(const Instance: IInterface; const AClass: TClass; out Obj): Boolean; overload;
function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean; overload;
function Supports(const Instance: TObject; const IID: Shortstring; out Intf): Boolean; overload;

function Supports(const Instance: IInterface; const AClass: TClass): Boolean; overload;
function Supports(const Instance: IInterface; const IID: TGUID): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGUID): Boolean; overload;
function Supports(const Instance: TObject; const IID: Shortstring): Boolean; overload;

function Supports(const AClass: TClass; const IID: TGUID): Boolean; overload;
function Supports(const AClass: TClass; const IID: Shortstring): Boolean; overload;

//function CreateGUID(out Guid: TGUID): HResult;
function TryStringToGUID(const S: string; out Guid: TGUID): Boolean;
function StringToGUID(const S: string): TGUID;
function GUIDToString(const GUID: TGUID): string;
function IsEqualGUID(const guid1, guid2: TGUID): Boolean;
function GuidCase(const GUID: TGUID; const List: array of TGuid): Integer;

