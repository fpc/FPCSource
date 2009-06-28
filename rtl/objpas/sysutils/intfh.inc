{
    *********************************************************************
    Copyright (C) 2002 Free Pascal Development Team

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
    *********************************************************************

    System Utilities For Free Pascal
}

const
  GUID_NULL: TGuid = '{00000000-0000-0000-0000-000000000000}';

function Supports(const Instance: IInterface; const IID: TGUID; out Intf): Boolean; overload;
function Supports(const Instance: TObject; const IID: TGUID; out Intf): Boolean; overload;
function Supports(const Instance: TObject; const IID: Shortstring; out Intf): Boolean; overload;
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

