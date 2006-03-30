{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by the Free Pascal development team

    This include file contains the implementation for variants
    support in FPC as far as it is part of the system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.


 **********************************************************************}

// Names match the ones in Borland varutils unit.

const
  VAR_OK            = HRESULT($00000000);
  VAR_PARAMNOTFOUND = HRESULT($80020004);
  VAR_TYPEMISMATCH  = HRESULT($80020005);
  VAR_BADVARTYPE    = HRESULT($80020008);
  VAR_EXCEPTION     = HRESULT($80020009);
  VAR_OVERFLOW      = HRESULT($8002000A);
  VAR_BADINDEX      = HRESULT($8002000B);
  VAR_ARRAYISLOCKED = HRESULT($8002000D);
  VAR_NOTIMPL       = HRESULT($80004001);
  VAR_OUTOFMEMORY   = HRESULT($8007000E);
  VAR_INVALIDARG    = HRESULT($80070057);
  VAR_UNEXPECTED    = HRESULT($8000FFFF);

