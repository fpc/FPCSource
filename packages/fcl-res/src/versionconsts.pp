{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by Giulio Bernardi

    Constants used by version information resource

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit versionconsts;

{$MODE OBJFPC}

interface

const
//FileFlags
  VS_FF_DEBUG           = $00000001;
  VS_FF_PRERELEASE      = $00000002;
  VS_FF_PATCHED         = $00000004;
  VS_FF_PRIVATEBUILD    = $00000008;
  VS_FF_INFOINFERRED    = $00000010;
  VS_FF_SPECIALBUILD    = $00000020;
  VS_FFI_FILEFLAGSMASK  = $0000003F;
//FileOS
  VOS_UNKNOWN           = $00000000;
  VOS_DOS               = $00010000;
  VOS_OS216             = $00020000;
  VOS_OS232             = $00030000;
  VOS_NT                = $00040000;
  VOS__BASE             = $00000000;
  VOS__WINDOWS16        = $00000001;
  VOS__PM16             = $00000002;
  VOS__PM32             = $00000003;
  VOS__WINDOWS32        = $00000004;
  VOS_DOS_WINDOWS16     = $00010001;
  VOS_DOS_WINDOWS32     = $00010004;
  VOS_OS216_PM16        = $00020002;
  VOS_OS232_PM32        = $00030003;
  VOS_NT_WINDOWS32      = $00040004;
//FileType
  VFT_UNKNOWN           = $00000000;
  VFT_APP               = $00000001;
  VFT_DLL               = $00000002;
  VFT_DRV               = $00000003;
  VFT_FONT              = $00000004;
  VFT_VXD               = $00000005;
  VFT_STATIC_LIB        = $00000007;
//FileSubType - VFT_DRV
  VFT2_UNKNOWN          = $00000000;
  VFT2_DRV_PRINTER      = $00000001;
  VFT2_DRV_KEYBOARD     = $00000002;
  VFT2_DRV_LANGUAGE     = $00000003;
  VFT2_DRV_DISPLAY      = $00000004;
  VFT2_DRV_MOUSE        = $00000005;
  VFT2_DRV_NETWORK      = $00000006;
  VFT2_DRV_SYSTEM       = $00000007;
  VFT2_DRV_INSTALLABLE  = $00000008;
  VFT2_DRV_SOUND        = $00000009;
  VFT2_DRV_COMM         = $0000000A;
//VFT2_DRV_VERSIONED_PRINTER = ????
//FileSubType - VFT_FONT
  VFT2_FONT_RASTER      = $00000001;
  VFT2_FONT_VECTOR      = $00000002;
  VFT2_FONT_TRUETYPE    = $00000003;

implementation

end.
