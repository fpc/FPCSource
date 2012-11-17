{
    Free Pascal port of the OpenPTC C++ library.
    Copyright (C) 2001-2003, 2006, 2009, 2010  Nikolay Nikolov (nickysn@users.sourceforge.net)
    Original C++ version by Glenn Fiedler (ptc@gaffer.org)

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version
    with the following modification:

    As a special exception, the copyright holders of this library give you
    permission to link this library with independent modules to produce an
    executable, regardless of the license terms of these independent modules,and
    to copy and distribute the resulting executable under terms of your choice,
    provided that you also meet, for each linked independent module, the terms
    and conditions of the license of that module. An independent module is a
    module which is not derived from or based on this library. If you modify
    this library, you may extend this exception to your version of the library,
    but you are not obligated to do so. If you do not wish to do so, delete this
    exception statement from your version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

procedure DirectXCheck(AResult: HRESULT);
begin
  if AResult = DD_OK then
    exit;

  case AResult of
    DDERR_ALREADYINITIALIZED     : raise TPTCError.Create('DDERR_ALREADYINITIALIZED');
    DDERR_CANNOTATTACHSURFACE    : raise TPTCError.Create('DDERR_CANNOTATTACHSURFACE');
    DDERR_CANNOTDETACHSURFACE    : raise TPTCError.Create('DDERR_CANNOTDETACHSURFACE');
    DDERR_CURRENTLYNOTAVAIL      : raise TPTCError.Create('DDERR_CURRENTLYNOTAVAIL');
    DDERR_EXCEPTION              : raise TPTCError.Create('DDERR_EXCEPTION');
    DDERR_GENERIC                : raise TPTCError.Create('DDERR_GENERIC');
    DDERR_HEIGHTALIGN            : raise TPTCError.Create('DDERR_HEIGHTALIGN');
    DDERR_INCOMPATIBLEPRIMARY    : raise TPTCError.Create('DDERR_INCOMPATIBLEPRIMARY');
    DDERR_INVALIDCAPS            : raise TPTCError.Create('DDERR_INVALIDCAPS');
    DDERR_INVALIDCLIPLIST        : raise TPTCError.Create('DDERR_INVALIDCLIPLIST');
    DDERR_INVALIDMODE            : raise TPTCError.Create('DDERR_INVALIDMODE');
    DDERR_INVALIDOBJECT          : raise TPTCError.Create('DDERR_INVALIDOBJECT');
    DDERR_INVALIDPARAMS          : raise TPTCError.Create('DDERR_INVALIDPARAMS');
    DDERR_INVALIDPIXELFORMAT     : raise TPTCError.Create('DDERR_INVALIDPIXELFORMAT');
    DDERR_INVALIDRECT            : raise TPTCError.Create('DDERR_INVALIDRECT');
    DDERR_LOCKEDSURFACES         : raise TPTCError.Create('DDERR_LOCKEDSURFACES');
    DDERR_NO3D                   : raise TPTCError.Create('DDERR_NO3D');
    DDERR_NOALPHAHW              : raise TPTCError.Create('DDERR_NOALPHAHW');
    DDERR_NOSTEREOHARDWARE       : raise TPTCError.Create('DDERR_NOSTEREOHARDWARE');
    DDERR_NOSURFACELEFT          : raise TPTCError.Create('DDERR_NOSURFACELEFT');
    DDERR_NOCLIPLIST             : raise TPTCError.Create('DDERR_CLIPLIST');
    DDERR_NOCOLORCONVHW          : raise TPTCError.Create('DDERR_NOCOLORCONVHW');
    DDERR_NOCOOPERATIVELEVELSET  : raise TPTCError.Create('DDERR_NOCOOPERATIVELEVELSET');
    DDERR_NOCOLORKEY             : raise TPTCError.Create('DDERR_NOCOLORKEY');
    DDERR_NOCOLORKEYHW           : raise TPTCError.Create('DDERR_NOCOLORKEYHW');
    DDERR_NODIRECTDRAWSUPPORT    : raise TPTCError.Create('DDERR_NODIRECTDRAWSUPPORT');
    DDERR_NOEXCLUSIVEMODE        : raise TPTCError.Create('DDERR_NOEXCLUSIVEMODE');
    DDERR_NOFLIPHW               : raise TPTCError.Create('DDERR_NOFLIPHW');
    DDERR_NOGDI                  : raise TPTCError.Create('DDERR_NOGDI');
    DDERR_NOMIRRORHW             : raise TPTCError.Create('DDERR_NOMIRRORHW');
    DDERR_NOTFOUND               : raise TPTCError.Create('DDERR_NOTFOUND');
    DDERR_NOOVERLAYHW            : raise TPTCError.Create('DDERR_NOOVERLAYHW');
    DDERR_OVERLAPPINGRECTS       : raise TPTCError.Create('DDERR_OVERLAPPINGRECTS');
    DDERR_NORASTEROPHW           : raise TPTCError.Create('DDERR_NORASTEROPHW');
    DDERR_NOROTATIONHW           : raise TPTCError.Create('DDERR_NOROTATIONHW');
    DDERR_NOSTRETCHHW            : raise TPTCError.Create('DDERR_NOSTRETCHHW');
    DDERR_NOT4BITCOLOR           : raise TPTCError.Create('DDERR_NOT4BITCOLOR');
    DDERR_NOT4BITCOLORINDEX      : raise TPTCError.Create('DDERR_NOT4BITCOLORINDEX');
    DDERR_NOT8BITCOLOR           : raise TPTCError.Create('DDERR_NOT8BITCOLOR');
    DDERR_NOTEXTUREHW            : raise TPTCError.Create('DDERR_NOTEXTUREHW');
    DDERR_NOVSYNCHW              : raise TPTCError.Create('DDERR_NOVSYNCHW');
    DDERR_NOZBUFFERHW            : raise TPTCError.Create('DDERR_NOZBUFFERHW');
    DDERR_NOZOVERLAYHW           : raise TPTCError.Create('DDERR_NOZOVERLAYHW');
    DDERR_OUTOFCAPS              : raise TPTCError.Create('DDERR_OUTOFCAPS');
    DDERR_OUTOFMEMORY            : raise TPTCError.Create('DDERR_OUTOFMEMORY');
    DDERR_OUTOFVIDEOMEMORY       : raise TPTCError.Create('DDERR_OUTOFVIDEOMEMORY');
    DDERR_OVERLAYCANTCLIP        : raise TPTCError.Create('DDERR_OVERLAYCANTCLIP');
    DDERR_OVERLAYCOLORKEYONLYONEACTIVE: raise TPTCError.Create('DDERR_OVERLAYCOLORKEYONLYONEACTIVE');
    DDERR_PALETTEBUSY            : raise TPTCError.Create('DDERR_PALETTEBUSY');
    DDERR_COLORKEYNOTSET         : raise TPTCError.Create('DDERR_COLORKEYNOTSET');
    DDERR_SURFACEALREADYATTACHED : raise TPTCError.Create('DDERR_SURFACEALREADYATTACHED');
    DDERR_SURFACEALREADYDEPENDENT: raise TPTCError.Create('DDERR_SURFACEALREADYDEPENDENT');
    DDERR_SURFACEBUSY            : raise TPTCError.Create('DDERR_SURFACEBUSY');
    DDERR_CANTLOCKSURFACE        : raise TPTCError.Create('DDERR_CANTLOCKSURFACE');
    DDERR_SURFACEISOBSCURED      : raise TPTCError.Create('DDERR_SURFACEISOBSCURED');
    DDERR_SURFACELOST            : raise TPTCError.Create('DDERR_SURFACELOST');
    DDERR_SURFACENOTATTACHED     : raise TPTCError.Create('DDERR_SURFACENOTATTACHED');
    DDERR_TOOBIGHEIGHT           : raise TPTCError.Create('DDERR_TOOBIGHEIGHT');
    DDERR_TOOBIGSIZE             : raise TPTCError.Create('DDERR_TOOBIGSIZE');
    DDERR_TOOBIGWIDTH            : raise TPTCError.Create('DDERR_TOOBIGWIDTH');
    DDERR_UNSUPPORTED            : raise TPTCError.Create('DDERR_UNSUPPORTED');
    DDERR_UNSUPPORTEDFORMAT      : raise TPTCError.Create('DDERR_UNSUPPORTEDFORMAT');
    DDERR_UNSUPPORTEDMASK        : raise TPTCError.Create('DDERR_UNSUPPORTEDMASK');
    DDERR_INVALIDSTREAM          : raise TPTCError.Create('DDERR_INVALIDSTREAM');
    DDERR_VERTICALBLANKINPROGRESS: raise TPTCError.Create('DDERR_VERTICALBLANKINPROGRESS');
    DDERR_WASSTILLDRAWING        : raise TPTCError.Create('DDERR_WASSTILLDRAWING');
    DDERR_DDSCAPSCOMPLEXREQUIRED : raise TPTCError.Create('DDERR_DDSCAPSCOMPLEXREQUIRED');
    DDERR_XALIGN                 : raise TPTCError.Create('DDERR_XALIGN');
    DDERR_INVALIDDIRECTDRAWGUID  : raise TPTCError.Create('DDERR_INVALIDDIRECTDRAWGUID');
    DDERR_DIRECTDRAWALREADYCREATED: raise TPTCError.Create('DDERR_DIRECTDRAWALREADYCREATED');
    DDERR_NODIRECTDRAWHW         : raise TPTCError.Create('DDERR_NODIRECTDRAWHW');
    DDERR_PRIMARYSURFACEALREADYEXISTS: raise TPTCError.Create('DDERR_PRIMARYSURFACEALREADYEXISTS');
    DDERR_NOEMULATION            : raise TPTCError.Create('DDERR_NOEMULATION');
    DDERR_REGIONTOOSMALL         : raise TPTCError.Create('DDERR_REGIONTOOSMALL');
    DDERR_CLIPPERISUSINGHWND     : raise TPTCError.Create('DDERR_CLIPPERISUSINGHWND');
    DDERR_NOCLIPPERATTACHED      : raise TPTCError.Create('DDERR_NOCLIPPERATTACHED');
    DDERR_NOHWND                 : raise TPTCError.Create('DDERR_NOHWND');
    DDERR_HWNDSUBCLASSED         : raise TPTCError.Create('DDERR_HWNDSUBCLASSED');
    DDERR_HWNDALREADYSET         : raise TPTCError.Create('DDERR_HWNDALREADYSET');
    DDERR_NOPALETTEATTACHED      : raise TPTCError.Create('DDERR_NOPALETTEATTACHED');
    DDERR_NOPALETTEHW            : raise TPTCError.Create('DDERR_NOPALETTEHW');
    DDERR_BLTFASTCANTCLIP        : raise TPTCError.Create('DDERR_BLTFASTCANTCLIP');
    DDERR_NOBLTHW                : raise TPTCError.Create('DDERR_NOBLTHW');
    DDERR_NODDROPSHW             : raise TPTCError.Create('DDERR_NODDROPSHW');
    DDERR_OVERLAYNOTVISIBLE      : raise TPTCError.Create('DDERR_OVERLAYNOTVISIBLE');
    DDERR_NOOVERLAYDEST          : raise TPTCError.Create('DDERR_NOOVERLAYDEST');
    DDERR_INVALIDPOSITION        : raise TPTCError.Create('DDERR_INVALIDPOSITION');
    DDERR_NOTAOVERLAYSURFACE     : raise TPTCError.Create('DDERR_NOTAOVERLAYSURFACE');
    DDERR_EXCLUSIVEMODEALREADYSET: raise TPTCError.Create('DDERR_EXCLUSIVEMODEALREADYSET');
    DDERR_NOTFLIPPABLE           : raise TPTCError.Create('DDERR_NOTFLIPPABLE');
    DDERR_CANTDUPLICATE          : raise TPTCError.Create('DDERR_CANTDUPLICATE');
    DDERR_NOTLOCKED              : raise TPTCError.Create('DDERR_NOTLOCKED');
    DDERR_CANTCREATEDC           : raise TPTCError.Create('DDERR_CANTCREATEDC');
    DDERR_NODC                   : raise TPTCError.Create('DDERR_NODC');
    DDERR_WRONGMODE              : raise TPTCError.Create('DDERR_WRONGMODE');
    DDERR_IMPLICITLYCREATED      : raise TPTCError.Create('DDERR_IMPLICITLYCREATED');
    DDERR_NOTPALETTIZED          : raise TPTCError.Create('DDERR_NOPALETTIZED');
    DDERR_UNSUPPORTEDMODE        : raise TPTCError.Create('DDERR_UNSUPPORTEDMODE');
    DDERR_NOMIPMAPHW             : raise TPTCError.Create('DDERR_NOMIPMAPHW');
    DDERR_INVALIDSURFACETYPE     : raise TPTCError.Create('DDERR_INVALIDSURFACETYPE');
    DDERR_NOOPTIMIZEHW           : raise TPTCError.Create('DDERR_NOOPTIMIZEHW');
    DDERR_NOTLOADED              : raise TPTCError.Create('DDERR_NOTLOADED');
    DDERR_NOFOCUSWINDOW          : raise TPTCError.Create('DDERR_NOFOCUSWINDOW');
    DDERR_NOTONMIPMAPSUBLEVEL    : raise TPTCError.Create('DDERR_NOTONMIPMAPSUBLEVEL');
    DDERR_DCALREADYCREATED       : raise TPTCError.Create('DDERR_DCALREADYCREATED');
    DDERR_NONONLOCALVIDMEM       : raise TPTCError.Create('DDERR_NONONLOCALVIDMEM');
    DDERR_CANTPAGELOCK           : raise TPTCError.Create('DDERR_CANTPAGELOCK');
    DDERR_CANTPAGEUNLOCK         : raise TPTCError.Create('DDERR_CANTPAGEUNLOCK');
    DDERR_NOTPAGELOCKED          : raise TPTCError.Create('DDERR_NOTPAGELOCKED');
    DDERR_MOREDATA               : raise TPTCError.Create('DDERR_MOREDATA');
    DDERR_EXPIRED                : raise TPTCError.Create('DDERR_EXPIRED');
    DDERR_TESTFINISHED           : raise TPTCError.Create('DDERR_TESTFINISHED');
    DDERR_NEWMODE                : raise TPTCError.Create('DDERR_NEWMODE');
    DDERR_D3DNOTINITIALIZED      : raise TPTCError.Create('DDERR_D3DNOTINITIALIZED');
    DDERR_VIDEONOTACTIVE         : raise TPTCError.Create('DDERR_VIDEONOTACTIVE');
    DDERR_NOMONITORINFORMATION   : raise TPTCError.Create('DDERR_NOMONITORINFORMATION');
    DDERR_NODRIVERSUPPORT        : raise TPTCError.Create('DDERR_NODRIVERSUPPORT');
    DDERR_DEVICEDOESNTOWNSURFACE : raise TPTCError.Create('DDERR_DEVICEDOESNTOWNSURFACE');
    DDERR_NOTINITIALIZED         : raise TPTCError.Create('DDERR_NOTINITIALIZED');
  end;

  raise TPTCError.Create('DDERR $' + HexStr(AResult, 8));
end;

procedure DirectXCheck(AResult: HRESULT; const AMessage: String);
begin
  try
    DirectXCheck(AResult);
  except
    on error: TPTCError do
      raise TPTCError.Create(AMessage, error);
  end;
end;
