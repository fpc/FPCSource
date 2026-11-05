{
   Dummy unit to compile everything in one go

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the
   Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit buildwinceunits;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  WinceApi.Aygshell, WinApi.Commctrl, WinApi.Commdlg, WinceApi.Iphlpapi, WinceApi.Notify, WinceApi.Oleauto, MacOsApi.Power, WinApi.Shellapi, WinceApi.Simmgr, WinceApi.Tapi,
  WinceApi.Gpsapi, WinceApi.Todaycmn, WinceApi.Windbase, WinceApi.Cesync, WinceApi.Gx, WinceApi.Winioctl, WinceApi.Msgqueue, WinceApi.Pm, WinceApi.Service, WinceApi.Htmlctrl,
  WinceApi.Sipapi, WinceApi.Cpl, WinceApi.Bt_api, WinceApi.Bt_sdp, WinceApi.Bthapi, WinceApi.Bthutil, WinceApi.Pimstore, WinceApi.Ril, SinclairApi.Sms, WinceApi.Ws2bth,
  WinceApi.Keybd, WinceApi.Nled, WinceApi.Phone, WinceApi.Connmgr, WinceApi.Devload, WinceApi.Devmgmt, WinceApi.Mmreg, WinApi.Mmsystem, WinceApi.Msacm,
  WinApi.Wininet, WinceApi.Ras, WinceApi.Raserror, WinceApi.Sip, WinceApi.Projects, WinceApi.Wap, WinceApi.Tsp, WinceApi.Extapi, WinceAPI.Imm, WinceApi.Rapitypes,
  WinceApi.Storemgr, WinceApi.Pnp, WinceApi.Tlhelp32;
{$ELSE FPC_DOTTEDUNITS}
uses
  aygshell, commctrl, commdlg, iphlpapi, notify, oleauto, power, shellapi, simmgr, tapi,
  gpsapi, todaycmn, windbase, cesync, gx, winioctl, msgqueue, pm, service, htmlctrl,
  sipapi, cpl, bt_api, bt_sdp, bthapi, bthutil, pimstore, ril, sms, ws2bth,
  keybd, nled, phone, connmgr, devload, devmgmt, mmreg, mmsystem, msacm,
  wininet, ras, raserror, sip, projects, wap, tsp, extapi, imm, rapitypes,
  storemgr, pnp, tlhelp32;
{$ENDIF FPC_DOTTEDUNITS}

implementation

end.
