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
  WinceAPI.Aygshell, WinApi.Commctrl, WinApi.Commdlg, WinceAPI.Iphlpapi, WinceAPI.Notify, WinceAPI.Oleauto, MacOsApi.Power, WinApi.Shellapi, WinceAPI.Simmgr, WinceAPI.Tapi,
  WinceAPI.Gpsapi, WinceAPI.Todaycmn, WinceAPI.Windbase, WinceAPI.Cesync, WinceAPI.Gx, WinceAPI.Winioctl, WinceAPI.Msgqueue, WinceAPI.Pm, WinceAPI.Service, WinceAPI.Htmlctrl,
  WinceAPI.Sipapi, WinceAPI.Cpl, WinceAPI.Bt_api, WinceAPI.Bt_sdp, WinceAPI.Bthapi, WinceAPI.Bthutil, WinceAPI.Pimstore, WinceAPI.Ril, SinclairApi.Sms, WinceAPI.Ws2bth,
  WinceAPI.Keybd, WinceAPI.Nled, WinceAPI.Phone, WinceAPI.Connmgr, WinceAPI.Devload, WinceAPI.Devmgmt, WinceAPI.Mmreg, WinApi.Mmsystem, WinceAPI.Msacm,
  WinApi.Wininet, WinceAPI.Ras, WinceAPI.Raserror, WinceAPI.Sip, WinceAPI.Projects, WinceAPI.Wap, WinceAPI.Tsp, WinceAPI.Extapi, WinceAPI.Imm, WinceAPI.Rapitypes,
  WinceAPI.Storemgr, WinceAPI.Pnp, WinceAPI.Tlhelp32;
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
