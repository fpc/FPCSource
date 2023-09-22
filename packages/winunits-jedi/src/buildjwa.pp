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
   Free Software Foundation, Inc.,i 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit buildjwa;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
    WinApi.Jedi.Wintype, WinApi.Jedi.Winbase, WinApi.Jedi.Winnt, 
    WinApi.Jedi.Lmerr, WinApi.Jedi.Lmmsg, WinApi.Jedi.Aclui, WinApi.Jedi.Adsdb, WinApi.Jedi.Lmerrlog, WinApi.Jedi.Lmjoin, WinApi.Jedi.Authz,
    WinApi.Jedi.Bits, WinApi.Jedi.Lmremutl, WinApi.Jedi.Lmrepl, WinApi.Jedi.Lmserver, WinApi.Jedi.Lmshare, WinApi.Jedi.Lmsname,
    WinApi.Jedi.Lmstats, WinApi.Jedi.Accctrl, WinApi.Jedi.Aclapi, WinApi.Jedi.Cderr, WinApi.Jedi.Cpl, WinApi.Jedi.Activeds, WinApi.Jedi.Dbt, WinApi.Jedi.Dde,
    WinApi.Jedi.Adserr, WinApi.Jedi.Adshlp, WinApi.Jedi.Adsnms, WinApi.Jedi.Adsprop, WinApi.Jedi.Adssts, WinApi.Jedi.Adtgen,
    WinApi.Jedi.Af_irda, WinApi.Jedi.Atalkwsh, WinApi.Jedi.Authif, WinApi.Jedi.Dlgs, WinApi.Jedi.Dssec, WinApi.Jedi.Batclass,
    WinApi.Jedi.Excpt, WinApi.Jedi.Ime, WinApi.Jedi.Bits1_5, WinApi.Jedi.Bitscfg, WinApi.Jedi.Bitsmsg, WinApi.Jedi.Blberr,
    WinApi.Jedi.Bluetoothapis, WinApi.Jedi.Bthdef, WinApi.Jedi.Bthsdpdef, WinApi.Jedi.Bugcodes, WinApi.Jedi.Lmat, WinApi.Jedi.Lmsvc,
    WinApi.Jedi.Cmnquery, WinApi.Jedi.Colordlg, WinApi.Jedi.Lmuse, WinApi.Jedi.Msi, WinApi.Jedi.Cplext, WinApi.Jedi.Cryptuiapi,
    WinApi.Jedi.Nb30, WinApi.Jedi.Netsh, WinApi.Jedi.Pbt, WinApi.Jedi.Pdh, WinApi.Jedi.Dhcpcsdk, WinApi.Jedi.Dhcpsapi, WinApi.Jedi.Dhcpssdk,
    WinApi.Jedi.Prsht, WinApi.Jedi.Psapi, WinApi.Jedi.Dsadmin, WinApi.Jedi.Dsclient, WinApi.Jedi.Dsgetdc, WinApi.Jedi.Dskquota,
    WinApi.Jedi.Dsquery, WinApi.Jedi.Dsrole, WinApi.Jedi.Qos, WinApi.Jedi.Qossp, WinApi.Jedi.Errorrep, WinApi.Jedi.Rpc, WinApi.Jedi.Sddl,
    WinApi.Jedi.Faxdev, WinApi.Jedi.Faxext, WinApi.Jedi.Faxmmc, WinApi.Jedi.Faxroute, WinApi.Jedi.Gpedit, WinApi.Jedi.Hherror,
    WinApi.Jedi.Htmlguid, WinApi.Jedi.Htmlhelp, WinApi.Jedi.Iaccess, WinApi.Jedi.Iadmext, WinApi.Jedi.Icmpapi, WinApi.Jedi.Iiscnfg,
    WinApi.Jedi.Imagehlp, WinApi.Jedi.Lmdfs, WinApi.Jedi.Imapierror, WinApi.Jedi.Sens, WinApi.Jedi.Sfc, WinApi.Jedi.Ioevent,
    WinApi.Jedi.Ipexport, WinApi.Jedi.Iphlpapi, WinApi.Jedi.Ipifcons, WinApi.Jedi.Ipinfoid, WinApi.Jedi.Iprtrmib,
    WinApi.Jedi.Iptypes, WinApi.Jedi.Isguids, WinApi.Jedi.Issper16, WinApi.Jedi.Lmaccess, WinApi.Jedi.Lmalert, WinApi.Jedi.Lmapibuf,
    WinApi.Jedi.Snmp, WinApi.Jedi.Sspi, WinApi.Jedi.Lmaudit, WinApi.Jedi.Lmconfig, WinApi.Jedi.Lmcons, WinApi.Jedi.Wpapi,
    WinApi.Jedi.Wsipx, WinApi.Jedi.Wsrm, WinApi.Jedi.Lmuseflg, WinApi.Jedi.Lmwksta, WinApi.Jedi.Loadperf, WinApi.Jedi.Lpmapi,
    WinApi.Jedi.Mciavi, WinApi.Jedi.Mprerror, WinApi.Jedi.Wsvns, WinApi.Jedi.Imapi, WinApi.Jedi.Msidefs, WinApi.Jedi.Msiquery,
    WinApi.Jedi.Mstask, WinApi.Jedi.Mstcpip, WinApi.Jedi.Mswsock, WinApi.Jedi.Nspapi, WinApi.Jedi.Ntddpar, WinApi.Jedi.Ntdsapi,
    WinApi.Jedi.Ntdsbcli, WinApi.Jedi.Ntdsbmsg, WinApi.Jedi.Ntldap, WinApi.Jedi.Ntquery, WinApi.Jedi.Ntsecapi, WinApi.Jedi.Ntstatus,
    WinApi.Jedi.Objsel, WinApi.Jedi.Patchapi, WinApi.Jedi.Patchwiz, WinApi.Jedi.Pdhmsg, WinApi.Jedi.Powrprof, WinApi.Jedi.Profinfo,
    WinApi.Jedi.Protocol, WinApi.Jedi.Qosname, WinApi.Jedi.Qospol, WinApi.Jedi.Reason, WinApi.Jedi.Regstr, WinApi.Jedi.Rpcasync,
    WinApi.Jedi.Rpcdce, WinApi.Jedi.Rpcnsi, WinApi.Jedi.Rpcnterr, WinApi.Jedi.Rpcssl, WinApi.Jedi.Scesvc, WinApi.Jedi.Schedule,
    WinApi.Jedi.Schemadef, WinApi.Jedi.Secext, WinApi.Jedi.Security, WinApi.Jedi.Sensapi, WinApi.Jedi.Shlguid, WinApi.Jedi.Sisbkup,
    WinApi.Jedi.Sporder, WinApi.Jedi.Srrestoreptapi, WinApi.Jedi.Subauth, WinApi.Jedi.Svcguid, WinApi.Jedi.Tlhelp32,
    WinApi.Jedi.Tmschema, WinApi.Jedi.Traffic, WinApi.Jedi.Userenv, WinApi.Jedi.Uxtheme, WinApi.Jedi.Wbemcli, WinApi.Jedi.Winable,
    WinApi.Jedi.Winber, WinApi.Jedi.Wincon, WinApi.Jedi.Wincpl, WinApi.Jedi.Wincred, WinApi.Jedi.Wincrypt,
    WinApi.Jedi.Windns, WinApi.Jedi.Winefs, WinApi.Jedi.Winerror, WinApi.Jedi.Winfax, WinApi.Jedi.Wingdi, WinApi.Jedi.Winioctl,
    WinApi.Jedi.Winldap, WinApi.Jedi.Winnetwk, WinApi.Jedi.Winnls, WinApi.Jedi.Winperf, WinApi.Jedi.Winreg, WinApi.Jedi.Winresrc,
    WinApi.Jedi.Winsafer, WinApi.Jedi.Winsock, WinApi.Jedi.Winsock2, WinApi.Jedi.Winsvc, WinApi.Jedi.Winuser,
    WinApi.Jedi.Winver, WinApi.Jedi.Winwlx, WinApi.Jedi.Wmistr, WinApi.Jedi.Wownt16, WinApi.Jedi.Wownt32, WinApi.Jedi.Wpapimsg,
    WinApi.Jedi.Wpcrsmsg, WinApi.Jedi.Wpftpmsg, WinApi.Jedi.Wppstmsg, WinApi.Jedi.Wpspihlp, WinApi.Jedi.Wptypes,
    WinApi.Jedi.Wpwizmsg, WinApi.Jedi.Ws2atm, WinApi.Jedi.Ws2bth, WinApi.Jedi.Ws2dnet, WinApi.Jedi.Ws2spi, WinApi.Jedi.Ws2tcpip,
    WinApi.Jedi.Wshisotp, WinApi.Jedi.Wsnetbs, WinApi.Jedi.Wsnwlink, WinApi.Jedi.Wtsapi32, WinApi.Jedi.Zmouse, WinApi.Jedi.Sensevts,
    WinApi.Jedi.Adstlb, WinApi.Jedi.Native, WinApi.Jedi.Windows, WinApi.Jedi.Carderr,WinApi.Jedi.Winsta, WinApi.Jedi.Vista,WinApi.Jedi.Winternl;
{$ELSE FPC_DOTTEDUNITS}
uses
    jwawintype, jwawinbase, jwawinnt, 
    jwalmerr, jwalmmsg, jwaaclui, jwaadsdb, jwalmerrlog, jwalmjoin, jwaauthz,
    jwabits, jwalmremutl, jwalmrepl, jwalmserver, jwalmshare, jwalmsname,
    jwalmstats, jwaaccctrl, jwaaclapi, jwacderr, jwacpl, jwaactiveds, jwadbt, jwadde,
    jwaadserr, jwaadshlp, jwaadsnms, jwaadsprop, jwaadssts, jwaadtgen,
    jwaaf_irda, jwaatalkwsh, jwaauthif, jwadlgs, jwadssec, jwabatclass,
    jwaexcpt, jwaime, jwabits1_5, jwabitscfg, jwabitsmsg, jwablberr,
    jwabluetoothapis, jwabthdef, jwabthsdpdef, jwabugcodes, jwalmat, jwalmsvc,
    jwacmnquery, jwacolordlg, jwalmuse, jwamsi, jwacplext, jwacryptuiapi,
    jwanb30, jwanetsh, jwapbt, jwapdh, jwadhcpcsdk, jwadhcpsapi, jwadhcpssdk,
    jwaprsht, jwapsapi, jwadsadmin, jwadsclient, jwadsgetdc, jwadskquota,
    jwadsquery, jwadsrole, jwaqos, jwaqossp, jwaerrorrep, jwarpc, jwasddl,
    jwafaxdev, jwafaxext, jwafaxmmc, jwafaxroute, jwagpedit, jwahherror,
    jwahtmlGuid, jwahtmlhelp, jwaiaccess, jwaiadmext, jwaicmpapi, jwaiiscnfg,
    jwaimagehlp, jwalmdfs, jwaimapierror, jwasens, jwasfc, jwaioevent,
    jwaipexport, jwaiphlpapi, jwaipifcons, jwaipinfoid, jwaiprtrmib,
    jwaiptypes, jwaisguids, jwaissper16, jwalmaccess, jwalmalert, jwalmapibuf,
    jwasnmp, jwasspi, jwalmaudit, jwalmconfig, jwalmcons, jwawpapi,
    jwawsipx, jwawsrm, jwalmuseflg, jwalmwksta, jwaloadperf, jwalpmapi,
    jwamciavi, jwamprerror, jwawsvns, jwaimapi, jwamsidefs, jwamsiquery,
    jwamstask, jwamstcpip, jwamswsock, jwanspapi, jwantddpar, jwantdsapi,
    jwantdsbcli, jwantdsbmsg, jwantldap, jwantquery, jwantsecapi, jwantstatus,
    jwaObjsel, jwapatchapi, jwapatchwiz, jwapdhmsg, jwapowrprof, jwaprofinfo,
    jwaprotocol, jwaqosname, jwaqospol, jwareason, jwaregstr, jwarpcasync,
    jwarpcdce, jwarpcnsi, jwarpcnterr, jwarpcssl, jwascesvc, jwaschedule,
    jwaschemadef, jwasecext, jwasecurity, jwasensapi, jwashlguid, jwasisbkup,
    jwasporder, jwasrrestoreptapi, jwasubauth, jwasvcguid, jwatlhelp32,
    jwatmschema, jwatraffic, jwauserenv, jwauxtheme, jwawbemcli, jwawinable,
    jwawinber, jwawincon, jwawincpl, jwawincred, jwawincrypt,
    jwawindns, jwawinefs, jwawinerror, jwawinfax, jwawingdi, jwawinioctl,
    jwawinldap, jwawinnetwk, jwawinnls, jwawinperf, jwawinreg, jwawinresrc,
    jwawinsafer, jwawinsock, jwawinsock2, jwawinsvc, jwawinuser,
    jwawinver, jwawinwlx, jwawmistr, jwawownt16, jwawownt32, jwawpapimsg,
    jwawpcrsmsg, jwawpftpmsg, jwawppstmsg, jwawpspihlp, jwawptypes,
    jwawpwizmsg, jwaws2atm, jwaws2bth, jwaws2dnet, jwaws2spi, jwaws2tcpip,
    jwawshisotp, jwawsnetbs, jwawsnwlink, jwawtsapi32, jwazmouse, jwasensevts,
    jwaadstlb, jwanative, jwawindows, jwacarderr,jwawinsta, jwavista,jwawinternl;
{$ENDIF FPC_DOTTEDUNITS}

implementation

end.
