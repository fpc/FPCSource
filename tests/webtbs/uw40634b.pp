{***************************************************************************
 *                                                                         *
 * This unit is distributed under the LGPL version 2                       *
 *                                                                         *
 * Additionally this unit can be used under any newer version (3 or up)    *
 * of the LGPL                                                             *
 *                                                                         *
 * Users are also granted the same "linking exception" as defined          *
 * for the LCL.                                                            *
 * See the LCL license for details                                         *
 *                                                                         *
 *                                                                         *
 ***************************************************************************
 @author(Martin Friebe)
}
unit uw40634b;

{$mode objfpc}{$H+}
{$INTERFACES CORBA} // no ref counting needed

interface

uses
  Classes, SysUtils, Types;

type
  TDBGState = integer;
  IDbgWatchValueIntf = interface end;
  IDbgWatchDataIntf = interface end;

  {$REGION ***** Internal types ***** }

  IInternalDbgMonitorIntfType  = interface end;
  IInternalDbgSupplierIntfType = interface end;

  generic IInternalDbgMonitorIntf<_SUPPLIER_INTF> = interface(IInternalDbgMonitorIntfType)
    procedure RemoveSupplier(ASupplier: _SUPPLIER_INTF);
  end;

  generic IInternalDbgSupplierIntf<_MONITOR_INTF> = interface(IInternalDbgSupplierIntfType)
    procedure SetMonitor1(AMonitor: _MONITOR_INTF);
    procedure SetMonitor2(AMonitor: _MONITOR_INTF);
    procedure SetMonitor3(AMonitor: _MONITOR_INTF);
  end;

  {$ENDREGION}

type

  IDbgWatchesSupplierIntf = interface;

  IDbgWatchesMonitorIntf  = interface(specialize IInternalDbgMonitorIntf<IDbgWatchesSupplierIntf>)
    ['{42A7069E-D5DD-4350-A592-2000F67DC7E9}']
    procedure InvalidateWatchValues;
    procedure DoStateChange(const AOldState, ANewState: TDBGState); //deprecated;
  end;

  IDbgWatchesSupplierIntf = interface(specialize IInternalDbgSupplierIntf<IDbgWatchesMonitorIntf>)
    ['{F893B607-C295-4A3A-8253-FAB3D03C5AD5}']
    procedure RequestData(AWatchValue: IDbgWatchValueIntf);
  end;



implementation


end.

