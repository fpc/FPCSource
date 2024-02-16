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
unit uw40634a;

{$mode objfpc}{$H+}
{$INTERFACES CORBA} // no ref counting needed

interface

uses
  Classes, SysUtils, uw40634b;

type

TNormalClass = class
  strict protected
    procedure SetMonitor(AMonitor: IDbgWatchesMonitorIntf); virtual; abstract;
    procedure RequestData(AWatchValue: IDbgWatchValueIntf); virtual; abstract;
    function Monitor:TObject;virtual; abstract;
end;

  { TInternalDbgSupplierBase }

  generic TInternalDbgSupplierBase<
    _BASE: TObject;
    _SUPPLIER_INTF: IInternalDbgSupplierIntfType;
    _MONITOR_INTF //: IInternalDbgMonitorIntfType
    >
    = class(_BASE)
  strict private
    FMonitor: _MONITOR_INTF;


  // ********************************************************************************
      (* "private" is CORRECTLY not working
         all others should work, but have different error   *)

  //private
  strict protected
    procedure SetMonitor1(AMonitor: _MONITOR_INTF);virtual; abstract;
  protected
    procedure SetMonitor2(AMonitor: _MONITOR_INTF);virtual; abstract;
  public
    procedure SetMonitor3(AMonitor: _MONITOR_INTF);virtual; abstract;
  // ********************************************************************************
  protected

    property Monitor: _MONITOR_INTF read FMonitor;
  end;

type

  { TWatchesSupplierClassTemplate }

  generic TWatchesSupplierClassTemplate<_BASE: TObject> = class(
    specialize TInternalDbgSupplierBase<_BASE, IDbgWatchesSupplierIntf, IDbgWatchesMonitorIntf>,
    IDbgWatchesSupplierIntf
  )
  protected
  public
    procedure RequestData(AWatchValue: IDbgWatchValueIntf); virtual; abstract;
  end;


implementation

end.
