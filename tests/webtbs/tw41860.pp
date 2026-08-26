{ %OPT=-Un }

unit tw41860_with_a_rather_long_name_to_increase_the_identifier_length;

{$mode objfpc}{$H+}
{$interfaces corba}

interface

type
  IDbgSyncAbleOriginIntf = interface ['{D565EBFB-3143-413D-98A6-C951504DFFBD}']
  end;

  IDbgExeProcSelectorIntf = interface ['{BAB83BC7-1376-48B3-BB6D-FE7BC5539EB8}']
  end;

  IDbgSynchronizedEntryIntf = interface ['{A05705C7-7443-43A0-9B32-EDAD5D888051}']
  end;

  IDbgSynchronizedTargetEntryIntf = interface(IDbgSynchronizedEntryIntf) ['{7C13D736-DC63-41B1-9105-5721DF39DB8B}']
  end;

  IDbgExeProcSelectorListIntf = interface(IDbgSyncAbleOriginIntf) ['{24D3B4F5-CB17-4F37-B7B6-347277E5D3FB}']
  end;

  generic TDbgSynchronizedListTemplate<_BASE: class; _SRC_LIST: IDbgSyncAbleOriginIntf> = class(_BASE)
  end;

  generic TDbgSynchronizedListExTemplate<_BASE: class; _SRC_LIST: IDbgSyncAbleOriginIntf> =
    class(specialize TDbgSynchronizedListTemplate<_BASE, _SRC_LIST>)
  end;

  generic TDbgExeProcSelectorListTemplate<_BASE: TObject; _ITM: class> = class(_BASE)
  end;

  generic TDbgExeProcSelectorTemplate<_Base: TObject> = class(_Base, IDbgExeProcSelectorIntf)
  end;

  generic TDbgExeProcSelectorSyncTargetgTemplate<_BASE: TObject> = class(
    specialize TDbgExeProcSelectorTemplate<_BASE>,
    IDbgSynchronizedTargetEntryIntf
  )
  end;

  generic TDbgExeProcSelectorSyncTargetListTemplate<_BASE, _ITEM: class> = class(
    specialize TDbgExeProcSelectorListTemplate<
       {Base} specialize TDbgSynchronizedListExTemplate<_BASE, IDbgExeProcSelectorListIntf>,
       {Item} specialize TDbgExeProcSelectorSyncTargetgTemplate<_ITEM>
     >
  )
  public type
    TDbgExeProcSelectorSyncUpdateAction = (uaChanged, uaAdded, uaDelete);
  end;

implementation

end.

