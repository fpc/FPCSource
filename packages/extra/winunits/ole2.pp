{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Implemtents some stuff of OLE2, tries to be Delphi compatible

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE OBJFPC}
unit ole2;

  interface

    uses
      windows;

    const
      GUID_NULL: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($00,$00,$00,$00,$00,$00,$00,$00));
      IID_IUnknown: TGUID = (D1:$00000000;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IClassFactory: TGUID = (D1:$00000001;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IMarshal: TGUID = (D1:$00000003;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IMalloc: TGUID = (D1:$00000002;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IStdMarshalInfo: TGUID = (D1:$00000018;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IExternalConnection: TGUID = (D1:$00000019;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IEnumUnknown: TGUID = (D1:$00000100;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IBindCtx: TGUID = (D1:$0000000E;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IEnumMoniker: TGUID = (D1:$00000102;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IRunnableObject: TGUID = (D1:$00000126;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IRunningObjectTable: TGUID = (D1:$00000010;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IPersist: TGUID = (D1:$0000010C;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IPersistStream: TGUID = (D1:$00000109;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IMoniker: TGUID = (D1:$0000000F;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IEnumString: TGUID = (D1:$00000101;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IStream: TGUID = (D1:$0000000C;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IEnumStatStg: TGUID = (D1:$0000000D;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IStorage: TGUID = (D1:$0000000B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IPersistFile: TGUID = (D1:$0000010B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IPersistStorage: TGUID = (D1:$0000010A;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_ILockBytes: TGUID = (D1:$0000000A;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IEnumFormatEtc: TGUID = (D1:$00000103;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IEnumStatData: TGUID = (D1:$00000105;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IRootStorage: TGUID = (D1:$00000012;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IAdviseSink: TGUID = (D1:$0000010F;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IAdviseSink2: TGUID = (D1:$00000125;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IDataObject: TGUID = (D1:$0000010E;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IDataAdviseHolder: TGUID = (D1:$00000110;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IMessageFilter: TGUID = (D1:$00000016;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IRpcChannelBuffer: TGUID = (D1:$D5F56B60;D2:$593B;D3:$101A;D4:($B5,$69,$08,$00,$2B,$2D,$BF,$7A));
      IID_IRpcProxyBuffer: TGUID = (D1:$D5F56A34;D2:$593B;D3:$101A;D4:($B5,$69,$08,$00,$2B,$2D,$BF,$7A));
      IID_IRpcStubBuffer: TGUID = (D1:$D5F56AFC;D2:$593B;D3:$101A;D4:($B5,$69,$08,$00,$2B,$2D,$BF,$7A));
      IID_IPSFactoryBuffer: TGUID = (D1:$D5F569D0;D2:$593B;D3:$101A;D4:($B5,$69,$08,$00,$2B,$2D,$BF,$7A));
      IID_ICreateTypeInfo: TGUID = (D1:$00020405;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_ICreateTypeLib: TGUID = (D1:$00020406;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IDispatch: TGUID = (D1:$00020400;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IEnumVariant: TGUID = (D1:$00020404;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_ITypeComp: TGUID = (D1:$00020403;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_ITypeInfo: TGUID = (D1:$00020401;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_ITypeLib: TGUID = (D1:$00020402;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IErrorInfo: TGUID = (D1:$1CF2B120;D2:$547D;D3:$101B;D4:($8E,$65,$08,$00,$2B,$2B,$D1,$19));
      IID_ICreateErrorInfo: TGUID = (D1:$22F03340;D2:$547D;D3:$101B;D4:($8E,$65,$08,$00,$2B,$2B,$D1,$19));
      IID_ISupportErrorInfo: TGUID = (D1:$DF0B3D60;D2:$548F;D3:$101B;D4:($8E,$65,$08,$00,$2B,$2B,$D1,$19));
      IID_IOleAdviseHolder: TGUID = (D1:$00000111;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleCache: TGUID = (D1:$0000011E;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleCache2: TGUID = (D1:$00000128;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleCacheControl: TGUID = (D1:$00000129;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IParseDisplayName: TGUID = (D1:$0000011A;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleContainer: TGUID = (D1:$0000011B;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleClientSite: TGUID = (D1:$00000118;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleObject: TGUID = (D1:$00000112;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleWindow: TGUID = (D1:$00000114;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleLink: TGUID = (D1:$0000011D;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleItemContainer: TGUID = (D1:$0000011C;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleInPlaceUIWindow: TGUID = (D1:$00000115;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleInPlaceActiveObject: TGUID = (D1:$00000117;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleInPlaceFrame: TGUID = (D1:$00000116;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleInPlaceObject: TGUID = (D1:$00000113;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IOleInPlaceSite: TGUID = (D1:$00000119;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IViewObject: TGUID = (D1:$0000010D;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IViewObject2: TGUID = (D1:$00000127;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IDropSource: TGUID = (D1:$00000121;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IDropTarget: TGUID = (D1:$00000122;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));
      IID_IEnumOleVerb: TGUID = (D1:$00000104;D2:$0000;D3:$0000;D4:($C0,$00,$00,$00,$00,$00,$00,$46));

  implementation

end.
