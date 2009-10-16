{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Florian Klaempfl
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$H+}
{$inline on}
unit comobj;

  interface

{define DEBUG_COM}

    uses
      Windows,Types,Variants,Sysutils,ActiveX,contnrs;

    type
      EOleError = class(Exception);

      EOleSysError = class(EOleError)
      private
        FErrorCode: HRESULT;
      public
        constructor Create(const Msg: string; aErrorCode: HRESULT;aHelpContext: Integer);
        property ErrorCode: HRESULT read FErrorCode write FErrorCode;
      end;

      EOleException = class(EOleSysError)
      private
        FHelpFile: string;
        FSource: string;
      public
        constructor Create(const Msg: string; aErrorCode: HRESULT;const aSource,aHelpFile: string;aHelpContext: Integer);
        property HelpFile: string read FHelpFile write FHelpFile;
        property Source: string read FSource write FSource;
      end;

      EOleRegistrationError = class(EOleError);

      TComServerObject = class(TObject)
      protected
        function CountObject(Created: Boolean): Integer; virtual; abstract;
        function CountFactory(Created: Boolean): Integer; virtual; abstract;
        function GetHelpFileName: string; virtual; abstract;
        function GetServerFileName: string; virtual; abstract;
        function GetServerKey: string; virtual; abstract;
        function GetServerName: string; virtual; abstract;
        function GetStartSuspended: Boolean; virtual; abstract;
        function GetTypeLib: ITypeLib; virtual; abstract;
        procedure SetHelpFileName(const Value: string); virtual; abstract;
      public
        property HelpFileName: string read GetHelpFileName write SetHelpFileName;
        property ServerFileName: string read GetServerFileName;
        property ServerKey: string read GetServerKey;
        property ServerName: string read GetServerName;
        property TypeLib: ITypeLib read GetTypeLib;
        property StartSuspended: Boolean read GetStartSuspended;
      end;

      TComObjectFactory = class;

      TFactoryProc = procedure(Factory: TComObjectFactory) of object;

      { TComClassManager }

      TComClassManager = class(TObject)
      private
        fClassFactoryList: TObjectList;
      public
        constructor Create;
        destructor Destroy; override;
        procedure AddObjectFactory(factory: TComObjectFactory);
        procedure RemoveObjectFactory(factory: TComObjectFactory);
        procedure ForEachFactory(ComServer: TComServerObject; FactoryProc: TFactoryProc);
        function GetFactoryFromClass(ComClass: TClass): TComObjectFactory;
        function GetFactoryFromClassID(const ClassID: TGUID): TComObjectFactory;
      end;

      IServerExceptionHandler = interface
        ['{6A8D432B-EB81-11D1-AAB1-00C04FB16FBC}']
        procedure OnException(const ServerClass, ExceptionClass, ErrorMessage: WideString;
          ExceptAddr: PtrInt; const ErrorIID, ProgID: WideString; var Handled: Integer; var Result: HResult); dispid 2;
      end;

      TComObject = class(TObject, IUnknown, ISupportErrorInfo)
      private
        FController : Pointer;
        FFactory : TComObjectFactory;
        FRefCount : Integer;
        FServerExceptionHandler : IServerExceptionHandler;
        FCounted : Boolean;
        function GetController : IUnknown;
      protected
        { IUnknown }
        function IUnknown.QueryInterface = ObjQueryInterface;
        function IUnknown._AddRef = ObjAddRef;
        function IUnknown._Release = ObjRelease;

        { IUnknown methods for other interfaces }
        function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;

        { ISupportErrorInfo }
        function InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
      public
        constructor Create;
        constructor CreateAggregated(const Controller: IUnknown);
        constructor CreateFromFactory(Factory: TComObjectFactory; const Controller: IUnknown);
        destructor Destroy; override;
        procedure Initialize; virtual;
        function ObjAddRef: Integer; virtual; stdcall;
        function ObjQueryInterface(const IID: TGUID; out Obj): HResult; virtual; stdcall;
        function ObjRelease: Integer; virtual; stdcall;
        function SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult; override;
        property Controller: IUnknown read GetController;
        property Factory: TComObjectFactory read FFactory;
        property RefCount: Integer read FRefCount;
        property ServerExceptionHandler: IServerExceptionHandler read FServerExceptionHandler write FServerExceptionHandler;
      end;
      TComClass = class of TComObject;

      TClassInstancing = (ciInternal, ciSingleInstance, ciMultiInstance);
      TThreadingModel = (tmSingle, tmApartment, tmFree, tmBoth, tmNeutral);

      TComObjectFactory = class(TObject, IUnknown, IClassFactory, IClassFactory2)
      private
        FRefCount : Integer;
        //Next: TComObjectFactory;
        FComServer: TComServerObject;
        FComClass: TClass;
        FClassID: TGUID;
        FClassName: string;
        FDescription: string;
        FErrorIID: TGUID;
        FInstancing: TClassInstancing;
        FLicString: WideString;
        //FRegister: Longint;
        FShowErrors: Boolean;
        FSupportsLicensing: Boolean;
        FThreadingModel: TThreadingModel;
        function GetProgID: string;
      protected
        { IUnknown }
        function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
        function _AddRef: Integer; stdcall;
        function _Release: Integer; stdcall;
        { IClassFactory }
        function CreateInstance(const UnkOuter: IUnknown; const IID: TGUID;
          out Obj): HResult; stdcall;
        function LockServer(fLock: BOOL): HResult; stdcall;
        { IClassFactory2 }
        function GetLicInfo(var licInfo: TLicInfo): HResult; stdcall;
        function RequestLicKey(dwResrved: DWORD; out bstrKey: WideString): HResult; stdcall;
        function CreateInstanceLic(const unkOuter: IUnknown; const unkReserved: IUnknown;
          const iid: TIID; const bstrKey: WideString; out vObject): HResult; stdcall;
      public
        constructor Create(ComServer: TComServerObject; ComClass: TComClass;
          const ClassID: TGUID; const Name, Description: string;
          Instancing: TClassInstancing; ThreadingModel: TThreadingModel = tmSingle);
        destructor Destroy; override;
        function CreateComObject(const Controller: IUnknown): TComObject; virtual;
        procedure RegisterClassObject;
        procedure UpdateRegistry(Register: Boolean); virtual;
        property ClassID: TGUID read FClassID;
        property ClassName: string read FClassName;
        property ComClass: TClass read FComClass;
        property ComServer: TComServerObject read FComServer;
        property Description: string read FDescription;
        property ErrorIID: TGUID read FErrorIID write FErrorIID;
        property LicString: WideString read FLicString write FLicString;
        property ProgID: string read GetProgID;
        property Instancing: TClassInstancing read FInstancing;
        property ShowErrors: Boolean read FShowErrors write FShowErrors;
        property SupportsLicensing: Boolean read FSupportsLicensing write FSupportsLicensing;
        property ThreadingModel: TThreadingModel read FThreadingModel;
      end;

      { TTypedComObject }

      TTypedComObject = class(TComObject, IProvideClassInfo)
        function GetClassInfo(out pptti : ITypeInfo):HResult; StdCall;
      end;

      TTypedComClass = class of TTypedComObject;

      { TTypedComObjectFactory }

      TTypedComObjectFactory = class(TComObjectFactory)
      private
        FClassInfo: ITypeInfo;
      public
        constructor Create(AComServer: TComServerObject; TypedComClass: TTypedComClass; const AClassID: TGUID;
          AInstancing: TClassInstancing; AThreadingModel: TThreadingModel = tmSingle);
        function GetInterfaceTypeInfo(TypeFlags: Integer) : ITypeInfo;
        procedure UpdateRegistry(Register: Boolean);override;
        property ClassInfo : ITypeInfo read FClassInfo;
      end;


    function CreateClassID : ansistring;

    function CreateComObject(const ClassID: TGUID) : IUnknown;
    function CreateRemoteComObject(const MachineName : WideString;const ClassID : TGUID) : IUnknown;
    function CreateOleObject(const ClassName : string) : IDispatch;
    function GetActiveOleObject(const ClassName: string) : IDispatch;

    procedure OleCheck(Value : HResult);inline;
    procedure OleError(Code: HResult);

    function ProgIDToClassID(const id : string) : TGUID;
    function ClassIDToProgID(const classID: TGUID): string;

    procedure DispatchInvoke(const Dispatch: IDispatch; CallDesc: PCallDesc;
       DispIDs: PDispIDList; Params: Pointer; Result: PVariant);
    procedure DispatchInvokeError(Status: HRESULT; const ExceptInfo: TExcepInfo);

    function HandleSafeCallException(ExceptObject: TObject; ExceptAddr: Pointer; const ErrorIID: TGUID; const ProgID,
      HelpFileName: WideString): HResult;

    function ComClassManager : TComClassManager;


    type
      TCoCreateInstanceExProc = function(const clsid: TCLSID; unkOuter: IUnknown; dwClsCtx: DWORD; ServerInfo: PCoServerInfo;
      dwCount: ULONG; rgmqResults: PMultiQIArray): HResult stdcall;
      TCoInitializeExProc = function (pvReserved: Pointer;
      coInit: DWORD): HResult; stdcall;
      TCoAddRefServerProcessProc = function : ULONG; stdcall;
      TCoReleaseServerProcessProc = function : ULONG; stdcall;
      TCoResumeClassObjectsProc = function : HResult; stdcall;
      TCoSuspendClassObjectsProc = function : HResult; stdcall;

    const
      CoCreateInstanceEx : TCoCreateInstanceExProc = nil;
      CoInitializeEx : TCoInitializeExProc = nil;
      CoAddRefServerProcess : TCoAddRefServerProcessProc = nil;
      CoReleaseServerProcess : TCoReleaseServerProcessProc = nil;
      CoResumeClassObjects : TCoResumeClassObjectsProc = nil;
      CoSuspendClassObjects : TCoSuspendClassObjectsProc = nil;
      CoInitFlags : Longint = -1;

implementation

    uses
      ComConst,Ole2, Registry;

    var
      Uninitializing : boolean;

    function HandleSafeCallException(ExceptObject: TObject; ExceptAddr: Pointer; const ErrorIID: TGUID; const ProgID,
      HelpFileName: WideString): HResult;
{$ifndef wince}
      var
        _CreateErrorInfo : ICreateErrorInfo;
        ErrorInfo : IErrorInfo;
{$endif wince}
      begin
        Result:=E_UNEXPECTED;
{$ifndef wince}
        if Succeeded(CreateErrorInfo(_CreateErrorInfo)) then
          begin
            _CreateErrorInfo.SetGUID(ErrorIID);
            if ProgID<>'' then
              _CreateErrorInfo.SetSource(PWidechar(ProgID));
            if HelpFileName<>'' then
              _CreateErrorInfo.SetHelpFile(PWidechar(HelpFileName));
            if ExceptObject is Exception then
              begin
                _CreateErrorInfo.SetDescription(PWidechar(Widestring(Exception(ExceptObject).Message)));
                _CreateErrorInfo.SetHelpContext(Exception(ExceptObject).HelpContext);
                if (ExceptObject is EOleSyserror) and (EOleSysError(ExceptObject).ErrorCode<0) then
                  Result:=EOleSysError(ExceptObject).ErrorCode
              end;
            if _CreateErrorInfo.QueryInterface(IErrorInfo,ErrorInfo)=S_OK then
              SetErrorInfo(0,ErrorInfo);
          end;
{$endif wince}
      end;


    constructor EOleSysError.Create(const Msg: string; aErrorCode: HRESULT; aHelpContext: Integer);
      var
        m : string;
      begin
        if Msg='' then
          m:=SysErrorMessage(aErrorCode)
        else
          m:=Msg;
        inherited CreateHelp(m,HelpContext);
        FErrorCode:=aErrorCode;
      end;


    constructor EOleException.Create(const Msg: string; aErrorCode: HRESULT;const aSource,aHelpFile: string; aHelpContext: Integer);
      begin
        inherited Create(Msg,aErrorCode,aHelpContext);
        FHelpFile:=aHelpFile;
        FSource:=aSource;
      end;

    {$define FPC_COMOBJ_HAS_CREATE_CLASS_ID}
    function CreateClassID : ansistring;
      var
         ClassID : TCLSID;
         p : PWideChar;
      begin
         CoCreateGuid(ClassID);
         StringFromCLSID(ClassID,p);
         result:=p;
         CoTaskMemFree(p);
      end;


   function CreateComObject(const ClassID : TGUID) : IUnknown;
     begin
       OleCheck(CoCreateInstance(ClassID,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IUnknown,result));
     end;


   function CreateRemoteComObject(const MachineName : WideString;const ClassID : TGUID) : IUnknown;
     var
       flags : DWORD;
       localhost : array[0..MAX_COMPUTERNAME_LENGTH] of WideChar;
       server : TCoServerInfo;
       mqi : TMultiQI;
       size : DWORD;
     begin
       if not(assigned(CoCreateInstanceEx)) then
         raise Exception.CreateRes(@SDCOMNotInstalled);

       FillChar(server,sizeof(server),0);
       server.pwszName:=PWideChar(MachineName);

       FillChar(mqi,sizeof(mqi),0);
       mqi.iid:=@IID_IUnknown;

       flags:=CLSCTX_LOCAL_SERVER or CLSCTX_REMOTE_SERVER or CLSCTX_INPROC_SERVER;

       { actually a remote call? }
{$ifndef wince}
       //roozbeh although there is a way to retrive computer name...HKLM\Ident\Name..but are they same?
	     size:=sizeof(localhost);
       if (MachineName<>'') and
          (not(GetComputerNameW(localhost,size)) or
          (WideCompareText(localhost,MachineName)<>0)) then
           flags:=CLSCTX_REMOTE_SERVER;
{$endif}

       OleCheck(CoCreateInstanceEx(ClassID,nil,flags,@server,1,@mqi));
       OleCheck(mqi.hr);
       Result:=mqi.itf;
     end;


   function CreateOleObject(const ClassName : string) : IDispatch;
     var
       id : TCLSID;
     begin
        id:=ProgIDToClassID(ClassName);
        OleCheck(CoCreateInstance(id,nil,CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER,IDispatch,result));
     end;


   function GetActiveOleObject(const ClassName : string) : IDispatch;
{$ifndef wince}
     var
     	 intf : IUnknown;
       id : TCLSID;
     begin
       id:=ProgIDToClassID(ClassName);
       OleCheck(GetActiveObject(id,nil,intf));
       OleCheck(intf.QueryInterface(IDispatch,Result));
     end;
{$else}
     begin
       Result:=nil;
     end;
{$endif wince}


   procedure OleError(Code: HResult);
     begin
       raise EOleSysError.Create('',Code,0);
     end;


   procedure OleCheck(Value : HResult);inline;
     begin
       if not(Succeeded(Value)) then
         OleError(Value);
     end;


   function ProgIDToClassID(const id : string) : TGUID;
     begin
       OleCheck(CLSIDFromProgID(PWideChar(WideString(id)),result));
     end;


   function ClassIDToProgID(const classID: TGUID): string;
     var
       progid : LPOLESTR;
     begin
       OleCheck(ProgIDFromCLSID(@classID,progid));
       result:=progid;
       CoTaskMemFree(progid);
     end;


   procedure SafeCallErrorHandler(err : HResult;addr : pointer);
{$ifndef wince}
     var
       info : IErrorInfo;
       descr,src,helpfile : widestring;
       helpctx : DWORD;
{$endif wince}
     begin
{$ifndef wince}
       if GetErrorInfo(0,info)=S_OK then
         begin
           info.GetDescription(descr);
           info.GetSource(src);
           info.GetHelpFile(helpfile);
           info.GetHelpContext(helpctx);
           raise EOleException.Create(descr,err,src,helpfile,helpctx) at addr;
         end
       else
{$endif wince}
         raise EOleException.Create('',err,'','',0) at addr;
     end;


    procedure DispatchInvokeError(Status: HRESULT; const ExceptInfo: TExcepInfo);
      begin
        if Status=DISP_E_EXCEPTION then
          raise EOleException.Create(ExceptInfo.Description,ExceptInfo.scode,ExceptInfo.Source,
            ExceptInfo.HelpFile,ExceptInfo.dwHelpContext)
        else
          raise EOleSysError.Create('',Status,0);
      end;

    var
      _ComClassManager : TComClassManager;

    function ComClassManager: TComClassManager;
      begin
        if not(assigned(_ComClassManager)) then
          _ComClassManager:=TComClassManager.Create;
        Result:=_ComClassManager;
      end;


    constructor TComClassManager.Create;
      begin
        fClassFactoryList := TObjectList.create(true);
      end;


    destructor TComClassManager.Destroy;
      begin
        fClassFactoryList.Free;
      end;

    procedure TComClassManager.AddObjectFactory(factory: TComObjectFactory);
      begin
{$ifdef DEBUG_COM}
        WriteLn('AddObjectFactory: ', GUIDToString(factory.FClassID), ' ', factory.FClassName);
{$endif}
        fClassFactoryList.Add(factory);
      end;

    procedure TComClassManager.RemoveObjectFactory(
          factory: TComObjectFactory);
      begin
        fClassFactoryList.Remove(factory);
      end;

    procedure TComClassManager.ForEachFactory(ComServer: TComServerObject;
      FactoryProc: TFactoryProc);
      var
        i: Integer;
        obj: TComObjectFactory;
      begin
{$ifdef DEBUG_COM}
        WriteLn('ForEachFactory');
{$endif}
        for i := 0 to fClassFactoryList.Count - 1 do
        begin
          obj := TComObjectFactory(fClassFactoryList[i]);
          if obj.ComServer = ComServer then
            FactoryProc(obj);
        end;
      end;


    function TComClassManager.GetFactoryFromClass(ComClass: TClass
      ): TComObjectFactory;
      var
        i: Integer;
      begin
{$ifdef DEBUG_COM}
        WriteLn('GetFactoryFromClass: ', ComClass.ClassName);
{$endif}
        for i := 0 to fClassFactoryList.Count - 1 do
        begin
          Result := TComObjectFactory(fClassFactoryList[i]);
          if ComClass = Result.ComClass then
            Exit();
        end;
        Result := nil;
      end;


    function TComClassManager.GetFactoryFromClassID(const ClassID: TGUID
      ): TComObjectFactory;
      var
        i: Integer;
      begin
{$ifdef DEBUG_COM}
        WriteLn('GetFactoryFromClassID: ', GUIDToString(ClassId));
{$endif}
        for i := 0 to fClassFactoryList.Count - 1 do
        begin
          Result := TComObjectFactory(fClassFactoryList[i]);
          if IsEqualGUID(ClassID, Result.ClassID) then
            Exit();
        end;
{$ifdef DEBUG_COM}
        WriteLn('GetFactoryFromClassID not found: ', GUIDToString(ClassId));
{$endif}
        Result := nil;
      end;


    function TComObject.GetController: IUnknown;
      begin
        Result:=IUnknown(Controller);
      end;


    function TComObject.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      begin
        if assigned(FController) then
          Result:=IUnknown(FController).QueryInterface(IID,Obj)
        else
          Result:=ObjQueryInterface(IID,Obj);
      end;


    function TComObject._AddRef: Integer; stdcall;
      begin
        if assigned(FController) then
          Result:=IUnknown(FController)._AddRef
        else
          Result:=ObjAddRef;
      end;


    function TComObject._Release: Integer; stdcall;
      begin
        if assigned(FController) then
          Result:=IUnknown(FController)._Release
        else
          Result:=ObjRelease;
      end;


    function TComObject.InterfaceSupportsErrorInfo(const iid: TIID): HResult; stdcall;
      begin
        if assigned(GetInterfaceEntry(iid)) then
          Result:=S_OK
        else
          Result:=S_FALSE;
      end;


    constructor TComObject.Create;
      begin
         CreateFromFactory(ComClassManager.GetFactoryFromClass(ClassType),nil);
      end;


    constructor TComObject.CreateAggregated(const Controller: IUnknown);
      begin
        CreateFromFactory(ComClassManager.GetFactoryFromClass(ClassType),Controller);
      end;


    constructor TComObject.CreateFromFactory(Factory: TComObjectFactory;
      const Controller: IUnknown);
      begin
        FFactory:=Factory;
        FRefCount:=1;
        FController:=Pointer(Controller);
        FFactory.Comserver.CountObject(True);
        FCounted:=true;
        Initialize;
        Dec(FRefCount);
      end;


    destructor TComObject.Destroy;
      begin
        if not(Uninitializing) then
          begin
            if assigned(FFactory) and FCounted then
              FFactory.Comserver.CountObject(false);
{$ifndef wince}
            if FRefCount>0 then
              CoDisconnectObject(Self,0);
{$endif wince}
          end;
      end;


    procedure TComObject.Initialize;
      begin
      end;


    function TComObject.ObjAddRef: Integer; stdcall;
      begin
        Result:=InterlockedIncrement(FRefCount);
      end;


    function TComObject.ObjQueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      begin
        if GetInterface(IID,Obj) then
          Result:=S_OK
        else
          Result:=E_NOINTERFACE;
      end;


    function TComObject.ObjRelease: Integer; stdcall;
      begin
        Result:=InterlockedDecrement(FRefCount);
        if Result=0 then
          Self.Destroy;
      end;


    function TComObject.SafeCallException(ExceptObject: TObject; ExceptAddr: Pointer): HResult;
      var
        Message: string;
        Handled: Integer;
      begin
        Handled:=0;
        Result:=0;
        if assigned(ServerExceptionHandler) then
          begin
            if ExceptObject is Exception then
              Message:=Exception(ExceptObject).Message;

            ServerExceptionHandler.OnException(ClassName,ExceptObject.ClassName,
              Message,PtrInt(ExceptAddr),WideString(GUIDToString(FFactory.ErrorIID)),
              FFactory.ProgID,Handled,Result);
          end;
        if Handled=0 then
          Result:=HandleSafeCallException(ExceptObject,ExceptAddr,FFactory.ErrorIID,
            FFactory.ProgID,FFactory.ComServer.HelpFileName);
      end;


    function TComObjectFactory.GetProgID: string;
      begin
        RunError(217);
      end;


    function TComObjectFactory.QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
      begin
        if GetInterface(IID,Obj) then
          Result:=S_OK
        else
          Result:=E_NOINTERFACE;
      end;


    function TComObjectFactory._AddRef: Integer; stdcall;
      begin
        Result:=InterlockedIncrement(FRefCount);
      end;


    function TComObjectFactory._Release: Integer; stdcall;
      begin
        Result:=InterlockedDecrement(FRefCount);
        if Result=0 then
          Self.Destroy;
      end;


    function TComObjectFactory.CreateInstance(const UnkOuter: IUnknown;
      const IID: TGUID; out Obj): HResult; stdcall;
      var
        comObject: TComObject;
      begin
{$ifdef DEBUG_COM}
        WriteLn('CreateInstance: ', GUIDToString(IID));
{$endif}
        comObject := CreateComObject(UnkOuter);
        if comObject.GetInterface(IID, Obj) then
          Result := S_OK
        else
          Result := E_NOINTERFACE;
      end;


    function TComObjectFactory.LockServer(fLock: BOOL): HResult; stdcall;
      begin
{$ifdef DEBUG_COM}
        WriteLn('LockServer: ', fLock);
{$endif}
        RunError(217);
      end;


    function TComObjectFactory.GetLicInfo(var licInfo: TLicInfo): HResult; stdcall;
      begin
{$ifdef DEBUG_COM}
        WriteLn('GetLicInfo');
{$endif}
        RunError(217);
      end;


    function TComObjectFactory.RequestLicKey(dwResrved: DWORD; out bstrKey: WideString): HResult; stdcall;
      begin
{$ifdef DEBUG_COM}
        WriteLn('RequestLicKey');
{$endif}
        RunError(217);
      end;


    function TComObjectFactory.CreateInstanceLic(const unkOuter: IUnknown;
      const unkReserved: IUnknown; const iid: TIID; const bstrKey: WideString; out
      vObject): HResult; stdcall;
      begin
{$ifdef DEBUG_COM}
        WriteLn('CreateInstanceLic');
{$endif}
        RunError(217);
      end;


    constructor TComObjectFactory.Create(ComServer: TComServerObject;
      ComClass: TComClass; const ClassID: TGUID; const Name,
      Description: string; Instancing: TClassInstancing;
      ThreadingModel: TThreadingModel);
      begin
{$ifdef DEBUG_COM}
        WriteLn('TComObjectFactory.Create');
{$endif}
        FRefCount := 1;
        FClassID := ClassID;
        FThreadingModel := ThreadingModel;
        FDescription := Description;
        FClassName := Name;
        FComServer := ComServer;
        FComClass := ComClass;
        FInstancing := Instancing;;
        ComClassManager.AddObjectFactory(Self);
      end;


    destructor TComObjectFactory.Destroy;
      begin
        ComClassManager.RemoveObjectFactory(Self);
        //RunError(217);
      end;


    function TComObjectFactory.CreateComObject(const Controller: IUnknown
      ): TComObject;
      begin
{$ifdef DEBUG_COM}
        WriteLn('TComObjectFactory.CreateComObject');
{$endif}
        Result := TComClass(FComClass).Create();
      end;


    procedure TComObjectFactory.RegisterClassObject;
      begin
        RunError(217);
      end;


(* Copy from Sample.RGS (http://www.codeproject.com/KB/atl/RegistryMap.aspx)
HKCR
{
    %PROGID%.%VERSION% = s '%DESCRIPTION%'
    {
        CLSID = s '%CLSID%'
    }
    %PROGID% = s '%DESCRIPTION%'
    {
        CLSID = s '%CLSID%'
        CurVer = s '%PROGID%.%VERSION%'
    }
    NoRemove CLSID
    {
        ForceRemove %CLSID% = s '%DESCRIPTION%'
        {
            ProgID = s '%PROGID%.%VERSION%'
            VersionIndependentProgID = s '%PROGID%'
            ForceRemove 'Programmable'
            InprocServer32 = s '%MODULE%'
            {
                val ThreadingModel = s '%THREADING%'
            }
            'TypeLib' = s '%LIBID%'
        }
    }
}
*)

    procedure TComObjectFactory.UpdateRegistry(Register: Boolean);
      var
        reg: TRegistry;
      begin
        RunError(217);

        //todo: finish this
        if Register then
        begin
          reg := TRegistry.Create;
          reg.RootKey := HKEY_CLASSES_ROOT;
          reg.OpenKey(FClassName + '.1', True);
          reg.WriteString('', Description);
          reg.WriteString('CLSID', GUIDToString(ClassID));
          reg.CloseKey;

          reg.OpenKey(FClassName, True);
          reg.WriteString('', Description);
          reg.WriteString('CLSID', GUIDToString(ClassID));
          reg.WriteString('CurVer', FClassName + '.1');
          reg.CloseKey;

          reg.OpenKey('CLSID\' + GUIDToString(ClassID), True);
          reg.WriteString('', Description);
          reg.WriteString('ProgID', FClassName);
          reg.WriteString('VersionIndependentProgID', FClassName);
          reg.WriteString('InprocServer32', 'MODULENAME');
          reg.CloseKey;

          reg.Free;

        end;
        //This should be in typedcomobject
        //reg.WriteString('TypeLib', FClassName);

      end;


{ $define DEBUG_COMDISPATCH}
    procedure DispatchInvoke(const Dispatch: IDispatch; CallDesc: PCallDesc;
      DispIDs: PDispIDList; Params: Pointer; Result: PVariant);

      var
        { we can't pass pascal ansistrings to COM routines so we've to convert them
          to/from widestring. This array contains the mapping to do so
        }
        StringMap : array[0..255] of record passtr : pansistring; comstr : pwidechar; end;
        invokekind,
        i : longint;
        invokeresult : HResult;
        exceptioninfo : TExcepInfo;
        dispparams : TDispParams;
        NextString : SizeInt;
        Arguments : array[0..255] of TVarData;
        CurrType : byte;
        MethodID : TDispID;
      begin
        NextString:=0;
        fillchar(dispparams,sizeof(dispparams),0);
        try
{$ifdef DEBUG_COMDISPATCH}
          writeln('DispatchInvoke: Got ',CallDesc^.ArgCount,' arguments   NamedArgs = ',CallDesc^.NamedArgCount);
{$endif DEBUG_COMDISPATCH}
          { copy and prepare arguments }
          for i:=0 to CallDesc^.ArgCount-1 do
            begin
{$ifdef DEBUG_COMDISPATCH}
              writeln('DispatchInvoke: Params = ',hexstr(PtrInt(Params),SizeOf(Pointer)*2));
{$endif DEBUG_COMDISPATCH}
              { get plain type }
              CurrType:=CallDesc^.ArgTypes[i] and $3f;
              { by reference? }
              if (CallDesc^.ArgTypes[i] and $80)<>0 then
                begin
                  case CurrType of
                    varStrArg:
                      begin
{$ifdef DEBUG_COMDISPATCH}
                        writeln('Translating var ansistring argument ',PString(Params^)^);
{$endif DEBUG_COMDISPATCH}
                        StringMap[NextString].ComStr:=StringToOleStr(PString(Params^)^);
                        StringMap[NextString].PasStr:=PString(Params^);
                        Arguments[i].VType:=varOleStr or varByRef;
                        Arguments[i].VPointer:=StringMap[NextString].ComStr;
                        inc(NextString);
                        inc(PPointer(Params));
                      end;
                    varVariant:
                      begin
{$ifdef DEBUG_COMDISPATCH}
                        writeln('Got ref. variant containing type: ',PVarData(PPointer(Params)^)^.VType);
{$endif DEBUG_COMDISPATCH}
                        if PVarData(PPointer(Params)^)^.VType=varString then
                          begin
{$ifdef DEBUG_COMDISPATCH}
                            writeln('  Casting nested varString: ',Ansistring(PVarData(Params^)^.vString));
{$endif DEBUG_COMDISPATCH}
                            VarCast(PVariant(Params^)^,PVariant(Params^)^,varOleStr);
                          end;

                        Arguments[i].VType:=varVariant or varByRef;
                        Arguments[i].VPointer:=PPointer(Params)^;
                        inc(PPointer(Params));
                      end
                    else
                      begin
{$ifdef DEBUG_COMDISPATCH}
                        write('DispatchInvoke: Got ref argument with type = ',CurrType);
                        case CurrType of
                          varOleStr:
                            write(' Value = ',pwidestring(PPointer(Params)^)^);
                        end;
                        writeln;
{$endif DEBUG_COMDISPATCH}
                        Arguments[i].VType:=CurrType or VarByRef;
                        Arguments[i].VPointer:=PPointer(Params)^;
                        inc(PPointer(Params));
                      end;
                  end
                end
              else
                case CurrType of
                  varStrArg:
                    begin
{$ifdef DEBUG_COMDISPATCH}
                      writeln('Translating ansistring argument ',PString(Params)^);
{$endif DEBUG_COMDISPATCH}
                      StringMap[NextString].ComStr:=StringToOleStr(PString(Params)^);
                      StringMap[NextString].PasStr:=nil;
                      Arguments[i].VType:=varOleStr;
                      Arguments[i].VPointer:=StringMap[NextString].ComStr;
                      inc(NextString);
                      inc(PPointer(Params));
                    end;

                  varVariant:
                    begin
{$ifdef DEBUG_COMDISPATCH}
                      writeln('Unimplemented variant dispatch');
{$endif DEBUG_COMDISPATCH}
                    end;
                  varCurrency,
                  varDouble,
                  VarDate:
                    begin
{$ifdef DEBUG_COMDISPATCH}
                      writeln('Got 8 byte float argument');
{$endif DEBUG_COMDISPATCH}
                      Arguments[i].VType:=CurrType;
                      move(PPointer(Params)^,Arguments[i].VDouble,sizeof(Double));
                      inc(PDouble(Params));
                    end;
                  else
                    begin
{$ifdef DEBUG_COMDISPATCH}
                      write('DispatchInvoke: Got argument with type ',CurrType);
                      case CurrType of
                        varOleStr:
                          write(' Value = ',pwidestring(Params)^);
                        else
                          write(' Value = ',hexstr(PtrInt(PPointer(Params)^),SizeOf(Pointer)*2));
                      end;
                      writeln;
{$endif DEBUG_COMDISPATCH}
                      Arguments[i].VType:=CurrType;
                      Arguments[i].VPointer:=PPointer(Params)^;
                      inc(PPointer(Params));
                    end;
                end;
            end;

          { finally prepare the call }
          with DispParams do
            begin
              rgvarg:=@Arguments;
              cNamedArgs:=CallDesc^.NamedArgCount;
              if cNamedArgs=0 then
                rgdispidNamedArgs:=nil
              else
                rgdispidNamedArgs:=@DispIDs^[1];
              cArgs:=CallDesc^.ArgCount;
            end;
          InvokeKind:=CallDesc^.CallType;
          MethodID:=DispIDs^[0];
          case InvokeKind of
            DISPATCH_PROPERTYPUT:
              begin
                if (Arguments[0].VType and varTypeMask) = varDispatch then
                  InvokeKind:=DISPATCH_PROPERTYPUTREF;
                { first name is actually the name of the property to set }
                DispIDs^[0]:=DISPID_PROPERTYPUT;
                DispParams.rgdispidNamedArgs:=@DispIDs^[0];
                inc(DispParams.cNamedArgs);
              end;
            DISPATCH_METHOD:
              if assigned(Result) and (CallDesc^.ArgCount=0) then
                InvokeKind:=DISPATCH_METHOD or DISPATCH_PROPERTYGET;
          end;
{$ifdef DEBUG_COMDISPATCH}
          writeln('DispatchInvoke: MethodID: ',MethodID,' InvokeKind: ',InvokeKind);
{$endif DEBUG_COMDISPATCH}
          { do the call and check the result }
          invokeresult:=Dispatch.Invoke(MethodID,GUID_NULL,0,InvokeKind,DispParams,result,@exceptioninfo,nil);
          if invokeresult<>0 then
            DispatchInvokeError(invokeresult,exceptioninfo);

          { translate strings back }
          for i:=0 to NextString-1 do
            if assigned(StringMap[i].passtr) then
              OleStrToStrVar(StringMap[i].comstr,StringMap[i].passtr^);
        finally
          for i:=0 to NextString-1 do
            SysFreeString(StringMap[i].ComStr);
        end;
      end;


    procedure SearchIDs(const DispatchInterface : IDispatch; Names: PChar;
      Count: Integer; IDs: PDispIDList);
      var
      	res : HRESULT;
      	NamesArray : ^PWideChar;
      	NamesData : PWideChar;
      	OrigNames : PChar;
        NameCount,
      	NameLen,
      	NewNameLen,
        CurrentNameDataUsed,
      	CurrentNameDataSize : SizeInt;
      	i : longint;
      begin
      	getmem(NamesArray,Count*sizeof(PWideChar));
      	CurrentNameDataSize:=256;
      	CurrentNameDataUsed:=0;
      	getmem(NamesData,CurrentNameDataSize);
        NameCount:=0;
   	    OrigNames:=Names;
{$ifdef DEBUG_COMDISPATCH}
        writeln('SearchIDs: Searching ',Count,' IDs');
{$endif DEBUG_COMDISPATCH}
      	for i:=1 to Count do
      	  begin
       	    NameLen:=strlen(Names);
{$ifdef DEBUG_COMDISPATCH}
            writeln('SearchIDs: Original name: ',Names,' Len: ',NameLen);
{$endif DEBUG_COMDISPATCH}
      	    NewNameLen:=MultiByteToWideChar(0,0,Names,NameLen,nil,0)+1;
      	    if (CurrentNameDataUsed+NewNameLen)*2>CurrentNameDataSize then
      	      begin
      	      	inc(CurrentNameDataSize,256);
      	        reallocmem(NamesData,CurrentNameDataSize);
      	      end;
      	    NamesArray[i-1]:=@NamesData[CurrentNameDataUsed];
      	    MultiByteToWideChar(0,0,Names,NameLen,@NamesData[CurrentNameDataUsed],NewNameLen);
      	    NamesData[CurrentNameDataUsed+NewNameLen-1]:=#0;
{$ifdef DEBUG_COMDISPATCH}
            writeln('SearchIDs: Translated name: ',WideString(PWideChar(@NamesData[CurrentNameDataUsed])));
{$endif DEBUG_COMDISPATCH}
      	    inc(CurrentNameDataUsed,NewNameLen);
      	    inc(Names,NameLen+1);
            inc(NameCount);
      	  end;
      	res:=DispatchInterface.GetIDsOfNames(GUID_NULL,NamesArray,NameCount,
{$ifdef wince}
		     LOCALE_SYSTEM_DEFAULT
{$else wince}
         GetThreadLocale
{$endif wince}
         ,IDs);
{$ifdef DEBUG_COMDISPATCH}
        writeln('SearchIDs: GetIDsOfNames result = ',hexstr(res,SizeOf(HRESULT)*2));
        for i:=0 to Count-1 do
          writeln('SearchIDs: ID[',i,'] = ',ids^[i]);
{$endif DEBUG_COMDISPATCH}
      	if res=DISP_E_UNKNOWNNAME then
      	  raise EOleError.createresfmt(@snomethod,[OrigNames])
      	else
      	  OleCheck(res);
      	freemem(NamesArray);
      	freemem(NamesData);
      end;


    procedure ComObjDispatchInvoke(dest : PVariant;const source : Variant;
        calldesc : pcalldesc;params : pointer);cdecl;
      var
      	dispatchinterface : pointer;
      	ids : array[0..255] of TDispID;
      begin
        fillchar(ids,sizeof(ids),0);
{$ifdef DEBUG_COMDISPATCH}
        writeln('ComObjDispatchInvoke called');
        writeln('ComObjDispatchInvoke: @CallDesc = $',hexstr(PtrInt(CallDesc),SizeOf(Pointer)*2),' CallDesc^.ArgCount = ',CallDesc^.ArgCount);
{$endif DEBUG_COMDISPATCH}
      	if tvardata(source).vtype=VarDispatch then
      	  dispatchinterface:=tvardata(source).vdispatch
      	else if tvardata(source).vtype=(VarDispatch or VarByRef) then
      	  dispatchinterface:=pvardata(tvardata(source).vpointer)^.vdispatch
      	else
      	  raise eoleerror.createres(@SVarNotObject);
      	SearchIDs(IDispatch(dispatchinterface),@CallDesc^.ArgTypes[CallDesc^.ArgCount],
          CallDesc^.NamedArgCount+1,@ids);
      	if assigned(dest) then
      	  VarClear(dest^);
      	DispatchInvoke(IDispatch(dispatchinterface),calldesc,@ids,params,dest);
      end;


{ $define DEBUG_DISPATCH}
    procedure DoDispCallByID(res : Pointer; const disp : IDispatch;desc : PDispDesc; params : Pointer);
      var
        exceptioninfo : TExcepInfo;
        dispparams : TDispParams;
        flags : WORD;
        invokeresult : HRESULT;
        preallocateddata : array[0..15] of TVarData;
        Arguments : ^TVarData;
        NamedArguments : PPointer;
        CurrType : byte;
        namedcount,i : byte;
      begin
        { use preallocated space, i.e. can we avoid a getmem call? }
        if desc^.calldesc.argcount<=Length(preallocateddata) then
          Arguments:=@preallocateddata
        else
          GetMem(Arguments,desc^.calldesc.argcount*sizeof(TVarData));

        { prepare parameters }
        for i:=0 to desc^.CallDesc.ArgCount-1 do
          begin
{$ifdef DEBUG_DISPATCH}
            writeln('DoDispCallByID: Params = ',hexstr(PtrInt(Params),SizeOf(Pointer)*2));
{$endif DEBUG_DISPATCH}
            { get plain type }
            CurrType:=desc^.CallDesc.ArgTypes[i] and $3f;
            { by reference? }
            if (desc^.CallDesc.ArgTypes[i] and $80)<>0 then
              begin
{$ifdef DEBUG_DISPATCH}
                write('DispatchInvoke: Got ref argument with type = ',CurrType);
                writeln;
{$endif DEBUG_DISPATCH}
                Arguments[i].VType:=CurrType or VarByRef;
                Arguments[i].VPointer:=PPointer(Params)^;
                inc(PPointer(Params));
              end
            else
              begin
{$ifdef DEBUG_DISPATCH}
                writeln('DispatchInvoke: Got ref argument with type = ',CurrType);
{$endif DEBUG_DISPATCH}
                case CurrType of
                  varVariant:
                    begin
                      Arguments[i].VType:=CurrType;
                      move(PVarData(Params)^,Arguments[i],sizeof(TVarData));
                      inc(PVarData(Params));
                    end;
                  varCurrency,
                  varDouble,
                  VarDate:
                    begin
{$ifdef DEBUG_DISPATCH}
                      writeln('DispatchInvoke: Got 8 byte float argument');
{$endif DEBUG_DISPATCH}
                      Arguments[i].VType:=CurrType;
                      move(PPointer(Params)^,Arguments[i].VDouble,sizeof(Double));
                      inc(PDouble(Params));
                    end;
                else
                  begin
{$ifdef DEBUG_DISPATCH}
                    writeln('DispatchInvoke: Got argument with type ',CurrType);
{$endif DEBUG_DISPATCH}
                    Arguments[i].VType:=CurrType;
                    Arguments[i].VPointer:=PPointer(Params)^;
                    inc(PPointer(Params));
                  end;
              end;
            end;
          end;
        dispparams.cArgs:=desc^.calldesc.argcount;
        dispparams.rgvarg:=pointer(Arguments);

        { handle properties properly here ! }
        namedcount:=desc^.calldesc.namedargcount;
        if desc^.calldesc.calltype=DISPATCH_PROPERTYPUT then
          inc(namedcount)
        else
          NamedArguments:=@desc^.CallDesc.ArgTypes[desc^.CallDesc.ArgCount];
        dispparams.cNamedArgs:=namedcount;
        dispparams.rgdispidNamedArgs:=pointer(NamedArguments);
        flags:=0;
        invokeresult:=disp.Invoke(
                desc^.DispId, { DispID: LongInt; }
                GUID_NULL, { const iid : TGUID; }
                0, { LocaleID : longint; }
                flags, { Flags: Word; }
                dispparams, { var params; }
                res,@exceptioninfo,nil { VarResult,ExcepInfo,ArgErr : pointer) }
          );
        if invokeresult<>0 then
          DispatchInvokeError(invokeresult,exceptioninfo);
        if desc^.calldesc.argcount>Length(preallocateddata) then
          FreeMem(Arguments);
      end;

    { TTypedComObject }

    function TTypedComObject.GetClassInfo(out pptti: ITypeInfo): HResult;stdcall;
      begin
        Result:=S_OK;
        pptti:=TTypedComObjectFactory(factory).classinfo;
      end;


    { TTypedComObjectFactory }

    constructor TTypedComObjectFactory.Create(AComServer: TComServerObject; TypedComClass: TTypedComClass; const AClassID: TGUID;
      AInstancing: TClassInstancing; AThreadingModel: TThreadingModel = tmSingle);
      var
        TypedName, TypedDescription: WideString;
      begin
        //TDB get name and description from typelib (check if this is a valid guid)
        OleCheck(AComServer.GetTypeLib.GetTypeInfoOfGuid(AClassID, FClassInfo));
        //bug FPC 0010569 - http://msdn2.microsoft.com/en-us/library/ms221396(VS.85).aspx
        OleCheck(FClassInfo.GetDocumentation(-1, TypedName, TypedDescription, PLongWord(nil)^, PWideString(nil)^));
        inherited Create(AComServer, TypedComClass, AClassID, TypedName, TypedDescription, AInstancing, AThreadingModel);
      end;


    function TTypedComObjectFactory.GetInterfaceTypeInfo(TypeFlags: Integer): ITypeInfo;
      begin
        RunError(217);
      end;


    procedure TTypedComObjectFactory.UpdateRegistry(Register: Boolean);
      begin
        inherited UpdateRegistry(Register);
        // 'TypeLib' = s '%LIBID%' missing ??? or does TComServer register it ?
        //un/register typed library
        RunError(217);
      end;


const
  Initialized : boolean = false;
var
  Ole32Dll : HModule;

initialization
  Uninitializing:=false;
  _ComClassManager:=nil;
  Ole32Dll:=GetModuleHandle('ole32.dll');
  if Ole32Dll<>0 then
    begin
      Pointer(CoCreateInstanceEx):=GetProcAddress(Ole32Dll,'CoCreateInstanceEx');
      Pointer(CoInitializeEx):=GetProcAddress(Ole32Dll,'CoInitializeEx');
      Pointer(CoAddRefServerProcess):=GetProcAddress(Ole32Dll,'CoAddRefServerProcess');
      Pointer(CoReleaseServerProcess):=GetProcAddress(Ole32Dll,'CoReleaseServerProcess');
      Pointer(CoResumeClassObjects):=GetProcAddress(Ole32Dll,'CoResumeClassObjects');
      Pointer(CoSuspendClassObjects):=GetProcAddress(Ole32Dll,'CoSuspendClassObjects');
    end;

  if not(IsLibrary) then
{$ifndef wince}
    if (CoInitFlags=-1) or not(assigned(comobj.CoInitializeEx)) then
      Initialized:=Succeeded(CoInitialize(nil))
    else
{$endif wince}
      Initialized:=Succeeded(comobj.CoInitializeEx(nil, CoInitFlags));

  SafeCallErrorProc:=@SafeCallErrorHandler;
  VarDispProc:=@ComObjDispatchInvoke;
  DispCallByIDProc:=@DoDispCallByID;
finalization
  Uninitializing:=true;
  _ComClassManager.Free;
  VarDispProc:=nil;
  SafeCallErrorProc:=nil;
  if Initialized then
    CoUninitialize;
end.

