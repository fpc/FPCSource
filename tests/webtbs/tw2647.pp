{$mode Delphi}
type
  ISensLogon = interface (IDispatch)
  ['{d597bab3-5b9f-11d1-8dd2-00aa004abd5e}']
    function Logon(bstrUserName: WideString): HRESULT; stdcall; dispid 1;
    function Logoff(bstrUserName: WideString): HRESULT; stdcall; dispid 2;
    function StartShell(bstrUserName: WideString): HRESULT; stdcall; dispid 3;
    function DisplayLock(bstrUserName: WideString): HRESULT; stdcall; dispid 4;
    function DisplayUnlock(bstrUserName: WideString): HRESULT; stdcall; dispid 5;
    function StartScreenSaver(bstrUserName: WideString): HRESULT; stdcall; dispid 6;
    function StopScreenSaver(bstrUserName: WideString): HRESULT; stdcall; dispid 7;
  end;

begin
end.
