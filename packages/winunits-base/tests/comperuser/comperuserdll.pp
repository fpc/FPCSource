{
    In-process (InprocServer32) COM server for the per-user registration test.
    Register per user with "regsvr32 /n /i:user comperuserdll.dll".
}
library comperuserdll;

{$mode objfpc}{$H+}

uses
  ComObj, ComServ, comperuserintf;

type
  TPerUserDllObject = class(TComObject, ITestPerUser)
  public
    // Return PerUserAnswer
    function Answer: Integer; stdcall;
  end;


function TPerUserDllObject.Answer: Integer; stdcall;

begin
  Result:=PerUserAnswer;
end;


exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer,
  DllInstall;

begin
  TComObjectFactory.Create(ComServer,TPerUserDllObject,CLASS_PerUserDll,'PerUserDll',
    'FPC per-user registration test (dll)',ciMultiInstance,tmApartment);
end.
