{
    Class and interface identifiers shared by the per-user COM registration test programs.
}
unit comperuserintf;

{$mode objfpc}{$H+}

interface

const
  CLASS_PerUserExe: TGUID = '{BBE8C509-1AD5-4D11-A40F-01917A6E8B8F}';
  CLASS_PerUserDll: TGUID = '{716300D4-C660-4D06-A91B-7114FD1348CC}';
  IID_ITestPerUser: TGUID = '{41BF6A94-33B2-4235-8CEA-BE45EB852B4F}';
  LIBID_PerUserTest: TGUID = '{363FA7C4-4049-42C9-A4D9-DEEDA79A53FD}';
  PerUserTypeLibName = 'FPCPerUserTest';
  PerUserAnswer = 42;

type
  // Interface implemented by the in-process test server
  ITestPerUser = interface(IUnknown)
    ['{41BF6A94-33B2-4235-8CEA-BE45EB852B4F}']
    // Return PerUserAnswer
    function Answer: Integer; stdcall;
  end;

implementation

end.
