unit tfuncref56;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}
{$modeswitch functionreferences}

interface

type
  TCallback = reference to procedure(aSelf: TObject);
  TMethodImplementationCallback = reference to procedure(aUserData: Pointer);

  TTest = class
    function CreateImplementation(aCallback: TCallback): TMethodImplementationCallback;
  end;


implementation

function TTest.CreateImplementation(aCallback: TCallback): TMethodImplementationCallback;

  procedure SelfCallback(aUserData: Pointer);
  begin
    aCallback(TObject(aUserData));
  end;

begin
  Result := @SelfCallback;
end;

end.

