program tw40828;

{$ifdef fpc}
{$mode delphi}
{$endif}

uses
  SysUtils, Rtti;

type
  TCurrencyHandler = procedure (Sender: TObject; Cur: Currency) of object;

procedure DoTest;
var
  Context: TRttiContext;
  Ty: TRttiType;
  P: TRttiParameter;
begin
  Context := TRttiContext.Create(True);
  try
    Ty := Context.GetType(TypeInfo(TCurrencyHandler));
    for P in (Ty as TRttiMethodType).GetParameters() do
      WriteLn(P.Name, ': ', P.ParamType.Name);
  finally
    Context.Free;
  end;
end;

begin
  DoTest;
  WriteLn('OK');
end.
