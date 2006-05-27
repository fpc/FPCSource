{ %FAIL }
{$mode objfpc}
type
  IGUserData = interface(IInvokable)
    ['{35377B66-6B1B-11D9-8827-00055DDDEA00}']
    (* userdata *)
    function GetUserData(key: UTF8String): IInterface;
    procedure SetUserData(key: UTF8String; const v: IInterface);

    function GetUserDataVariant(key: UTF8String): Variant;
    procedure SetUserDataVariant(key: UTF8String; const v: Variant);

    property UserData[key: UTF8String]: IInterface read GetUserData write SetUserData;
  end;
begin
end.
