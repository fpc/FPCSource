program classmeth;

{$mode delphi}

type
 TElCustomCryptoProviderClass = class of TElCustomCryptoProvider;
 TElCustomCryptoProvider = class
   class procedure SetAsDefault;
   class procedure DoSetAsDefault(Value : TElCustomCryptoProviderClass);
 end;

 tc2 = class(TElCustomCryptoProvider)
   class procedure SetAsDefault; //reintroduce;
 end;


var
  x: TElCustomCryptoProviderClass;

class procedure TElCustomCryptoProvider.SetAsDefault;
begin
 DoSetAsDefault(Self); /// Illegal expression
end;

class procedure TElCustomCryptoProvider.DoSetAsDefault(Value : TElCustomCryptoProviderClass);
begin
// SetDefaultCryptoProviderType(Value);
  x:=value;
end;

class procedure tc2.SetAsDefault;
begin
  DoSetAsDefault(Self);
end;

begin
  TElCustomCryptoProvider.SetAsDefault;
  if x<>TElCustomCryptoProvider then
    raise JLException.create('first');
  tc2.SetAsDefault;
  if x<>tc2 then
    raise JLException.create('second');
end.
