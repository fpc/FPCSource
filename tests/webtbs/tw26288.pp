unit tw26288;

{$mode objfpc}{$h+}

interface

uses
  Classes, SysUtils;
  
type  
  { TGenVector }
  generic TGenVector<_TItem_> = class
  public type
    TItemToString = function (const Item: _TItem_) : String of object;
    
  strict private 
    fOnItemToString: TItemToString;
    
    procedure SetOnItemToString(AValue: TItemToString);
    
  public
    constructor Create;
    
    function DefaultItemToString(const Item: _TItem_) : String; virtual;
    
    property OnItemToString : TItemToString read fOnItemToString 
      write SetOnItemToString;
  end;
  
implementation

{--- TGenVector.Create ---}
constructor TGenVector.Create;
begin
  SetOnItemToString(nil);
end;

{--- TGenVector.DefaultItemToString ---}
function TGenVector.DefaultItemToString(const Item: _TItem_): String;
begin
  raise Exception.Create('Method not redefined');
  Result := '';
end;

{--- TGenVector.SetOnItemToString ---}
procedure TGenVector.SetOnItemToString(AValue: TItemToString);
begin
  if AValue = nil then
    fOnItemToString := @DefaultItemToString
  else
    fOnItemToString := AValue;
end;

end.
