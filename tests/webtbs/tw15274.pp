{ %norun }

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

type

  { TItem }

  TItem = class
  public
    procedure DoSomething;
  end;

  { TConainer }

  TContainer = class
  public
    constructor Create;
  end;

{ TConainer }

constructor TContainer.Create;
begin
  inherited Create;
  with TItem.Create do
   ;
//   begin end;
end;

{ TItem }

procedure TItem.DoSomething;
begin
end;

begin
end.


