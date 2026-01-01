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

  { TContainer }

  TContainer = class
  public
    constructor Create;
  end;

{ TContainer }

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


