{$mode objfpc}{$H+}

uses
 Classes, SysUtils
  { add your units here };

type
  TNotifyEventExt = procedure(ASender: TObject; ANotifyType: integer) of
object;

  TNotify = class
  strict private
    FOnNotify: TNotifyEventExt;
  strict  protected
    procedure test;
  public
    constructor Create;
  end;


constructor TNotify.Create;
begin
  inherited Create;
  FOnNotify := nil;
end;

procedure TNotify.test;
begin
   FOnNotify(nil, 0); //project1.pas(30,13) Fatal: Syntax error, ";" expected but "(" found
end;

begin
end.

