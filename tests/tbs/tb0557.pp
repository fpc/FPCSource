program rtti;

{$ifdef fpc}
{$mode objfpc}{$H+}
{$apptype console}
{$endif}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, typinfo
  { you can add units after this };

type
  { TSomeBaseClass }

  TSomeBaseClass = class(TPersistent)
  private
    FSomeProperty: Integer;
  public
    property SomeProperty: Integer read FSomeProperty write FSomeProperty default 10;
  end;

  { TSomeDerivedClass }

  TSomeDerivedClass = class(TSomeBaseClass)
  private
    FOwnProperty: Integer;
  published
    property SomeProperty;
    property OwnProperty: Integer read FOwnProperty write FOwnProperty default 11;
  end;

var
  BC : TSomeBaseClass;
  DC: TSomeDerivedClass;
  Info: PPropInfo;
begin
  DC := TSomeDerivedClass.Create;
  Info := GetPropInfo(DC, 'SomeProperty');
  if (Info^.Default<>10) then
    Halt(1);
  Info := GetPropInfo(DC, 'OwnProperty');
  if Info^.Default<>11 then
    Halt(2);
end.

