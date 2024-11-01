{$ifdef fpc}
{$mode objfpc}
{$endif fpc}
unit uimplements8;

interface

uses
  classes;

type
  to1 = class(TObject,IInterface)
    fi : IInterface;
    property i : IInterface read fi implements IInterface;
  end;

implementation

end.
