unit uw4541;

{$mode objfpc}{$H+}

interface

uses
  Classes;

type

  TA = class(TComponent)
    D: TComponent;
  end;

  TB= class(TComponent)
  private
    FA: TA;
  public
    property C: TComponent read FA.D;
  end;

implementation

end.

