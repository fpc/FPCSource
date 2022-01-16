{ Note: this is a vastly reduced variant of the example attached to bug report #31945 }

unit tw31945;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  { GprAvgLvlTreeNode }

  generic GprAvgLvlTreeNode<T> = class
  public
    procedure ConsistencyCheck; virtual;
  end;

implementation

{ GprAvgLvlTreeNode }

procedure GprAvgLvlTreeNode.ConsistencyCheck;

  procedure E(Msg: string);
  begin
    raise Exception.Create('GprAvgLvlTreeNode.ConsistencyCheck: '+Msg);
  end;

begin
  E('Hello World');
end;

var
  t: specialize GprAvgLvlTreeNode<LongInt>;
initialization

end.

