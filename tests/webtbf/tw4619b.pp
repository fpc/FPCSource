{ %fail }

{ Source provided for Free Pascal Bug Report 4619 }
{ Submitted by "Christian Iversen" on  2005-12-19 }
{ e-mail: chrivers@iversen-net.dk }

{$mode delphi}

type
  TStatement = class
  end;

  TBlock = class(TStatement)
  protected
    function GetStat(const Index: LongWord): TStatement;
  public
    property Statement[const Index: Integer]: TStatement read GetStat; default;
  end;

    function TBlock.GetStat(const Index: LongWord): TStatement;
begin
end;

begin
end.
