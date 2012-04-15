{ %norun}
program tw20421;

{$mode delphi}

type
  TAncestor = class
  private
    function GetTest(index: integer): integer;
  public
    property test[index: integer]: integer read GetTest;
  end;

  TDescendant = class(TAncestor)
  private
    procedure SetTest(index, value: integer);
  public
    property test write SetTest;
  end;

{ TAncestor }

function TAncestor.GetTest(index: integer): integer;
begin

end;

{ TDescendant }

procedure TDescendant.SetTest(index, value: integer);
begin

end;

begin

end.