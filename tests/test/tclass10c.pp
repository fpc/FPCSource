program tclass10c;
{$ifdef fpc}
  {$mode delphi}
{$endif}

// check that "protected" or any other section resets the section type to accept regular fields

type
  Tfoo=class
  private
    class var
      f1: Integer;
  protected
    f2: Integer;
  public
    class property pf1: Integer read f1;
    property pf2: Integer read f2;
  end;

begin
end.                 
