program tclass10a;
{$ifdef fpc}
  {$mode delphi}
{$endif}

// check that "protected" or any other section resets the section type to accept regular fields

type
  Tfoo=class
  private
    type
      TF = (one,two,three);
  protected
    f: TF;
  end;

begin
end.                 
