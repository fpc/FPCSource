{ %FAIL}
program tclass10b;
{$ifdef fpc}
  {$mode delphi}
{$endif}

// check that "protected" or any other section resets the section type to accept regular fields

type
  Tfoo=class
  type private
    TF = (one,two,three);
  type protected
    f: TF;
  end;

begin
end.
