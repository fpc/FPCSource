{ %FAIL }
{ %NORUN }
program thintdir2b;

// don't allow to use hint modifier twice

{$mode delphi}
type
  TTest = class(TObject)
  public
    procedure Test(); virtual; deprecated 'Do not use this method'; abstract; deprecated 'Use that method';
  end;
begin
end.                      
