{ %norun }

program T001;
{$MODE DELPHI}
{$R-}
type
  TLanguages = (
    lOne,
    lTwo,
    lThree,
    lFour
  );
 
const
  LANGUAGE_NONE = TLanguages(255);
 
type
 TLanguage = record
   Index : TLanguages;
 end;
 
var
  Lang: TLanguages;
  CurrentLanguage: TLanguage = (
    Index:  LANGUAGE_NONE
  );
 
begin
  Lang := LANGUAGE_NONE;
end.
