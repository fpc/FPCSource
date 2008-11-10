program TestDateVariantConversion;

uses variants;

var dt : TDateTime;
    v : variant;
    s : String;

begin
  dt := 40000;
  v := dt;
  s := v;
  // It should return the date, depending on the localisation settings
  if s = '40000' then halt(1); 
end.
