{ %NORUN }

program tw39876a;

{$mode delphi}

type
   TInputTypes = (itText, itPassWord, itRadio, itCheckbox, itHidden, itTel,
                 itNumber, itUrl, itEmail, itRrange, itDate, itMonth, itWeek);

var
   ts: string;
   anIT: TInputTypes;

begin
   ts := 'itradio, tuesday, 3';
   ReadStr(ts, anIT); // <== this is not compiled ...
end.

