{$mode objfpc}
type
  TSynIdentChars = set of char;
  tobj = class
    function GetIdentChars: TSynIdentChars; virtual;
    procedure p(s : tSynIdentChars);
    property IdentChars: TSynIdentChars read GetIdentChars;
  end;

function tobj.GetIdentChars: TSynIdentChars;
  begin
  end;

procedure tobj.p(s : tSynIdentChars);
   begin
     p(IdentChars+['-']);
   end;

begin
end.
