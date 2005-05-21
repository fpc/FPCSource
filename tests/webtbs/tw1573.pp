{ %version=1.1 }

{$mode objfpc}

type
        TCheck=class(TObject);


var
        vlA : TCheck;
        vlB : TObject;

procedure aa(const ParXX :array of TObject);
begin
        // writeln(cardinal(ParXX[0]),' ', cardinal(ParXX[1]));
        if (ParXX[0]<>vlA) or (ParXX[1]<>vlB) then
          begin
             writeln('error');
             halt(1);
          end;
end;

begin
        vlB := TObject.Create;
        vlA := TCheck.Create;
        aa([vlA,vlB]);
end.
