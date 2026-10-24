{ %skiptarget=msdos,go32v2 }
program tdb13;

{ An object field's size is not checked as a scalar field's is, as in Delphi:
  a TArrayField can be created -- its constructor sets Size to 10, the number
  of its elements -- and its Size set again, and so can a TObjectField's; an
  ordinary field still refuses a size }

{$mode objfpc}{$H+}

uses sysutils, db;

var
  F: TArrayField;
  O: TObjectField;
  { TField's own check, which TObjectField used to inherit }
  I: TBooleanField;
  Refused: boolean;
begin
  O:=TObjectField.Create(nil);
  O.Size:=5;
  if O.Size<>5 then
    begin
      writeln('the object field''s size is ',O.Size,', not 5');
      halt(4);
    end;
  O.Free;
  I:=TBooleanField.Create(nil);
  Refused:=false;
  try
    I.Size:=4;
  except
    on E: EDatabaseError do
      Refused:=true;
  end;
  I.Free;
  if not Refused then
    begin
      writeln('a boolean field took a size');
      halt(5);
    end;
  F:=nil;
  try
    F:=TArrayField.Create(nil);
  except
    on E: Exception do
      begin
        writeln('TArrayField.Create raised ',E.ClassName,': ',E.Message);
        halt(1);
      end;
  end;
  if F.Size<>10 then
    begin
      writeln('the size is ',F.Size,', not 10');
      halt(2);
    end;
  F.Size:=3;
  if F.Size<>3 then
    begin
      writeln('the size is ',F.Size,', not 3');
      halt(3);
    end;
  F.Free;
  writeln('ok');
end.
