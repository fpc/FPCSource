{
  This test causes an internal error 200309041 on i8086
}

var
  Abbrev_Offsets : array of QWord;

function Pos() : Int64;
begin
end;

procedure ReadAbbrevTable;
  var
    nr: Int64;
  begin
    Abbrev_Offsets[nr]:=Pos;
  end;

begin
end.
