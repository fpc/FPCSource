{ %fail }

{$ifdef fpc}{$mode objfpc}{$endif}

type
  TStream = class(TObject)
    function Seek(offset:Int64):Int64;
  end;

  TSeek64 = function(offset:Int64):Int64 of object;

function TStream.Seek(offset:Int64):Int64;
begin
end;

var
  s : TStream;
  p : TSeek64;
  c : TClass;
begin
  s:=TStream.Create;
  c:=s.Classtype;
  { This is not valid, @c should be used }
  p:=@TStream(c).Seek;
end.
