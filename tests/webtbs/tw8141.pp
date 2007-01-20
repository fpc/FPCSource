{ %norun }
uses
  Classes, SysUtils;

type
  ARec = packed record
  {$ifdef cpu64}
    hi, lo : DWord;
  {$else}
    hi, lo : Word;
  {$endif}
  end;

var
  List : TList;
begin
  with ARec(Pointer(List.Last)) do
    lo := 1;
end.
