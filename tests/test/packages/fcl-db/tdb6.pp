program bugVarNullToString;

{$Mode ObjFpc}
{$H+}

uses
  Variants, Db, SysUtils;
  
var 
  V: Variant;
  S: String;
  List: TLookupList;  
begin
  List := TLookupList.Create;
  List.Add(1, 'X');
  List.Add(2, 'Y');
  
  V := List.ValueOfKey(1);
  S := VarToStr(V);
  WriteLn(Format('Key: %d Value: "%s"', [1, S]));
  if s<>'X' then Halt(1); 

  V := List.ValueOfKey(2);
  S := VarToStr(V);
  WriteLn(Format('Key: %d Value: "%s"', [2, S]));
  if s<>'Y' then Halt(1); 
  
  V := List.ValueOfKey(3);
  S := VarToStr(V);
  WriteLn(Format('Key: %d Value: "%s"', [3, S]));
  if s<>'' then Halt(1); 

  List.Free;
end.
