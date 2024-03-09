program jsonpath;

{$mode objfpc}{$h+}

uses sysutils, classes,iostream,fpjson, jsonparser;

Procedure Usage(Msg : String);

begin
  Writeln('Usage : jsonpath <path> [<file>]');
  Writeln('Reads JSON from <file> or standard input if the <file> argument is not present');
  Writeln('calculates <path> and prints path to stdout');
  Writeln('if <path> does not exist in the JSON, null is printed');
  Halt(Ord(Msg<>''));
end;

Var
  S : TStream;
  M : TMemoryStream;
  D,P : TJSONData;
  
begin
  if (ParamCount<1) then
    Usage('Need path expression');
  if (ParamStr(1)='-h') or (ParamStr(1)='--help') then
    usage('');
  if (ParamCount>1) then
    S:=TFileStream.Create(ParamStr(2),fmOpenRead or fmShareDenyNone)
  else
    S:=TIOStream.Create(iosInput);
  D:=Nil;
  M:=Nil;
  P:=Nil;
  try
    M:=TMemoryStream.Create;
    M.CopyFrom(S,0);
    M.Position:=0;
    D:=GetJSON(M);
    if D<>Nil then
      P:=D.FindPath(ParamStr(1));
    if Not Assigned(P) then
      Writeln('null')
    else  
      if P.JSONType in [jtArray,jtObject] then
        Writeln(P.AsJSON)
      else
        Writeln(P.AsString);  
  Finally
    M.Free;
    S.Free;
    D.Free;
  end;
end.
