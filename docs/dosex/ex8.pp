Program Example8;
uses Dos;

{ Program to demonstrate the GetFAttr function. }

var
  Attr : Word;
  f    : File;
begin
  Assign(f,ParamStr(1));
  GetFAttr(f,Attr);
  WriteLn('File ',ParamStr(1),' has attribute ',Attr);
  if (Attr and $20)<>0 then WriteLn('- Archive');
  if (Attr and $10)<>0 then WriteLn('- Directory');
  if (Attr and $4)<>0 then WriteLn('- Read-Only');
  if (Attr and $2)<>0 then WriteLn('- System');
  if (Attr and $1)<>0 then WriteLn('- Hidden');
end.
