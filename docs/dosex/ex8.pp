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
  if (Attr and archive)<>0 then WriteLn('- Archive');
  if (Attr and directory)<>0 then WriteLn('- Directory');
  if (Attr and readonly)<>0 then WriteLn('- Read-Only');
  if (Attr and sysfile)<>0 then WriteLn('- System');
  if (Attr and hidden)<>0 then WriteLn('- Hidden');
end.
