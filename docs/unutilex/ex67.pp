Program Example67;

uses UnixUtil;

{ Program to demonstrate the FSplit function. }

var
  Path,Name,Ext : string;

begin
  FSplit(ParamStr(1),Path,Name,Ext);
  WriteLn('Split ',ParamStr(1),' in:');
  WriteLn('Path     : ',Path);
  WriteLn('Name     : ',Name);
  WriteLn('Extension: ',Ext);
end.
