{
    Writes comperuser.tlb, the empty type library embedded in comperuserexe.
}
program mktlb;

{$mode objfpc}{$H+}

uses
  SysUtils, Windows, ActiveX, ComObj, comperuserintf;

var
  lLib: ICreateTypeLib2;
  lFileName, lName: WideString;

begin
  lFileName:=ExpandFileName('comperuser.tlb');
  lName:=PerUserTypeLibName;
  OleCheck(CreateTypeLib2(SYS_WIN32,PWideChar(lFileName),lLib));
  OleCheck(lLib.SetName(PWideChar(lName)));
  OleCheck(lLib.SetGuid(LIBID_PerUserTest));
  OleCheck(lLib.SetVersion(1,0));
  OleCheck(lLib.SetLcid(0));
  OleCheck(lLib.SaveAllChanges);
  Writeln('Wrote ',lFileName);
end.
