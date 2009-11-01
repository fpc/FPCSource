program tclassinfo1;

{$apptype console}
{$mode objfpc}{$H+}
uses
  Classes;
begin
  WriteLn(TObject.ClassInfo = TypeInfo(TObject));
  WriteLn(TPersistent.ClassInfo = TypeInfo(TPersistent));
end.

