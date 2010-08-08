{ %target=win32,win64 }
unit tw14553;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ComObj, Variants;

Procedure MI_Connect(H1,H2,H3:String);
Procedure MI_Disconnect;

Var MI1Open,MI2Open,MI3Open:Boolean;
    MI1,MI2,MI3:Variant;

implementation

Procedure MI_Connect(H1,H2,H3:String);
Begin
 MI1:=CreateOleObject('MapInfo.Application');
 MI2:=CreateOleObject('MapInfo.Application');
 MI3:=CreateOleObject('MapInfo.Application');

 MI1.&do('Set Application Window '+H1);
 MI2.&do('Set Application Window '+H2);
 MI3.&do('Set Application Window '+H3);

 MI1.&do('Set Next Document Parent '+H1+' Style 1');
 MI2.&do('Set Next Document Parent '+H2+' Style 1');
 MI3.&do('Set Next Document Parent '+H3+' Style 1');

 MI1Open:=False;
 MI2Open:=False;
 MI3Open:=False;
End;

Procedure MI_Disconnect;
Begin
 MI1:=UnAssigned;
 MI2:=UnAssigned;
 MI3:=UnAssigned;
End;

end.
