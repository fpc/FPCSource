{ Source provided for Free Pascal Bug Report 3216 }
{ Submitted by "Mark Honman" on  2004-07-17 }
{ e-mail: wania_mark@yahoo.co.uk }
program OLEVariantPropertyBug;

{$mode objfpc}{$H+}

uses
  Classes;

type TShowProblem = class
    function GetValue(Index : integer) : OleVariant; stdcall;
    property IWontCompile[Index : integer] : OleVariant read GetValue;
  end;

{ Note :
  if any of the following is changed, the problem disappears
    * make it a scalar (non-indexed) property
    * remove the stdcall directive
    * change the type of the property to something else
  although quite obscure, it would be very useful to have this fixed as it will
  greatly assist in interfacing to external OLE objects - in this case the ADO
  library for access to MS SQL Server.

  Identified with FPC 1.9.3 on Windows ME.
}

function TShowProblem.GetValue(Index : integer) : OleVariant; stdcall;
begin
end;

begin

end.
