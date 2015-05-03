{ Base Google REST classes

  Copyright (C) 2015 Michael Van Canneyt michael@freepascal.org

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit googlebase;

{$mode objfpc}{$H+}
{ $DEFINE DEBUGBASEOBJMEMLEAK}

interface

uses
  typinfo, Classes, SysUtils, restbase;

Type
  EGoogleAPI = Class(ERestAPI);

Type

  TGoogleBaseObject = CLass(TBaseObject);
  TGoogleBaseObjectClass = Class of TGoogleBaseObject;
  TGoogleObjectArray =  Array of TGoogleBaseObject;

  { TGoogleBaseObjectList }

  TGoogleBaseObjectList = Class(TBaseObjectList)
  private
    function GetO(Aindex : Integer): TGoogleBaseObject;
    procedure SetO(Aindex : Integer; AValue: TGoogleBaseObject);
  Protected
    Class Function ObjectClass : TBaseObjectClass; Override;
  Public
    Function AddGoogleObject(Const AKind : String) : TGoogleBaseObject; virtual;
    Property GoogleObjects [Aindex : Integer] : TGoogleBaseObject Read GetO Write SetO; default;
  end;

Function  GoogleFactory : TObjectFactory;

implementation

Function GoogleFactory : TObjectFactory;

begin
  Result:=RestFactory;
end;

{ TGoogleBaseObjectList }

function TGoogleBaseObjectList.GetO(Aindex: Integer): TGoogleBaseObject;
begin
  Result:=TGoogleBaseObject(Inherited GetO(AIndex))
end;

procedure TGoogleBaseObjectList.SetO(Aindex: Integer; AValue: TGoogleBaseObject
  );
begin
  Inherited SetO(AIndex,AValue);
end;


class function TGoogleBaseObjectList.ObjectClass: TBaseObjectClass;
begin
  Result:=TGoogleBaseObject;
end;

function TGoogleBaseObjectList.AddGoogleObject(const AKind: String
  ): TGoogleBaseObject;
begin
  Result:=AddObject(AKind) as TGoogleBaseObject;
end;


end.

