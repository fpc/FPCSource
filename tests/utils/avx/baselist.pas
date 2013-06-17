{

  Copyright (C) <avx-testfile-generator> <Torsten Grundke>

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 2 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}

{$mode objfpc}

unit baselist;

interface

uses Contnrs, Classes;

type

	// basisliste
	TBaseList = class(TPersistent)
	private

	protected
		FList: TObjectlist;
	public
		constructor Create; virtual;
		destructor Destroy; override;
		function count: integer;
		procedure clear;
	end;

implementation

{ TBaseList }

uses SysUtils;

procedure TBaseList.clear;
begin
  FList.Clear;
end;

function TBaseList.count: integer;
begin
	 result := FList.Count;
end;

constructor TBaseList.Create;
begin
	 inherited;
	 FList := TObjectList.Create;
end;

destructor TBaseList.Destroy;
begin
	 FreeAndNil(FList);
	 inherited;
end;

end.
