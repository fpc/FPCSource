
{$mode objfpc}{$H+}

uses
  Classes, SysUtils
  { add your units here };

type
  TSingle3DVector = array[0..2] of single;
  TSingle4DVector = array[0..3] of single;

function VectorLength(const aV3D : TSingle3DVector) : single; overload;
begin
end;

function VectorLength(const aV4D : TSingle4DVector) : single; overload;
begin
end;

function VectorLength(const aVxD : array of single) : single; overload;
begin
end;


begin
end.
