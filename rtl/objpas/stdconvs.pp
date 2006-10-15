{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2004 by Marco van de Voort
   member of the Free Pascal development team.

   An implementation for unit stdconv, 

   Based on list of function of delphibasics.co.uk and #7482.

   Quantities are mostly taken from my HP48g/gx or the unix units program

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

unit stdconvs;

interface

{$mode objfpc}
{$H+}

function CelsiusToFahrenheit(const AValue: Double): Double;
function FahrenheitToCelsius(const AValue: Double): Double;
function CelsiusToKelvin    (const AValue: Double): Double;
function KelvinToCelsius    (const AValue: Double): Double;

implementation

function FahrenheitToCelsius(const AValue: Double): Double;
begin
  result:= 32.0 + ((9.0 * AValue)/ 5.0);
end;

function CelsiusToFahrenheit(const AValue: Double): Double;
begin
  result:= (5.0/9.0)  * (Avalue - 32.0);
end;

function CelsiusToKelvin    (const AValue: Double): Double;
begin
  result:=AValue+273.15;
end;

function KelvinToCelsius    (const AValue: Double): Double;
begin
  result:=AValue-273.15;
end;

end.
