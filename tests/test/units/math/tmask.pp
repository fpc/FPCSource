{ %OPT=-CE }
program fpu;

{$mode delphi}

uses SysUtils,Math;

var
  f1,f2 : double;
  caught: boolean;

{$include tmask.inc}

