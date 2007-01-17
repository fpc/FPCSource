program fpu;

{$mode delphi}

uses SysUtils,Math;

var
  f1,f2 : single;
  caught: boolean;

{$include tmask.inc}
