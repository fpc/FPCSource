{
    This file is part of the Free Component Library

    Unit with the interface definitions, common between client and server

    Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit myapi;

{$mode ObjFPC}{$H+}

interface

uses sysutils; // for TStringArray

Type
  { enable RTTI for methods! }
  {$M+}
  IMyInterface = interface ['{E4C73198-0831-47B9-944C-E2D7EFAE1C6A}']
   procedure SayHello;
   function Echo(args : Array of string) : String;
   function DoSum(a,b : Integer) : integer;
   function Split(aLine,aSep : string) : TStringArray;
   function DoVarTest(var aArg: String): Boolean;
  end;

  IMyOtherInterface = interface ['{4D52BEE3-F709-44AC-BD31-870CBFF44632}']
    Function SayHello : string;
    function Echo(args : TStringArray) : String;
  end;

implementation

end.

