{ %FAIL }

unit tw34691;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TObjA = class
    Icon: String;
  end;

  {$M+}
  TObjB = class
    FObjA: TObjA;

  published
    property Icon: String read FObjA.Icon;
  end;

implementation

end.

