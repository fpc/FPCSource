{ %FAIL }

unit tw34691;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  {$M+}
  TObjA = class
  public
    Icon: String;
  end;

  TObjB = class
    FObjA: TObjA;

  published
    property Icon: String read FObjA.Icon;
  end;

implementation

end.

