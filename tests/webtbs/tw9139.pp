{$mode objfpc}{$H+}
{.$define second_test}

type
  TTestClass = class of TTestBase;

  TTestBase = class(TObject)
  public
    class function ClassMetadataStr: string;
    class function InternalMetadataStr: string; virtual;
  end;

  TTestImpl = class(TTestBase)
  public
    class function InternalMetadataStr: string; override;
  end;

class function TTestBase.ClassMetadataStr: string;
var
  VMetadataMethod, VParentMetadataMethod: function: string of object;
{$ifdef second_test}
  VClass: TTestClass;
{$endif}
begin
  if Self <> TTestBase then
  begin
    writeln('pass 1');
    VMetadataMethod := @InternalMetadataStr;
    writeln('pass 2');
{$ifndef second_test}
    VParentMetadataMethod := @TTestClass(ClassParent).InternalMetadataStr;
{$else}
    VClass := TTestClass(ClassParent);
    writeln('pass 2.1');
    VParentMetadataMethod := @VClass.InternalMetadataStr;
{$endif}
    writeln('pass 3');
    if TMethod(VMetadataMethod).Code <> TMethod(VParentMetadataMethod).Code then
      begin
        Result := VParentMetadataMethod();
        writeln('result: ',result);
        if Result<>'parent meth' then
          halt(1);
      end
    else
      halt(2);
    writeln('pass 4');
  end else
    Result := 'base result';
end;

class function TTestBase.InternalMetadataStr: string;
begin
  Result := 'parent meth';
end;

class function TTestImpl.InternalMetadataStr: string;
begin
  Result := 'some stuff';
end;

var
  VTestClass: TTestClass;
begin
  VTestClass := TTestBase;
  writeln('TTestBase result:');
  writeln(VTestClass.ClassMetadataStr);
  writeln;
  VTestClass := TTestImpl;
  writeln('TTestImpl result:');
  writeln(VTestClass.ClassMetadataStr);
end.
