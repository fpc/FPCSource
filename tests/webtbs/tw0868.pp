{$mode objfpc}
{$H+}
type
  TTreeData = record
    Key: String;
    Data: Integer;
  end;

  TNode = class
    data: TTreeData;
  end;

  TStrIntDic = class
    FNode: TNode;
    destructor Destroy; override;
    procedure Add(const Key: String; Data: Integer);
  end;

destructor TStrIntDic.Destroy;
begin
  FNode.Free;
  inherited Destroy;
end;

procedure TStrIntDic.Add(const Key: String; Data: Integer);
var
  T: TTreeData;
begin
  T.Key:=Key;
  T.Data:=Data;
  FNode:=TNode.Create;
  FNode.data:=T;
end;

procedure Test;
var
  SD: TStrIntDic;
begin
  SD:=TStrIntDic.Create;
  try
    SD.Add('asdf', 2);
  finally
    SD.Free;
  end;
end;

begin
  Test;
  write('Test for bug 868 completed.');
  {readln;}
end.
