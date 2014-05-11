program sqlparser;

{**
 * example usage of fpc TSqlParser
 *
 * @author : Fajar Khairil
*}


{$mode objfpc}{$H+}

uses
  Classes, fpsqlparser, fpsqltree;
  
var
  parser : TSQLParser;
  aInput: TStringStream;
  source: TSQLSelectStatement;
  QExpresion: TSQLBinaryExpression;
  stmtSelect: TSQLSelectStatement;
  lField: TSQLTableFieldDef;
  lTable: TSQLSimpleTableReference;
  lWhere: TSQLBinaryExpression;
  leftValue: TSQLIdentifierName;
  rightValue: TSQLIntegerLiteral;
  
begin
  aInput := TStringStream.Create('SELECT name,age FROM PERSON WHERE id = 5');
  parser := TSQLParser.Create(aInput);
  try
    source := TSQLSelectStatement(parser.Parse);

    //TABLES AND FIELDS
    Writeln('Tables : ',source.Tables[0].GetAsSQL([]));
    Writeln('Fields : ',source.Fields[0].GetAsSQL([])+','+source.Fields[1].GetAsSQL([]) );

    //WHERE CLAUSE
    QExpresion := TSQLBinaryExpression(source.Where);
    WriteLn('Where Expr Operation : ', QExpresion.Operation );
    WriteLn('Where Expr Left : ', QExpresion.Left.GetAsSQL([]) );
    WriteLn('Where Expr Right : ', QExpresion.Right.GetAsSQL([]) );
    WriteLn('-----------PARSING DONE-----------');
    WriteLn();


    WriteLn('----------Reversing From Tree To Sql Statement ----------');
    stmtSelect := TSQLSelectStatement.Create(nil);

    //build Table
    lTable := TSQLSimpleTableReference.Create(stmtSelect);
    stmtSelect.Tables.add(lTable);
    lTable.ObjectName := TSQLIdentifierName.Create(lTable);
    lTable.ObjectName.Name:= 'persons';
    lTable.AliasName := TSQLIdentifierName.Create(lTable);
    lTable.AliasName.Name:= 'p';

    //Build Fields
    lField := TSQLTableFieldDef.Create(stmtSelect);
    lField.FieldName := TSQLIdentifierName.Create(lField);
    lField.FieldName.Name:= lTable.AliasName.Name+'.name';
    stmtSelect.Fields.Add(lField);

    lField := TSQLTableFieldDef.Create(stmtSelect);
    lField.FieldName := TSQLIdentifierName.Create(lField);
    lField.FieldName.Name:= lTable.AliasName.Name+'.age';
    stmtSelect.Fields.Add(lField);
    //end Build Fields

    //Where Expression
    lWhere := TSQLBinaryExpression.Create(stmtSelect);
    stmtSelect.Where := lWhere;
    lWhere.Operation:= boEQ;

    //left side of BinaryExpression
    lWhere.Left := TSQLIdentifierExpression.Create(lWhere);
    leftValue := TSQLIdentifierName.Create(lWhere.left);
    TSQLIdentifierExpression(lWhere.Left).Identifier :=  leftValue;
    leftValue.Name:= lTable.AliasName.Name+'.id';

    //right side of BinaryExpression
    lWhere.right := TSQLLiteralExpression.Create(lWhere);
    rightValue := TSQLIntegerLiteral.Create(lWhere.right);
    TSQLLiteralExpression(lWhere.right).Literal := rightValue;
    rightValue.Value:= 5;
    //end Where Expression

    //kick!!
    WriteLn( stmtSelect.GetAsSQL([sfoDoubleQuotes,sfoBackQuoteIdentifier]) );

    WriteLn('-----------Reverse DONE-----------');
    WriteLn();
  finally
    stmtSelect.Free;
    source.free;
    aInput.Free;
    parser.Free;
  end;
end.

