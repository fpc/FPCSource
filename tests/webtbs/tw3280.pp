{ Source provided for Free Pascal Bug Report 3280 }
{ Submitted by "Alan Mead" on  2004-08-29 }
{ e-mail: cubrewer@yahoo.com }
program example;

var
  err : boolean;

type
  MatrixType = record
    Mat: array of array of real;
    Rows: Integer;
    Cols: Integer;
    Name: String;
    Transpose: Boolean;
    Diagonal: Boolean;
  end;



  procedure MatrixNew(var Mat: MatrixType; Rows, Cols: Integer; Name: String; Transpose: Boolean; Diagonal: Boolean);
    begin
      writeln('Found ',Rows,' rows and ',Cols,' columns.');
      {SetLength(Mat.Mat,0,0);}
      SetLength(Mat.Mat,Rows,Cols);
      Mat.Rows := Rows;
      Mat.Cols := Cols;
      Mat.Name := Name;
      Mat.Transpose := Transpose;
      Mat.Diagonal := Diagonal;
      writeln('Created matrix with ',Rows,' rows and ',Cols,' columns.'); { debug }
    end;


  procedure ShowResults(RetainedDimensions: Integer);
    var
      i:Integer;
      xv: MatrixType;
    begin

      for i := 0 to 2 do
        begin
          writeln('Retained dimensions = ',RetainedDimensions); { debug }
          MatrixNew(xv,RetainedDimensions,1,'Term vector',FALSE,FALSE);
          writeln('(',xv.Rows,',',xv.Cols,')');
          if (xv.Rows<>3) or (xv.Cols<>1) then
            err:=true;
        end;
    end;

begin
  ShowResults(3);
  if err then
    halt(1);
end.
