{*****************************************************************************
                           Methods of the vector object
*****************************************************************************}
constructor objectname.init_zero;

begin
    data[0]:=0;
    data[1]:=0;
{$if vecsize>=3}
    data[2]:=0;
{$endif}
{$if vecsize>=4}
    data[3]:=0;
{$endif}
end;

constructor objectname.init_one;

begin
    data[0]:=1;
    data[1]:=1;
{$if vecsize>=3}
    data[2]:=1;
{$endif}
{$if vecsize>=4}
    data[3]:=1;
{$endif}
end;

constructor objectname.init(a,b
                           {$if vecsize>=3},c{$endif}
                           {$if vecsize>=4},d{$endif}:datatype);

begin
    data[0]:=a;
    data[1]:=b;
{$if vecsize>=3}
    data[2]:=c;
{$endif}
{$if vecsize>=4}
    data[3]:=d;
{$endif}
end;

function objectname.length:datatype;

begin
    length:=sqrt(data[0]*data[0]
                +data[1]*data[1]
                {$if vecsize>=3}+data[2]*data[2]{$endif}
                {$if vecsize>=4}+data[3]*data[3]{$endif});
end;

function objectname.squared_length:datatype;

begin
    squared_length:=data[0]*data[0]
                    +data[1]*data[1]
                    {$if vecsize>=3}+data[2]*data[2]{$endif}
                    {$if vecsize>=4}+data[3]*data[3]{$endif};
end;

{*****************************************************************************
                            Conversion from vector2
*****************************************************************************}


{$if (vecsize<>2) or (datatype<>single)}
operator := (const v:Tvector2_single) result:objectname;

begin
  result.data[0]:=v.data[0];
  result.data[1]:=v.data[1];
{$if vecsize>=3}
  result.data[2]:=0;
{$endif}
{$if vecsize>=4}
  result.data[3]:=0;
{$endif}
end;
{$endif}

{$if (vecsize<>2) or (datatype<>double)}
operator := (const v:Tvector2_double) result:objectname;

begin
    result.data[0]:=v.data[0];
    result.data[1]:=v.data[1];
{$if vecsize>=3}
    result.data[2]:=0;
{$endif}
{$if vecsize>=4}
    result.data[3]:=0;
{$endif}
end;
{$endif}

{$if (vecsize<>2) or (datatype<>extended)}
operator := (const v:Tvector2_extended) result:objectname;

begin
    result.data[0]:=v.data[0];
    result.data[1]:=v.data[1];
{$if vecsize>=3}
    result.data[2]:=0;
{$endif}
{$if vecsize>=4}
    result.data[3]:=0;
{$endif}
end;
{$endif}

{*****************************************************************************
                            Conversion from vector3
*****************************************************************************}


{$if (vecsize<>3) or (datatype<>single)}
operator := (const v:Tvector3_single) result:objectname;

begin
  result.data[0]:=v.data[0];
  result.data[1]:=v.data[1];
{$if vecsize>=3}
  result.data[2]:=v.data[2];
{$endif}
{$if vecsize>=4}
  result.data[3]:=0;
{$endif}
end;
{$endif}

{$if (vecsize<>3) or (datatype<>double)}
operator := (const v:Tvector3_double) result:objectname;

begin
    result.data[0]:=v.data[0];
    result.data[1]:=v.data[1];
{$if vecsize>=3}
    result.data[2]:=v.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=0;
{$endif}
end;
{$endif}

{$if (vecsize<>3) or (datatype<>extended)}
operator := (const v:Tvector3_extended) result:objectname;

begin
    result.data[0]:=v.data[0];
    result.data[1]:=v.data[1];
{$if vecsize>=3}
    result.data[2]:=v.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=0;
{$endif}
end;
{$endif}

{*****************************************************************************
                            Conversion from vector4
*****************************************************************************}


{$if (vecsize<>4) or (datatype<>single)}
operator := (const v:Tvector4_single) result:objectname;

begin
  result.data[0]:=v.data[0];
  result.data[1]:=v.data[1];
{$if vecsize>=3}
  result.data[2]:=v.data[2];
{$endif}
{$if vecsize>=4}
  result.data[3]:=v.data[3];
{$endif}
end;
{$endif}

{$if (vecsize<>4) or (datatype<>double)}
operator := (const v:Tvector4_double) result:objectname;

begin
    result.data[0]:=v.data[0];
    result.data[1]:=v.data[1];
{$if vecsize>=3}
    result.data[2]:=v.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=v.data[3];
{$endif}
end;
{$endif}

{$if (vecsize<>4) or (datatype<>extended)}
operator := (const v:Tvector4_extended) result:objectname;

begin
    result.data[0]:=v.data[0];
    result.data[1]:=v.data[1];
{$if vecsize>=3}
    result.data[2]:=v.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=v.data[3];
{$endif}
end;
{$endif}

{*****************************************************************************
                            Vector to vector operations
*****************************************************************************}

operator + (const x,y:objectname) result:objectname;

{Adds the elements of both vectors together.}

begin
    result.data[0]:=x.data[0]+y.data[0];
    result.data[1]:=x.data[1]+y.data[1];
{$if vecsize>=3}
    result.data[2]:=x.data[2]+y.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=x.data[3]+y.data[3];
{$endif}
end;

operator - (const x:objectname) result:objectname;

{Negates the elements of a vector.}

begin
    result.data[0]:=-x.data[0];
    result.data[1]:=-x.data[1];
{$if vecsize>=3}
    result.data[2]:=-x.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=-x.data[3];
{$endif}
end;

operator - (const x,y:objectname) result:objectname;

{Subtracts the elements of both vectors together.}

begin
    result.data[0]:=x.data[0]-y.data[0];
    result.data[1]:=x.data[1]-y.data[1];
{$if vecsize>=3}
    result.data[2]:=x.data[2]-y.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=x.data[3]-y.data[3];
{$endif}
end;

operator * (const x,y:objectname) result:objectname;

{Multiplies the elements of two vectors.}

begin
    result.data[0]:=x.data[0]*y.data[0];
    result.data[1]:=x.data[1]*y.data[1];
{$if vecsize>=3}
    result.data[2]:=x.data[2]*y.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=x.data[3]*y.data[3];
{$endif}
end;

operator / (const x,y:objectname) result:objectname;

{Divides the elements of two vectors.

 In most cases, you will want to avoid this and multiply by the inverse.
 In case you need to preserve accuracy, dividing might be better though.}

begin
    result.data[0]:=x.data[0]/y.data[0];
    result.data[1]:=x.data[1]/y.data[1];
{$if vecsize>=3}
    result.data[2]:=x.data[2]/y.data[2];
{$endif}
{$if vecsize>=4}
    result.data[3]:=x.data[3]/y.data[3];
{$endif}
end;

operator ** (const x,y:objectname) result:datatype;

{Calculates the inproduct of two vectors.}

begin
  result:=x.data[0]*y.data[0]
         +x.data[1]*y.data[1]
         {$if vecsize>=3}+x.data[2]*y.data[2]{$endif}
         {$if vecsize>=4}+x.data[3]*y.data[3]{$endif};
end;

{$if vecsize=3}
operator >< (const x,y:objectname) result:objectname;

{Calculates the exproduct of two vectors. The exproduct exists only for
 3-dimensional vectors}

begin
  result.data[0]:=x.data[1]*y.data[2]-x.data[2]*y.data[1];
  result.data[1]:=x.data[2]*y.data[0]-x.data[0]*y.data[2];
  result.data[2]:=x.data[0]*y.data[1]-x.data[1]*y.data[0];
end;
{$endif}

{*****************************************************************************
                            Vector/scalar operations
*****************************************************************************}

operator + (const x:objectname;y:datatype) result:objectname;

{Adds a scalar to all vector elements.}

begin
    result.data[0]:=x.data[0]+y;
    result.data[1]:=x.data[1]+y;
{$if vecsize>=3}
    result.data[2]:=x.data[2]+y;
{$endif}
{$if vecsize>=4}
    result.data[3]:=x.data[3]+y;
{$endif}
end;

operator - (const x:objectname;y:datatype) result:objectname;

{Subtracts a scalar to all vector elements.}

begin
    result.data[0]:=x.data[0]-y;
    result.data[1]:=x.data[1]-y;
{$if vecsize>=3}
    result.data[2]:=x.data[2]-y;
{$endif}
{$if vecsize>=4}
    result.data[3]:=x.data[3]-y;
{$endif}
end;

operator * (const x:objectname;y:datatype) result:objectname;

{Multiplies all vector elements by a scalar.}

begin
    result.data[0]:=x.data[0]*y;
    result.data[1]:=x.data[1]*y;
{$if vecsize>=3}
    result.data[2]:=x.data[2]*y;
{$endif}
{$if vecsize>=4}
    result.data[3]:=x.data[3]*y;
{$endif}
end;

operator / (const x:objectname;y:datatype) result:objectname;

{Divides all vector elements by a scalar.}

begin
    result.data[0]:=x.data[0]/y;
    result.data[1]:=x.data[1]/y;
{$if vecsize>=3}
    result.data[2]:=x.data[2]/y;
{$endif}
{$if vecsize>=4}
    result.data[3]:=x.data[3]/y;
{$endif}
end;
