  type
     AbsRectType = record
          left : SWord;
          top : SWord;
          right : SWord;
          bottom : SWord;
       end;

     PointType = record
          x : SWord;
          y : SWord;
       end;

     RectangleType = record
          topLeft : PointType;
          extent : PointType;
       end;

     RectanglePtr = ^RectangleType;

  procedure RctSetRectangle(r:RectanglePtr; left:SWord; top:SWord; width:SWord; height:SWord);systrap sysTrapRctSetRectangle;
  procedure RctCopyRectangle(srcRect:RectanglePtr; dstRect:RectanglePtr);systrap sysTrapRctCopyRectangle;
  procedure RctInsetRectangle(r:RectanglePtr; insetAmt:SWord);systrap sysTrapRctInsetRectangle;
  procedure RctOffsetRectangle(r:RectanglePtr; deltaX:SWord; deltaY:SWord);systrap sysTrapRctOffsetRectangle;
  function RctPtInRectangle(x:SWord; y:SWord; r:RectanglePtr):Boolean;systrap sysTrapRctPtInRectangle;
  procedure RctGetIntersection(r1:RectanglePtr; r2:RectanglePtr; r3:RectanglePtr);systrap sysTrapRctGetIntersection;

  $Log$
  Revision 1.3  2002-09-07 16:01:26  peter
    * old logs removed and tabs fixed

}
