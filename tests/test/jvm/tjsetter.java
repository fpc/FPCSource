import org.freepascal.test.jsetter.*;

public class tjsetter {

public static void main(String[] args)
{
  tjsetterchild c;

  c = new tjsetterchild();
  c.SetVal(2);
  if (c.get() != 2)
    java.lang.Runtime.getRuntime().exit(1);
  c = new tjsetterchild2();
  c.SetVal(2);
  if (c.get() != 1)
    java.lang.Runtime.getRuntime().exit(2);
}

}
