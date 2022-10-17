import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class BebDeliver extends ProtoNotification
{
  public static final short NOTIFICATION_ID = 202;
  private final Host p;
  private final int m;
  public BebDeliver (Host p, int m)
  {
    super(NOTIFICATION_ID);
    this.p = p;
    this.m = m;
  }
  public Host getP ()
  {
    return p;
  }
  public int getM ()
  {
    return m;
  }
}