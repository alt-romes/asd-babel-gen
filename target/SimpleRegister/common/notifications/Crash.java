import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class Crash extends ProtoNotification
{
  public static final short NOTIFICATION_ID = 105;
  private final Host p;
  public Crash (Host p)
  {
    super(NOTIFICATION_ID);
    this.p = p;
  }
  public Host getP ()
  {
    return p;
  }
}