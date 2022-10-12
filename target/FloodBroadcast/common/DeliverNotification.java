import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class DeliverNotification extends ProtoNotification
{
  public final short NOTIFICATION_ID = 202;
  private final UUID mid;
  private final Unknown38 s;
  private final Unknown39 m;
  public DeliverNotification (UUID mid, Unknown38 s, Unknown39 m)
  {
    super(NOTIFICATION_ID);
    this.mid = mid;
    this.s = s;
    this.m = m;
  }
  public UUID getMid ()
  {
    return mid;
  }
  public Unknown38 getS ()
  {
    return s;
  }
  public Unknown39 getM ()
  {
    return m;
  }
}