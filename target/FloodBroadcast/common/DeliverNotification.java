import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class DeliverNotification extends ProtoNotification
{
  public final short NOTIFICATION_ID = 202;
  private final UUID mid;
  private final Host s;
  private final byte[] m;
  public DeliverNotification (UUID mid, Host s, byte[] m)
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
  public Host getS ()
  {
    return s;
  }
  public byte[] getM ()
  {
    return m;
  }
}