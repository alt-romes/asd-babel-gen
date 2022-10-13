import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class BroadcastRequest extends ProtoRequest
{
  public final short REQUEST_ID = 201;
  private final UUID mid;
  private final Host s;
  private final byte[] m;
  public BroadcastRequest (UUID mid, Host s, byte[] m)
  {
    super(REQUEST_ID);
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