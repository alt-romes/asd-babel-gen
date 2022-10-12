import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class BroadcastRequest extends ProtoRequest
{
  public final short REQUEST_ID = 201;
  private final UUID mid;
  private final Unknown38 s;
  private final Unknown39 m;
  public BroadcastRequest (UUID mid, Unknown38 s, Unknown39 m)
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
  public Unknown38 getS ()
  {
    return s;
  }
  public Unknown39 getM ()
  {
    return m;
  }
}