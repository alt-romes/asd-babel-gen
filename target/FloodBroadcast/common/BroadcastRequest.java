public class BroadcastRequest extends ProtoRequest
{
  public final short REQUEST_ID = 101;
  private final Unknown21 mid;
  private final Unknown22 s;
  private final Unknown23 m;
  public BroadcastRequest (Unknown21 mid, Unknown22 s, Unknown23 m)
  {
    super(REQUEST_ID);
    this.mid = mid;
    this.s = s;
    this.m = m;
  }
  public Unknown21 getMid ()
  {
    return mid;
  }
  public Unknown22 getS ()
  {
    return s;
  }
  public Unknown23 getM ()
  {
    return m;
  }
}