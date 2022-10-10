public class BroadcastRequest extends ProtoRequest
{
  public final short REQUEST_ID = 101;
  private Unknown0 mid;
  private Unknown1 s;
  private Unknown2 m;
  public BroadcastRequest (Unknown0 mid, Unknown1 s, Unknown2 m)
  {
    super(REQUEST_ID);
    this.mid = mid;
    this.s = s;
    this.m = m;
  }
  public Unknown0 getMid ()
  {
    return mid;
  }
  public Unknown1 getS ()
  {
    return s;
  }
  public Unknown2 getM ()
  {
    return m;
  }
}