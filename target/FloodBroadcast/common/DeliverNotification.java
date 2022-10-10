public class DeliverNotification extends ProtoNotification
{
  public final short NOTIFICATION_ID = 102;
  private final Unknown21 mid;
  private final Unknown22 s;
  private final Unknown23 m;
  public DeliverNotification (Unknown21 mid, Unknown22 s, Unknown23 m)
  {
    super(NOTIFICATION_ID);
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