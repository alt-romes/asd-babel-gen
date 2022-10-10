public class DeliverNotification extends ProtoNotification
{
  public final short NOTIFICATION_ID = 102;
  private Unknown3 mid;
  private Unknown4 s;
  private Unknown5 m;
  public DeliverNotification (Unknown3 mid, Unknown4 s, Unknown5 m)
  {
    super(NOTIFICATION_ID);
    this.mid = mid;
    this.s = s;
    this.m = m;
  }
  public Unknown3 getMid ()
  {
    return mid;
  }
  public Unknown4 getS ()
  {
    return s;
  }
  public Unknown5 getM ()
  {
    return m;
  }
}