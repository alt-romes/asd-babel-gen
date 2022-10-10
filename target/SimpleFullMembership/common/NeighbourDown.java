public class NeighbourDown extends ProtoNotification
{
  public final short NOTIFICATION_ID = 202;
  private Unknown7 p;
  public NeighbourDown (Unknown7 p)
  {
    super(NOTIFICATION_ID);
    this.p = p;
  }
  public Unknown7 getP ()
  {
    return p;
  }
}