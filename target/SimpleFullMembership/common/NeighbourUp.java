public class NeighbourUp extends ProtoNotification
{
  public final short NOTIFICATION_ID = 201;
  private Unknown6 p;
  public NeighbourUp (Unknown6 p)
  {
    super(NOTIFICATION_ID);
    this.p = p;
  }
  public Unknown6 getP ()
  {
    return p;
  }
}