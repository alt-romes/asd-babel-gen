public class NeighbourDown extends ProtoNotification
{
  public final short NOTIFICATION_ID = 202;
  private final Set<Host> downNeighbours;
  public NeighbourDown (Set<Host> downNeighbours)
  {
    super(NOTIFICATION_ID);
    this.downNeighbours = downNeighbours;
  }
  public Set<Host> getDownNeighbours ()
  {
    return downNeighbours;
  }
}