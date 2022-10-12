import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class NeighbourDown extends ProtoNotification
{
  public final short NOTIFICATION_ID = 102;
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