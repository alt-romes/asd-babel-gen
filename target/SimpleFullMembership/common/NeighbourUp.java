import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class NeighbourUp extends ProtoNotification
{
  public final short NOTIFICATION_ID = 201;
  private final Set<Host> upNeighbours;
  public NeighbourUp (Set<Host> upNeighbours)
  {
    super(NOTIFICATION_ID);
    this.upNeighbours = upNeighbours;
  }
  public Set<Host> getUpNeighbours ()
  {
    return upNeighbours;
  }
}