import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class RregReadReturn extends ProtoNotification
{
  public static final short NOTIFICATION_ID = 103;
  private final int value;
  public RregReadReturn (int value)
  {
    super(NOTIFICATION_ID);
    this.value = value;
  }
  public int getValue ()
  {
    return value;
  }
}