import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SampleTimer extends ProtoTimer
{
  public static final short TIMER_ID = 104;
  public SampleTimer ()
  {
    super(TIMER_ID);
  }
  public ProtoTimer clone ()
  {
    return this;
  }
}