import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class BebBroadcast extends ProtoRequest
{
  public static final short REQUEST_ID = 201;
  private final int m;
  public BebBroadcast (int m)
  {
    super(REQUEST_ID);
    this.m = m;
  }
  public int getM ()
  {
    return m;
  }
}