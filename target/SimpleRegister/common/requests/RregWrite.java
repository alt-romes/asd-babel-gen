import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class RregWrite extends ProtoRequest
{
  public static final short REQUEST_ID = 102;
  private final int v;
  public RregWrite (int v)
  {
    super(REQUEST_ID);
    this.v = v;
  }
  public int getV ()
  {
    return v;
  }
}