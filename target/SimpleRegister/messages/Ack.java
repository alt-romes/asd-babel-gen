import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class Ack extends ProtoMessage
{
  public static final short MSG_ID = 106;
  private final int i;
  public Ack (int i)
  {
    super(MSG_ID);
    this.i = i;
  }
  public int getI ()
  {
    return i;
  }
  public String toString ()
  {
    return "Ack{}";
  }
  public static ISerializer<Ack> serializer = new ISerializer<Ack>()
                                              {
                                                public void serialize (Ack msg, ByteBuf out) throws IOException
                                                {
                                                  out.writeInt(msg.getI());
                                                }
                                                public Ack deserialize (ByteBuf in) throws IOException
                                                {
                                                  int i = in.readInt();
                                                  return new Ack(i);
                                                }
                                              };
}