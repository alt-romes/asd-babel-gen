import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class BroadcastMessage extends ProtoMessage
{
  public static final short MSG_ID = 203;
  private final int m;
  public BroadcastMessage (int m)
  {
    super(MSG_ID);
    this.m = m;
  }
  public int getM ()
  {
    return m;
  }
  public String toString ()
  {
    return "BroadcastMessage{}";
  }
  public static ISerializer<BroadcastMessage> serializer = new ISerializer<BroadcastMessage>()
                                                           {
                                                             public void serialize (BroadcastMessage msg, ByteBuf out) throws IOException
                                                             {
                                                               out.writeInt(msg.getM());
                                                             }
                                                             public BroadcastMessage deserialize (ByteBuf in) throws IOException
                                                             {
                                                               int m = in.readInt();
                                                               return new BroadcastMessage(m);
                                                             }
                                                           };
}