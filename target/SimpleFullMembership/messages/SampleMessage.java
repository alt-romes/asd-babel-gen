import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SampleMessage extends ProtoMessage
{
  public static final short MSG_ID = 104;
  private final Set<Host> sample;
  public SampleMessage (Set<Host> sample)
  {
    super(MSG_ID);
    this.sample = sample;
  }
  public Set<Host> getSample ()
  {
    return sample;
  }
  public String toString ()
  {
    return "SampleMessage{}";
  }
  public static ISerializer<SampleMessage> serializer = new ISerializer<SampleMessage>()
                                                        {
                                                          public void serialize (SampleMessage msg, ByteBuf out) throws IOException
                                                          {
                                                            out.writeInt(msg.getSample().size());
                                                            for (Host x : msg.getSample()) {
                                                                                             Host.serializer.serialize(x, out);
                                                                                           }
                                                          }
                                                          public SampleMessage deserialize (ByteBuf in) throws IOException
                                                          {
                                                          }
                                                        };
}