import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SampleMessage extends ProtoMessage
{
    public static final short MSG_ID = 104;
    private final Set<Unknown27> sample;
    public SampleMessage (Set<Unknown27> sample)
    {
        super(MSG_ID);
        this.sample = sample;
    }
    public Set<Unknown27> getSample ()
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
        }
        public SampleMessage deserialize (ByteBuf in) throws IOException
        {
        }
    };
}
