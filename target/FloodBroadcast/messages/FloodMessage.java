import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class FloodMessage extends ProtoMessage
{
    public static final short MSG_ID = 203;
    private final UUID mid;
    private final Host s;
    private final byte[] m;
    public FloodMessage (UUID mid, Host s, byte[] m)
    {
        super(MSG_ID);
        this.mid = mid;
        this.s = s;
        this.m = m;
    }
    public UUID getMid ()
    {
        return mid;
    }
    public Host getS ()
    {
        return s;
    }
    public byte[] getM ()
    {
        return m;
    }
    public String toString ()
    {
        return "FloodMessage{}";
    }
    public static ISerializer<FloodMessage> serializer = new ISerializer<FloodMessage>()
    {
        public void serialize (FloodMessage msg, ByteBuf out) throws IOException
        {
        }
        public FloodMessage deserialize (ByteBuf in) throws IOException
        {
        }
    };
}
