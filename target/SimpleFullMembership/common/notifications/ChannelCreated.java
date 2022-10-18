import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class ChannelCreated extends ProtoNotification
{
  public static final short NOTIFICATION_ID = 101;
  private final int channelId;
  public ChannelCreated (int channelId)
  {
    super(NOTIFICATION_ID);
    this.channelId = channelId;
  }
  public int getChannelId ()
  {
    return channelId;
  }
}