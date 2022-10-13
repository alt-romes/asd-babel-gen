import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class ChannelCreated extends ProtoNotification
{
  public final short NOTIFICATION_ID = 101;
  private final Unknown47 channelId;
  public ChannelCreated (Unknown47 channelId)
  {
    super(NOTIFICATION_ID);
    this.channelId = channelId;
  }
  public Unknown47 getChannelId ()
  {
    return channelId;
  }
}