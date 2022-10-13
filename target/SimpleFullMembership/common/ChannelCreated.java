import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class ChannelCreated extends ProtoNotification
{
  public final short NOTIFICATION_ID = 101;
  private final Unknown54 channelId;
  public ChannelCreated (Unknown54 channelId)
  {
    super(NOTIFICATION_ID);
    this.channelId = channelId;
  }
  public Unknown54 getChannelId ()
  {
    return channelId;
  }
}