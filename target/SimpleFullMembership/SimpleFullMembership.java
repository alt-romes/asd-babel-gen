import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SimpleFullMembership extends GenericProtocol
{
  public final String PROTO_NAME = "SimpleFullMembership";
  public final short PROTO_ID = 100;
  private Unknown15 self;
  private Set<Set<Host>> membership;
  private Unknown16 subsetSize;
  private Unknown17 T;
  public SimpleFullMembership () throws HandlerRegistrationException
  {
    super(PROTO_NAME, PROTO_ID);
    subscribeNotification(ChannelClosed.NOTIFICATION_ID, this :: uponChannelClosed);
  }
  private void init (Unknown15 myself, Unknown16 ssSize, Unknown17 t, Set<Host> contact)
  {
    self = myself;
    membership = new HashSet<Set<Host>>();
    if (contact.equals(new HashSet<Host>()))
    {
      membership.add(contact);
    }
    subsetSize = ssSize;
    T = t;
  }
  private void uponSampleMessage (SampleMessage msg, Host s, short sourceProto)
  {
    for (Set<Host> p : msg.getSample()) {
                                          if (!membership.contains(p))
                                          {
                                            membership.add(p);
                                            triggerNotification(new NeighbourUp(p));
                                          }
                                        }
  }
  private void uponChannelClosed (ChannelClosed notification, short sourceProto)
  {
    if (membership.contains(notification.getP()))
    {
      membership.remove(notification.getP());
      triggerNotification(new NeighbourDown(notification.getP()));
    }
  }
}