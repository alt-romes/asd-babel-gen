import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SimpleFullMembership extends GenericProtocol
{
  public final String PROTO_NAME = "SimpleFullMembership";
  public final short PROTO_ID = 100;
  private Unknown15 self;
  private Set<Host> membership;
  private Unknown16 subsetSize;
  private Unknown17 tau;
  public SimpleFullMembership () throws HandlerRegistrationException
  {
    super(PROTO_NAME, PROTO_ID);
  }
  private void init (Unknown15 myself, Unknown16 ssSize, Unknown17 t, Host contact)
  {
    self = myself;
    membership = new HashSet<Host>();
    if (contact != null)
    {
      membership.add(contact);
    }
    subsetSize = ssSize;
    tau = t;
  }
  private void uponSampleMessage (SampleMessage msg, Host s, short sourceProto)
  {
    for (Host p : msg.getSample()) {
                                     if (!membership.contains(p))
                                     {
                                       membership.add(p);
                                       triggerNotification(new NeighbourUp(new HashSet<Host>(Arrays.asList(p))));
                                     }
                                   }
  }
}