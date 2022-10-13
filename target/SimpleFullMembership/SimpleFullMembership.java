import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SimpleFullMembership extends GenericProtocol
{
  public final String PROTO_NAME = "SimpleFullMembership";
  public final short PROTO_ID = 100;
  private Host self;
  private Set<Host> membership;
  private Unknown17 subsetSize;
  private int tau;
  public SimpleFullMembership () throws HandlerRegistrationException
  {
    super(PROTO_NAME, PROTO_ID);
  }
  private void init (Host myself, Unknown17 ssSize, int t, Host contact)
  {
    self = myself;
    membership = new HashSet<Host>();
    if (contact != null)
    {
      membership.add(contact);
    }
    subsetSize = ssSize;
    tau = t;
    setupPeriodicTimer(new SampleTimer(), tau, tau);
  }
  private void uponSampleTimer (SampleTimer timer, short timerId)
  {
    if (membership.size() >= 1)
    {
      Host target = random(membership);
      Set<Host> sample = new HashSet<Host>(Arrays.asList(self));
      sample = random2(subsetSize, membership.remove(target));
      sendMsg(new SampleMessage(sample), target);
    }
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
