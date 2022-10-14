import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SimpleFullMembership extends GenericProtocol
{
  public static final String PROTO_NAME = "SimpleFullMembership";
  public static final short PROTO_ID = 100;
  private Host self;
  private Set<Host> membership;
  private Set<Host> pending;
  private Unknown31 subsetSize;
  private int tau;
  public SimpleFullMembership () throws HandlerRegistrationException
  {
    super(PROTO_NAME, PROTO_ID);
    registerTimerHandler(SampleTimer.TIMER_ID, this :: uponSampleTimer);
  }
  private void init (Host myself, Unknown31 ssSize, int t, Host contact)
  {
    self = myself;
    membership = new HashSet<Host>();
    pending = new HashSet<Host>();
    subsetSize = ssSize;
    tau = t;
    triggerNotification(new ChannelCreated(0));
    if (contact != null)
    {
      pending.add(contact);
    }
    setupPeriodicTimer(new SampleTimer(), tau, tau);
  }
  private void uponSampleMessage (SampleMessage msg, Host s, short sourceProto)
  {
    for (Host p : msg.getSample()) {
                                     if (!p.equals(self) & !membership.contains(p) & !pending.contains(p))
                                     {
                                       pending.add(p);
                                     }
                                   }
  }
  private void uponSampleTimer (SampleTimer timer, short timerId)
  {
    if (membership.size() >= 1)
    {
      Host target = getRandom(membership);
      Set<Host> sample = getRandomSubsetExcluding(subsetSize, membership, target);
      sample.add(self);
      sendMsg(new SampleMessage(sample), target);
    }
  }
  private void getRandom (Set<Host> ms)
  {
  }
  private void getRandomSubsetExcluding (Unknown31 ms, Set<Host> ss, Host t)
  {
  }
}