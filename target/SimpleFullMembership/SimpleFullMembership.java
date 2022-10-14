import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SimpleFullMembership extends GenericProtocol
{
  public static final String PROTO_NAME = "SimpleFullMembership";
  public static final short PROTO_ID = 100;
  private Unknown27 self;
  private Set<Unknown27> membership;
  private Set<Unknown27> pending;
  private Unknown32 subsetSize;
  private int tau;
  public SimpleFullMembership () throws HandlerRegistrationException
  {
    super(PROTO_NAME, PROTO_ID);
    registerTimerHandler(SampleTimer.TIMER_ID, this :: uponSampleTimer);
  }
  private void init (Unknown27 myself, Unknown32 ssSize, int t, Unknown27 contact)
  {
    self = myself;
    membership = new HashSet<Unknown27>();
    pending = new HashSet<Unknown27>();
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
    for (Unknown27 p : msg.getSample()) {
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
      Set<Unknown27> sample = getRandomSubsetExcluding(subsetSize, membership, target);
      sample.add(self);
      sendMsg(new SampleMessage(sample), target);
    }
  }
  private void getRandom (Set<Unknown27> ms)
  {
  }
  private void getRandomSubsetExcluding (Unknown32 ms, Set<Unknown27> ss, Host t)
  {
  }
}