import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class BestEffortBroadcast extends GenericProtocol
{
  public static final String PROTO_NAME = "BestEffortBroadcast";
  public static final short PROTO_ID = 200;
  private Set<Host> processes;
  public BestEffortBroadcast () throws HandlerRegistrationException
  {
    super(PROTO_NAME, PROTO_ID);
    registerRequestHandler(BebBroadcast.REQUEST_ID, this :: uponBebBroadcast);
  }
  private void init (Set<Host> ps)
  {
    processes = ps;
  }
  private void uponBebBroadcast (BebBroadcast request, short sourceProto)
  {
    for (Host q : processes) {
                               sendMsg(new BroadcastMessage(request.getM()), q);
                             }
  }
  private void uponBroadcastMessage (BroadcastMessage msg, Host q, short sourceProto)
  {
    triggerNotification(new BebDeliver(q, msg.getM()));
  }
}