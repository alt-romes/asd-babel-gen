import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class FloodBroadcast extends GenericProtocol
{
  public final String PROTO_NAME = "FloodBroadcast";
  public final short PROTO_ID = 200;
  private Host myself;
  private Set<Host> neighbours;
  private Set<UUID> received;
  private boolean channelReady;
  public FloodBroadcast () throws HandlerRegistrationException
  {
    super(PROTO_NAME, PROTO_ID);
    registerRequestHandler(BroadcastRequest.REQUEST_ID, this :: uponBroadcastRequest);
    subscribeNotification(NeighbourDown.NOTIFICATION_ID, this :: uponNeighbourDown);
    subscribeNotification(NeighbourUp.NOTIFICATION_ID, this :: uponNeighbourUp);
    subscribeNotification(ChannelCreated.NOTIFICATION_ID, this :: uponChannelCreated);
  }
  private void init (Host self)
  {
    myself = self;
    neighbours = new HashSet<Host>();
    received = new HashSet<UUID>();
    channelReady = false;
  }
  private void uponChannelCreated (ChannelCreated notification, short sourceProto)
  {
    channelReady = true;
  }
  private void uponBroadcastRequest (BroadcastRequest request, short sourceProto)
  {
    if (channelReady)
    {
      sendMsg(new FloodMessage(request.getMid(), request.getS(), request.getM()), myself);
    }
  }
  private void uponFloodMessage (FloodMessage msg, Host from, short sourceProto)
  {
    if (!received.contains(msg.getMid()))
    {
      received.add(msg.getMid());
      triggerNotification(new DeliverNotification(msg.getMid(), msg.getS(), msg.getM()));
      for (Host host : neighbours) {
                                     if (!host.equals(from))
                                     {
                                       sendMsg(new FloodMessage(msg.getMid(), msg.getS(), msg.getM()), host);
                                     }
                                   }
    }
  }
  private void uponNeighbourUp (NeighbourUp notification, short sourceProto)
  {
    for (Host h : notification.getUpNeighbours()) {
                                                    neighbours.add(h);
                                                  }
  }
  private void uponNeighbourDown (NeighbourDown notification, short sourceProto)
  {
    for (Host h : notification.getDownNeighbours()) {
                                                      neighbours.remove(h);
                                                    }
  }
}