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
    subscribeNotification(ChannelCreated.NOTIFICATION_ID, this :: uponChannelCreated);
    subscribeNotification(NeighbourDown.NOTIFICATION_ID, this :: uponNeighbourDown);
    subscribeNotification(NeighbourUp.NOTIFICATION_ID, this :: uponNeighbourUp);
  }
  private void init (Host self)
  {
    myself = self;
    neighbours = new HashSet<Host>();
    received = new HashSet<UUID>();
    channelReady = false;
  }
  private void uponBroadcastRequest (BroadcastRequest request, short sourceProto)
  {
    if (channelReady)
    {
      processFloodMessage(myself, request.getMid(), request.getS(), request.getM());
    }
  }
  private void uponFloodMessage (FloodMessage msg, Host from, short sourceProto)
  {
    processFloodMessage(from, msg.getMid(), msg.getS(), msg.getM());
  }
  private void processFloodMessage (Host from, UUID mid, Host s, byte[] m)
  {
    if (!received.contains(mid))
    {
      received.add(mid);
      triggerNotification(new DeliverNotification(mid, s, m));
      for (Host host : neighbours) {
                                     if (!host.equals(from))
                                     {
                                       sendMsg(new FloodMessage(mid, s, m), host);
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
  private void uponChannelCreated (ChannelCreated notification, short sourceProto)
  {
    channelReady = true;
  }
}