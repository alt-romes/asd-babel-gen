import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class SimpleRegister extends GenericProtocol
{
    public static final String PROTO_NAME = "SimpleRegister";
    public static final short PROTO_ID = 100;
    private int value;
    private Set<Host> writeSet;
    private Set<Host> correct;
    public SimpleRegister () throws HandlerRegistrationException
    {
        super(PROTO_NAME, PROTO_ID);
        registerRequestHandler(RregWrite.REQUEST_ID, this :: uponRregWrite);
        registerRequestHandler(RregRead.REQUEST_ID, this :: uponRregRead);
        subscribeNotification(Crash.NOTIFICATION_ID, this :: uponCrash);
        subscribeNotification(BebDeliver.NOTIFICATION_ID, this :: uponBebDeliver);
    }
    private void init (Set<Host> pi)
    {
        value = 0;
        writeSet = new HashSet<Host>();
        correct = pi;
    }
    private void uponRregRead (RregRead request, short sourceProto)
    {
        triggerNotification(new RregReadReturn(value));
    }
    private void uponRregWrite (RregWrite request, short sourceProto)
    {
        sendRequest(new BebBroadcast(request.getV()), "TODO");
    }
    private void uponBebDeliver (BebDeliver notification, short sourceProto)
    {
        value = notification.getV();
        sendMsg(new Ack(0), notification.getS());
    }
    private void uponAck (Ack msg, Host s, short sourceProto)
    {
        writeSet.add(s);
        checkAcks();
    }
    private void checkAcks ()
    {
        if (writeSet.containsAll(correct))
        {
            writeSet = new HashSet<Host>();
            triggerNotification(new RregWriteReturn());
        }
    }
    private void uponCrash (Crash notification, short sourceProto)
    {
        correct.remove(notification.getP());
        checkAcks();
    }
}
