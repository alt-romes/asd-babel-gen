
## Generating your babel code, or how to abuse syntatically valid pseudo code

The syntax and semantics are explained below and then an example tying it
together is given. This example is the pseudo code that matches the
`babel-example` babel project.

### Send and Receive

A protocol running on a process exchanges messages with the same protocol on the
same or on another process by using the `send` and `receive` primitives.

#### Send

The `send` primitive takes at least two required arguments, and then optional
data that should be sent in the message. The first argument is the type of the
message, the second argument is the destination.

## Timers

We can setup periodic and fire-once timers using the `setup` `periodic` `timer` keywords. 
They should be followed by a timer identifier, and in parenthesis, at least one
argument which is the time after which the timer will fire. Additional arguments
to the timer might be passed after the timeout value and can be used when
handling a timer fire.
```c
// Setup timer to fire in 5 seconds
setup timer SimpleTimer(5, arg1, arg2)

// Setup timer to fire every 60 seconds
setup periodic timer SimplePeriodicTimer(60, arg1)
```

To handle timer events we must register the handlers using the `upon` `timer`
keywords followed by the name of the timer. The arguments passed to the handler
are the additional arguments added on setup.
```c
upon timer SimpleTimer(arg1, arg2) do:
    // timer fired
    // code using arg1 and arg2

upon timer SimplePeriodicTimer(arg1) do:
    // timer fired
    // code using arg1 and arg2
```


### Example

A Flood Broadcast protocol:

protocols/FloodBroadcast
```c
Interface:
  Requests:
    broadcastRequest(mid : UUID, s : Host, m : byte[])
  Indications:
    deliverNotification(mid : UUID, s, m : byte[])

State:
  myself
  neighbours
  received

Upon Init(self) do:
  myself     <- self
  neighbours <- {}
  received   <- {}

Upon broadcastRequest(mid, s, m) do:
  If channelReady Then
    Call processFloodMessage(myself, mid, s, m)

Upon Receive(FloodMessage, from, mid, s, m) do:
  Call processFloodMessage(from, mid, s, m)

Procedure processFloodMessage(from, mid, s, m) do
  If mid ∉ received Then
    received <- received U {mid}
    Trigger deliverNotification(mid, s, m)
    Foreach host ∈ neighbours do:
      if host != from then
        Trigger Send(FloodMessage, host, mid, s, m)

Upon neighbourUp(upNeighbours) do:
  Foreach h ∈ upNeighbours do:
    neighbours <- neighbours U {h}

Upon neighbourDown(downNeighbours) do:
  Foreach h ∈ downNeighbours do:
    neighbours <- neighbours \ {h}
```

and another file in the same directory

protocols/overlay
```c
Interface:
    Indications:
        neighbourUp(upNeighbours)
        neighbourDown(downNeighbours)

State:

// Nothing else, we want to implement this one by hand besides teh indications
```

Upon running `cabal run asd protocols` we'd get the generated babel code in a `target/` directory:

```
$ tree target/
├── FloodBroadcast
│   ├── FloodBroadcast.java
│   ├── common
│   │   ├── notifications
│   │   │   └── DeliverNotification.java
│   │   └── requests
│   │       └── BroadcastRequest.java
│   └── messages
│       └── FloodMessage.java
├── Overlay
│   ├── Overlay.java
│   └── common
│       └── notifications
│           ├── NeighbourDown.java
│           └── NeighbourUp.java
```

with the following files:

target/FloodBroadcast/FloodBroadcast.java
```java
import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class FloodBroadcast extends GenericProtocol
{
    public static final String PROTO_NAME = "FloodBroadcast";
    public static final short PROTO_ID = 100;
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
    }
    private void init (Host self)
    {
        myself = self;
        neighbours = new HashSet<Host>();
        received = new HashSet<UUID>();
        channelReady = true;
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
}
```

target/FloodBroadcast/messages/FloodMessage.java
```java
import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class FloodMessage extends ProtoMessage
{
    public static final short MSG_ID = 103;
    private final UUID mid;
    private final Host s;
    private final byte[] m;
    public FloodMessage (UUID mid, Host s, byte[] m)
    {
        super(MSG_ID);
        this.mid = mid;
        this.s = s;
        this.m = m;
    }
    public UUID getMid ()
    {
        return mid;
    }
    public Host getS ()
    {
        return s;
    }
    public byte[] getM ()
    {
        return m;
    }
    public String toString ()
    {
        return "FloodMessage{}";
    }
    public static ISerializer<FloodMessage> serializer = new ISerializer<FloodMessage>()
    {
        public void serialize (FloodMessage msg, ByteBuf out) throws IOException
        {
            out.writeLong(msg.getMid().getMostSignificantBits());
            out.writeLong(msg.getMid().getLeastSignificantBits());
            Host.serializer.serialize(msg.getS(), out);
            out.writeInt(msg.getM().length);
            if (msg.getM().length > 0)
            {
                out.writeBytes(msg.getM());
            }
        }
        public FloodMessage deserialize (ByteBuf in) throws IOException
        {
            long firstLong;
            long secondLong;
            int size;
            firstLong = in.readLong();
            secondLong = in.readLong();
            UUID mid = new UUID(firstLong, secondLong);
            Host s = Host.serializer.deserialize(in);
            size = in.readInt();
            byte m = new byte[size];
            if (size > 0)
                in.readBytes(m);
            return new FloodMessage(mid, s, m);
        }
    };
}
```

target/FloodBroadcast/common/requests/BroadcastRequest.java
```java
import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class BroadcastRequest extends ProtoRequest
{
    public static final short REQUEST_ID = 101;
    private final UUID mid;
    private final Host s;
    private final byte[] m;
    public BroadcastRequest (UUID mid, Host s, byte[] m)
    {
        super(REQUEST_ID);
        this.mid = mid;
        this.s = s;
        this.m = m;
    }
    public UUID getMid ()
    {
        return mid;
    }
    public Host getS ()
    {
        return s;
    }
    public byte[] getM ()
    {
        return m;
    }
}
```

target/FloodBroadcast/common/notifications/DeliverNotification.java
```java
import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class DeliverNotification extends ProtoNotification
{
    public static final short NOTIFICATION_ID = 102;
    private final UUID mid;
    private final Host s;
    private final byte[] m;
    public DeliverNotification (UUID mid, Host s, byte[] m)
    {
        super(NOTIFICATION_ID);
        this.mid = mid;
        this.s = s;
        this.m = m;
    }
    public UUID getMid ()
    {
        return mid;
    }
    public Host getS ()
    {
        return s;
    }
    public byte[] getM ()
    {
        return m;
    }
}
```

and

target/Overlay/Overlay.java
```java
import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class Overlay extends GenericProtocol
{
    public static final String PROTO_NAME = "Overlay";
    public static final short PROTO_ID = 200;
    public Overlay () throws HandlerRegistrationException
    {
        super(PROTO_NAME, PROTO_ID);
    }
}
```

target/Overlay/common/notifications/NeighbourDown.java
```java
import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class NeighbourDown extends ProtoNotification
{
    public static final short NOTIFICATION_ID = 202;
    private final Set<Host> downNeighbours;
    public NeighbourDown (Set<Host> downNeighbours)
    {
        super(NOTIFICATION_ID);
        this.downNeighbours = downNeighbours;
    }
    public Set<Host> getDownNeighbours ()
    {
        return downNeighbours;
    }
}
```

target/Overlay/common/notifications/NeighbourUp.java
```java
import java.util.*;
import pt.unl.fct.di.novasys.babel.*;
public class NeighbourUp extends ProtoNotification
{
    public static final short NOTIFICATION_ID = 201;
    private final Set<Host> upNeighbours;
    public NeighbourUp (Set<Host> upNeighbours)
    {
        super(NOTIFICATION_ID);
        this.upNeighbours = upNeighbours;
    }
    public Set<Host> getUpNeighbours ()
    {
        return upNeighbours;
    }
}
```

And yes, we just wrote that pseudo code and it inferred all the remaining types and generated all that code in that hiearchy automatically.
