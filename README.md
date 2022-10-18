
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
